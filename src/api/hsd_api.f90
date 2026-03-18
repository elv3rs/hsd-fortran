!> Unified HSD API module
!> Merges functionality from accessors, mutators, and query modules.
module hsd_api
  use hsd_constants, only: dp, sp
  use hsd_utils, only: to_lower, string_buffer_t
  use hsd_error, only: HSD_STAT_OK, HSD_STAT_NOT_FOUND, HSD_STAT_TYPE_ERROR
  use hsd_types, only: hsd_node, hsd_table, hsd_value, new_table, new_value, &
    & VALUE_TYPE_NONE, VALUE_TYPE_STRING, VALUE_TYPE_INTEGER, &
    & VALUE_TYPE_REAL, VALUE_TYPE_LOGICAL, VALUE_TYPE_ARRAY, &
    & VALUE_TYPE_COMPLEX
  implicit none (type, external)
  private

  ! --- Declarations ---
!> HSD query and navigation operations
!>
!> This module provides functionality for navigating HSD tree structures,
!> introspecting node types, and performing tree operations like merging
!> and cloning.

  !> Pointer wrapper for returning references to existing child nodes
  type :: hsd_child_ptr
    class(hsd_node), pointer :: ptr => null()
  end type hsd_child_ptr

  !> Pointer wrapper for returning references to table children only.
  !>
  !> Extends hsd_child_ptr to keep a single shared pointer representation.
  type, extends(hsd_child_ptr) :: hsd_table_ptr
  end type hsd_table_ptr

  ! Public types
  public :: hsd_child_ptr, hsd_table_ptr

  ! Public procedures
  public :: hsd_get_child, hsd_get_table
  public :: hsd_has_child
  public :: hsd_remove_child
  public :: hsd_get_type, hsd_is_table, hsd_is_value, hsd_is_array
  public :: hsd_child_count, hsd_get_keys
  public :: hsd_get_attrib, hsd_has_attrib, hsd_set_attrib
  public :: hsd_rename_child
  public :: hsd_get_choice
  public :: hsd_get_children
  public :: hsd_get_child_tables
  public :: hsd_merge, hsd_clone
  public :: hsd_table_equal
  public :: hsd_set_processed
  public :: hsd_has_value_children
  public :: hsd_get_name

!> HSD data accessors (getters)
!>
!> This module provides interfaces and implementations for retrieving data
!> from HSD tables. It supports type-safe access to scalars, arrays, and
!> matrices.

  ! Public interfaces
  public :: hsd_get, hsd_get_or_set, hsd_get_matrix
  public :: hsd_get_inline_text

  !> Generic interface for getting values
  !>
  !> All procedures accept an optional `stat` parameter for error status.
  interface hsd_get
    module procedure :: hsd_get_string
    module procedure :: hsd_get_integer
    module procedure :: hsd_get_real_dp
    module procedure :: hsd_get_real_sp
    module procedure :: hsd_get_logical
    module procedure :: hsd_get_complex_dp
    module procedure :: hsd_get_integer_array
    module procedure :: hsd_get_real_dp_array
    module procedure :: hsd_get_real_sp_array
    module procedure :: hsd_get_logical_array
    module procedure :: hsd_get_string_array
    module procedure :: hsd_get_complex_dp_array
  end interface hsd_get

  !> Generic interface for getting values with default, writing default back to tree if absent
  !>
  !> If the key is not found, the default value is
  !> written back into the tree. This is critical for generating processed output
  !> (e.g., dftb_pin.hsd) that contains all defaults.
  !> stat is HSD_STAT_NOT_FOUND when default is used, HSD_STAT_OK when key existed.
  interface hsd_get_or_set
    module procedure :: hsd_get_or_set_string
    module procedure :: hsd_get_or_set_integer
    module procedure :: hsd_get_or_set_real_dp
    module procedure :: hsd_get_or_set_real_sp
    module procedure :: hsd_get_or_set_logical
    module procedure :: hsd_get_or_set_complex_dp
    module procedure :: hsd_get_or_set_integer_array
    module procedure :: hsd_get_or_set_real_dp_array
    module procedure :: hsd_get_or_set_real_sp_array
    module procedure :: hsd_get_or_set_logical_array
  end interface hsd_get_or_set

  !> Generic interface for getting 2D matrices
  interface hsd_get_matrix
    module procedure :: hsd_get_integer_matrix
    module procedure :: hsd_get_real_dp_matrix
    module procedure :: hsd_get_complex_dp_matrix
  end interface hsd_get_matrix

!> HSD data mutators (setters)
!>
!> This module provides interfaces and implementations for modifying HSD tables.
!> It supports type-safe setting of scalars and arrays, with automatic path
!> creation for nested structures.

  ! Public interface
  public :: hsd_set
  public :: hsd_clear_children

  !> Generic interface for setting values by path
  interface hsd_set
    module procedure :: hsd_set_string
    module procedure :: hsd_set_integer
    module procedure :: hsd_set_real_dp
    module procedure :: hsd_set_real_sp
    module procedure :: hsd_set_logical
    module procedure :: hsd_set_complex_dp
    module procedure :: hsd_set_integer_array
    module procedure :: hsd_set_real_dp_array
    module procedure :: hsd_set_real_sp_array
    module procedure :: hsd_set_logical_array
    module procedure :: hsd_set_complex_dp_array
    module procedure :: hsd_set_string_array
    module procedure :: hsd_set_integer_matrix
    module procedure :: hsd_set_real_dp_matrix
    module procedure :: hsd_set_complex_dp_matrix
  end interface hsd_set


  contains

  ! --- Implementations ---

  !> Normalize a path string by removing leading/trailing slashes and collapsing
  !> consecutive slashes. E.g. "/Geometry//Periodic/" → "Geometry/Periodic"
  pure function normalize_path(path) result(normalized)
    character(len=*), intent(in) :: path
    character(len=:), allocatable :: normalized

    integer :: i, n, out_len
    logical :: prev_was_slash

    n = len_trim(path)
    if (n == 0) then
      normalized = ""
      return
    end if

    block
      character(len=n) :: buf
      out_len = 0
      prev_was_slash = .true.  ! treat start as after slash to skip leading "/"

      do i = 1, n
        if (path(i:i) == '/') then
          if (.not. prev_was_slash) then
            out_len = out_len + 1
            buf(out_len:out_len) = '/'
          end if
          prev_was_slash = .true.
        else
          out_len = out_len + 1
          buf(out_len:out_len) = path(i:i)
          prev_was_slash = .false.
        end if
      end do

      ! Remove trailing slash
      if (out_len > 0) then
        if (buf(out_len:out_len) == '/') out_len = out_len - 1
      end if

      if (out_len > 0) then
        normalized = buf(1:out_len)
      else
        normalized = ""
      end if
    end block

  end function normalize_path

  !> Resolve a path into parent table + leaf name.
  subroutine resolve_path_parent_(table, path, parent_table, child_name, stat)
    type(hsd_table), intent(in), target :: table
    character(len=*), intent(in) :: path
    type(hsd_table), pointer, intent(out) :: parent_table
    character(len=:), allocatable, intent(out) :: child_name
    integer, intent(out) :: stat

    class(hsd_node), pointer :: parent_node
    character(len=:), allocatable :: norm, parent_path
    integer :: last_slash, local_stat

    nullify(parent_table)
    child_name = ""
    norm = normalize_path(path)
    if (len(norm) == 0) then
      stat = HSD_STAT_NOT_FOUND
      return
    end if

    last_slash = index(norm, "/", back=.true.)
    if (last_slash > 0) then
      parent_path = norm(1:last_slash-1)
      child_name = norm(last_slash+1:)
      call hsd_get_child(table, parent_path, parent_node, local_stat)
      if (local_stat /= HSD_STAT_OK .or. .not. associated(parent_node)) then
        stat = HSD_STAT_NOT_FOUND
        return
      end if
      select type (parent_node)
      type is (hsd_table)
        parent_table => parent_node
      class default
        stat = HSD_STAT_TYPE_ERROR
        return
      end select
    else
      parent_table => table
      child_name = norm
    end if

    stat = HSD_STAT_OK
  end subroutine resolve_path_parent_

  !> Check if a table has a child with given name
  function hsd_has_child(table, name) result(has)
    type(hsd_table), intent(in), target :: table
    character(len=*), intent(in) :: name
    logical :: has

    class(hsd_node), pointer :: child
    integer :: stat

    if (index(name, "/") > 0) then
      ! Path-based lookup - navigate through nested tables
      call hsd_get_child(table, name, child, stat)
      has = (stat == HSD_STAT_OK .and. associated(child))
    else
      has = table%has_child(name)
    end if

  end function hsd_has_child

  !> Remove a child from a table by name
  !>
  !> Supports path-based navigation with "/" separator for nested tables.
  !> The last component of the path is the child to remove.
  subroutine hsd_remove_child(table, path, stat)
    type(hsd_table), intent(inout), target :: table
    character(len=*), intent(in) :: path
    integer, intent(out), optional :: stat

    type(hsd_table), pointer :: parent_table
    character(len=:), allocatable :: child_name
    integer :: local_stat

    call resolve_path_parent_(table, path, parent_table, child_name, local_stat)
    if (local_stat /= HSD_STAT_OK) then
      if (present(stat)) stat = local_stat
      return
    end if

    call parent_table%remove_child_by_name(child_name, local_stat)
    if (present(stat)) stat = local_stat

  end subroutine hsd_remove_child

  !> Get the type of a value at the given path
  !>
  !> Returns one of: VALUE_TYPE_NONE (not found or is table), VALUE_TYPE_STRING,
  !> VALUE_TYPE_INTEGER, VALUE_TYPE_REAL, VALUE_TYPE_LOGICAL, VALUE_TYPE_ARRAY,
  !> VALUE_TYPE_COMPLEX
  function hsd_get_type(table, path) result(val_type)
    type(hsd_table), intent(in), target :: table
    character(len=*), intent(in) :: path
    integer :: val_type

    class(hsd_node), pointer :: child
    integer :: local_stat

    val_type = VALUE_TYPE_NONE
    call hsd_get_child(table, path, child, local_stat)

    if (local_stat /= 0 .or. .not. associated(child)) return

    select type (child)
    type is (hsd_value)
      val_type = child%value_type
    end select

  end function hsd_get_type

  !> Check if the node at path is a table (container)
  function hsd_is_table(table, path) result(is_tbl)
    type(hsd_table), intent(in), target :: table
    character(len=*), intent(in) :: path
    logical :: is_tbl

    class(hsd_node), pointer :: child
    integer :: local_stat

    is_tbl = .false.
    call hsd_get_child(table, path, child, local_stat)

    if (local_stat /= 0 .or. .not. associated(child)) return

    select type (child)
    type is (hsd_table)
      is_tbl = .true.
    end select

  end function hsd_is_table

  !> Check if the node at path is a value (leaf)
  function hsd_is_value(table, path) result(is_val)
    type(hsd_table), intent(in), target :: table
    character(len=*), intent(in) :: path
    logical :: is_val

    class(hsd_node), pointer :: child
    integer :: local_stat

    is_val = .false.
    call hsd_get_child(table, path, child, local_stat)

    if (local_stat /= 0 .or. .not. associated(child)) return

    select type (child)
    type is (hsd_value)
      is_val = .true.
    end select

  end function hsd_is_value

  !> Check if the node at path contains array data
  function hsd_is_array(table, path) result(is_arr)
    type(hsd_table), intent(in), target :: table
    character(len=*), intent(in) :: path
    logical :: is_arr

    is_arr = (hsd_get_type(table, path) == VALUE_TYPE_ARRAY)

  end function hsd_is_array

  !> Get the number of children in a table at the given path
  !>
  !> Returns 0 if path not found or is not a table
  function hsd_child_count(table, path) result(count)
    type(hsd_table), intent(in), target :: table
    character(len=*), intent(in) :: path
    integer :: count

    class(hsd_node), pointer :: child
    integer :: local_stat

    count = 0

    if (len_trim(path) == 0) then
      ! Empty path means the root table itself
      count = table%num_children
      return
    end if

    call hsd_get_child(table, path, child, local_stat)

    if (local_stat /= 0 .or. .not. associated(child)) return

    select type (child)
    type is (hsd_table)
      count = child%num_children
    end select

  end function hsd_child_count

  !> Get the keys (child names) from a table at the given path
  subroutine hsd_get_keys(table, path, keys, stat)
    type(hsd_table), intent(in), target :: table
    character(len=*), intent(in) :: path
    character(len=:), allocatable, intent(out) :: keys(:)
    integer, intent(out), optional :: stat

    class(hsd_node), pointer :: child
    integer :: local_stat

    if (present(stat)) stat = HSD_STAT_OK

    if (len_trim(path) == 0) then
      ! Empty path means the root table itself
      call table%get_keys(keys)
      return
    end if

    call hsd_get_child(table, path, child, local_stat)

    if (local_stat /= 0 .or. .not. associated(child)) then
      allocate(character(len=1) :: keys(0))
      if (present(stat)) stat = HSD_STAT_NOT_FOUND
      return
    end if

    select type (child)
    type is (hsd_table)
      call child%get_keys(keys)
    class default
      allocate(character(len=1) :: keys(0))
      if (present(stat)) stat = HSD_STAT_TYPE_ERROR
    end select

  end subroutine hsd_get_keys

  !> Get a child node by path (using / as separator)
  subroutine hsd_get_child(table, path, child, stat)
    type(hsd_table), intent(in), target :: table
    character(len=*), intent(in) :: path
    class(hsd_node), pointer, intent(out) :: child
    integer, intent(out), optional :: stat

    character(len=:), allocatable :: norm

    child => null()
    ! stat will be overriden by subroutine below.
    if (present(stat)) stat = HSD_STAT_OK

    norm = normalize_path(path)
    if (len(norm) == 0) then
      if (present(stat)) stat = HSD_STAT_NOT_FOUND
      return
    end if

    ! Delegate to recursive helper
    call get_first_child_table(table, norm, child, stat)

  end subroutine hsd_get_child

  !> Helper to navigate path and get child
  recursive subroutine get_first_child_table(table, path, child, stat)
    type(hsd_table), intent(in), target :: table
    character(len=*), intent(in) :: path
    class(hsd_node), pointer, intent(out) :: child
    integer, intent(out), optional :: stat

    character(len=:), allocatable :: remaining, segment
    class(hsd_node), pointer :: current
    integer :: sep_pos

    child => null()
    remaining = path

    ! Get first segment
    sep_pos = index(remaining, "/")
    if (sep_pos > 0) then
      segment = remaining(1:sep_pos-1)
      remaining = remaining(sep_pos+1:)
    else
      segment = remaining
      remaining = ""
    end if

    ! Find child with this name
    call table%get_child_by_name(segment, current)

    if (.not. associated(current)) then
      if (present(stat)) stat = HSD_STAT_NOT_FOUND
      return
    end if

    ! If no more path, return this node
    if (len_trim(remaining) == 0) then
      child => current
      if (present(stat)) stat = HSD_STAT_OK
      return
    end if

    ! Otherwise, recurse into child table
    select type (current)
    type is (hsd_table)
      call get_first_child_table(current, remaining, child, stat)
    class default
      if (present(stat)) stat = HSD_STAT_NOT_FOUND
    end select

  end subroutine get_first_child_table

  !> Get a table child by path
  subroutine hsd_get_table(table, path, child_table, stat)
    type(hsd_table), intent(in), target :: table
    character(len=*), intent(in) :: path
    type(hsd_table), pointer, intent(out) :: child_table
    integer, intent(out), optional :: stat

    class(hsd_node), pointer :: child
    integer :: local_stat

    child_table => null()
    call hsd_get_child(table, path, child, local_stat)

    if (associated(child)) then
      select type (child)
      type is (hsd_table)
        child_table => child
        child_table%processed = .true.
        if (present(stat)) stat = HSD_STAT_OK
      class default
        if (present(stat)) stat = HSD_STAT_NOT_FOUND
      end select
    else
      if (present(stat)) stat = local_stat
    end if

  end subroutine hsd_get_table

  !> Get an attribute from a node at the given path
  !>
  !> Example: For `LatticeConstant [Angstrom] = 5.4`, the attribute is "Angstrom"
  subroutine hsd_get_attrib(table, path, attrib, stat)
    type(hsd_table), intent(in), target :: table
    character(len=*), intent(in) :: path
    character(len=:), allocatable, intent(out) :: attrib
    integer, intent(out), optional :: stat

    class(hsd_node), pointer :: child
    integer :: local_stat

    call hsd_get_child(table, path, child, local_stat)

    if (local_stat /= 0 .or. .not. associated(child)) then
      if (present(stat)) stat = HSD_STAT_NOT_FOUND
      attrib = ""
      return
    end if

    ! Node exists - return OK regardless of whether attribute is set
    if (allocated(child%attrib)) then
      attrib = child%attrib
    else
      attrib = ""
    end if
    if (present(stat)) stat = HSD_STAT_OK

  end subroutine hsd_get_attrib

  !> Check if a node at the given path has an attribute
  function hsd_has_attrib(table, path) result(has)
    type(hsd_table), intent(in), target :: table
    character(len=*), intent(in) :: path
    logical :: has

    class(hsd_node), pointer :: child
    integer :: local_stat

    has = .false.
    call hsd_get_child(table, path, child, local_stat)

    if (local_stat /= 0 .or. .not. associated(child)) return

    has = allocated(child%attrib)

  end function hsd_has_attrib

  !> Set an attribute on a node at the given path
  !>
  !> Example: Setting "Angstrom" on `LatticeConstant` makes it render as
  !> `LatticeConstant [Angstrom] = 5.4`
  subroutine hsd_set_attrib(table, path, attrib, stat)
    type(hsd_table), intent(inout), target :: table
    character(len=*), intent(in) :: path
    character(len=*), intent(in) :: attrib
    integer, intent(out), optional :: stat

    class(hsd_node), pointer :: child
    integer :: local_stat

    call hsd_get_child(table, path, child, local_stat)

    if (local_stat /= 0 .or. .not. associated(child)) then
      if (present(stat)) stat = HSD_STAT_NOT_FOUND
      return
    end if

    child%attrib = attrib
    if (present(stat)) stat = HSD_STAT_OK

  end subroutine hsd_set_attrib

  !> Rename a child of a table
  !>
  !> Finds the child with `old_name` and changes its name to `new_name`.
  !> The child's position in the table is preserved. The name index is
  !> invalidated and rebuilt on next lookup.
  subroutine hsd_rename_child(table, old_name, new_name, stat)
    type(hsd_table), intent(inout), target :: table
    character(len=*), intent(in) :: old_name
    character(len=*), intent(in) :: new_name
    integer, intent(out), optional :: stat

    class(hsd_node), pointer :: child
    integer :: local_stat
    type(hsd_table), pointer :: parent_table
    character(len=:), allocatable :: child_old_name

    call resolve_path_parent_(table, old_name, parent_table, child_old_name, local_stat)
    if (local_stat /= HSD_STAT_OK) then
      if (present(stat)) stat = local_stat
      return
    end if

    ! Find the child by name
    call parent_table%get_child_by_name(child_old_name, child)

    if (.not. associated(child)) then
      if (present(stat)) stat = HSD_STAT_NOT_FOUND
      return
    end if

    ! Rename it
    child%name = new_name

    if (present(stat)) stat = HSD_STAT_OK

  end subroutine hsd_rename_child

  !> Get a polymorphic child for dispatch (choice pattern)
  !>
  !> This is a convenience for the common HSD pattern where a table has a single
  !> child whose name is the selector. For example:
  !>   Driver = ConjugateGradient { ... }
  !> Here the child's name ("ConjugateGradient") determines the variant and its
  !> contents are the variant's parameters.
  !>
  !> If `path` is empty, looks at the direct children of `table`.
  !> Returns the name and typed table pointer of the first table child found.
  subroutine hsd_get_choice(table, path, choice_name, choice_table, stat)
    type(hsd_table), intent(in), target :: table
    character(len=*), intent(in) :: path
    character(len=:), allocatable, intent(out) :: choice_name
    type(hsd_table), pointer, intent(out) :: choice_table
    integer, intent(out), optional :: stat

    class(hsd_node), pointer :: parent_node, child
    type(hsd_table), pointer :: parent_table
    integer :: ii, local_stat

    choice_name = ""
    choice_table => null()

    ! Navigate to parent
    if (len_trim(path) > 0) then
      call hsd_get_child(table, path, parent_node, local_stat)
      if (local_stat /= 0 .or. .not. associated(parent_node)) then
        if (present(stat)) stat = HSD_STAT_NOT_FOUND
        return
      end if
      select type (parent_node)
      type is (hsd_table)
        parent_table => parent_node
      class default
        if (present(stat)) stat = HSD_STAT_TYPE_ERROR
        return
      end select
    else
      parent_table => table
    end if

    ! Find first table child
    do ii = 1, parent_table%num_children
      call parent_table%get_child(ii, child)
      if (.not. associated(child)) cycle
      select type (child)
      type is (hsd_table)
        if (allocated(child%name)) then
          choice_name = to_lower(child%name)
        end if
        choice_table => child
        choice_table%processed = .true.
        if (present(stat)) stat = HSD_STAT_OK
        return
      end select
    end do

    ! No table child found - check if there's a value child (leaf dispatch)
    do ii = 1, parent_table%num_children
      call parent_table%get_child(ii, child)
      if (.not. associated(child)) cycle
      select type (child)
      type is (hsd_value)
        if (allocated(child%string_value)) then
          choice_name = to_lower(child%string_value)
        else if (allocated(child%name)) then
          choice_name = to_lower(child%name)
        end if
        if (present(stat)) stat = HSD_STAT_OK
        return
      end select
    end do

    if (present(stat)) stat = HSD_STAT_NOT_FOUND

  end subroutine hsd_get_choice

  !> Get all children of a table that match a given name (case-insensitive)
  !>
  !> Supports path-based lookup: "Geometry/Atom" will navigate to the
  !> "Geometry" table then collect all children named "Atom".
  !> Returns an array of hsd_child_ptr pointing to the matching children.
  !> If no children match, an empty (size-0) array is returned and stat is OK.
  subroutine hsd_get_children(table, path, children, stat)
    type(hsd_table), intent(in), target :: table
    character(len=*), intent(in) :: path
    type(hsd_child_ptr), allocatable, intent(out) :: children(:)
    integer, intent(out), optional :: stat

    call collect_named_children_(table, path, children, stat, tables_only=.false.)

  end subroutine hsd_get_children


  !> Get all table children of a table that match a given name (case-insensitive)
  !>
  !> Like hsd_get_children but returns only table-type children.
  !> Supports path-based lookup: "Geometry/Atom" will navigate to the
  !> "Geometry" table then collect all table children named "Atom".
  !> If no children match, an empty (size-0) array is returned.
  subroutine hsd_get_child_tables(table, path, children, stat)
    type(hsd_table), intent(in), target :: table
    character(len=*), intent(in) :: path
    type(hsd_table_ptr), allocatable, intent(out) :: children(:)
    integer, intent(out), optional :: stat

    character(len=:), allocatable :: child_name
    class(hsd_node), pointer :: child
    type(hsd_table), pointer :: parent_table
    integer :: local_stat, i, count
    character(len=:), allocatable :: lower_name

    call resolve_path_parent_(table, path, parent_table, child_name, local_stat)
    if (local_stat /= HSD_STAT_OK) then
      allocate(children(0))
      if (present(stat)) stat = local_stat
      return
    end if

    lower_name = to_lower(child_name)

    ! First pass: count table matches
    count = 0
    do i = 1, parent_table%num_children
      call parent_table%get_child(i, child)
      if (.not. associated(child)) cycle
      if (.not. allocated(child%name)) cycle
      if (to_lower(child%name) /= lower_name) cycle
      select type (child)
      type is (hsd_table)
        count = count + 1
      end select
    end do

    ! Allocate result
    allocate(children(count))

    ! Second pass: fill pointers (table children only)
    count = 0
    do i = 1, parent_table%num_children
      call parent_table%get_child(i, child)
      if (.not. associated(child)) cycle
      if (.not. allocated(child%name)) cycle
      if (to_lower(child%name) /= lower_name) cycle
      select type (child)
      type is (hsd_table)
        count = count + 1
        children(count)%ptr => child
        child%processed = .true.
      end select
    end do

    if (present(stat)) stat = HSD_STAT_OK

  end subroutine hsd_get_child_tables


  !> Merge two HSD tables (overlay pattern)
  !>
  !> Values from `overlay` are merged into `base`. If a key exists in both,
  !> the value from `overlay` takes precedence (unless it's a table,
  !> in which case they are merged recursively).
  recursive subroutine hsd_merge(base, overlay, stat)
    type(hsd_table), intent(inout) :: base
    type(hsd_table), intent(in) :: overlay
    integer, intent(out), optional :: stat

    class(hsd_node), pointer :: overlay_child, base_child
    type(hsd_table) :: cloned_table
    type(hsd_value) :: cloned_value
    integer :: i, local_stat

    if (present(stat)) stat = HSD_STAT_OK

    ! Iterate over overlay children
    do i = 1, overlay%num_children
      call overlay%get_child(i, overlay_child)
      if (.not. associated(overlay_child)) cycle
      if (.not. allocated(overlay_child%name)) cycle

      ! Check if base has this child
      call base%get_child_by_name(overlay_child%name, base_child)

      if (.not. associated(base_child)) then
        ! Child doesn't exist in base - clone and add it
        select type (overlay_child)
        type is (hsd_table)
          call clone_table(overlay_child, cloned_table)
          call base%add_child(cloned_table)
        type is (hsd_value)
          call clone_value(overlay_child, cloned_value)
          call base%add_child(cloned_value)
        end select
      else
        ! Child exists - handle based on type
        select type (overlay_child)
        type is (hsd_table)
          ! If both are tables, merge recursively
          select type (base_child)
          type is (hsd_table)
            call hsd_merge(base_child, overlay_child, local_stat)
            if (present(stat) .and. local_stat /= HSD_STAT_OK) stat = local_stat
          class default
            ! Base is not a table but overlay is - skip (could log warning)
          end select
        type is (hsd_value)
          ! Overlay value replaces base value
          select type (base_child)
          type is (hsd_value)
            call clone_value(overlay_child, cloned_value)
            ! Replace the value content
            base_child%value_type = cloned_value%value_type
            ! Copy attribute from overlay
            if (allocated(cloned_value%attrib)) then
              base_child%attrib = cloned_value%attrib
            else
              if (allocated(base_child%attrib)) deallocate(base_child%attrib)
            end if
            ! Clear stale fields before overwriting
            if (allocated(base_child%string_value)) &
                & deallocate(base_child%string_value)
            ! Copy new values from clone
            if (allocated(cloned_value%string_value)) &
                & base_child%string_value = cloned_value%string_value
          class default
            ! Type mismatch - skip
          end select
        end select
      end if
    end do

  end subroutine hsd_merge

  !> Clone a table (deep copy)
  recursive subroutine clone_table(source, dest)
    type(hsd_table), intent(in) :: source
    type(hsd_table), intent(out) :: dest

    class(hsd_node), pointer :: child
    type(hsd_table) :: cloned_subtable
    type(hsd_value) :: cloned_value
    integer :: i

    call new_table(dest, name=source%name)
    if (allocated(source%attrib)) dest%attrib = source%attrib
    dest%line = source%line

    do i = 1, source%num_children
      call source%get_child(i, child)
      if (.not. associated(child)) cycle

      select type (child)
      type is (hsd_table)
        call clone_table(child, cloned_subtable)
        call dest%add_child(cloned_subtable)
      type is (hsd_value)
        call clone_value(child, cloned_value)
        call dest%add_child(cloned_value)
      end select
    end do

  end subroutine clone_table

  !> Clone a value (deep copy)
  subroutine clone_value(source, dest)
    type(hsd_value), intent(in) :: source
    type(hsd_value), intent(out) :: dest

    call new_value(dest, name=source%name)
    if (allocated(source%attrib)) dest%attrib = source%attrib
    dest%line = source%line
    dest%value_type = source%value_type

    if (allocated(source%string_value)) dest%string_value = source%string_value

  end subroutine clone_value

  !> Deep clone an entire HSD table tree
  subroutine hsd_clone(source, dest, stat)
    type(hsd_table), intent(in) :: source
    type(hsd_table), intent(out) :: dest
    integer, intent(out), optional :: stat

    call clone_table(source, dest)
    if (present(stat)) stat = HSD_STAT_OK

  end subroutine hsd_clone

    !> Compare two HSD tables for structural and value equality
    !>
    !> Returns .true. if both tables have the same children (by name),
    !> the same structure (tables vs values), and the same values.
    !> Comparison is recursive for nested tables.
    !> Child order does not matter — children are matched by name.
    !> Name comparison is case-insensitive to match HSD conventions.
    recursive function hsd_table_equal(a, b) result(equal)
      type(hsd_table), intent(in), target :: a
      type(hsd_table), intent(in), target :: b
      logical :: equal

      class(hsd_node), pointer :: child_a, child_b
      integer :: i

      equal = .false.

      ! Quick check: same number of children
      if (a%num_children /= b%num_children) return

      ! Check that every child in a has a matching child in b
      do i = 1, a%num_children
        call a%get_child(i, child_a)
        if (.not. associated(child_a)) return
        if (.not. allocated(child_a%name)) return

        ! Look for matching child in b
        call b%get_child_by_name(child_a%name, child_b)
        if (.not. associated(child_b)) return

        ! Compare node types and values
        if (.not. nodes_equal(child_a, child_b)) return
      end do

      equal = .true.

    end function hsd_table_equal

    !> Compare two nodes for equality (recursive for tables)
    recursive function nodes_equal(a, b) result(equal)
      class(hsd_node), intent(in), target :: a
      class(hsd_node), intent(in), target :: b
      logical :: equal

      equal = .false.

      ! Both must be the same dynamic type
      select type (a)
      type is (hsd_table)
        select type (b)
        type is (hsd_table)
          equal = hsd_table_equal(a, b)
        end select

      type is (hsd_value)
        select type (b)
        type is (hsd_value)
          equal = values_equal(a, b)
        end select
      end select

    end function nodes_equal

    !> Compare two value nodes for equality
    function values_equal(a, b) result(equal)
      type(hsd_value), intent(in) :: a
      type(hsd_value), intent(in) :: b
      logical :: equal

      equal = .false.

      ! Must have the same value type
      if (a%value_type /= b%value_type) return

      ! Compare string_value
      if (allocated(a%string_value) .and. allocated(b%string_value)) then
        if (a%string_value /= b%string_value) return
      else if (allocated(a%string_value) .neqv. allocated(b%string_value)) then
        return
      end if

      equal = .true.

    end function values_equal


  !> Set the processed flag on a table and optionally all its descendants.
  !>
  !> When `recursive` is `.true.`, walks the entire subtree rooted at `table`
  !> and sets `%processed = .true.` on every node (tables and values).
  !> When `recursive` is `.false.` (the default), only the given table itself
  !> is marked.
  recursive subroutine hsd_set_processed(table, recursive)
    type(hsd_table), intent(inout), target :: table
    logical, intent(in), optional :: recursive

    logical :: do_recurse
    integer :: ii
    class(hsd_node), pointer :: child

    do_recurse = .false.
    if (present(recursive)) do_recurse = recursive

    table%processed = .true.

    if (.not. do_recurse) return

    do ii = 1, table%num_children
      call table%get_child(ii, child)
      if (.not. associated(child)) cycle

      select type (child)
      type is (hsd_table)
        call hsd_set_processed(child, recursive=.true.)
      type is (hsd_value)
        child%processed = .true.
      end select
    end do

  end subroutine hsd_set_processed


  !> Check whether a table has any value children (inline data).
  function hsd_has_value_children(table) result(has)
    type(hsd_table), intent(in), target :: table
    logical :: has

    integer :: ii
    class(hsd_node), pointer :: child

    has = .false.
    do ii = 1, table%num_children
      call table%get_child(ii, child)
      if (.not. associated(child)) cycle
      select type (child)
      type is (hsd_value)
        has = .true.
        return
      end select
    end do

  end function hsd_has_value_children


  !> Get the lowercased name of a node.
  !>
  !> If the node's name is unset or blank, returns the `default` string
  !> (which itself defaults to "" if not provided).
  subroutine hsd_get_name(node, name, default)
    class(hsd_node), intent(in) :: node
    character(len=:), allocatable, intent(out) :: name
    character(len=*), intent(in), optional :: default

    character(len=:), allocatable :: fallback

    if (present(default)) then
      fallback = default
    else
      fallback = ""
    end if

    if (allocated(node%name)) then
      if (len_trim(node%name) > 0) then
        name = to_lower(node%name)
      else
        name = fallback
      end if
    else
      name = fallback
    end if

  end subroutine hsd_get_name

  !> Get the inline text VALUE node from a table node.
  !>
  !> Looks for a child named "#text" and returns the hsd_value pointer.
  subroutine get_inline_value_(table, val_node, stat)
    type(hsd_table), intent(in), target :: table
    type(hsd_value), pointer, intent(out) :: val_node
    integer, intent(out) :: stat

    class(hsd_node), pointer :: child

    nullify(val_node)
    call table%get_child_by_name("#text", child)
    if (associated(child)) then
      select type (child)
      type is (hsd_value)
        val_node => child
        stat = HSD_STAT_OK
        return
      end select
    end if

    stat = HSD_STAT_NOT_FOUND

  end subroutine get_inline_value_

  !> Helper: Get the value node at the given path, handling inline text tables transparently
  subroutine get_value_node_(table, path, val_node, stat)
    type(hsd_table), intent(in), target :: table
    character(len=*), intent(in) :: path
    type(hsd_value), pointer, intent(out) :: val_node
    integer, intent(out) :: stat

    class(hsd_node), pointer :: child

    nullify(val_node)

    call hsd_get_child(table, path, child, stat)
    if (stat /= 0 .or. .not. associated(child)) then
      stat = HSD_STAT_NOT_FOUND
      return
    end if

    child%processed = .true.

    select type (child)
    type is (hsd_value)
      val_node => child
      stat = HSD_STAT_OK
    type is (hsd_table)
      ! Try to extract inline value
      call get_inline_value_(child, val_node, stat)
      if (stat == HSD_STAT_NOT_FOUND) stat = HSD_STAT_TYPE_ERROR
    class default
      stat = HSD_STAT_TYPE_ERROR
    end select
  end subroutine get_value_node_

  !> Get string value by path
  subroutine hsd_get_string(table, path, val, stat)
    type(hsd_table), intent(in), target :: table
    character(len=*), intent(in) :: path
    character(len=:), allocatable, intent(out) :: val
    integer, intent(out), optional :: stat

    type(hsd_value), pointer :: vnode
    integer :: local_stat

    call get_value_node_(table, path, vnode, local_stat)

    if (local_stat == HSD_STAT_OK) then
      call vnode%get_string(val, local_stat)
    else
      val = ""
    end if

    if (present(stat)) stat = local_stat

  end subroutine hsd_get_string

  !> Get integer value by path
  subroutine hsd_get_integer(table, path, val, stat)
    type(hsd_table), intent(in), target :: table
    character(len=*), intent(in) :: path
    integer, intent(out) :: val
    integer, intent(out), optional :: stat

    type(hsd_value), pointer :: vnode
    integer :: local_stat

    call get_value_node_(table, path, vnode, local_stat)

    if (local_stat == HSD_STAT_OK) then
      call vnode%get_integer(val, local_stat)
    else
      val = 0
    end if

    if (present(stat)) stat = local_stat
  end subroutine hsd_get_integer

  !> Get double precision real value by path
  subroutine hsd_get_real_dp(table, path, val, stat)
    type(hsd_table), intent(in), target :: table
    character(len=*), intent(in) :: path
    real(dp), intent(out) :: val
    integer, intent(out), optional :: stat

    type(hsd_value), pointer :: vnode
    integer :: local_stat

    call get_value_node_(table, path, vnode, local_stat)

    if (local_stat == HSD_STAT_OK) then
      call vnode%get_real(val, local_stat)
    else
      val = 0.0_dp
    end if

    if (present(stat)) stat = local_stat
  end subroutine hsd_get_real_dp

  !> Get single precision real value by path
  subroutine hsd_get_real_sp(table, path, val, stat)
    type(hsd_table), intent(in), target :: table
    character(len=*), intent(in) :: path
    real(sp), intent(out) :: val
    integer, intent(out), optional :: stat

    real(dp) :: val_dp
    integer :: local_stat

    call hsd_get_real_dp(table, path, val_dp, local_stat)
    val = real(val_dp, sp)
    if (present(stat)) stat = local_stat

  end subroutine hsd_get_real_sp

  !> Get logical value by path
  subroutine hsd_get_logical(table, path, val, stat)
    type(hsd_table), intent(in), target :: table
    character(len=*), intent(in) :: path
    logical, intent(out) :: val
    integer, intent(out), optional :: stat

    type(hsd_value), pointer :: vnode
    integer :: local_stat

    call get_value_node_(table, path, vnode, local_stat)

    if (local_stat == HSD_STAT_OK) then
      call vnode%get_logical(val, local_stat)
    else
      val = .false.
    end if

    if (present(stat)) stat = local_stat
  end subroutine hsd_get_logical

  !> Get complex value by path
  subroutine hsd_get_complex_dp(table, path, val, stat)
    type(hsd_table), intent(in), target :: table
    character(len=*), intent(in) :: path
    complex(dp), intent(out) :: val
    integer, intent(out), optional :: stat

    type(hsd_value), pointer :: vnode
    integer :: local_stat

    call get_value_node_(table, path, vnode, local_stat)

    if (local_stat == HSD_STAT_OK) then
      call vnode%get_complex(val, local_stat)
    else
      val = (0.0_dp, 0.0_dp)
    end if

    if (present(stat)) stat = local_stat
  end subroutine hsd_get_complex_dp

  !> Get integer array by path (supports space/comma/newline separated values)
  subroutine hsd_get_integer_array(table, path, val, stat)
    type(hsd_table), intent(in), target :: table
    character(len=*), intent(in) :: path
    integer, allocatable, intent(out) :: val(:)
    integer, intent(out), optional :: stat

    class(hsd_node), pointer :: child
    integer :: local_stat

    call hsd_get_child(table, path, child, local_stat)

    if (local_stat /= 0 .or. .not. associated(child)) then
      if (present(stat)) stat = HSD_STAT_NOT_FOUND
      allocate(val(0))
      return
    end if

    child%processed = .true.

    select type (child)
    type is (hsd_value)
      call child%get_int_array(val, local_stat)
      if (present(stat)) stat = local_stat
    type is (hsd_table)
      block
        type(hsd_value), pointer :: vnode
        call get_inline_value_(child, vnode, local_stat)
        if (local_stat == 0) then
          call vnode%get_int_array(val, local_stat)
          if (present(stat)) stat = local_stat
        else
          if (present(stat)) stat = HSD_STAT_TYPE_ERROR
          allocate(val(0))
        end if
      end block
    class default
      if (present(stat)) stat = HSD_STAT_TYPE_ERROR
      allocate(val(0))
    end select

  end subroutine hsd_get_integer_array

  !> Get double precision real array by path
  subroutine hsd_get_real_dp_array(table, path, val, stat)
    type(hsd_table), intent(in), target :: table
    character(len=*), intent(in) :: path
    real(dp), allocatable, intent(out) :: val(:)
    integer, intent(out), optional :: stat

    class(hsd_node), pointer :: child
    integer :: local_stat

    call hsd_get_child(table, path, child, local_stat)

    if (local_stat /= 0 .or. .not. associated(child)) then
      if (present(stat)) stat = HSD_STAT_NOT_FOUND
      allocate(val(0))
      return
    end if

    child%processed = .true.

    select type (child)
    type is (hsd_value)
      call child%get_real_array(val, local_stat)
      if (present(stat)) stat = local_stat
    type is (hsd_table)
      block
        type(hsd_value), pointer :: vnode
        call get_inline_value_(child, vnode, local_stat)
        if (local_stat == 0) then
          call vnode%get_real_array(val, local_stat)
          if (present(stat)) stat = local_stat
        else
          if (present(stat)) stat = HSD_STAT_TYPE_ERROR
          allocate(val(0))
        end if
      end block
    class default
      if (present(stat)) stat = HSD_STAT_TYPE_ERROR
      allocate(val(0))
    end select

  end subroutine hsd_get_real_dp_array

  !> Get single precision real array by path
  subroutine hsd_get_real_sp_array(table, path, val, stat)
    type(hsd_table), intent(in), target :: table
    character(len=*), intent(in) :: path
    real(sp), allocatable, intent(out) :: val(:)
    integer, intent(out), optional :: stat

    real(dp), allocatable :: val_dp(:)
    integer :: local_stat

    call hsd_get_real_dp_array(table, path, val_dp, local_stat)

    if (local_stat /= 0) then
      if (present(stat)) stat = local_stat
      allocate(val(0))
      return
    end if

    allocate(val(size(val_dp)))
    val = real(val_dp, sp)
    if (present(stat)) stat = HSD_STAT_OK

  end subroutine hsd_get_real_sp_array

  !> Get logical array by path
  subroutine hsd_get_logical_array(table, path, val, stat)
    type(hsd_table), intent(in), target :: table
    character(len=*), intent(in) :: path
    logical, allocatable, intent(out) :: val(:)
    integer, intent(out), optional :: stat

    class(hsd_node), pointer :: child
    integer :: local_stat

    call hsd_get_child(table, path, child, local_stat)

    if (local_stat /= 0 .or. .not. associated(child)) then
      if (present(stat)) stat = HSD_STAT_NOT_FOUND
      allocate(val(0))
      return
    end if

    child%processed = .true.

    select type (child)
    type is (hsd_value)
      call child%get_logical_array(val, local_stat)
      if (present(stat)) stat = local_stat
    type is (hsd_table)
      block
        type(hsd_value), pointer :: vnode
        call get_inline_value_(child, vnode, local_stat)
        if (local_stat == 0) then
          call vnode%get_logical_array(val, local_stat)
          if (present(stat)) stat = local_stat
        else
          if (present(stat)) stat = HSD_STAT_TYPE_ERROR
          allocate(val(0))
        end if
      end block
    class default
      if (present(stat)) stat = HSD_STAT_TYPE_ERROR
      allocate(val(0))
    end select

  end subroutine hsd_get_logical_array

  !> Get string array by path (preserves quoted strings)
  subroutine hsd_get_string_array(table, path, val, stat)
    type(hsd_table), intent(in), target :: table
    character(len=*), intent(in) :: path
    character(len=:), allocatable, intent(out) :: val(:)
    integer, intent(out), optional :: stat

    class(hsd_node), pointer :: child
    integer :: local_stat

    call hsd_get_child(table, path, child, local_stat)

    if (local_stat /= 0 .or. .not. associated(child)) then
      if (present(stat)) stat = HSD_STAT_NOT_FOUND
      allocate(character(len=1) :: val(0))
      return
    end if

    child%processed = .true.

    select type (child)
    type is (hsd_value)
      call child%get_string_array(val, local_stat)
      if (present(stat)) stat = local_stat
    type is (hsd_table)
      block
        type(hsd_value), pointer :: vnode
        call get_inline_value_(child, vnode, local_stat)
        if (local_stat == 0) then
          call vnode%get_string_array(val, local_stat)
          if (present(stat)) stat = local_stat
        else
          if (present(stat)) stat = HSD_STAT_TYPE_ERROR
          allocate(character(len=1) :: val(0))
        end if
      end block
    class default
      if (present(stat)) stat = HSD_STAT_TYPE_ERROR
      allocate(character(len=1) :: val(0))
    end select

  end subroutine hsd_get_string_array

  !> Get complex array by path
  subroutine hsd_get_complex_dp_array(table, path, val, stat)
    type(hsd_table), intent(in), target :: table
    character(len=*), intent(in) :: path
    complex(dp), allocatable, intent(out) :: val(:)
    integer, intent(out), optional :: stat

    class(hsd_node), pointer :: child
    integer :: local_stat

    call hsd_get_child(table, path, child, local_stat)

    if (local_stat /= 0 .or. .not. associated(child)) then
      if (present(stat)) stat = HSD_STAT_NOT_FOUND
      allocate(val(0))
      return
    end if

    child%processed = .true.

    select type (child)
    type is (hsd_value)
      call child%get_complex_array(val, local_stat)
      if (present(stat)) stat = local_stat
    type is (hsd_table)
      block
        type(hsd_value), pointer :: vnode
        call get_inline_value_(child, vnode, local_stat)
        if (local_stat == 0) then
          call vnode%get_complex_array(val, local_stat)
          if (present(stat)) stat = local_stat
        else
          if (present(stat)) stat = HSD_STAT_TYPE_ERROR
          allocate(val(0))
        end if
      end block
    class default
      if (present(stat)) stat = HSD_STAT_TYPE_ERROR
      allocate(val(0))
    end select

  end subroutine hsd_get_complex_dp_array

  !> Get 2D integer matrix by path (rows separated by newlines or semicolons)
  !> Handles both value nodes and table nodes (where content is in unnamed children)
  !>
  !> If `order` is present and set to "column-major", the returned matrix is
  !> transposed so that text rows map to Fortran columns (column-major layout).
  !> Default is text-layout (row-major).
  subroutine hsd_get_integer_matrix(table, path, val, nrows, ncols, stat, order)
    type(hsd_table), intent(in), target :: table
    character(len=*), intent(in) :: path
    integer, allocatable, intent(out) :: val(:,:)
    integer, intent(out) :: nrows, ncols
    integer, intent(out), optional :: stat
    character(len=*), intent(in), optional :: order

    class(hsd_node), pointer :: child
    integer :: local_stat

    call hsd_get_child(table, path, child, local_stat)

    if (local_stat /= 0 .or. .not. associated(child)) then
      if (present(stat)) stat = HSD_STAT_NOT_FOUND
      allocate(val(0,0))
      nrows = 0
      ncols = 0
      return
    end if

    child%processed = .true.

    select type (child)
    type is (hsd_value)
      call child%get_int_matrix(val, nrows, ncols, local_stat)
      if (present(stat)) stat = local_stat
    type is (hsd_table)
      ! Table nodes store matrix data as unnamed child values
      call get_int_matrix_from_table(child, val, nrows, ncols, local_stat)
      if (present(stat)) stat = local_stat
    class default
      if (present(stat)) stat = HSD_STAT_TYPE_ERROR
      allocate(val(0,0))
      nrows = 0
      ncols = 0
    end select

    ! Transpose if column-major order requested
    if (present(order)) then
      if (order == "column-major" .and. nrows > 0 .and. ncols > 0) then
        block
          integer, allocatable :: tmp(:,:)
          integer :: swap
          allocate(tmp(ncols, nrows))
          tmp = transpose(val)
          call move_alloc(tmp, val)
          swap = nrows
          nrows = ncols
          ncols = swap
        end block
      end if
    end if

  end subroutine hsd_get_integer_matrix

  !> Get 2D real matrix by path
  !> Handles both value nodes and table nodes (where content is in unnamed children)
  !>
  !> If `order` is present and set to "column-major", the returned matrix is
  !> transposed so that text rows map to Fortran columns (column-major layout).
  !> Default is text-layout (row-major).
  subroutine hsd_get_real_dp_matrix(table, path, val, nrows, ncols, stat, order)
    type(hsd_table), intent(in), target :: table
    character(len=*), intent(in) :: path
    real(dp), allocatable, intent(out) :: val(:,:)
    integer, intent(out) :: nrows, ncols
    integer, intent(out), optional :: stat
    character(len=*), intent(in), optional :: order

    class(hsd_node), pointer :: child
    integer :: local_stat

    call hsd_get_child(table, path, child, local_stat)

    if (local_stat /= 0 .or. .not. associated(child)) then
      if (present(stat)) stat = HSD_STAT_NOT_FOUND
      allocate(val(0,0))
      nrows = 0
      ncols = 0
      return
    end if

    child%processed = .true.

    select type (child)
    type is (hsd_value)
      call child%get_real_matrix(val, nrows, ncols, local_stat)
      if (present(stat)) stat = local_stat
    type is (hsd_table)
      ! Table nodes store matrix data as unnamed child values
      call get_real_matrix_from_table(child, val, nrows, ncols, local_stat)
      if (present(stat)) stat = local_stat
    class default
      if (present(stat)) stat = HSD_STAT_TYPE_ERROR
      allocate(val(0,0))
      nrows = 0
      ncols = 0
    end select

    ! Transpose if column-major order requested
    if (present(order)) then
      if (order == "column-major" .and. nrows > 0 .and. ncols > 0) then
        block
          real(dp), allocatable :: tmp(:,:)
          integer :: swap
          allocate(tmp(ncols, nrows))
          tmp = transpose(val)
          call move_alloc(tmp, val)
          swap = nrows
          nrows = ncols
          ncols = swap
        end block
      end if
    end if

  end subroutine hsd_get_real_dp_matrix

  !> Extract integer matrix from table with unnamed value children
  subroutine get_int_matrix_from_table(tbl, mat, nrows, ncols, stat)
    type(hsd_table), intent(in) :: tbl
    integer, allocatable, intent(out) :: mat(:,:)
    integer, intent(out) :: nrows, ncols, stat

    class(hsd_node), pointer :: child
    character(len=:), allocatable :: combined_text, str_val
    integer :: i, local_stat

    ! Combine all unnamed value children into single text
    combined_text = ""
    do i = 1, tbl%num_children
      call tbl%get_child(i, child)
      if (associated(child)) then
        select type (child)
        type is (hsd_value)
          ! Include unnamed, empty-named, or #text-named value nodes
          block
            logical :: is_text_child
            is_text_child = .not. allocated(child%name)
            if (.not. is_text_child) &
                & is_text_child = (len_trim(child%name) == 0 .or. child%name == "#text")
            if (is_text_child) then
              call child%get_string(str_val, local_stat)
              if (local_stat == 0 .and. len_trim(str_val) > 0) then
                if (len(combined_text) > 0) then
                  combined_text = combined_text // char(10) // str_val
                else
                  combined_text = str_val
                end if
              end if
            end if
          end block
        end select
      end if
    end do

    if (len_trim(combined_text) == 0) then
      allocate(mat(0,0))
      nrows = 0
      ncols = 0
      stat = HSD_STAT_OK
      return
    end if

    ! Parse the combined text as a matrix
    block
      type(hsd_value) :: temp_val
      call new_value(temp_val)
      call temp_val%set_raw(combined_text)
      call temp_val%get_int_matrix(mat, nrows, ncols, stat)
      call temp_val%destroy()
    end block

  end subroutine get_int_matrix_from_table

  !> Extract real matrix from table with unnamed value children
  subroutine get_real_matrix_from_table(tbl, mat, nrows, ncols, stat)
    type(hsd_table), intent(in) :: tbl
    real(dp), allocatable, intent(out) :: mat(:,:)
    integer, intent(out) :: nrows, ncols, stat

    class(hsd_node), pointer :: child
    character(len=:), allocatable :: combined_text, str_val
    integer :: i, local_stat

    ! Combine all unnamed value children into single text
    combined_text = ""
    do i = 1, tbl%num_children
      call tbl%get_child(i, child)
      if (associated(child)) then
        select type (child)
        type is (hsd_value)
          ! Include unnamed, empty-named, or #text-named value nodes
          block
            logical :: is_text_child
            is_text_child = .not. allocated(child%name)
            if (.not. is_text_child) &
                & is_text_child = (len_trim(child%name) == 0 .or. child%name == "#text")
            if (is_text_child) then
              call child%get_string(str_val, local_stat)
              if (local_stat == 0 .and. len_trim(str_val) > 0) then
                if (len(combined_text) > 0) then
                  combined_text = combined_text // char(10) // str_val
                else
                  combined_text = str_val
                end if
              end if
            end if
          end block
        end select
      end if
    end do

    if (len_trim(combined_text) == 0) then
      allocate(mat(0,0))
      nrows = 0
      ncols = 0
      stat = HSD_STAT_OK
      return
    end if

    ! Parse the combined text as a matrix
    block
      type(hsd_value) :: temp_val
      call new_value(temp_val)
      call temp_val%set_raw(combined_text)
      call temp_val%get_real_matrix(mat, nrows, ncols, stat)
      call temp_val%destroy()
    end block

  end subroutine get_real_matrix_from_table

  !> Get 2D complex matrix by path
  !> Handles both value nodes and table nodes (where content is in unnamed children)
  !>
  !> If `order` is present and set to "column-major", the returned matrix is
  !> transposed so that text rows map to Fortran columns (column-major layout).
  !> Default is text-layout (row-major).
  subroutine hsd_get_complex_dp_matrix(table, path, val, nrows, ncols, stat, order)
    type(hsd_table), intent(in), target :: table
    character(len=*), intent(in) :: path
    complex(dp), allocatable, intent(out) :: val(:,:)
    integer, intent(out) :: nrows, ncols
    integer, intent(out), optional :: stat
    character(len=*), intent(in), optional :: order

    class(hsd_node), pointer :: child
    integer :: local_stat

    call hsd_get_child(table, path, child, local_stat)

    if (local_stat /= 0 .or. .not. associated(child)) then
      if (present(stat)) stat = HSD_STAT_NOT_FOUND
      allocate(val(0,0))
      nrows = 0
      ncols = 0
      return
    end if

    child%processed = .true.

    select type (child)
    type is (hsd_value)
      call child%get_complex_matrix(val, nrows, ncols, local_stat)
      if (present(stat)) stat = local_stat
    type is (hsd_table)
      ! Table nodes store matrix data as unnamed child values
      call get_complex_matrix_from_table(child, val, nrows, ncols, local_stat)
      if (present(stat)) stat = local_stat
    class default
      if (present(stat)) stat = HSD_STAT_TYPE_ERROR
      allocate(val(0,0))
      nrows = 0
      ncols = 0
    end select

    ! Transpose if column-major order requested
    if (present(order)) then
      if (order == "column-major" .and. nrows > 0 .and. ncols > 0) then
        block
          complex(dp), allocatable :: tmp(:,:)
          integer :: swap
          allocate(tmp(ncols, nrows))
          tmp = transpose(val)
          call move_alloc(tmp, val)
          swap = nrows
          nrows = ncols
          ncols = swap
        end block
      end if
    end if

  end subroutine hsd_get_complex_dp_matrix

  !> Extract complex matrix from table with unnamed value children
  subroutine get_complex_matrix_from_table(tbl, mat, nrows, ncols, stat)
    type(hsd_table), intent(in) :: tbl
    complex(dp), allocatable, intent(out) :: mat(:,:)
    integer, intent(out) :: nrows, ncols, stat

    class(hsd_node), pointer :: child
    character(len=:), allocatable :: combined_text, str_val
    integer :: i, local_stat

    ! Combine all unnamed value children into single text
    combined_text = ""
    do i = 1, tbl%num_children
      call tbl%get_child(i, child)
      if (associated(child)) then
        select type (child)
        type is (hsd_value)
          ! Include unnamed, empty-named, or #text-named value nodes
          block
            logical :: is_text_child
            is_text_child = .not. allocated(child%name)
            if (.not. is_text_child) &
                & is_text_child = (len_trim(child%name) == 0 .or. child%name == "#text")
            if (is_text_child) then
              call child%get_string(str_val, local_stat)
              if (local_stat == 0 .and. len_trim(str_val) > 0) then
                if (len(combined_text) > 0) then
                  combined_text = combined_text // char(10) // str_val
                else
                  combined_text = str_val
                end if
              end if
            end if
          end block
        end select
      end if
    end do

    if (len_trim(combined_text) == 0) then
      allocate(mat(0,0))
      nrows = 0
      ncols = 0
      stat = HSD_STAT_OK
      return
    end if

    ! Parse the combined text as a matrix
    block
      type(hsd_value) :: temp_val
      call new_value(temp_val)
      call temp_val%set_raw(combined_text)
      call temp_val%get_complex_matrix(mat, nrows, ncols, stat)
      call temp_val%destroy()
    end block

  end subroutine get_complex_matrix_from_table

  ! ===== helpers =====

  !> Copy local status into optional output status.
  pure subroutine set_stat_from_local_(local_stat, stat)
    integer, intent(in) :: local_stat
    integer, intent(out), optional :: stat

    if (present(stat)) stat = local_stat
  end subroutine set_stat_from_local_

  !> Common helper for hsd_get_or_set status and optional child return.
  subroutine finalize_get_or_set_(table, path, local_stat, stat, child)
    type(hsd_table), intent(inout), target :: table
    character(len=*), intent(in) :: path
    integer, intent(in) :: local_stat
    integer, intent(out), optional :: stat
    type(hsd_table), pointer, intent(out), optional :: child

    if (local_stat /= HSD_STAT_OK) then
      call set_stat_from_local_(local_stat, stat)
    else
      call set_stat_from_local_(HSD_STAT_OK, stat)
    end if

    if (present(child)) then
      call hsd_get_table(table, path, child)
      if (.not. associated(child)) child => table
    end if

  end subroutine finalize_get_or_set_

  !> Get or create a value child; fails with TYPE_ERROR if final node is not a value.
  subroutine get_or_create_value_child_(table, path, vchild, stat)
    type(hsd_table), intent(inout), target :: table
    character(len=*), intent(in) :: path
    type(hsd_value), pointer, intent(out) :: vchild
    integer, intent(out) :: stat

    class(hsd_node), pointer :: child
    integer :: local_stat

    nullify(vchild)
    call get_or_create_child(table, path, child, local_stat)
    if (local_stat /= HSD_STAT_OK) then
      stat = local_stat
      return
    end if

    select type (child)
    type is (hsd_value)
      vchild => child
      stat = HSD_STAT_OK
    class default
      stat = HSD_STAT_TYPE_ERROR
    end select
  end subroutine get_or_create_value_child_

  !> Collect child references matching the final path segment.
  subroutine collect_named_children_(table, path, children, stat, tables_only)
    type(hsd_table), intent(in), target :: table
    character(len=*), intent(in) :: path
    type(hsd_child_ptr), allocatable, intent(out) :: children(:)
    integer, intent(out), optional :: stat
    logical, intent(in) :: tables_only

    character(len=:), allocatable :: child_name
    class(hsd_node), pointer :: child
    type(hsd_table), pointer :: parent_table
    integer :: local_stat, i, count
    character(len=:), allocatable :: lower_name

    call resolve_path_parent_(table, path, parent_table, child_name, local_stat)
    if (local_stat /= HSD_STAT_OK) then
      allocate(children(0))
      call set_stat_from_local_(local_stat, stat)
      return
    end if

    lower_name = to_lower(child_name)

    count = 0
    do i = 1, parent_table%num_children
      call parent_table%get_child(i, child)
      if (.not. associated(child)) cycle
      if (.not. allocated(child%name)) cycle
      if (to_lower(child%name) /= lower_name) cycle
      if (tables_only) then
        select type (child)
        type is (hsd_table)
          count = count + 1
        end select
      else
        count = count + 1
      end if
    end do

    allocate(children(count))
    count = 0
    do i = 1, parent_table%num_children
      call parent_table%get_child(i, child)
      if (.not. associated(child)) cycle
      if (.not. allocated(child%name)) cycle
      if (to_lower(child%name) /= lower_name) cycle
      if (tables_only) then
        select type (child)
        type is (hsd_table)
          count = count + 1
          children(count)%ptr => child
          child%processed = .true.
        end select
      else
        count = count + 1
        children(count)%ptr => child
        child%processed = .true.
      end if
    end do

    call set_stat_from_local_(HSD_STAT_OK, stat)
  end subroutine collect_named_children_

  !> Mark a named child node as processed
  subroutine mark_child_processed_(table, path)
    type(hsd_table), intent(inout), target :: table
    character(len=*), intent(in) :: path

    class(hsd_node), pointer :: child
    integer :: local_stat

    call hsd_get_child(table, path, child, local_stat)
    if (local_stat == 0 .and. associated(child)) then
      child%processed = .true.
    end if

  end subroutine mark_child_processed_

  ! ===== hsd_get_or_set implementations =====

  !> Get string value with default, writing default back to tree if absent
  subroutine hsd_get_or_set_string(table, path, val, default, stat, child)
    type(hsd_table), intent(inout), target :: table
    character(len=*), intent(in) :: path
    character(len=:), allocatable, intent(out) :: val
    character(len=*), intent(in) :: default
    integer, intent(out), optional :: stat
    type(hsd_table), pointer, intent(out), optional :: child

    integer :: local_stat

    call hsd_get_string(table, path, val, local_stat)

    if (local_stat /= 0) then
      val = default
      call hsd_set(table, path, default)
      call mark_child_processed_(table, path)
    end if

    call finalize_get_or_set_(table, path, local_stat, stat, child)

  end subroutine hsd_get_or_set_string

  !> Get integer value with default, writing default back to tree if absent
  subroutine hsd_get_or_set_integer(table, path, val, default, stat, child)
    type(hsd_table), intent(inout), target :: table
    character(len=*), intent(in) :: path
    integer, intent(out) :: val
    integer, intent(in) :: default
    integer, intent(out), optional :: stat
    type(hsd_table), pointer, intent(out), optional :: child

    integer :: local_stat

    call hsd_get_integer(table, path, val, local_stat)

    if (local_stat /= 0) then
      val = default
      call hsd_set(table, path, default)
      call mark_child_processed_(table, path)
    end if

    call finalize_get_or_set_(table, path, local_stat, stat, child)

  end subroutine hsd_get_or_set_integer

  !> Get double precision real value with default, writing default back to tree if absent
  subroutine hsd_get_or_set_real_dp(table, path, val, default, stat, child)
    type(hsd_table), intent(inout), target :: table
    character(len=*), intent(in) :: path
    real(dp), intent(out) :: val
    real(dp), intent(in) :: default
    integer, intent(out), optional :: stat
    type(hsd_table), pointer, intent(out), optional :: child

    integer :: local_stat

    call hsd_get_real_dp(table, path, val, local_stat)

    if (local_stat /= 0) then
      val = default
      call hsd_set(table, path, default)
      call mark_child_processed_(table, path)
    end if

    call finalize_get_or_set_(table, path, local_stat, stat, child)

  end subroutine hsd_get_or_set_real_dp

  !> Get single precision real value with default, writing default back to tree if absent
  subroutine hsd_get_or_set_real_sp(table, path, val, default, stat, child)
    type(hsd_table), intent(inout), target :: table
    character(len=*), intent(in) :: path
    real(sp), intent(out) :: val
    real(sp), intent(in) :: default
    integer, intent(out), optional :: stat
    type(hsd_table), pointer, intent(out), optional :: child

    integer :: local_stat

    call hsd_get_real_sp(table, path, val, local_stat)

    if (local_stat /= 0) then
      val = default
      call hsd_set(table, path, real(default, dp))
      call mark_child_processed_(table, path)
    end if

    call finalize_get_or_set_(table, path, local_stat, stat, child)

  end subroutine hsd_get_or_set_real_sp

  !> Get logical value with default, writing default back to tree if absent
  subroutine hsd_get_or_set_logical(table, path, val, default, stat, child)
    type(hsd_table), intent(inout), target :: table
    character(len=*), intent(in) :: path
    logical, intent(out) :: val
    logical, intent(in) :: default
    integer, intent(out), optional :: stat
    type(hsd_table), pointer, intent(out), optional :: child

    integer :: local_stat

    call hsd_get_logical(table, path, val, local_stat)

    if (local_stat /= 0) then
      val = default
      call hsd_set(table, path, default)
      call mark_child_processed_(table, path)
    end if

    call finalize_get_or_set_(table, path, local_stat, stat, child)

  end subroutine hsd_get_or_set_logical

  !> Get complex value with default, writing default back to tree if absent
  subroutine hsd_get_or_set_complex_dp(table, path, val, default, stat, child)
    type(hsd_table), intent(inout), target :: table
    character(len=*), intent(in) :: path
    complex(dp), intent(out) :: val
    complex(dp), intent(in) :: default
    integer, intent(out), optional :: stat
    type(hsd_table), pointer, intent(out), optional :: child

    integer :: local_stat

    call hsd_get_complex_dp(table, path, val, local_stat)

    if (local_stat /= 0) then
      val = default
      call hsd_set(table, path, default)
      call mark_child_processed_(table, path)
    end if

    call finalize_get_or_set_(table, path, local_stat, stat, child)

  end subroutine hsd_get_or_set_complex_dp

  !> Get integer array with default, writing default back to tree if absent
  subroutine hsd_get_or_set_integer_array(table, path, val, default, stat, child)
    type(hsd_table), intent(inout), target :: table
    character(len=*), intent(in) :: path
    integer, allocatable, intent(out) :: val(:)
    integer, intent(in) :: default(:)
    integer, intent(out), optional :: stat
    type(hsd_table), pointer, intent(out), optional :: child

    integer :: local_stat

    call hsd_get_integer_array(table, path, val, local_stat)

    if (local_stat /= 0) then
      val = default
      call hsd_set(table, path, default)
      call mark_child_processed_(table, path)
    end if

    call finalize_get_or_set_(table, path, local_stat, stat, child)

  end subroutine hsd_get_or_set_integer_array

  !> Get double precision real array with default, writing default back to tree if absent
  subroutine hsd_get_or_set_real_dp_array(table, path, val, default, stat, child)
    type(hsd_table), intent(inout), target :: table
    character(len=*), intent(in) :: path
    real(dp), allocatable, intent(out) :: val(:)
    real(dp), intent(in) :: default(:)
    integer, intent(out), optional :: stat
    type(hsd_table), pointer, intent(out), optional :: child

    integer :: local_stat

    call hsd_get_real_dp_array(table, path, val, local_stat)

    if (local_stat /= 0) then
      val = default
      call hsd_set(table, path, default)
      call mark_child_processed_(table, path)
    end if

    call finalize_get_or_set_(table, path, local_stat, stat, child)

  end subroutine hsd_get_or_set_real_dp_array

  !> Get single precision real array with default, writing default back to tree if absent
  subroutine hsd_get_or_set_real_sp_array(table, path, val, default, stat, child)
    type(hsd_table), intent(inout), target :: table
    character(len=*), intent(in) :: path
    real(sp), allocatable, intent(out) :: val(:)
    real(sp), intent(in) :: default(:)
    integer, intent(out), optional :: stat
    type(hsd_table), pointer, intent(out), optional :: child

    integer :: local_stat

    call hsd_get_real_sp_array(table, path, val, local_stat)

    if (local_stat /= 0) then
      val = default
      call hsd_set(table, path, real(default, dp))
      call mark_child_processed_(table, path)
    end if

    call finalize_get_or_set_(table, path, local_stat, stat, child)

  end subroutine hsd_get_or_set_real_sp_array

  !> Get logical array with default, writing default back to tree if absent
  subroutine hsd_get_or_set_logical_array(table, path, val, default, stat, child)
    type(hsd_table), intent(inout), target :: table
    character(len=*), intent(in) :: path
    logical, allocatable, intent(out) :: val(:)
    logical, intent(in) :: default(:)
    integer, intent(out), optional :: stat
    type(hsd_table), pointer, intent(out), optional :: child

    integer :: local_stat

    call hsd_get_logical_array(table, path, val, local_stat)

    if (local_stat /= 0) then
      val = default
      call hsd_set(table, path, default)
      call mark_child_processed_(table, path)
    end if

    call finalize_get_or_set_(table, path, local_stat, stat, child)

  end subroutine hsd_get_or_set_logical_array


  !> Get concatenated text content of all unnamed value children.
  !>
  !> Iterates children of `table`, collecting text from unnamed or "#text"
  !> `hsd_value` nodes. Multiple values are separated by spaces.
  subroutine hsd_get_inline_text(table, text, stat)
    type(hsd_table), intent(in), target :: table
    character(len=:), allocatable, intent(out) :: text
    integer, intent(out), optional :: stat

    integer :: ii, local_stat
    class(hsd_node), pointer :: child
    character(len=:), allocatable :: piece

    text = ""
    do ii = 1, table%num_children
      call table%get_child(ii, child)
      if (.not. associated(child)) cycle
      select type (v => child)
      type is (hsd_value)
        block
          logical :: is_anon_val
          is_anon_val = .true.
          if (allocated(v%name)) then
            if (len_trim(v%name) > 0 .and. v%name /= "#text") then
              is_anon_val = .false.
            end if
          end if
        if (is_anon_val) then
          call v%get_string(piece, local_stat)
          if (local_stat == HSD_STAT_OK .and. allocated(piece)) then
            if (len(text) > 0) then
              text = text // " " // piece
            else
              text = piece
            end if
          end if
        end if
        end block
      end select
    end do

    if (present(stat)) then
      if (len(text) > 0) then
        stat = HSD_STAT_OK
      else
        stat = HSD_STAT_NOT_FOUND
      end if
    end if

  end subroutine hsd_get_inline_text


  !> Set string value by path
  subroutine hsd_set_string(table, path, val, stat)
    type(hsd_table), intent(inout) :: table
    character(len=*), intent(in) :: path
    character(len=*), intent(in) :: val
    integer, intent(out), optional :: stat

    type(hsd_value), pointer :: vchild
    integer :: local_stat

    call get_or_create_value_child_(table, path, vchild, local_stat)

    if (local_stat /= HSD_STAT_OK) then
      call set_stat_from_local_(local_stat, stat)
      return
    end if

    call vchild%set_string(val)
    call set_stat_from_local_(HSD_STAT_OK, stat)

  end subroutine hsd_set_string

  !> Set integer value by path
  subroutine hsd_set_integer(table, path, val, stat)
    type(hsd_table), intent(inout) :: table
    character(len=*), intent(in) :: path
    integer, intent(in) :: val
    integer, intent(out), optional :: stat

    type(hsd_value), pointer :: vchild
    integer :: local_stat

    call get_or_create_value_child_(table, path, vchild, local_stat)

    if (local_stat /= HSD_STAT_OK) then
      call set_stat_from_local_(local_stat, stat)
      return
    end if

    call vchild%set_integer(val)
    call set_stat_from_local_(HSD_STAT_OK, stat)

  end subroutine hsd_set_integer

  !> Set double precision real value by path
  subroutine hsd_set_real_dp(table, path, val, stat)
    type(hsd_table), intent(inout) :: table
    character(len=*), intent(in) :: path
    real(dp), intent(in) :: val
    integer, intent(out), optional :: stat

    type(hsd_value), pointer :: vchild
    integer :: local_stat

    call get_or_create_value_child_(table, path, vchild, local_stat)

    if (local_stat /= HSD_STAT_OK) then
      call set_stat_from_local_(local_stat, stat)
      return
    end if

    call vchild%set_real(val)
    call set_stat_from_local_(HSD_STAT_OK, stat)

  end subroutine hsd_set_real_dp

  !> Set single precision real value by path
  subroutine hsd_set_real_sp(table, path, val, stat)
    type(hsd_table), intent(inout) :: table
    character(len=*), intent(in) :: path
    real(sp), intent(in) :: val
    integer, intent(out), optional :: stat

    call hsd_set_real_dp(table, path, real(val, dp), stat)

  end subroutine hsd_set_real_sp

  !> Set logical value by path
  subroutine hsd_set_logical(table, path, val, stat)
    type(hsd_table), intent(inout) :: table
    character(len=*), intent(in) :: path
    logical, intent(in) :: val
    integer, intent(out), optional :: stat

    type(hsd_value), pointer :: vchild
    integer :: local_stat

    call get_or_create_value_child_(table, path, vchild, local_stat)

    if (local_stat /= HSD_STAT_OK) then
      call set_stat_from_local_(local_stat, stat)
      return
    end if

    call vchild%set_logical(val)
    call set_stat_from_local_(HSD_STAT_OK, stat)

  end subroutine hsd_set_logical

  !> Set complex value by path
  subroutine hsd_set_complex_dp(table, path, val, stat)
    type(hsd_table), intent(inout) :: table
    character(len=*), intent(in) :: path
    complex(dp), intent(in) :: val
    integer, intent(out), optional :: stat

    type(hsd_value), pointer :: vchild
    integer :: local_stat

    call get_or_create_value_child_(table, path, vchild, local_stat)

    if (local_stat /= HSD_STAT_OK) then
      call set_stat_from_local_(local_stat, stat)
      return
    end if

    call vchild%set_complex(val)
    call set_stat_from_local_(HSD_STAT_OK, stat)

  end subroutine hsd_set_complex_dp

  !> Set integer array by path
  subroutine hsd_set_integer_array(table, path, val, stat)
    type(hsd_table), intent(inout) :: table
    character(len=*), intent(in) :: path
    integer, intent(in) :: val(:)
    integer, intent(out), optional :: stat

    class(hsd_node), pointer :: child
    integer :: local_stat, i
    character(len=32) :: buffer
    type(string_buffer_t) :: buf

    call get_or_create_child(table, path, child, local_stat)

    if (local_stat /= 0) then
      if (present(stat)) stat = local_stat
      return
    end if

    select type (child)
    type is (hsd_value)
      call buf%init(size(val) * 12)
      do i = 1, size(val)
        write(buffer, '(I0)') val(i)
        if (i > 1) call buf%append_char(' ')
        call buf%append_str(trim(adjustl(buffer)))
      end do
      call child%set_raw(buf%get_string())
    class default
      if (present(stat)) stat = HSD_STAT_TYPE_ERROR
      return
    end select

    if (present(stat)) stat = HSD_STAT_OK

  end subroutine hsd_set_integer_array

  !> Set double precision real array by path
  subroutine hsd_set_real_dp_array(table, path, val, stat)
    type(hsd_table), intent(inout) :: table
    character(len=*), intent(in) :: path
    real(dp), intent(in) :: val(:)
    integer, intent(out), optional :: stat

    class(hsd_node), pointer :: child
    integer :: local_stat, i
    character(len=32) :: buffer
    type(string_buffer_t) :: buf

    call get_or_create_child(table, path, child, local_stat)

    if (local_stat /= 0) then
      if (present(stat)) stat = local_stat
      return
    end if

    select type (child)
    type is (hsd_value)
      call buf%init(size(val) * 16)
      do i = 1, size(val)
        write(buffer, '(G0)') val(i)
        if (i > 1) call buf%append_char(' ')
        call buf%append_str(trim(adjustl(buffer)))
      end do
      call child%set_raw(buf%get_string())
    class default
      if (present(stat)) stat = HSD_STAT_TYPE_ERROR
      return
    end select

    if (present(stat)) stat = HSD_STAT_OK

  end subroutine hsd_set_real_dp_array

  !> Set single precision real array by path
  subroutine hsd_set_real_sp_array(table, path, val, stat)
    type(hsd_table), intent(inout) :: table
    character(len=*), intent(in) :: path
    real(sp), intent(in) :: val(:)
    integer, intent(out), optional :: stat

    real(dp), allocatable :: val_dp(:)

    allocate(val_dp(size(val)))
    val_dp = real(val, dp)
    call hsd_set_real_dp_array(table, path, val_dp, stat)

  end subroutine hsd_set_real_sp_array

  !> Set logical array by path
  subroutine hsd_set_logical_array(table, path, val, stat)
    type(hsd_table), intent(inout) :: table
    character(len=*), intent(in) :: path
    logical, intent(in) :: val(:)
    integer, intent(out), optional :: stat

    class(hsd_node), pointer :: child
    integer :: local_stat, i
    type(string_buffer_t) :: buf

    call get_or_create_child(table, path, child, local_stat)

    if (local_stat /= 0) then
      if (present(stat)) stat = local_stat
      return
    end if

    select type (child)
    type is (hsd_value)
      call buf%init(size(val) * 4)
      do i = 1, size(val)
        if (i > 1) call buf%append_char(' ')
        if (val(i)) then
          call buf%append_str("Yes")
        else
          call buf%append_str("No")
        end if
      end do
      call child%set_raw(buf%get_string())
    class default
      if (present(stat)) stat = HSD_STAT_TYPE_ERROR
      return
    end select

    if (present(stat)) stat = HSD_STAT_OK

  end subroutine hsd_set_logical_array

  !> Set complex array by path
  subroutine hsd_set_complex_dp_array(table, path, val, stat)
    type(hsd_table), intent(inout) :: table
    character(len=*), intent(in) :: path
    complex(dp), intent(in) :: val(:)
    integer, intent(out), optional :: stat

    class(hsd_node), pointer :: child
    integer :: local_stat, i
    character(len=64) :: buffer
    type(string_buffer_t) :: buf

    call get_or_create_child(table, path, child, local_stat)

    if (local_stat /= 0) then
      if (present(stat)) stat = local_stat
      return
    end if

    select type (child)
    type is (hsd_value)
      call buf%init(size(val) * 32)
      do i = 1, size(val)
        if (i > 1) call buf%append_char(' ')
        if (aimag(val(i)) >= 0.0_dp) then
          write(buffer, '(G0,"+",G0,"i")') real(val(i)), aimag(val(i))
        else
          write(buffer, '(G0,G0,"i")') real(val(i)), aimag(val(i))
        end if
        call buf%append_str(trim(adjustl(buffer)))
      end do
      call child%set_raw(buf%get_string())
    class default
      if (present(stat)) stat = HSD_STAT_TYPE_ERROR
      return
    end select

    if (present(stat)) stat = HSD_STAT_OK

  end subroutine hsd_set_complex_dp_array

  !> Set string array by path
  subroutine hsd_set_string_array(table, path, val, stat)
    type(hsd_table), intent(inout) :: table
    character(len=*), intent(in) :: path
    character(len=*), intent(in) :: val(:)
    integer, intent(out), optional :: stat

    class(hsd_node), pointer :: child
    integer :: local_stat, i
    type(string_buffer_t) :: buf

    call get_or_create_child(table, path, child, local_stat)

    if (local_stat /= 0) then
      if (present(stat)) stat = local_stat
      return
    end if

    select type (child)
    type is (hsd_value)
      call buf%init(size(val) * 32)
      do i = 1, size(val)
        if (i > 1) call buf%append_char(' ')
        ! Quote strings containing spaces
        if (index(val(i), ' ') > 0) then
          call buf%append_char('"')
          call buf%append_str(trim(val(i)))
          call buf%append_char('"')
        else
          call buf%append_str(trim(val(i)))
        end if
      end do
      call child%set_raw(buf%get_string())
    class default
      if (present(stat)) stat = HSD_STAT_TYPE_ERROR
      return
    end select

    if (present(stat)) stat = HSD_STAT_OK

  end subroutine hsd_set_string_array

  !> Set integer matrix by path
  subroutine hsd_set_integer_matrix(table, path, val, stat)
    type(hsd_table), intent(inout) :: table
    character(len=*), intent(in) :: path
    integer, intent(in) :: val(:,:)
    integer, intent(out), optional :: stat

    class(hsd_node), pointer :: child
    integer :: local_stat, ir, ic
    character(len=32) :: buffer
    type(string_buffer_t) :: buf

    call get_or_create_child(table, path, child, local_stat)

    if (local_stat /= 0) then
      if (present(stat)) stat = local_stat
      return
    end if

    select type (child)
    type is (hsd_value)
      call buf%init(size(val) * 12)
      do ir = 1, size(val, 1)
        if (ir > 1) call buf%append_str(new_line('a'))
        do ic = 1, size(val, 2)
          write(buffer, '(I0)') val(ir, ic)
          if (ic > 1) call buf%append_char(' ')
          call buf%append_str(trim(adjustl(buffer)))
        end do
      end do
      call child%set_raw(buf%get_string())
    class default
      if (present(stat)) stat = HSD_STAT_TYPE_ERROR
      return
    end select

    if (present(stat)) stat = HSD_STAT_OK

  end subroutine hsd_set_integer_matrix

  !> Set double precision real matrix by path
  subroutine hsd_set_real_dp_matrix(table, path, val, stat)
    type(hsd_table), intent(inout) :: table
    character(len=*), intent(in) :: path
    real(dp), intent(in) :: val(:,:)
    integer, intent(out), optional :: stat

    class(hsd_node), pointer :: child
    integer :: local_stat, ir, ic
    character(len=32) :: buffer
    type(string_buffer_t) :: buf

    call get_or_create_child(table, path, child, local_stat)

    if (local_stat /= 0) then
      if (present(stat)) stat = local_stat
      return
    end if

    select type (child)
    type is (hsd_value)
      call buf%init(size(val) * 16)
      do ir = 1, size(val, 1)
        if (ir > 1) call buf%append_str(new_line('a'))
        do ic = 1, size(val, 2)
          write(buffer, '(G0)') val(ir, ic)
          if (ic > 1) call buf%append_char(' ')
          call buf%append_str(trim(adjustl(buffer)))
        end do
      end do
      call child%set_raw(buf%get_string())
    class default
      if (present(stat)) stat = HSD_STAT_TYPE_ERROR
      return
    end select

    if (present(stat)) stat = HSD_STAT_OK

  end subroutine hsd_set_real_dp_matrix

  !> Set complex double precision matrix by path
  subroutine hsd_set_complex_dp_matrix(table, path, val, stat)
    type(hsd_table), intent(inout) :: table
    character(len=*), intent(in) :: path
    complex(dp), intent(in) :: val(:,:)
    integer, intent(out), optional :: stat

    class(hsd_node), pointer :: child
    integer :: local_stat, ir, ic
    character(len=64) :: buffer
    type(string_buffer_t) :: buf

    call get_or_create_child(table, path, child, local_stat)

    if (local_stat /= 0) then
      if (present(stat)) stat = local_stat
      return
    end if

    select type (child)
    type is (hsd_value)
      call buf%init(size(val) * 32)
      do ir = 1, size(val, 1)
        if (ir > 1) call buf%append_str(new_line('a'))
        do ic = 1, size(val, 2)
          if (ic > 1) call buf%append_char(' ')
          if (aimag(val(ir, ic)) >= 0.0_dp) then
            write(buffer, '(G0,"+",G0,"i")') real(val(ir, ic)), aimag(val(ir, ic))
          else
            write(buffer, '(G0,G0,"i")') real(val(ir, ic)), aimag(val(ir, ic))
          end if
          call buf%append_str(trim(adjustl(buffer)))
        end do
      end do
      call child%set_raw(buf%get_string())
    class default
      if (present(stat)) stat = HSD_STAT_TYPE_ERROR
      return
    end select

    if (present(stat)) stat = HSD_STAT_OK

  end subroutine hsd_set_complex_dp_matrix

  !> Get or create a child node by path, creating intermediate tables as needed
  subroutine get_or_create_child(table, path, child, stat)
    type(hsd_table), intent(inout), target :: table
    character(len=*), intent(in) :: path
    class(hsd_node), pointer, intent(out) :: child
    integer, intent(out), optional :: stat

    character(len=:), allocatable :: remaining, segment
    class(hsd_node), pointer :: current
    type(hsd_table), pointer :: current_table
    type(hsd_table) :: new_tbl
    type(hsd_value) :: new_val
    integer :: sep_pos, i

    child => null()
    remaining = path
    current_table => table

    do while (len_trim(remaining) > 0)
      ! Get next segment
      sep_pos = index(remaining, "/")
      if (sep_pos > 0) then
        segment = remaining(1:sep_pos-1)
        remaining = remaining(sep_pos+1:)
      else
        segment = remaining
        remaining = ""
      end if

      ! Look for existing child
      call current_table%get_child_by_name(segment, current)

      if (.not. associated(current)) then
        ! Need to create node
        if (len_trim(remaining) > 0) then
          ! More path segments: create table
          call new_table(new_tbl, name=to_lower(segment))
          call current_table%add_child(new_tbl)
          ! Get the newly added child
          do i = current_table%num_children, 1, -1
            call current_table%get_child(i, current)
            if (associated(current)) then
              if (allocated(current%name)) then
                if (to_lower(current%name) == to_lower(segment)) exit
              end if
            end if
          end do
        else
          ! Final segment: create value node
          call new_value(new_val, name=to_lower(segment))
          call current_table%add_child(new_val)
          ! Get the newly added child
          do i = current_table%num_children, 1, -1
            call current_table%get_child(i, current)
            if (associated(current)) then
              if (allocated(current%name)) then
                if (to_lower(current%name) == to_lower(segment)) exit
              end if
            end if
          end do
          child => current
          if (present(stat)) stat = HSD_STAT_OK
          return
        end if
      end if

      ! Navigate deeper if more path remains
      if (len_trim(remaining) > 0) then
        select type (current)
        type is (hsd_table)
          current_table => current
        class default
          ! Path segment is not a table, cannot navigate
          if (present(stat)) stat = HSD_STAT_NOT_FOUND
          return
        end select
      else
        child => current
        if (present(stat)) stat = HSD_STAT_OK
        return
      end if
    end do

    if (present(stat)) stat = HSD_STAT_NOT_FOUND

  end subroutine get_or_create_child


  !> Remove all children from a table node.
  !>
  !> After this call, the table has zero children. The children array and hash
  !> index are fully deallocated so subsequent add_child calls re-initialize
  !> correctly.
  subroutine hsd_clear_children(table)
    type(hsd_table), intent(inout) :: table

    integer :: ii
    class(hsd_node), pointer :: child

    ! Destroy each child node
    do ii = 1, table%num_children
      call table%get_child(ii, child)
      if (associated(child)) then
        call child%destroy()
        deallocate(table%children(ii)%node)
      end if
    end do

    table%num_children = 0
    if (allocated(table%children)) deallocate(table%children)

  end subroutine hsd_clear_children


end module hsd_api
