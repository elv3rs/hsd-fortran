!> HSD query and navigation operations
!>
!> This module provides functionality for navigating HSD tree structures,
!> introspecting node types, and performing tree operations like merging
!> and cloning.
module hsd_query
  use hsd_utils, only: to_lower
  use hsd_error, only: HSD_STAT_OK, HSD_STAT_NOT_FOUND, HSD_STAT_TYPE_ERROR
  use hsd_types, only: hsd_node, hsd_table, hsd_value, new_table, new_value, &
    VALUE_TYPE_NONE, VALUE_TYPE_STRING, VALUE_TYPE_INTEGER, VALUE_TYPE_REAL, &
    VALUE_TYPE_LOGICAL, VALUE_TYPE_ARRAY, VALUE_TYPE_COMPLEX
  implicit none (type, external)
  private

  !> Pointer wrapper for returning references to existing child nodes
  type :: hsd_child_ptr
    class(hsd_node), pointer :: ptr => null()
  end type hsd_child_ptr

  ! Public types
  public :: hsd_child_ptr

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
  public :: hsd_merge, hsd_clone
  public :: hsd_table_equal

contains

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

  !> Check if a table has a child with given name
  function hsd_has_child(table, name, case_insensitive) result(has)
    type(hsd_table), intent(in), target :: table
    character(len=*), intent(in) :: name
    logical, intent(in), optional :: case_insensitive
    logical :: has

    class(hsd_node), pointer :: child
    integer :: stat

    if (index(name, "/") > 0) then
      ! Path-based lookup - navigate through nested tables
      call hsd_get_child(table, name, child, stat)
      has = (stat == HSD_STAT_OK .and. associated(child))
    else
      has = table%has_child(name, case_insensitive)
    end if

  end function hsd_has_child

  !> Remove a child from a table by name
  !>
  !> Supports path-based navigation with "/" separator for nested tables.
  !> The last component of the path is the child to remove.
  subroutine hsd_remove_child(table, path, stat, case_insensitive)
    type(hsd_table), intent(inout) :: table
    character(len=*), intent(in) :: path
    integer, intent(out), optional :: stat
    logical, intent(in), optional :: case_insensitive

    class(hsd_node), pointer :: parent_node
    type(hsd_table), pointer :: parent_table
    character(len=:), allocatable :: child_name, parent_path, norm
    integer :: last_slash, local_stat

    norm = normalize_path(path)
    if (len(norm) == 0) then
      if (present(stat)) stat = HSD_STAT_NOT_FOUND
      return
    end if

    ! Find the last slash to separate parent path from child name
    last_slash = index(norm, "/", back=.true.)

    if (last_slash > 0) then
      parent_path = norm(1:last_slash-1)
      child_name = norm(last_slash+1:)

      ! Get the parent table
      call hsd_get_child(table, parent_path, parent_node, local_stat)
      if (local_stat /= HSD_STAT_OK .or. .not. associated(parent_node)) then
        if (present(stat)) stat = HSD_STAT_NOT_FOUND
        return
      end if

      select type (parent_node)
      type is (hsd_table)
        parent_table => parent_node
        call parent_table%remove_child_by_name(child_name, local_stat, case_insensitive)
        if (present(stat)) stat = local_stat
      class default
        if (present(stat)) stat = HSD_STAT_TYPE_ERROR
      end select
    else
      ! No path separator - remove directly from the root table
      child_name = norm
      call table%remove_child_by_name(child_name, local_stat, case_insensitive)
      if (present(stat)) stat = local_stat
    end if

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
    call table%get_child_by_name(segment, current, case_insensitive=.true.)

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

    child_table => null()
    call hsd_get_child(table, path, child, stat)

    if (associated(child)) then
      select type (child)
      type is (hsd_table)
        child_table => child
      class default
        if (present(stat)) stat = HSD_STAT_NOT_FOUND
      end select
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
  subroutine hsd_rename_child(table, old_name, new_name, stat, case_insensitive)
    type(hsd_table), intent(inout), target :: table
    character(len=*), intent(in) :: old_name
    character(len=*), intent(in) :: new_name
    integer, intent(out), optional :: stat
    logical, intent(in), optional :: case_insensitive

    class(hsd_node), pointer :: child
    integer :: last_slash, local_stat
    type(hsd_table), pointer :: parent_table
    class(hsd_node), pointer :: parent_node
    character(len=:), allocatable :: parent_path, child_old_name, norm
    logical :: ci

    ci = .true.
    if (present(case_insensitive)) ci = case_insensitive

    norm = normalize_path(old_name)
    if (len(norm) == 0) then
      if (present(stat)) stat = HSD_STAT_NOT_FOUND
      return
    end if

    ! Support path-based navigation: parent/old_name -> parent/new_name
    last_slash = index(norm, "/", back=.true.)

    if (last_slash > 0) then
      parent_path = norm(1:last_slash-1)
      child_old_name = norm(last_slash+1:)

      call hsd_get_child(table, parent_path, parent_node, local_stat)
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
      child_old_name = norm
    end if

    ! Find the child by name
    call parent_table%get_child_by_name(child_old_name, child, case_insensitive=ci)

    if (.not. associated(child)) then
      if (present(stat)) stat = HSD_STAT_NOT_FOUND
      return
    end if

    ! Rename it
    child%name = new_name
    ! Invalidate the hash index so next lookup rebuilds it
    call parent_table%invalidate_index()

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
          choice_name = child%name
        end if
        choice_table => child
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
          choice_name = child%string_value
        else if (allocated(child%name)) then
          choice_name = child%name
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

    character(len=:), allocatable :: norm, parent_path, child_name
    class(hsd_node), pointer :: parent_node, child
    type(hsd_table), pointer :: parent_table
    integer :: last_slash, local_stat, i, count
    character(len=:), allocatable :: lower_name

    norm = normalize_path(path)
    if (len(norm) == 0) then
      allocate(children(0))
      if (present(stat)) stat = HSD_STAT_NOT_FOUND
      return
    end if

    ! Split into parent path + leaf name
    last_slash = index(norm, "/", back=.true.)

    if (last_slash > 0) then
      parent_path = norm(1:last_slash-1)
      child_name = norm(last_slash+1:)

      call hsd_get_child(table, parent_path, parent_node, local_stat)
      if (local_stat /= HSD_STAT_OK .or. .not. associated(parent_node)) then
        allocate(children(0))
        if (present(stat)) stat = HSD_STAT_NOT_FOUND
        return
      end if

      select type (parent_node)
      type is (hsd_table)
        parent_table => parent_node
      class default
        allocate(children(0))
        if (present(stat)) stat = HSD_STAT_TYPE_ERROR
        return
      end select
    else
      parent_table => table
      child_name = norm
    end if

    lower_name = to_lower(child_name)

    ! First pass: count matches
    count = 0
    do i = 1, parent_table%num_children
      call parent_table%get_child(i, child)
      if (.not. associated(child)) cycle
      if (.not. allocated(child%name)) cycle
      if (to_lower(child%name) == lower_name) count = count + 1
    end do

    ! Allocate result
    allocate(children(count))

    ! Second pass: fill pointers
    count = 0
    do i = 1, parent_table%num_children
      call parent_table%get_child(i, child)
      if (.not. associated(child)) cycle
      if (.not. allocated(child%name)) cycle
      if (to_lower(child%name) == lower_name) then
        count = count + 1
        children(count)%ptr => child
      end if
    end do

    if (present(stat)) stat = HSD_STAT_OK

  end subroutine hsd_get_children

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
      call base%get_child_by_name(overlay_child%name, base_child, case_insensitive=.true.)

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
            if (allocated(base_child%raw_text)) &
                & deallocate(base_child%raw_text)
            if (allocated(base_child%int_array)) &
                & deallocate(base_child%int_array)
            if (allocated(base_child%real_array)) &
                & deallocate(base_child%real_array)
            if (allocated(base_child%logical_array)) &
                & deallocate(base_child%logical_array)
            if (allocated(base_child%string_array)) &
                & deallocate(base_child%string_array)
            if (allocated(base_child%complex_array)) &
                & deallocate(base_child%complex_array)
            if (allocated(base_child%int_matrix)) &
                & deallocate(base_child%int_matrix)
            if (allocated(base_child%real_matrix)) &
                & deallocate(base_child%real_matrix)
            ! Copy new values from clone
            if (allocated(cloned_value%string_value)) &
                & base_child%string_value = cloned_value%string_value
            base_child%int_value = cloned_value%int_value
            base_child%real_value = cloned_value%real_value
            base_child%logical_value = cloned_value%logical_value
            base_child%complex_value = cloned_value%complex_value
            if (allocated(cloned_value%raw_text)) &
                & base_child%raw_text = cloned_value%raw_text
            if (allocated(cloned_value%int_array)) &
                & allocate(base_child%int_array, source=cloned_value%int_array)
            if (allocated(cloned_value%real_array)) &
                & allocate(base_child%real_array, source=cloned_value%real_array)
            if (allocated(cloned_value%logical_array)) &
                & allocate( &
                & base_child%logical_array, source=cloned_value%logical_array)
            if (allocated(cloned_value%string_array)) &
                & allocate( &
                & base_child%string_array, source=cloned_value%string_array)
            if (allocated(cloned_value%complex_array)) &
                & allocate( &
                & base_child%complex_array, source=cloned_value%complex_array)
            if (allocated(cloned_value%int_matrix)) &
                & allocate( &
                & base_child%int_matrix, source=cloned_value%int_matrix)
            if (allocated(cloned_value%real_matrix)) &
                & allocate( &
                & base_child%real_matrix, source=cloned_value%real_matrix)
            base_child%nrows = cloned_value%nrows
            base_child%ncols = cloned_value%ncols
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
    dest%int_value = source%int_value
    dest%real_value = source%real_value
    dest%logical_value = source%logical_value
    dest%complex_value = source%complex_value

    if (allocated(source%raw_text)) dest%raw_text = source%raw_text
    if (allocated(source%int_array)) allocate(dest%int_array, source=source%int_array)
    if (allocated(source%real_array)) allocate(dest%real_array, source=source%real_array)
    if (allocated(source%logical_array)) allocate(dest%logical_array, source=source%logical_array)
    if (allocated(source%string_array)) allocate(dest%string_array, source=source%string_array)
    if (allocated(source%complex_array)) allocate(dest%complex_array, source=source%complex_array)
    if (allocated(source%int_matrix)) allocate(dest%int_matrix, source=source%int_matrix)
    if (allocated(source%real_matrix)) allocate(dest%real_matrix, source=source%real_matrix)
    dest%nrows = source%nrows
    dest%ncols = source%ncols

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
        call b%get_child_by_name(child_a%name, child_b, case_insensitive=.true.)
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

      ! Compare based on value type
      select case (a%value_type)
      case (VALUE_TYPE_NONE)
        equal = .true.

      case default
        ! For all typed values, compare the raw_text or string_value
        ! as canonical representation. This avoids needing to compare
        ! every possible cached field.
        if (allocated(a%raw_text) .and. allocated(b%raw_text)) then
          equal = (a%raw_text == b%raw_text)
          return
        end if
        if (allocated(a%string_value) .and. allocated(b%string_value)) then
          equal = (a%string_value == b%string_value)
          return
        end if
        ! Compare scalar fields for typed values set programmatically
        equal = scalars_equal(a, b)
      end select

    end function values_equal

    !> Compare scalar fields of two value nodes
    function scalars_equal(a, b) result(equal)
      type(hsd_value), intent(in) :: a
      type(hsd_value), intent(in) :: b
      logical :: equal

      equal = .false.

      select case (a%value_type)
      case (VALUE_TYPE_NONE)
        equal = .true.

      case (VALUE_TYPE_STRING)
        if (allocated(a%string_value) .and. allocated(b%string_value)) then
          equal = (a%string_value == b%string_value)
        else
          equal = (.not. allocated(a%string_value)) .and. &
              & (.not. allocated(b%string_value))
        end if

      case (VALUE_TYPE_INTEGER)
        equal = (a%int_value == b%int_value)

      case (VALUE_TYPE_REAL)
        equal = (a%real_value == b%real_value)

      case (VALUE_TYPE_LOGICAL)
        equal = (a%logical_value .eqv. b%logical_value)

      case (VALUE_TYPE_ARRAY)
        ! Arrays: compare raw_text if available
        if (allocated(a%raw_text) .and. allocated(b%raw_text)) then
          equal = (a%raw_text == b%raw_text)
        end if

      case (VALUE_TYPE_COMPLEX)
        equal = (a%complex_value == b%complex_value)

      case default
        equal = .false.

      end select

    end function scalars_equal

end module hsd_query

