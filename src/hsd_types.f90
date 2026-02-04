!> Data types for HSD nodes
!>
!> This module provides the tree structure for representing parsed HSD data.
!> The main types are:
!> - hsd_node   - Abstract base type for all nodes
!> - hsd_table  - Table (container) node for nested structures
!> - hsd_value  - Value (leaf) node for scalar and array data
!> - hsd_iterator - Iterator for traversing table children
!>
!> ## Cache-on-Read Mutation Behavior
!>
!> **IMPORTANT:** Some "read" operations on `hsd_value` (such as `value_get_int_array`,
!> `value_get_real_array`, etc.) use `intent(inout)` and mutate the internal state by
!> caching parsed array results.
!>
!> - The first call parses the raw text and stores it in a cache (e.g., `self%int_array`).
!> - Subsequent calls return the cached array without reparsing.
!> - This means these logically read-only operations have side effects, requiring
!>   `intent(inout)`.
!>
!> ### Thread Safety Implications
!>
!> - **Not thread-safe for concurrent reads:** If multiple threads access the same
!>   `hsd_value` concurrently, a race may occur on first access (cache population).
!> - **Safe after first access:** Once populated, concurrent reads are safe (immutable).
!> - **Workaround:** If thread safety is required, populate caches in a single-threaded
!>   context before concurrent access, or use external synchronization.
!>
!> ### Rationale
!>
!> - This design avoids repeated parsing and improves performance for repeated access.
!> - Purely read-only (side-effect-free) variants could be added in the future if needed.
!>
!> See also: [AGENTS.md](../AGENTS.md) for design notes and thread safety summary.
!> ## Memory Ownership Semantics
!>
!> The HSD tree uses a **copy-on-add** ownership model:
!>
!> - **table_add_child**: Creates a deep copy of the node via `allocate(source=child)`.
!>   The caller retains ownership of the original node and is responsible for
!>   deallocating it. The table owns the copy and will deallocate it when the
!>   table is destroyed or the child is removed.
!>
!> - **table_get_child, table_get_child_by_name**: Return pointers to nodes owned
!>   by the table. These pointers become invalid if the child is removed or the
!>   table is destroyed. Do NOT deallocate returned pointers.
!>
!> - **table_remove_child**: Deallocates the removed node. Any pointers previously
!>   obtained via get_child become invalid.
!>
!> - **table_destroy**: Recursively deallocates all children. Must be called
!>   explicitly to avoid memory leaks (Fortran finalizers are not used).
!>
!> ### Example - Proper Memory Management
!>
!> ```fortran
!> type(hsd_table) :: root, child_table
!> type(hsd_value) :: val
!>
!> call new_table(root, "root")
!> call new_value(val, "key")
!> call val%set_string("value")
!> call root%add_child(val)  ! root now owns a COPY of val
!> ! val can be reused or will be cleaned up when it goes out of scope
!>
!> call new_table(child_table, "section")
!> call root%add_child(child_table)  ! root owns a COPY
!>
!> ! When done, destroy the root (also destroys all children):
!> call root%destroy()
!> ```
module hsd_types
  use hsd_constants, only: dp, sp
  use hsd_utils, only: to_lower
  use hsd_error, only: HSD_STAT_OK, HSD_STAT_TYPE_ERROR, HSD_STAT_NOT_FOUND
  use hsd_hash_table, only: hsd_name_index_t
  implicit none (type, external)
  private

  public :: hsd_node, hsd_table, hsd_value, hsd_node_ptr, hsd_iterator
  public :: new_table, new_value
  public :: VALUE_TYPE_NONE, VALUE_TYPE_STRING, VALUE_TYPE_INTEGER
  public :: VALUE_TYPE_REAL, VALUE_TYPE_LOGICAL, VALUE_TYPE_ARRAY
  public :: VALUE_TYPE_COMPLEX

  !> Value type enumeration
  integer, parameter :: VALUE_TYPE_NONE = 0
  integer, parameter :: VALUE_TYPE_STRING = 1
  integer, parameter :: VALUE_TYPE_INTEGER = 2
  integer, parameter :: VALUE_TYPE_REAL = 3
  integer, parameter :: VALUE_TYPE_LOGICAL = 4
  integer, parameter :: VALUE_TYPE_ARRAY = 5
  integer, parameter :: VALUE_TYPE_COMPLEX = 6

  !> Abstract base type for all HSD nodes
  type, abstract :: hsd_node
    !> Node name (tag name)
    character(len=:), allocatable :: name
    !> Optional attribute (e.g., unit)
    character(len=:), allocatable :: attrib
    !> Line number where this node was defined (for error messages)
    integer :: line = 0
  contains
    procedure :: has_attrib => node_has_attrib
    procedure :: get_attrib => node_get_attrib
    procedure(node_destroy), deferred :: destroy
  end type hsd_node

  abstract interface
    subroutine node_destroy(self)
      import :: hsd_node
      implicit none (type, external)
      class(hsd_node), intent(inout) :: self
    end subroutine node_destroy
  end interface

  !> Pointer wrapper for polymorphic node storage
  type :: hsd_node_ptr
    class(hsd_node), allocatable :: node
  end type hsd_node_ptr

  !> Iterator for traversing table children
  type :: hsd_iterator
    !> Reference to the table being iterated
    type(hsd_table), pointer :: table => null()
    !> Current position (0 = before first)
    integer :: pos = 0
  contains
    procedure :: init => iterator_init
    procedure :: next => iterator_next
    procedure :: reset => iterator_reset
    procedure :: has_next => iterator_has_next
  end type hsd_iterator

  !> Table node (container for child nodes)
  type, extends(hsd_node) :: hsd_table
    !> Child nodes
    type(hsd_node_ptr), allocatable :: children(:)
    !> Number of children
    integer :: num_children = 0
    !> Allocated capacity
    integer :: capacity = 0
    !> Optional hash index for O(1) child lookup (built when num_children >= threshold)
    type(hsd_name_index_t) :: name_index
    !> Whether the hash index is active
    logical :: index_active = .false.
  contains
    procedure :: build_index => table_build_index
    procedure :: invalidate_index => table_invalidate_index
    procedure :: add_child => table_add_child
    procedure :: get_child => table_get_child
    procedure :: get_child_by_name => table_get_child_by_name
    procedure :: has_child => table_has_child
    procedure :: num_children_func => table_num_children
    procedure :: remove_child => table_remove_child
    procedure :: remove_child_by_name => table_remove_child_by_name
    procedure :: destroy => table_destroy
    procedure :: get_keys => table_get_keys
  end type hsd_table

  !> Value node (leaf node with data)
  type, extends(hsd_node) :: hsd_value
    !> Type of value stored
    integer :: value_type = VALUE_TYPE_NONE
    !> String value
    character(len=:), allocatable :: string_value
    !> Integer value
    integer :: int_value = 0
    !> Real value
    real(dp) :: real_value = 0.0_dp
    !> Logical value
    logical :: logical_value = .false.
    !> Complex value
    complex(dp) :: complex_value = (0.0_dp, 0.0_dp)
    !> Complex array values
    complex(dp), allocatable :: complex_array(:)
    !> String array (for multi-value or matrix data)
    character(len=:), allocatable :: raw_text
    !> Integer array values
    integer, allocatable :: int_array(:)
    !> Real array values
    real(dp), allocatable :: real_array(:)
    !> Logical array values
    logical, allocatable :: logical_array(:)
    !> String array values
    character(len=:), allocatable :: string_array(:)
    !> 2D integer matrix
    integer, allocatable :: int_matrix(:,:)
    !> 2D real matrix
    real(dp), allocatable :: real_matrix(:,:)
    !> Number of rows (for matrix data)
    integer :: nrows = 0
    !> Number of columns (for matrix data)
    integer :: ncols = 0
  contains
    procedure :: set_string => value_set_string
    procedure :: set_integer => value_set_integer
    procedure :: set_real => value_set_real
    procedure :: set_logical => value_set_logical
    procedure :: set_complex => value_set_complex
    procedure :: set_raw => value_set_raw
    procedure :: get_string => value_get_string
    procedure :: get_integer => value_get_integer
    procedure :: get_real => value_get_real
    procedure :: get_logical => value_get_logical
    procedure :: get_complex => value_get_complex
    procedure :: get_int_array => value_get_int_array
    procedure :: get_real_array => value_get_real_array
    procedure :: get_logical_array => value_get_logical_array
    procedure :: get_string_array => value_get_string_array
    procedure :: get_complex_array => value_get_complex_array
    procedure :: get_int_matrix => value_get_int_matrix
    procedure :: get_real_matrix => value_get_real_matrix
    procedure :: destroy => value_destroy
  end type hsd_value

contains

  !> Check if node has an attribute
  pure function node_has_attrib(self) result(has)
    class(hsd_node), intent(in) :: self
    logical :: has
    has = allocated(self%attrib)
  end function node_has_attrib

  !> Get node attribute (empty string if not set)
  pure function node_get_attrib(self) result(attrib)
    class(hsd_node), intent(in) :: self
    character(len=:), allocatable :: attrib
    if (allocated(self%attrib)) then
      attrib = self%attrib
    else
      attrib = ""
    end if
  end function node_get_attrib

  !> Create a new table
  subroutine new_table(table, name, attrib, line)
    type(hsd_table), intent(out) :: table
    character(len=*), intent(in), optional :: name
    character(len=*), intent(in), optional :: attrib
    integer, intent(in), optional :: line

    if (present(name)) table%name = name
    if (present(attrib)) then
      if (len_trim(attrib) > 0) table%attrib = attrib
    end if
    if (present(line)) table%line = line

    table%capacity = 8
    allocate(table%children(table%capacity))
    table%num_children = 0

  end subroutine new_table

  !> Create a new value node
  subroutine new_value(val, name, attrib, line)
    type(hsd_value), intent(out) :: val
    character(len=*), intent(in), optional :: name
    character(len=*), intent(in), optional :: attrib
    integer, intent(in), optional :: line

    if (present(name)) val%name = name
    if (present(attrib)) then
      if (len_trim(attrib) > 0) val%attrib = attrib
    end if
    if (present(line)) val%line = line
    val%value_type = VALUE_TYPE_NONE

  end subroutine new_value

  !> Build the hash index for O(1) child lookup
  !>
  !> This is called automatically when adding children.
  !> Can also be called explicitly to pre-build the index.
  subroutine table_build_index(self)
    class(hsd_table), intent(inout) :: self

    integer :: i

    call self%name_index%init(self%num_children * 2)

    do i = 1, self%num_children
      if (allocated(self%children(i)%node)) then
        if (allocated(self%children(i)%node%name)) then
          call self%name_index%insert(self%children(i)%node%name, i)
        end if
      end if
    end do

    self%index_active = .true.

  end subroutine table_build_index

  !> Invalidate the hash index (called when children are removed)
  subroutine table_invalidate_index(self)
    class(hsd_table), intent(inout) :: self

    if (self%index_active) then
      call self%name_index%clear()
      self%index_active = .false.
    end if

  end subroutine table_invalidate_index

  !> Add a child node to the table
  !>
  !> Creates a deep copy of the child node and adds it to the table.
  !> The table takes ownership of the copy and will deallocate it when
  !> the table is destroyed or the child is removed.
  !>
  !> @param[inout] self  The table to add the child to
  !> @param[in]    child The child node to copy and add
  !>
  !> @note The caller retains ownership of the original `child` argument.
  !>       The copy mechanism uses `allocate(source=child)` which performs
  !>       a deep copy of all components, including allocatable arrays.
  !>
  !> ## Performance
  !>
  !> Uses a hash index for O(1) name lookups.
  subroutine table_add_child(self, child)
    class(hsd_table), intent(inout) :: self
    class(hsd_node), intent(in) :: child

    type(hsd_node_ptr), allocatable :: tmp(:)
    integer :: new_capacity

    ! Grow array if needed
    if (self%num_children >= self%capacity) then
      new_capacity = self%capacity * 2
      allocate(tmp(new_capacity))
      tmp(1:self%num_children) = self%children(1:self%num_children)
      call move_alloc(tmp, self%children)
      self%capacity = new_capacity
    end if

    ! Add child
    self%num_children = self%num_children + 1
    allocate(self%children(self%num_children)%node, source=child)

    ! Update hash index
    if (.not. self%index_active) then
      call self%build_index()
    else if (allocated(child%name)) then
      call self%name_index%insert(child%name, self%num_children)
    end if

  end subroutine table_add_child

  !> Get child by index
  !>
  !> Returns a pointer to the child at the given index. The pointer is owned
  !> by the table - do NOT deallocate it. The pointer becomes invalid if the
  !> child is removed or the table is destroyed.
  !>
  !> @param[in]  self   The table to search
  !> @param[in]  index  1-based index of the child (1 to num_children)
  !> @param[out] child  Pointer to the child, or null() if index is out of range
  subroutine table_get_child(self, index, child)
    class(hsd_table), intent(in), target :: self
    integer, intent(in) :: index
    class(hsd_node), pointer, intent(out) :: child

    child => null()
    if (index >= 1 .and. index <= self%num_children) then
      if (allocated(self%children(index)%node)) then
        child => self%children(index)%node
      end if
    end if

  end subroutine table_get_child

  !> Get child by name
  !>
  !> Returns a pointer to the first child with the given name. The pointer is
  !> owned by the table - do NOT deallocate it. The pointer becomes invalid if
  !> the child is removed or the table is destroyed.
  !>
  !> @param[in]  self             The table to search
  !> @param[in]  name             Name to search for
  !> @param[out] child            Pointer to the child, or null() if not found
  !> @param[in]  case_insensitive If .true., ignore case when comparing names
  !>
  !> ## Performance
  !>
  !> Uses O(1) hash lookup for all table sizes.
  subroutine table_get_child_by_name(self, name, child, case_insensitive)
    class(hsd_table), intent(in), target :: self
    character(len=*), intent(in) :: name
    class(hsd_node), pointer, intent(out) :: child
    logical, intent(in), optional :: case_insensitive

    integer :: idx
    logical :: ignore_case, found

    child => null()
    ignore_case = .false.
    if (present(case_insensitive)) ignore_case = case_insensitive

    ! Ensure index is built if there are children but index isn't active
    ! (Should normally be active if added via add_child, but safety first)
    if (.not. self%index_active .and. self%num_children > 0) then
      ! Deeply constant intent(in) self prevents calling build_index directly
      ! but we can use a select type or just assume it's active.
      ! Since this is intent(in), we can't build it here.
      ! Let's assume it IS active if num_children > 0.
    end if

    if (self%index_active) then
      if (ignore_case) then
        idx = self%name_index%lookup_case_insensitive(name, found)
      else
        idx = self%name_index%lookup(name, found)
      end if

      if (found .and. idx >= 1 .and. idx <= self%num_children) then
        if (allocated(self%children(idx)%node)) then
          child => self%children(idx)%node
        end if
      end if
    end if

  end subroutine table_get_child_by_name

  !> Check if table has a child with given name
  function table_has_child(self, name, case_insensitive) result(has)
    class(hsd_table), intent(in) :: self
    character(len=*), intent(in) :: name
    logical, intent(in), optional :: case_insensitive
    logical :: has

    class(hsd_node), pointer :: child

    call self%get_child_by_name(name, child, case_insensitive)
    has = associated(child)

  end function table_has_child

  !> Get number of children
  pure function table_num_children(self) result(n)
    class(hsd_table), intent(in) :: self
    integer :: n
    n = self%num_children
  end function table_num_children

  !> Get list of all child names
  subroutine table_get_keys(self, keys)
    class(hsd_table), intent(in) :: self
    character(len=:), allocatable, intent(out) :: keys(:)

    integer :: i, max_len

    ! Find maximum key length
    max_len = 0
    do i = 1, self%num_children
      if (allocated(self%children(i)%node)) then
        if (allocated(self%children(i)%node%name)) then
          max_len = max(max_len, len(self%children(i)%node%name))
        end if
      end if
    end do

    ! Allocate and fill keys
    if (max_len > 0) then
      allocate(character(len=max_len) :: keys(self%num_children))
      do i = 1, self%num_children
        if (allocated(self%children(i)%node)) then
          if (allocated(self%children(i)%node%name)) then
            keys(i) = self%children(i)%node%name
          else
            keys(i) = ""
          end if
        end if
      end do
    else
      allocate(character(len=1) :: keys(0))
    end if

  end subroutine table_get_keys

  !> Remove child at given index
  !>
  !> Removes and deallocates the child at the given index. Children after
  !> the removed one are shifted to fill the gap. Any pointers to the removed
  !> child (obtained via get_child) become invalid after this call.
  !>
  !> @param[inout] self   The table to modify
  !> @param[in]    index  1-based index of the child to remove
  !> @param[out]   stat   Optional status: HSD_STAT_OK on success,
  !>                      HSD_STAT_NOT_FOUND if index is out of range
  subroutine table_remove_child(self, index, stat)
    class(hsd_table), intent(inout) :: self
    integer, intent(in) :: index
    integer, intent(out), optional :: stat

    integer :: i

    if (index < 1 .or. index > self%num_children) then
      if (present(stat)) stat = HSD_STAT_NOT_FOUND
      return
    end if

    ! Destroy the child node
    if (allocated(self%children(index)%node)) then
      call self%children(index)%node%destroy()
      deallocate(self%children(index)%node)
    end if

    ! Shift remaining children down
    do i = index, self%num_children - 1
      call move_alloc(self%children(i + 1)%node, self%children(i)%node)
    end do

    self%num_children = self%num_children - 1

    ! Rebuild index as indices have shifted
    if (self%num_children > 0) then
      call self%build_index()
    else
      call self%invalidate_index()
    end if

    if (present(stat)) stat = HSD_STAT_OK

  end subroutine table_remove_child

  !> Remove child by name
  subroutine table_remove_child_by_name(self, name, stat, case_insensitive)
    class(hsd_table), intent(inout) :: self
    character(len=*), intent(in) :: name
    integer, intent(out), optional :: stat
    logical, intent(in), optional :: case_insensitive

    integer :: idx
    logical :: ignore_case, found

    ignore_case = .false.
    if (present(case_insensitive)) ignore_case = case_insensitive

    if (self%index_active) then
      if (ignore_case) then
        idx = self%name_index%lookup_case_insensitive(name, found)
      else
        idx = self%name_index%lookup(name, found)
      end if
      if (found) then
        call self%remove_child(idx, stat)
        return
      end if
    end if

    if (present(stat)) stat = HSD_STAT_NOT_FOUND

  end subroutine table_remove_child_by_name

  !> Destroy table and all children
  !>
  !> Recursively deallocates all child nodes and frees all allocated memory.
  !> This must be called explicitly to avoid memory leaks - Fortran finalizers
  !> are not used for performance reasons. After calling destroy(), the table
  !> can be reused by calling new_table().
  !>
  !> @param[inout] self  The table to destroy
  !>
  !> @warning Any pointers to children obtained via get_child become invalid.
  recursive subroutine table_destroy(self)
    class(hsd_table), intent(inout) :: self
    integer :: i

    ! Destroy hash index
    call self%name_index%destroy()
    self%index_active = .false.

    do i = 1, self%num_children
      if (allocated(self%children(i)%node)) then
        call self%children(i)%node%destroy()
        deallocate(self%children(i)%node)
      end if
    end do

    if (allocated(self%children)) deallocate(self%children)
    if (allocated(self%name)) deallocate(self%name)
    if (allocated(self%attrib)) deallocate(self%attrib)

    self%num_children = 0
    self%capacity = 0

  end subroutine table_destroy

  !> Initialize iterator for a table
  subroutine iterator_init(self, table)
    class(hsd_iterator), intent(inout) :: self
    type(hsd_table), target, intent(in) :: table

    self%table => table
    self%pos = 0

  end subroutine iterator_init

  !> Advance to next child and return it
  !> Returns .false. if no more children
  function iterator_next(self, child) result(has_more)
    class(hsd_iterator), intent(inout) :: self
    class(hsd_node), pointer, intent(out) :: child
    logical :: has_more

    child => null()
    has_more = .false.

    if (.not. associated(self%table)) return

    self%pos = self%pos + 1
    if (self%pos <= self%table%num_children) then
      if (allocated(self%table%children(self%pos)%node)) then
        child => self%table%children(self%pos)%node
        has_more = .true.
      end if
    end if

  end function iterator_next

  !> Reset iterator to beginning
  subroutine iterator_reset(self)
    class(hsd_iterator), intent(inout) :: self
    self%pos = 0
  end subroutine iterator_reset

  !> Check if there are more children without advancing
  function iterator_has_next(self) result(has_more)
    class(hsd_iterator), intent(in) :: self
    logical :: has_more

    has_more = .false.
    if (associated(self%table)) then
      has_more = self%pos < self%table%num_children
    end if

  end function iterator_has_next

  !> Set string value
  subroutine value_set_string(self, val)
    class(hsd_value), intent(inout) :: self
    character(len=*), intent(in) :: val
    self%value_type = VALUE_TYPE_STRING
    self%string_value = val
  end subroutine value_set_string

  !> Set integer value
  subroutine value_set_integer(self, val)
    class(hsd_value), intent(inout) :: self
    integer, intent(in) :: val
    self%value_type = VALUE_TYPE_INTEGER
    self%int_value = val
  end subroutine value_set_integer

  !> Set real value
  subroutine value_set_real(self, val)
    class(hsd_value), intent(inout) :: self
    real(dp), intent(in) :: val
    self%value_type = VALUE_TYPE_REAL
    self%real_value = val
  end subroutine value_set_real

  !> Set logical value
  subroutine value_set_logical(self, val)
    class(hsd_value), intent(inout) :: self
    logical, intent(in) :: val
    self%value_type = VALUE_TYPE_LOGICAL
    self%logical_value = val
  end subroutine value_set_logical

  !> Set complex value
  subroutine value_set_complex(self, val)
    class(hsd_value), intent(inout) :: self
    complex(dp), intent(in) :: val
    self%value_type = VALUE_TYPE_COMPLEX
    self%complex_value = val
  end subroutine value_set_complex

  !> Set raw text (for arrays/matrices)
  subroutine value_set_raw(self, text)
    class(hsd_value), intent(inout) :: self
    character(len=*), intent(in) :: text
    self%value_type = VALUE_TYPE_STRING
    self%raw_text = text
    self%string_value = text
  end subroutine value_set_raw

  !> Get string value
  subroutine value_get_string(self, val, stat)
    class(hsd_value), intent(in) :: self
    character(len=:), allocatable, intent(out) :: val
    integer, intent(out), optional :: stat

    if (allocated(self%string_value)) then
      val = self%string_value
      if (present(stat)) stat = HSD_STAT_OK
    else if (allocated(self%raw_text)) then
      val = self%raw_text
      if (present(stat)) stat = HSD_STAT_OK
    else
      val = ""
      if (present(stat)) stat = HSD_STAT_NOT_FOUND
    end if

  end subroutine value_get_string

  !> Get integer value
  subroutine value_get_integer(self, val, stat)
    class(hsd_value), intent(in) :: self
    integer, intent(out) :: val
    integer, intent(out), optional :: stat

    integer :: io_stat

    if (self%value_type == VALUE_TYPE_INTEGER) then
      val = self%int_value
      if (present(stat)) stat = HSD_STAT_OK
    else if (allocated(self%string_value)) then
      read(self%string_value, *, iostat=io_stat) val
      if (io_stat /= 0) then
        if (present(stat)) stat = HSD_STAT_TYPE_ERROR
      else
        if (present(stat)) stat = HSD_STAT_OK
      end if
    else
      val = 0
      if (present(stat)) stat = HSD_STAT_NOT_FOUND
    end if

  end subroutine value_get_integer

  !> Get real value
  subroutine value_get_real(self, val, stat)
    class(hsd_value), intent(in) :: self
    real(dp), intent(out) :: val
    integer, intent(out), optional :: stat

    integer :: io_stat

    if (self%value_type == VALUE_TYPE_REAL) then
      val = self%real_value
      if (present(stat)) stat = HSD_STAT_OK
    else if (self%value_type == VALUE_TYPE_INTEGER) then
      val = real(self%int_value, dp)
      if (present(stat)) stat = HSD_STAT_OK
    else if (allocated(self%string_value)) then
      read(self%string_value, *, iostat=io_stat) val
      if (io_stat /= 0) then
        if (present(stat)) stat = HSD_STAT_TYPE_ERROR
      else
        if (present(stat)) stat = HSD_STAT_OK
      end if
    else
      val = 0.0_dp
      if (present(stat)) stat = HSD_STAT_NOT_FOUND
    end if

  end subroutine value_get_real

  !> Get logical value
  subroutine value_get_logical(self, val, stat)
    class(hsd_value), intent(in) :: self
    logical, intent(out) :: val
    integer, intent(out), optional :: stat

    character(len=:), allocatable :: lower_val

    if (self%value_type == VALUE_TYPE_LOGICAL) then
      val = self%logical_value
      if (present(stat)) stat = HSD_STAT_OK
    else if (allocated(self%string_value)) then
      lower_val = to_lower(trim(self%string_value))
      select case (lower_val)
      case ("yes", "on", "1", "true", ".true.")
        val = .true.
        if (present(stat)) stat = HSD_STAT_OK
      case ("no", "off", "0", "false", ".false.")
        val = .false.
        if (present(stat)) stat = HSD_STAT_OK
      case default
        val = .false.
        if (present(stat)) stat = HSD_STAT_TYPE_ERROR
      end select
    else
      val = .false.
      if (present(stat)) stat = HSD_STAT_NOT_FOUND
    end if

  end subroutine value_get_logical

  !> Get complex value
  !> Parses formats like: 4.0+9.0i, 2.0-3.0i, (1.0,2.0), 5.0+2.0j
  subroutine value_get_complex(self, val, stat)
    class(hsd_value), intent(in) :: self
    complex(dp), intent(out) :: val
    integer, intent(out), optional :: stat

    if (self%value_type == VALUE_TYPE_COMPLEX) then
      val = self%complex_value
      if (present(stat)) stat = HSD_STAT_OK
    else if (allocated(self%string_value)) then
      call parse_complex(trim(self%string_value), val, stat)
    else
      val = (0.0_dp, 0.0_dp)
      if (present(stat)) stat = HSD_STAT_NOT_FOUND
    end if

  end subroutine value_get_complex

  !> Get integer array from raw text (parses space/comma/newline separated values)
  !> Caches the parsed result for subsequent calls
  subroutine value_get_int_array(self, val, stat)
    class(hsd_value), intent(inout) :: self
    integer, allocatable, intent(out) :: val(:)
    integer, intent(out), optional :: stat

    character(len=:), allocatable :: text
    integer :: io_stat

    ! If already parsed, return cached array
    if (allocated(self%int_array)) then
      val = self%int_array
      if (present(stat)) stat = HSD_STAT_OK
      return
    end if

    ! Get source text
    if (allocated(self%raw_text)) then
      text = self%raw_text
    else if (allocated(self%string_value)) then
      text = self%string_value
    else
      allocate(val(0))
      if (present(stat)) stat = HSD_STAT_NOT_FOUND
      return
    end if

    ! Count and parse values
    call parse_int_array(text, val, io_stat)
    if (present(stat)) stat = io_stat

    ! Cache result for next access
    if (io_stat == 0) then
      self%int_array = val
    end if

  end subroutine value_get_int_array

  !> Get real array from raw text
  !> Caches the parsed result for subsequent calls
  subroutine value_get_real_array(self, val, stat)
    class(hsd_value), intent(inout) :: self
    real(dp), allocatable, intent(out) :: val(:)
    integer, intent(out), optional :: stat

    character(len=:), allocatable :: text
    integer :: io_stat

    ! If already parsed, return cached array
    if (allocated(self%real_array)) then
      val = self%real_array
      if (present(stat)) stat = HSD_STAT_OK
      return
    end if

    ! Get source text
    if (allocated(self%raw_text)) then
      text = self%raw_text
    else if (allocated(self%string_value)) then
      text = self%string_value
    else
      allocate(val(0))
      if (present(stat)) stat = HSD_STAT_NOT_FOUND
      return
    end if

    ! Count and parse values
    call parse_real_array(text, val, io_stat)
    if (present(stat)) stat = io_stat

    ! Cache result for next access
    if (io_stat == 0) then
      self%real_array = val
    end if

  end subroutine value_get_real_array

  !> Get logical array from raw text
  !> Caches the parsed result for subsequent calls
  subroutine value_get_logical_array(self, val, stat)
    class(hsd_value), intent(inout) :: self
    logical, allocatable, intent(out) :: val(:)
    integer, intent(out), optional :: stat

    character(len=:), allocatable :: text, tokens(:)
    integer :: i, n
    logical :: parse_ok

    if (allocated(self%logical_array)) then
      val = self%logical_array
      if (present(stat)) stat = HSD_STAT_OK
      return
    end if

    if (allocated(self%raw_text)) then
      text = self%raw_text
    else if (allocated(self%string_value)) then
      text = self%string_value
    else
      allocate(val(0))
      if (present(stat)) stat = HSD_STAT_NOT_FOUND
      return
    end if

    call tokenize_string(text, tokens)
    n = size(tokens)
    allocate(val(n))
    parse_ok = .true.

    do i = 1, n
      select case (to_lower(trim(tokens(i))))
      case ("yes", "on", "1", "true", ".true.")
        val(i) = .true.
      case ("no", "off", "0", "false", ".false.")
        val(i) = .false.
      case default
        val(i) = .false.
        parse_ok = .false.
        if (present(stat)) stat = HSD_STAT_TYPE_ERROR
        return
      end select
    end do

    if (present(stat)) stat = HSD_STAT_OK

    ! Cache result for next access
    if (parse_ok) then
      self%logical_array = val
    end if

  end subroutine value_get_logical_array

  !> Get complex array from raw text (parses space/comma separated complex values)
  !> Caches the parsed result for subsequent calls
  subroutine value_get_complex_array(self, val, stat)
    class(hsd_value), intent(inout) :: self
    complex(dp), allocatable, intent(out) :: val(:)
    integer, intent(out), optional :: stat

    character(len=:), allocatable :: text
    integer :: io_stat

    ! If already parsed, return cached array
    if (allocated(self%complex_array)) then
      val = self%complex_array
      if (present(stat)) stat = HSD_STAT_OK
      return
    end if

    ! Get source text
    if (allocated(self%raw_text)) then
      text = self%raw_text
    else if (allocated(self%string_value)) then
      text = self%string_value
    else
      allocate(val(0))
      if (present(stat)) stat = HSD_STAT_NOT_FOUND
      return
    end if

    ! Count and parse values
    call parse_complex_array(text, val, io_stat)
    if (present(stat)) stat = io_stat

    ! Cache result for next access
    if (io_stat == 0) then
      self%complex_array = val
    end if

  end subroutine value_get_complex_array

  !> Get string array from raw text (space-separated, quoted strings preserved)
  !> Caches the parsed result for subsequent calls
  subroutine value_get_string_array(self, val, stat)
    class(hsd_value), intent(inout) :: self
    character(len=:), allocatable, intent(out) :: val(:)
    integer, intent(out), optional :: stat

    character(len=:), allocatable :: text

    if (allocated(self%string_array)) then
      val = self%string_array
      if (present(stat)) stat = HSD_STAT_OK
      return
    end if

    if (allocated(self%raw_text)) then
      text = self%raw_text
    else if (allocated(self%string_value)) then
      text = self%string_value
    else
      allocate(character(len=1) :: val(0))
      if (present(stat)) stat = HSD_STAT_NOT_FOUND
      return
    end if

    call tokenize_quoted_string(text, val)
    if (present(stat)) stat = HSD_STAT_OK

    ! Cache result for next access
    self%string_array = val

  end subroutine value_get_string_array

  !> Get 2D integer matrix from raw text (rows separated by newlines or semicolons)
  !> Caches the parsed result for subsequent calls
  subroutine value_get_int_matrix(self, val, nrows, ncols, stat)
    class(hsd_value), intent(inout) :: self
    integer, allocatable, intent(out) :: val(:,:)
    integer, intent(out) :: nrows, ncols
    integer, intent(out), optional :: stat

    character(len=:), allocatable :: text
    integer :: io_stat

    if (allocated(self%int_matrix)) then
      val = self%int_matrix
      nrows = self%nrows
      ncols = self%ncols
      if (present(stat)) stat = HSD_STAT_OK
      return
    end if

    if (allocated(self%raw_text)) then
      text = self%raw_text
    else if (allocated(self%string_value)) then
      text = self%string_value
    else
      allocate(val(0,0))
      nrows = 0
      ncols = 0
      if (present(stat)) stat = HSD_STAT_NOT_FOUND
      return
    end if

    call parse_int_matrix(text, val, nrows, ncols, io_stat)
    if (present(stat)) stat = io_stat

    ! Cache result for next access
    if (io_stat == 0) then
      self%int_matrix = val
      self%nrows = nrows
      self%ncols = ncols
    end if

  end subroutine value_get_int_matrix

  !> Get 2D real matrix from raw text
  !> Caches the parsed result for subsequent calls
  subroutine value_get_real_matrix(self, val, nrows, ncols, stat)
    class(hsd_value), intent(inout) :: self
    real(dp), allocatable, intent(out) :: val(:,:)
    integer, intent(out) :: nrows, ncols
    integer, intent(out), optional :: stat

    character(len=:), allocatable :: text
    integer :: io_stat

    if (allocated(self%real_matrix)) then
      val = self%real_matrix
      nrows = self%nrows
      ncols = self%ncols
      if (present(stat)) stat = HSD_STAT_OK
      return
    end if

    if (allocated(self%raw_text)) then
      text = self%raw_text
    else if (allocated(self%string_value)) then
      text = self%string_value
    else
      allocate(val(0,0))
      nrows = 0
      ncols = 0
      if (present(stat)) stat = HSD_STAT_NOT_FOUND
      return
    end if

    call parse_real_matrix(text, val, nrows, ncols, io_stat)
    if (present(stat)) stat = io_stat

    ! Cache result for next access
    if (io_stat == 0) then
      self%real_matrix = val
      self%nrows = nrows
      self%ncols = ncols
    end if

  end subroutine value_get_real_matrix

  !> Parse space/comma-separated integers from text (dynamically sized)
  subroutine parse_int_array(text, arr, stat)
    character(len=*), intent(in) :: text
    integer, allocatable, intent(out) :: arr(:)
    integer, intent(out) :: stat

    character(len=:), allocatable :: tokens(:)
    integer :: i, n, val, io_stat

    call tokenize_string(text, tokens)
    n = size(tokens)

    allocate(arr(n))
    do i = 1, n
      read(tokens(i), *, iostat=io_stat) val
      if (io_stat /= 0) then
        deallocate(arr)
        allocate(arr(0))
        stat = io_stat
        return
      end if
      arr(i) = val
    end do

    stat = 0

  end subroutine parse_int_array

  !> Parse space/comma-separated reals from text (dynamically sized)
  subroutine parse_real_array(text, arr, stat)
    character(len=*), intent(in) :: text
    real(dp), allocatable, intent(out) :: arr(:)
    integer, intent(out) :: stat

    character(len=:), allocatable :: tokens(:)
    integer :: i, n, io_stat
    real(dp) :: val

    call tokenize_string(text, tokens)
    n = size(tokens)

    allocate(arr(n))
    do i = 1, n
      read(tokens(i), *, iostat=io_stat) val
      if (io_stat /= 0) then
        deallocate(arr)
        allocate(arr(0))
        stat = io_stat
        return
      end if
      arr(i) = val
    end do

    stat = 0

  end subroutine parse_real_array

  !> Tokenize string by whitespace and commas
  subroutine tokenize_string(text, tokens)
    character(len=*), intent(in) :: text
    character(len=:), allocatable, intent(out) :: tokens(:)

    integer :: i, start, max_len, token_count
    character(len=len(text)) :: temp_tokens(len(text))
    logical :: in_token

    ! First pass: count tokens and find max length
    token_count = 0
    max_len = 0
    in_token = .false.
    start = 1

    do i = 1, len(text)
      if (is_separator(text(i:i))) then
        if (in_token) then
          token_count = token_count + 1
          max_len = max(max_len, i - start)
          temp_tokens(token_count) = text(start:i-1)
          in_token = .false.
        end if
      else
        if (.not. in_token) then
          start = i
          in_token = .true.
        end if
      end if
    end do

    ! Handle last token
    if (in_token) then
      token_count = token_count + 1
      max_len = max(max_len, len(text) - start + 1)
      temp_tokens(token_count) = text(start:len(text))
    end if

    ! Allocate and copy
    if (token_count > 0 .and. max_len > 0) then
      allocate(character(len=max_len) :: tokens(token_count))
      do i = 1, token_count
        tokens(i) = trim(temp_tokens(i))
      end do
    else
      allocate(character(len=1) :: tokens(0))
    end if

  end subroutine tokenize_string

  !> Check if character is a separator (whitespace, comma, semicolon)
  pure function is_separator(ch) result(is_sep)
    character(len=1), intent(in) :: ch
    logical :: is_sep
    is_sep = (ch == ' ' .or. ch == char(9) .or. ch == char(10) .or. &
              ch == char(13) .or. ch == ',' .or. ch == ';')
  end function is_separator

  !> Tokenize string preserving quoted sections
  subroutine tokenize_quoted_string(text, tokens)
    character(len=*), intent(in) :: text
    character(len=:), allocatable, intent(out) :: tokens(:)

    integer :: i, start, max_len, token_count, tlen
    character(len=len(text)) :: temp_tokens(len(text))
    character(len=1) :: quote_char
    logical :: in_token, in_quote

    token_count = 0
    max_len = 0
    in_token = .false.
    in_quote = .false.
    quote_char = ' '
    start = 1
    tlen = len_trim(text)

    i = 1
    do while (i <= tlen)
      if (in_quote) then
        ! Look for closing quote
        if (text(i:i) == quote_char) then
          token_count = token_count + 1
          max_len = max(max_len, i - start - 1)
          if (i > start + 1) then
            temp_tokens(token_count) = text(start+1:i-1)
          else
            temp_tokens(token_count) = ""
          end if
          in_quote = .false.
          in_token = .false.
        end if
      else if (text(i:i) == '"' .or. text(i:i) == "'") then
        quote_char = text(i:i)
        in_quote = .true.
        start = i
        in_token = .true.
      else if (is_separator(text(i:i))) then
        if (in_token) then
          token_count = token_count + 1
          max_len = max(max_len, i - start)
          temp_tokens(token_count) = text(start:i-1)
          in_token = .false.
        end if
      else
        if (.not. in_token) then
          start = i
          in_token = .true.
        end if
      end if
      i = i + 1
    end do

    ! Handle last token
    if (in_token .and. .not. in_quote) then
      token_count = token_count + 1
      max_len = max(max_len, tlen - start + 1)
      temp_tokens(token_count) = text(start:tlen)
    end if

    ! Allocate and copy
    if (token_count > 0 .and. max_len > 0) then
      allocate(character(len=max_len) :: tokens(token_count))
      do i = 1, token_count
        tokens(i) = trim(temp_tokens(i))
      end do
    else
      allocate(character(len=1) :: tokens(0))
    end if

  end subroutine tokenize_quoted_string

  !> Parse 2D integer matrix (rows separated by newlines or semicolons)
  subroutine parse_int_matrix(text, mat, nrows, ncols, stat)
    character(len=*), intent(in) :: text
    integer, allocatable, intent(out) :: mat(:,:)
    integer, intent(out) :: nrows, ncols, stat

    character(len=:), allocatable :: rows(:), tokens(:)
    integer, allocatable :: row_vals(:)
    integer :: i, j, row_count, col_count, first_cols

    ! Split into rows by newlines
    call split_by_newlines(text, rows)
    row_count = size(rows)

    ! Count non-empty rows and determine column count
    nrows = 0
    ncols = 0
    first_cols = -1

    do i = 1, row_count
      if (len_trim(rows(i)) > 0) then
        call tokenize_string(rows(i), tokens)
        col_count = size(tokens)
        if (col_count > 0) then
          nrows = nrows + 1
          if (first_cols < 0) then
            first_cols = col_count
            ncols = col_count
          else if (col_count /= first_cols) then
            ! Inconsistent column count - use max
            ncols = max(ncols, col_count)
          end if
        end if
      end if
    end do

    if (nrows == 0 .or. ncols == 0) then
      allocate(mat(0,0))
      nrows = 0
      ncols = 0
      stat = 0
      return
    end if

    allocate(mat(nrows, ncols))
    mat = 0

    j = 0
    do i = 1, row_count
      if (len_trim(rows(i)) > 0) then
        call parse_int_array(rows(i), row_vals, stat)
        if (stat /= 0) then
          deallocate(mat)
          allocate(mat(0,0))
          nrows = 0
          ncols = 0
          return
        end if
        if (size(row_vals) > 0) then
          j = j + 1
          mat(j, 1:min(size(row_vals), ncols)) = row_vals(1:min(size(row_vals), ncols))
        end if
      end if
    end do

    stat = 0

  end subroutine parse_int_matrix

  !> Parse 2D real matrix
  subroutine parse_real_matrix(text, mat, nrows, ncols, stat)
    character(len=*), intent(in) :: text
    real(dp), allocatable, intent(out) :: mat(:,:)
    integer, intent(out) :: nrows, ncols, stat

    character(len=:), allocatable :: rows(:), tokens(:)
    real(dp), allocatable :: row_vals(:)
    integer :: i, j, row_count, col_count, first_cols

    call split_by_newlines(text, rows)
    row_count = size(rows)

    nrows = 0
    ncols = 0
    first_cols = -1

    do i = 1, row_count
      if (len_trim(rows(i)) > 0) then
        call tokenize_string(rows(i), tokens)
        col_count = size(tokens)
        if (col_count > 0) then
          nrows = nrows + 1
          if (first_cols < 0) then
            first_cols = col_count
            ncols = col_count
          else if (col_count /= first_cols) then
            ncols = max(ncols, col_count)
          end if
        end if
      end if
    end do

    if (nrows == 0 .or. ncols == 0) then
      allocate(mat(0,0))
      nrows = 0
      ncols = 0
      stat = 0
      return
    end if

    allocate(mat(nrows, ncols))
    mat = 0.0_dp

    j = 0
    do i = 1, row_count
      if (len_trim(rows(i)) > 0) then
        call parse_real_array(rows(i), row_vals, stat)
        if (stat /= 0) then
          deallocate(mat)
          allocate(mat(0,0))
          nrows = 0
          ncols = 0
          return
        end if
        if (size(row_vals) > 0) then
          j = j + 1
          mat(j, 1:min(size(row_vals), ncols)) = row_vals(1:min(size(row_vals), ncols))
        end if
      end if
    end do

    stat = 0

  end subroutine parse_real_matrix

  !> Split text by newlines
  subroutine split_by_newlines(text, lines)
    character(len=*), intent(in) :: text
    character(len=:), allocatable, intent(out) :: lines(:)

    integer :: i, start, line_count, max_len, tlen
    character(len=len(text)) :: temp_lines(len(text))

    line_count = 0
    max_len = 0
    start = 1
    tlen = len(text)

    do i = 1, tlen
      if (text(i:i) == char(10) .or. text(i:i) == ';') then
        line_count = line_count + 1
        max_len = max(max_len, i - start)
        if (i > start) then
          temp_lines(line_count) = text(start:i-1)
        else
          temp_lines(line_count) = ""
        end if
        start = i + 1
      end if
    end do

    ! Handle last line
    if (start <= tlen) then
      line_count = line_count + 1
      max_len = max(max_len, tlen - start + 1)
      temp_lines(line_count) = text(start:tlen)
    end if

    if (line_count > 0 .and. max_len > 0) then
      allocate(character(len=max_len) :: lines(line_count))
      do i = 1, line_count
        lines(i) = trim(temp_lines(i))
      end do
    else
      allocate(character(len=1) :: lines(1))
      lines(1) = text
    end if

  end subroutine split_by_newlines

  !> Destroy value
  subroutine value_destroy(self)
    class(hsd_value), intent(inout) :: self

    if (allocated(self%name)) deallocate(self%name)
    if (allocated(self%attrib)) deallocate(self%attrib)
    if (allocated(self%string_value)) deallocate(self%string_value)
    if (allocated(self%raw_text)) deallocate(self%raw_text)
    if (allocated(self%int_array)) deallocate(self%int_array)
    if (allocated(self%real_array)) deallocate(self%real_array)
    if (allocated(self%logical_array)) deallocate(self%logical_array)
    if (allocated(self%string_array)) deallocate(self%string_array)
    if (allocated(self%complex_array)) deallocate(self%complex_array)
    if (allocated(self%int_matrix)) deallocate(self%int_matrix)
    if (allocated(self%real_matrix)) deallocate(self%real_matrix)

    self%value_type = VALUE_TYPE_NONE
    self%nrows = 0
    self%ncols = 0

  end subroutine value_destroy

  !> Parse a single complex number from string
  !> Supports formats: 4.0+9.0i, 2.0-3.0i, (1.0,2.0), 5.0+2.0j, 3.5, pure imaginary 2.0i
  subroutine parse_complex(str, val, stat)
    character(len=*), intent(in) :: str
    complex(dp), intent(out) :: val
    integer, intent(out), optional :: stat

    character(len=:), allocatable :: work
    integer :: i, sign_pos, io_stat
    real(dp) :: re, im
    character(len=1) :: ch

    work = adjustl(trim(str))

    ! Handle empty string
    if (len_trim(work) == 0) then
      val = (0.0_dp, 0.0_dp)
      if (present(stat)) stat = HSD_STAT_TYPE_ERROR
      return
    end if

    ! Handle Fortran-style (re,im) format
    if (work(1:1) == '(') then
      i = index(work, ')')
      if (i > 2) then
        work = work(2:i-1)
        i = index(work, ',')
        if (i > 0) then
          read(work(1:i-1), *, iostat=io_stat) re
          if (io_stat /= 0) then
            val = (0.0_dp, 0.0_dp)
            if (present(stat)) stat = io_stat
            return
          end if
          read(work(i+1:), *, iostat=io_stat) im
          if (io_stat /= 0) then
            val = (0.0_dp, 0.0_dp)
            if (present(stat)) stat = io_stat
            return
          end if
          val = cmplx(re, im, dp)
          if (present(stat)) stat = HSD_STAT_OK
          return
        end if
      end if
    end if

    ! Handle a+bi or a-bi format (also handles j instead of i)
    ! Find the + or - that separates real and imaginary parts
    ! (must skip the first char and any exponent signs)
    sign_pos = 0
    do i = 2, len_trim(work)
      ch = work(i:i)
      if ((ch == '+' .or. ch == '-')) then
        ! Make sure this isn't part of an exponent
        if (i > 1) then
          if (work(i-1:i-1) /= 'e' .and. work(i-1:i-1) /= 'E' .and. &
              work(i-1:i-1) /= 'd' .and. work(i-1:i-1) /= 'D') then
            sign_pos = i
          end if
        end if
      end if
    end do

    ! Check if last character is 'i' or 'j' (imaginary marker)
    ch = work(len_trim(work):len_trim(work))
    if (ch == 'i' .or. ch == 'I' .or. ch == 'j' .or. ch == 'J') then
      if (sign_pos > 0) then
        ! Format: a+bi or a-bi
        read(work(1:sign_pos-1), *, iostat=io_stat) re
        if (io_stat /= 0) then
          val = (0.0_dp, 0.0_dp)
          if (present(stat)) stat = io_stat
          return
        end if
        read(work(sign_pos:len_trim(work)-1), *, iostat=io_stat) im
        if (io_stat /= 0) then
          val = (0.0_dp, 0.0_dp)
          if (present(stat)) stat = io_stat
          return
        end if
        val = cmplx(re, im, dp)
        if (present(stat)) stat = HSD_STAT_OK
        return
      else
        ! Pure imaginary: bi
        read(work(1:len_trim(work)-1), *, iostat=io_stat) im
        if (io_stat /= 0) then
          val = (0.0_dp, 0.0_dp)
          if (present(stat)) stat = io_stat
          return
        end if
        val = cmplx(0.0_dp, im, dp)
        if (present(stat)) stat = HSD_STAT_OK
        return
      end if
    else
      ! Pure real number
      read(work, *, iostat=io_stat) re
      if (io_stat /= 0) then
        val = (0.0_dp, 0.0_dp)
        if (present(stat)) stat = io_stat
        return
      end if
      val = cmplx(re, 0.0_dp, dp)
      if (present(stat)) stat = HSD_STAT_OK
    end if

  end subroutine parse_complex

  !> Parse an array of complex numbers from text
  subroutine parse_complex_array(text, arr, stat)
    character(len=*), intent(in) :: text
    complex(dp), allocatable, intent(out) :: arr(:)
    integer, intent(out) :: stat

    character(len=:), allocatable :: tokens(:)
    integer :: i, n, io_stat
    complex(dp) :: val

    call tokenize_string(text, tokens)
    n = size(tokens)

    allocate(arr(n))
    do i = 1, n
      call parse_complex(tokens(i), val, io_stat)
      if (io_stat /= 0) then
        deallocate(arr)
        allocate(arr(0))
        stat = io_stat
        return
      end if
      arr(i) = val
    end do

    stat = 0

  end subroutine parse_complex_array

end module hsd_types
