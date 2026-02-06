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
!> **IMPORTANT:** Some "read" operations on `hsd_value` (such as
!> `value_get_int_array`, `value_get_real_array`, etc.) use
!> `intent(inout)` and mutate the internal state by caching parsed
!> array results.
!>
!> - The first call parses the raw text and stores it in a cache
!>   (e.g., `self%int_array`).
!> - Subsequent calls return the cached array without reparsing.
!> - This means these logically read-only operations have side effects,
!>   requiring `intent(inout)`.
!>
!> ### Thread Safety Implications
!>
!> - **Not thread-safe for concurrent reads:** If multiple threads
!>   access the same `hsd_value` concurrently, a race may occur on
!>   first access (cache population).
!> - **Safe after first access:** Once populated, concurrent reads are
!>   safe (immutable).
!> - **Workaround:** If thread safety is required, populate caches in
!>   a single-threaded context before concurrent access, or use
!>   external synchronization.
!>
!> ### Rationale
!>
!> - This design avoids repeated parsing and improves performance for
!>   repeated access.
!> - Purely read-only (side-effect-free) variants could be added in
!>   the future if needed.
!>
!> See also: [AGENTS.md](../AGENTS.md) for design notes and thread
!> safety summary.
!>
!> ## Memory Ownership Semantics
!>
!> The HSD tree uses a **copy-on-add** ownership model:
!>
!> - **table_add_child**: Creates a deep copy of the node via
!>   `allocate(source=child)`. The caller retains ownership of the
!>   original node.
!>
!> - **table_get_child, table_get_child_by_name**: Return pointers to
!>   nodes owned by the table. Do NOT deallocate returned pointers.
!>
!> - **table_remove_child**: Deallocates the removed node. Any pointers
!>   previously obtained via get_child become invalid.
!>
!> - **table_destroy**: Recursively deallocates all children. Must be
!>   called explicitly to avoid memory leaks.
!>
!> ## Module Structure
!>
!> Type definitions and constructors live in this parent module.
!> Implementations are split across two submodules for maintainability:
!> - **hsd_table_ops** — table and iterator operations
!> - **hsd_value_ops** — value setters, getters, and parse helpers
module hsd_types
  use hsd_constants, only: dp, sp
  use hsd_utils, only: to_lower
  use hsd_error, only: HSD_STAT_OK, HSD_STAT_TYPE_ERROR, &
      & HSD_STAT_NOT_FOUND
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
    !> Hash index for O(1) child lookup
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

  ! =================================================================
  ! Submodule procedure interfaces
  ! =================================================================

  interface

    ! --- Table operations (submodule hsd_table_ops) ---

    module subroutine table_build_index(self)
      implicit none (type, external)
      class(hsd_table), intent(inout) :: self
    end subroutine table_build_index

    module subroutine table_invalidate_index(self)
      implicit none (type, external)
      class(hsd_table), intent(inout) :: self
    end subroutine table_invalidate_index

    module subroutine table_add_child(self, child)
      implicit none (type, external)
      class(hsd_table), intent(inout) :: self
      class(hsd_node), intent(in) :: child
    end subroutine table_add_child

    module subroutine table_get_child(self, index, child)
      implicit none (type, external)
      class(hsd_table), intent(in), target :: self
      integer, intent(in) :: index
      class(hsd_node), pointer, intent(out) :: child
    end subroutine table_get_child

    module subroutine table_get_child_by_name( &
        & self, name, child, case_insensitive)
      implicit none (type, external)
      class(hsd_table), intent(in), target :: self
      character(len=*), intent(in) :: name
      class(hsd_node), pointer, intent(out) :: child
      logical, intent(in), optional :: case_insensitive
    end subroutine table_get_child_by_name

    module function table_has_child( &
        & self, name, case_insensitive) result(has)
      implicit none (type, external)
      class(hsd_table), intent(in) :: self
      character(len=*), intent(in) :: name
      logical, intent(in), optional :: case_insensitive
      logical :: has
    end function table_has_child

    pure module function table_num_children(self) result(n)
      implicit none (type, external)
      class(hsd_table), intent(in) :: self
      integer :: n
    end function table_num_children

    module subroutine table_get_keys(self, keys)
      implicit none (type, external)
      class(hsd_table), intent(in) :: self
      character(len=:), allocatable, intent(out) :: keys(:)
    end subroutine table_get_keys

    module subroutine table_remove_child(self, index, stat)
      implicit none (type, external)
      class(hsd_table), intent(inout) :: self
      integer, intent(in) :: index
      integer, intent(out), optional :: stat
    end subroutine table_remove_child

    module subroutine table_remove_child_by_name( &
        & self, name, stat, case_insensitive)
      implicit none (type, external)
      class(hsd_table), intent(inout) :: self
      character(len=*), intent(in) :: name
      integer, intent(out), optional :: stat
      logical, intent(in), optional :: case_insensitive
    end subroutine table_remove_child_by_name

    recursive module subroutine table_destroy(self)
      implicit none (type, external)
      class(hsd_table), intent(inout) :: self
    end subroutine table_destroy

    ! --- Iterator operations (submodule hsd_table_ops) ---

    module subroutine iterator_init(self, table)
      implicit none (type, external)
      class(hsd_iterator), intent(inout) :: self
      type(hsd_table), target, intent(in) :: table
    end subroutine iterator_init

    module function iterator_next(self, child) &
        & result(has_more)
      implicit none (type, external)
      class(hsd_iterator), intent(inout) :: self
      class(hsd_node), pointer, intent(out) :: child
      logical :: has_more
    end function iterator_next

    module subroutine iterator_reset(self)
      implicit none (type, external)
      class(hsd_iterator), intent(inout) :: self
    end subroutine iterator_reset

    module function iterator_has_next(self) result(has_more)
      implicit none (type, external)
      class(hsd_iterator), intent(in) :: self
      logical :: has_more
    end function iterator_has_next

    ! --- Value setters (submodule hsd_value_ops) ---

    module subroutine value_set_string(self, val)
      implicit none (type, external)
      class(hsd_value), intent(inout) :: self
      character(len=*), intent(in) :: val
    end subroutine value_set_string

    module subroutine value_set_integer(self, val)
      implicit none (type, external)
      class(hsd_value), intent(inout) :: self
      integer, intent(in) :: val
    end subroutine value_set_integer

    module subroutine value_set_real(self, val)
      implicit none (type, external)
      class(hsd_value), intent(inout) :: self
      real(dp), intent(in) :: val
    end subroutine value_set_real

    module subroutine value_set_logical(self, val)
      implicit none (type, external)
      class(hsd_value), intent(inout) :: self
      logical, intent(in) :: val
    end subroutine value_set_logical

    module subroutine value_set_complex(self, val)
      implicit none (type, external)
      class(hsd_value), intent(inout) :: self
      complex(dp), intent(in) :: val
    end subroutine value_set_complex

    module subroutine value_set_raw(self, text)
      implicit none (type, external)
      class(hsd_value), intent(inout) :: self
      character(len=*), intent(in) :: text
    end subroutine value_set_raw

    ! --- Value getters (submodule hsd_value_ops) ---

    module subroutine value_get_string(self, val, stat)
      implicit none (type, external)
      class(hsd_value), intent(in) :: self
      character(len=:), allocatable, intent(out) :: val
      integer, intent(out), optional :: stat
    end subroutine value_get_string

    module subroutine value_get_integer(self, val, stat)
      implicit none (type, external)
      class(hsd_value), intent(in) :: self
      integer, intent(out) :: val
      integer, intent(out), optional :: stat
    end subroutine value_get_integer

    module subroutine value_get_real(self, val, stat)
      implicit none (type, external)
      class(hsd_value), intent(in) :: self
      real(dp), intent(out) :: val
      integer, intent(out), optional :: stat
    end subroutine value_get_real

    module subroutine value_get_logical(self, val, stat)
      implicit none (type, external)
      class(hsd_value), intent(in) :: self
      logical, intent(out) :: val
      integer, intent(out), optional :: stat
    end subroutine value_get_logical

    module subroutine value_get_complex(self, val, stat)
      implicit none (type, external)
      class(hsd_value), intent(in) :: self
      complex(dp), intent(out) :: val
      integer, intent(out), optional :: stat
    end subroutine value_get_complex

    ! --- Array getters (submodule hsd_value_ops) ---

    module subroutine value_get_int_array(self, val, stat)
      implicit none (type, external)
      class(hsd_value), intent(inout) :: self
      integer, allocatable, intent(out) :: val(:)
      integer, intent(out), optional :: stat
    end subroutine value_get_int_array

    module subroutine value_get_real_array(self, val, stat)
      implicit none (type, external)
      class(hsd_value), intent(inout) :: self
      real(dp), allocatable, intent(out) :: val(:)
      integer, intent(out), optional :: stat
    end subroutine value_get_real_array

    module subroutine value_get_logical_array(self, val, stat)
      implicit none (type, external)
      class(hsd_value), intent(inout) :: self
      logical, allocatable, intent(out) :: val(:)
      integer, intent(out), optional :: stat
    end subroutine value_get_logical_array

    module subroutine value_get_string_array(self, val, stat)
      implicit none (type, external)
      class(hsd_value), intent(inout) :: self
      character(len=:), allocatable, intent(out) :: val(:)
      integer, intent(out), optional :: stat
    end subroutine value_get_string_array

    module subroutine value_get_complex_array(self, val, stat)
      implicit none (type, external)
      class(hsd_value), intent(inout) :: self
      complex(dp), allocatable, intent(out) :: val(:)
      integer, intent(out), optional :: stat
    end subroutine value_get_complex_array

    ! --- Matrix getters (submodule hsd_value_ops) ---

    module subroutine value_get_int_matrix( &
        & self, val, nrows, ncols, stat)
      implicit none (type, external)
      class(hsd_value), intent(inout) :: self
      integer, allocatable, intent(out) :: val(:,:)
      integer, intent(out) :: nrows, ncols
      integer, intent(out), optional :: stat
    end subroutine value_get_int_matrix

    module subroutine value_get_real_matrix( &
        & self, val, nrows, ncols, stat)
      implicit none (type, external)
      class(hsd_value), intent(inout) :: self
      real(dp), allocatable, intent(out) :: val(:,:)
      integer, intent(out) :: nrows, ncols
      integer, intent(out), optional :: stat
    end subroutine value_get_real_matrix

    ! --- Destructor (submodule hsd_value_ops) ---

    module subroutine value_destroy(self)
      implicit none (type, external)
      class(hsd_value), intent(inout) :: self
    end subroutine value_destroy

  end interface

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

    table%capacity = 4
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

end module hsd_types
