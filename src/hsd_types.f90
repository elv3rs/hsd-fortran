!> Data types for HSD nodes
!>
!> This module provides the tree structure for representing parsed HSD data.
!> The main types are:
!> - hsd_node     - Unified concrete node (table or value)
!> - hsd_node_ptr - Pointer wrapper for child storage
!> - hsd_iterator - Iterator for traversing table children
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
!> - **destroy**: Recursively deallocates all children. Must be
!>   called explicitly to avoid memory leaks.
!>
!> ## Module Structure
!>
!> Type definitions and constructors live in this parent module.
!> Implementations are split across two submodules for maintainability:
!> - **hsd_table_ops** — table and iterator operations
!> - **hsd_value_ops** — value setters, getters, and parse helpers
module hsd_types
  use hsd_constants, only: dp
  use hsd_utils, only: to_lower
  use hsd_error, only: HSD_STAT_OK, HSD_STAT_TYPE_ERROR, &
      & HSD_STAT_NOT_FOUND
  implicit none (type, external)
  private

  public :: hsd_node, hsd_node_ptr, hsd_iterator
  public :: new_table, new_value
  public :: NODE_TYPE_TABLE, NODE_TYPE_VALUE
  public :: VALUE_TYPE_NONE, VALUE_TYPE_STRING, VALUE_TYPE_INTEGER
  public :: VALUE_TYPE_REAL, VALUE_TYPE_LOGICAL, VALUE_TYPE_ARRAY
  public :: VALUE_TYPE_COMPLEX

  !> Node type enumeration
  integer, parameter :: NODE_TYPE_TABLE = 1
  integer, parameter :: NODE_TYPE_VALUE = 2

  !> Value type enumeration
  integer, parameter :: VALUE_TYPE_NONE = 0
  integer, parameter :: VALUE_TYPE_STRING = 1
  integer, parameter :: VALUE_TYPE_INTEGER = 2
  integer, parameter :: VALUE_TYPE_REAL = 3
  integer, parameter :: VALUE_TYPE_LOGICAL = 4
  integer, parameter :: VALUE_TYPE_ARRAY = 5
  integer, parameter :: VALUE_TYPE_COMPLEX = 6

  !> Pointer wrapper for node storage
  !>
  !> Uses a pointer (not allocatable) so that when the children array
  !> is reallocated during growth, pointers to child nodes obtained via
  !> get_child/get_child_by_name remain valid.
  type :: hsd_node_ptr
    type(hsd_node), pointer :: node => null()
  end type hsd_node_ptr

  !> Unified HSD node type (table or value)
  !>
  !> Component order: value fields (string_value) are placed before the
  !> self-referential children array to work around an Intel ifx compiler
  !> bug with component offset calculation for recursive types.
  type :: hsd_node
    !> Node name (tag name)
    character(len=:), allocatable :: name
    !> Optional attribute (e.g., unit)
    character(len=:), allocatable :: attrib
    !> Line number where this node was defined (for error messages)
    integer :: line = 0
    !> Whether this node has been accessed/processed
    logical :: processed = .false.
    !> Node type discriminator (NODE_TYPE_TABLE or NODE_TYPE_VALUE)
    integer :: node_type = 0
    !> Type of value stored (value only)
    integer :: value_type = VALUE_TYPE_NONE
    !> String representation of the value (value only)
    character(len=:), allocatable :: string_value
    !> Child nodes (table only)
    type(hsd_node_ptr), allocatable :: children(:)
    !> Number of children (table only)
    integer :: num_children = 0
  contains
    ! Common
    procedure :: has_attrib => node_has_attrib
    procedure :: get_attrib => node_get_attrib
    ! Table operations
    procedure :: add_child => table_add_child
    procedure :: get_child => table_get_child
    procedure :: get_child_by_name => table_get_child_by_name
    procedure :: has_child => table_has_child
    procedure :: num_children_func => table_num_children
    procedure :: remove_child => table_remove_child
    procedure :: remove_child_by_name => table_remove_child_by_name
    procedure :: get_keys => table_get_keys
    ! Value operations
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
    procedure :: get_complex_matrix => value_get_complex_matrix
    ! Destroy
    procedure :: destroy => node_destroy
  end type hsd_node

  !> Iterator for traversing table children
  type :: hsd_iterator
    !> Reference to the table being iterated
    type(hsd_node), pointer :: table => null()
    !> Current position (0 = before first)
    integer :: pos = 0
  contains
    procedure :: init => iterator_init
    procedure :: next => iterator_next
    procedure :: reset => iterator_reset
    procedure :: has_next => iterator_has_next
  end type hsd_iterator

  ! =================================================================
  ! Submodule procedure interfaces
  ! =================================================================

  interface

    ! --- Table operations (submodule hsd_table_ops) ---

    module subroutine table_add_child(self, child)
      implicit none (type, external)
      class(hsd_node), intent(inout) :: self
      type(hsd_node), intent(in) :: child
    end subroutine table_add_child

    module subroutine table_get_child( &
        & self, index, child)
      implicit none (type, external)
      class(hsd_node), intent(in), target :: self
      integer, intent(in) :: index
      type(hsd_node), pointer, intent(out) :: child
    end subroutine table_get_child

    module subroutine table_get_child_by_name( &
        & self, name, child)
      implicit none (type, external)
      class(hsd_node), intent(in), target :: self
      character(len=*), intent(in) :: name
      type(hsd_node), pointer, intent(out) :: child
    end subroutine table_get_child_by_name

    module function table_has_child( &
        & self, name) result(has)
      implicit none (type, external)
      class(hsd_node), intent(in) :: self
      character(len=*), intent(in) :: name
      logical :: has
    end function table_has_child

    pure module function table_num_children( &
        & self) result(n)
      implicit none (type, external)
      class(hsd_node), intent(in) :: self
      integer :: n
    end function table_num_children

    module subroutine table_get_keys(self, keys)
      implicit none (type, external)
      class(hsd_node), intent(in) :: self
      character(len=:), allocatable, intent(out) :: keys(:)
    end subroutine table_get_keys

    module subroutine table_remove_child( &
        & self, index, stat)
      implicit none (type, external)
      class(hsd_node), intent(inout) :: self
      integer, intent(in) :: index
      integer, intent(out), optional :: stat
    end subroutine table_remove_child

    module subroutine table_remove_child_by_name( &
        & self, name, stat)
      implicit none (type, external)
      class(hsd_node), intent(inout) :: self
      character(len=*), intent(in) :: name
      integer, intent(out), optional :: stat
    end subroutine table_remove_child_by_name

    recursive module subroutine node_destroy(self)
      implicit none (type, external)
      class(hsd_node), intent(inout) :: self
    end subroutine node_destroy

    ! --- Iterator operations (submodule hsd_table_ops) ---

    module subroutine iterator_init(self, table)
      implicit none (type, external)
      class(hsd_iterator), intent(inout) :: self
      type(hsd_node), target, intent(in) :: table
    end subroutine iterator_init

    module function iterator_next(self, child) &
        & result(has_more)
      implicit none (type, external)
      class(hsd_iterator), intent(inout) :: self
      type(hsd_node), pointer, intent(out) :: child
      logical :: has_more
    end function iterator_next

    module subroutine iterator_reset(self)
      implicit none (type, external)
      class(hsd_iterator), intent(inout) :: self
    end subroutine iterator_reset

    module function iterator_has_next( &
        & self) result(has_more)
      implicit none (type, external)
      class(hsd_iterator), intent(in) :: self
      logical :: has_more
    end function iterator_has_next

    ! --- Value setters (submodule hsd_value_ops) ---

    module subroutine value_set_string(self, val)
      implicit none (type, external)
      class(hsd_node), intent(inout) :: self
      character(len=*), intent(in) :: val
    end subroutine value_set_string

    module subroutine value_set_integer(self, val)
      implicit none (type, external)
      class(hsd_node), intent(inout) :: self
      integer, intent(in) :: val
    end subroutine value_set_integer

    module subroutine value_set_real(self, val)
      implicit none (type, external)
      class(hsd_node), intent(inout) :: self
      real(dp), intent(in) :: val
    end subroutine value_set_real

    module subroutine value_set_logical(self, val)
      implicit none (type, external)
      class(hsd_node), intent(inout) :: self
      logical, intent(in) :: val
    end subroutine value_set_logical

    module subroutine value_set_complex(self, val)
      implicit none (type, external)
      class(hsd_node), intent(inout) :: self
      complex(dp), intent(in) :: val
    end subroutine value_set_complex

    module subroutine value_set_raw(self, text)
      implicit none (type, external)
      class(hsd_node), intent(inout) :: self
      character(len=*), intent(in) :: text
    end subroutine value_set_raw

    ! --- Value getters (submodule hsd_value_ops) ---

    module subroutine value_get_string( &
        & self, val, stat)
      implicit none (type, external)
      class(hsd_node), intent(in) :: self
      character(len=:), allocatable, intent(out) :: val
      integer, intent(out), optional :: stat
    end subroutine value_get_string

    module subroutine value_get_integer( &
        & self, val, stat)
      implicit none (type, external)
      class(hsd_node), intent(in) :: self
      integer, intent(out) :: val
      integer, intent(out), optional :: stat
    end subroutine value_get_integer

    module subroutine value_get_real( &
        & self, val, stat)
      implicit none (type, external)
      class(hsd_node), intent(in) :: self
      real(dp), intent(out) :: val
      integer, intent(out), optional :: stat
    end subroutine value_get_real

    module subroutine value_get_logical( &
        & self, val, stat)
      implicit none (type, external)
      class(hsd_node), intent(in) :: self
      logical, intent(out) :: val
      integer, intent(out), optional :: stat
    end subroutine value_get_logical

    module subroutine value_get_complex( &
        & self, val, stat)
      implicit none (type, external)
      class(hsd_node), intent(in) :: self
      complex(dp), intent(out) :: val
      integer, intent(out), optional :: stat
    end subroutine value_get_complex

    ! --- Array getters (submodule hsd_value_ops) ---

    module subroutine value_get_int_array( &
        & self, val, stat)
      implicit none (type, external)
      class(hsd_node), intent(in) :: self
      integer, allocatable, intent(out) :: val(:)
      integer, intent(out), optional :: stat
    end subroutine value_get_int_array

    module subroutine value_get_real_array( &
        & self, val, stat)
      implicit none (type, external)
      class(hsd_node), intent(in) :: self
      real(dp), allocatable, intent(out) :: val(:)
      integer, intent(out), optional :: stat
    end subroutine value_get_real_array

    module subroutine value_get_logical_array( &
        & self, val, stat)
      implicit none (type, external)
      class(hsd_node), intent(in) :: self
      logical, allocatable, intent(out) :: val(:)
      integer, intent(out), optional :: stat
    end subroutine value_get_logical_array

    module subroutine value_get_string_array( &
        & self, val, stat)
      implicit none (type, external)
      class(hsd_node), intent(in) :: self
      character(len=:), allocatable, intent(out) :: val(:)
      integer, intent(out), optional :: stat
    end subroutine value_get_string_array

    module subroutine value_get_complex_array( &
        & self, val, stat)
      implicit none (type, external)
      class(hsd_node), intent(in) :: self
      complex(dp), allocatable, intent(out) :: val(:)
      integer, intent(out), optional :: stat
    end subroutine value_get_complex_array

    ! --- Matrix getters (submodule hsd_value_ops) ---

    module subroutine value_get_int_matrix( &
        & self, val, nrows, ncols, stat)
      implicit none (type, external)
      class(hsd_node), intent(in) :: self
      integer, allocatable, intent(out) :: val(:,:)
      integer, intent(out) :: nrows, ncols
      integer, intent(out), optional :: stat
    end subroutine value_get_int_matrix

    module subroutine value_get_real_matrix( &
        & self, val, nrows, ncols, stat)
      implicit none (type, external)
      class(hsd_node), intent(in) :: self
      real(dp), allocatable, intent(out) :: val(:,:)
      integer, intent(out) :: nrows, ncols
      integer, intent(out), optional :: stat
    end subroutine value_get_real_matrix

    module subroutine value_get_complex_matrix( &
        & self, val, nrows, ncols, stat)
      implicit none (type, external)
      class(hsd_node), intent(in) :: self
      complex(dp), allocatable, intent(out) :: val(:,:)
      integer, intent(out) :: nrows, ncols
      integer, intent(out), optional :: stat
    end subroutine value_get_complex_matrix

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

  !> Create a new table node
  subroutine new_table(table, name, attrib, line)
    type(hsd_node), intent(out) :: table
    character(len=*), intent(in), optional :: name
    character(len=*), intent(in), optional :: attrib
    integer, intent(in), optional :: line

    table%node_type = NODE_TYPE_TABLE
    if (present(name)) table%name = to_lower(name)
    if (present(attrib)) then
      if (len_trim(attrib) > 0) table%attrib = attrib
    end if
    if (present(line)) table%line = line

    allocate(table%children(4))
    table%num_children = 0

  end subroutine new_table

  !> Create a new value node
  subroutine new_value(val, name, attrib, line)
    type(hsd_node), intent(out) :: val
    character(len=*), intent(in), optional :: name
    character(len=*), intent(in), optional :: attrib
    integer, intent(in), optional :: line

    val%node_type = NODE_TYPE_VALUE
    if (present(name)) val%name = to_lower(name)
    if (present(attrib)) then
      if (len_trim(attrib) > 0) val%attrib = attrib
    end if
    if (present(line)) val%line = line
    val%value_type = VALUE_TYPE_NONE

  end subroutine new_value

end module hsd_types
