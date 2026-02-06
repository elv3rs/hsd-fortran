!> Visitor pattern for HSD tree traversal
!>
!> This module provides an abstract visitor type that can be extended
!> to implement custom tree traversal logic without needing to manually
!> iterate over nodes.
!>
!> Example usage:
!> ```fortran
!> type, extends(hsd_visitor) :: my_printer
!> contains
!>   procedure :: visit_table => print_table
!>   procedure :: visit_value => print_value
!> end type
!>
!> subroutine print_table(self, table, path, depth, stat)
!>   class(my_printer), intent(inout) :: self
!>   type(hsd_table), intent(in) :: table
!>   character(len=*), intent(in) :: path
!>   integer, intent(in) :: depth
!>   integer, intent(out), optional :: stat
!>   print *, "Table: ", path
!>   if (present(stat)) stat = 0
!> end subroutine
!> ```
module hsd_visitor
  use hsd_types, only: hsd_node, hsd_table, hsd_value
  implicit none (type, external)
  private

  public :: hsd_visitor_t, hsd_accept

  !> Abstract visitor type for tree traversal
  !>
  !> Extend this type and implement visit_table and visit_value
  !> to define custom behavior when visiting each node type.
  type, abstract :: hsd_visitor_t
  contains
    !> Called when visiting a table node
    procedure(visit_table_if), deferred :: visit_table
    !> Called when visiting a value node
    procedure(visit_value_if), deferred :: visit_value
    !> Called when leaving a table node (after children are visited)
    procedure :: leave_table => default_leave_table
  end type hsd_visitor_t

  abstract interface
    !> Visit a table node
    !>
    !> @param self The visitor instance
    !> @param table The table being visited
    !> @param path The path to this table from root (e.g., "parent/child")
    !> @param depth The depth in the tree (0 = root)
    !> @param stat Optional status (non-zero to stop traversal)
    subroutine visit_table_if(self, table, path, depth, stat)
      import :: hsd_visitor_t, hsd_table
      implicit none (type, external)
      class(hsd_visitor_t), intent(inout) :: self
      type(hsd_table), intent(in), target :: table
      character(len=*), intent(in) :: path
      integer, intent(in) :: depth
      integer, intent(out), optional :: stat
    end subroutine visit_table_if

    !> Visit a value node
    !>
    !> @param self The visitor instance
    !> @param val The value being visited
    !> @param path The path to this value from root
    !> @param depth The depth in the tree
    !> @param stat Optional status (non-zero to stop traversal)
    subroutine visit_value_if(self, val, path, depth, stat)
      import :: hsd_visitor_t, hsd_value
      implicit none (type, external)
      class(hsd_visitor_t), intent(inout) :: self
      type(hsd_value), intent(in) :: val
      character(len=*), intent(in) :: path
      integer, intent(in) :: depth
      integer, intent(out), optional :: stat
    end subroutine visit_value_if
  end interface

contains

  !> Default implementation of leave_table (does nothing)
  subroutine default_leave_table(self, table, path, depth, stat)
    class(hsd_visitor_t), intent(inout) :: self
    type(hsd_table), intent(in), target :: table
    character(len=*), intent(in) :: path
    integer, intent(in) :: depth
    integer, intent(out), optional :: stat
    if (present(stat)) stat = 0
  end subroutine default_leave_table

  !> Accept a visitor and traverse the tree
  !>
  !> Performs a depth-first traversal of the tree, calling the visitor's
  !> visit_table and visit_value methods for each node.
  !>
  !> @param root The root table to start traversal from
  !> @param visitor The visitor instance to call
  !> @param stat Optional status (non-zero if traversal stopped early)
  recursive subroutine hsd_accept(root, visitor, stat)
    type(hsd_table), intent(in), target :: root
    class(hsd_visitor_t), intent(inout) :: visitor
    integer, intent(out), optional :: stat

    call accept_table(root, visitor, "", 0, stat)

  end subroutine hsd_accept

  !> Internal recursive helper for table traversal
  recursive subroutine accept_table(table, visitor, path, depth, stat)
    type(hsd_table), intent(in), target :: table
    class(hsd_visitor_t), intent(inout) :: visitor
    character(len=*), intent(in) :: path
    integer, intent(in) :: depth
    integer, intent(out), optional :: stat

    class(hsd_node), pointer :: child
    character(len=:), allocatable :: child_path
    integer :: i, local_stat

    ! Initialize local_stat to success
    local_stat = 0

    ! Visit this table first
    call visitor%visit_table(table, path, depth, local_stat)
    if (local_stat /= 0) then
      if (present(stat)) stat = local_stat
      return
    end if

    ! Then visit all children
    do i = 1, table%num_children
      call table%get_child(i, child)
      if (.not. associated(child)) cycle

      ! Build child path
      if (len_trim(path) == 0) then
        if (allocated(child%name)) then
          child_path = child%name
        else
          child_path = ""
        end if
      else
        if (allocated(child%name)) then
          child_path = path // "/" // child%name
        else
          child_path = path
        end if
      end if

      select type (child)
      type is (hsd_table)
        call accept_table(child, visitor, child_path, depth + 1, local_stat)
        if (local_stat /= 0) then
          if (present(stat)) stat = local_stat
          return
        end if
      type is (hsd_value)
        call visitor%visit_value(child, child_path, depth + 1, local_stat)
        if (local_stat /= 0) then
          if (present(stat)) stat = local_stat
          return
        end if
      end select
    end do

    ! Call leave_table after processing children
    call visitor%leave_table(table, path, depth, local_stat)
    if (local_stat /= 0) then
      if (present(stat)) stat = local_stat
      return
    end if

    if (present(stat)) stat = 0

  end subroutine accept_table

end module hsd_visitor
