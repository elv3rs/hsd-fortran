!> Non-visitor recursive tree walker for HSD
!>
!> Provides a simpler alternative to the visitor pattern for tree traversal.
!> Instead of extending an abstract type, users pass procedure pointers
!> (callbacks) for handling table and value nodes.
!>
!> Example usage:
!> ```fortran
!> use hsd
!>
!> call hsd_walk(root, on_table=my_table_handler, on_value=my_value_handler)
!>
!> subroutine my_table_handler(table, path, depth, stat)
!>   type(hsd_table), intent(in), target :: table
!>   character(len=*), intent(in) :: path
!>   integer, intent(in) :: depth
!>   integer, intent(out), optional :: stat
!>   print *, "Table: ", trim(path), " depth=", depth
!>   if (present(stat)) stat = 0
!> end subroutine
!>
!> subroutine my_value_handler(val, path, depth, stat)
!>   type(hsd_value), intent(in) :: val
!>   character(len=*), intent(in) :: path
!>   integer, intent(in) :: depth
!>   integer, intent(out), optional :: stat
!>   print *, "Value: ", trim(path)
!>   if (present(stat)) stat = 0
!> end subroutine
!> ```
module hsd_walk_mod
  use hsd_types, only: hsd_node, hsd_table, hsd_value
  implicit none (type, external)
  private

  public :: hsd_walk

  abstract interface
    !> Callback for visiting a table node
    subroutine walk_table_callback(table, path, depth, stat)
      import :: hsd_table
      implicit none (type, external)
      type(hsd_table), intent(in), target :: table
      character(len=*), intent(in) :: path
      integer, intent(in) :: depth
      integer, intent(out), optional :: stat
    end subroutine walk_table_callback

    !> Callback for visiting a value node
    subroutine walk_value_callback(val, path, depth, stat)
      import :: hsd_value
      implicit none (type, external)
      type(hsd_value), intent(in) :: val
      character(len=*), intent(in) :: path
      integer, intent(in) :: depth
      integer, intent(out), optional :: stat
    end subroutine walk_value_callback
  end interface

contains

  !> Walk an HSD tree, calling callbacks for each node
  !>
  !> Performs a depth-first traversal of the tree. Either or both callbacks
  !> may be omitted — only the provided callbacks will be invoked.
  !>
  !> If a callback sets stat to a non-zero value, traversal stops immediately.
  !>
  !> @param root      The root table to start traversal from
  !> @param on_table  Optional callback invoked for each table node
  !> @param on_value  Optional callback invoked for each value node
  !> @param stat      Optional status (non-zero if traversal stopped early)
  subroutine hsd_walk(root, on_table, on_value, stat)
    type(hsd_table), intent(in), target :: root
    procedure(walk_table_callback), optional :: on_table
    procedure(walk_value_callback), optional :: on_value
    integer, intent(out), optional :: stat

    if (present(stat)) stat = 0

    call walk_table_recursive(root, "", 0, on_table, on_value, stat)

  end subroutine hsd_walk

  !> Internal recursive walker
  recursive subroutine walk_table_recursive( &
      & table, path, depth, on_table, on_value, stat)
    type(hsd_table), intent(in), target :: table
    character(len=*), intent(in) :: path
    integer, intent(in) :: depth
    procedure(walk_table_callback), optional :: on_table
    procedure(walk_value_callback), optional :: on_value
    integer, intent(out), optional :: stat

    class(hsd_node), pointer :: child
    character(len=:), allocatable :: child_path
    integer :: i, local_stat

    local_stat = 0

    ! Call table callback
    if (present(on_table)) then
      call on_table(table, path, depth, local_stat)
      if (local_stat /= 0) then
        if (present(stat)) stat = local_stat
        return
      end if
    end if

    ! Walk children
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
        call walk_table_recursive( &
            & child, child_path, depth + 1, on_table, on_value, stat)
        if (present(stat)) then
          if (stat /= 0) return
        end if
      type is (hsd_value)
        if (present(on_value)) then
          call on_value(child, child_path, depth + 1, local_stat)
          if (local_stat /= 0) then
            if (present(stat)) stat = local_stat
            return
          end if
        end if
      end select
    end do

    if (present(stat)) stat = 0

  end subroutine walk_table_recursive

end module hsd_walk_mod
