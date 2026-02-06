!> Helper module defining visitor types for the visitor demo.
!>
!> Separated from the main program because Fortran requires derived-type
!> extensions of abstract types to be defined in a module scope.
module visitor_types
  use hsd, only: hsd_table, hsd_value, hsd_visitor_t
  implicit none (type, external)
  private

  !> Visitor that prints the tree structure with indentation
  type, extends(hsd_visitor_t), public :: tree_printer
  contains
    procedure :: visit_table => printer_visit_table
    procedure :: visit_value => printer_visit_value
  end type tree_printer

  !> Visitor that counts tables and values
  type, extends(hsd_visitor_t), public :: tree_counter
    integer :: num_tables = 0
    integer :: num_values = 0
  contains
    procedure :: visit_table => counter_visit_table
    procedure :: visit_value => counter_visit_value
  end type tree_counter

contains

  subroutine printer_visit_table(self, table, path, depth, stat)
    class(tree_printer), intent(inout) :: self
    type(hsd_table), intent(in), target :: table
    character(len=*), intent(in) :: path
    integer, intent(in) :: depth
    integer, intent(out), optional :: stat

    if (len_trim(path) > 0) then
      print '(A,A,A)', repeat("  ", depth), "[table] ", trim(path)
    end if
    if (present(stat)) stat = 0

  end subroutine printer_visit_table

  subroutine printer_visit_value(self, val, path, depth, stat)
    class(tree_printer), intent(inout) :: self
    type(hsd_value), intent(in) :: val
    character(len=*), intent(in) :: path
    integer, intent(in) :: depth
    integer, intent(out), optional :: stat

    character(len=:), allocatable :: str_val

    call val%get_string(str_val)
    print '(A,A,A,A)', repeat("  ", depth), trim(path), " = ", str_val
    if (present(stat)) stat = 0

  end subroutine printer_visit_value

  subroutine counter_visit_table(self, table, path, depth, stat)
    class(tree_counter), intent(inout) :: self
    type(hsd_table), intent(in), target :: table
    character(len=*), intent(in) :: path
    integer, intent(in) :: depth
    integer, intent(out), optional :: stat

    self%num_tables = self%num_tables + 1
    if (present(stat)) stat = 0

  end subroutine counter_visit_table

  subroutine counter_visit_value(self, val, path, depth, stat)
    class(tree_counter), intent(inout) :: self
    type(hsd_value), intent(in) :: val
    character(len=*), intent(in) :: path
    integer, intent(in) :: depth
    integer, intent(out), optional :: stat

    self%num_values = self%num_values + 1
    if (present(stat)) stat = 0

  end subroutine counter_visit_value

end module visitor_types
