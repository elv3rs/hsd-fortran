!> Count visitor for testing
module count_visitor_mod
  use hsd, only: hsd_visitor_t, hsd_table, hsd_value
  implicit none (type, external)
  private
  public :: count_visitor

  type, extends(hsd_visitor_t) :: count_visitor
    integer :: table_count = 0
    integer :: value_count = 0
  contains
    procedure :: visit_table => cv_visit_table
    procedure :: visit_value => cv_visit_value
  end type count_visitor

contains

  subroutine cv_visit_table(self, table, path, depth, stat)
    class(count_visitor), intent(inout) :: self
    type(hsd_table), intent(in), target :: table
    character(len=*), intent(in) :: path
    integer, intent(in) :: depth
    integer, intent(out), optional :: stat

    self%table_count = self%table_count + 1
    if (present(stat)) stat = 0
  end subroutine cv_visit_table

  subroutine cv_visit_value(self, val, path, depth, stat)
    class(count_visitor), intent(inout) :: self
    type(hsd_value), intent(in) :: val
    character(len=*), intent(in) :: path
    integer, intent(in) :: depth
    integer, intent(out), optional :: stat

    self%value_count = self%value_count + 1
    if (present(stat)) stat = 0
  end subroutine cv_visit_value

end module count_visitor_mod
