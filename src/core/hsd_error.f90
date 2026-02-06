!> Error handling for the HSD parser
!>
!> This module provides error types and utilities for reporting parsing
!> errors with file location information.
module hsd_error
  implicit none (type, external)
  private

  public :: hsd_error_t
  public :: HSD_STAT_OK, HSD_STAT_SYNTAX_ERROR, HSD_STAT_UNCLOSED_TAG
  public :: HSD_STAT_UNCLOSED_ATTRIB, HSD_STAT_UNCLOSED_QUOTE
  public :: HSD_STAT_ORPHAN_TEXT, HSD_STAT_INCLUDE_CYCLE, HSD_STAT_INCLUDE_DEPTH
  public :: HSD_STAT_FILE_NOT_FOUND, HSD_STAT_IO_ERROR, HSD_STAT_TYPE_ERROR
  public :: HSD_STAT_NOT_FOUND, HSD_STAT_SCHEMA_ERROR
  public :: make_error, error_message
  public :: make_syntax_error, make_type_error

  !> Status codes
  integer, parameter :: HSD_STAT_OK = 0
  integer, parameter :: HSD_STAT_SYNTAX_ERROR = 1
  integer, parameter :: HSD_STAT_UNCLOSED_TAG = 2
  integer, parameter :: HSD_STAT_UNCLOSED_ATTRIB = 3
  integer, parameter :: HSD_STAT_UNCLOSED_QUOTE = 4
  integer, parameter :: HSD_STAT_ORPHAN_TEXT = 5
  integer, parameter :: HSD_STAT_INCLUDE_CYCLE = 6
  integer, parameter :: HSD_STAT_INCLUDE_DEPTH = 7
  integer, parameter :: HSD_STAT_FILE_NOT_FOUND = 8
  integer, parameter :: HSD_STAT_IO_ERROR = 9
  integer, parameter :: HSD_STAT_TYPE_ERROR = 10
  integer, parameter :: HSD_STAT_NOT_FOUND = 11
  integer, parameter :: HSD_STAT_SCHEMA_ERROR = 20

  !> Error type with detailed information
  type :: hsd_error_t
    !> Error code
    integer :: code = HSD_STAT_OK
    !> Human-readable error message
    character(len=:), allocatable :: message
    !> File where error occurred
    character(len=:), allocatable :: filename
    !> Line number where error started
    integer :: line_start = 0
    !> Line number where error ended
    integer :: line_end = 0
    !> Column number where error occurred (optional)
    integer :: column = 0
    !> Expected token or value (for context)
    character(len=:), allocatable :: expected
    !> Actual token or value that caused error
    character(len=:), allocatable :: actual
    !> Hint or suggestion for fixing the error
    character(len=:), allocatable :: hint
  contains
    procedure :: print => error_print
  end type hsd_error_t

contains

  !> Create an error with message and location
  subroutine make_error(error, code, message, filename, line_start, line_end, column, &
      expected, actual, hint)
    type(hsd_error_t), allocatable, intent(out) :: error
    integer, intent(in) :: code
    character(len=*), intent(in) :: message
    character(len=*), intent(in), optional :: filename
    integer, intent(in), optional :: line_start
    integer, intent(in), optional :: line_end
    integer, intent(in), optional :: column
    character(len=*), intent(in), optional :: expected
    character(len=*), intent(in), optional :: actual
    character(len=*), intent(in), optional :: hint

    allocate(error)
    error%code = code
    error%message = message

    if (present(filename)) then
      error%filename = filename
    else
      error%filename = "<unknown>"
    end if

    if (present(line_start)) then
      error%line_start = line_start
    end if

    if (present(line_end)) then
      error%line_end = line_end
    else if (present(line_start)) then
      error%line_end = line_start
    end if

    if (present(column)) then
      error%column = column
    end if

    if (present(expected)) then
      error%expected = expected
    end if

    if (present(actual)) then
      error%actual = actual
    end if

    if (present(hint)) then
      error%hint = hint
    end if

  end subroutine make_error

  !> Get a descriptive message for an error code
  pure function error_message(code) result(msg)
    integer, intent(in) :: code
    character(len=:), allocatable :: msg

    select case (code)
    case (HSD_STAT_OK)
      msg = "No error"
    case (HSD_STAT_SYNTAX_ERROR)
      msg = "Syntax error"
    case (HSD_STAT_UNCLOSED_TAG)
      msg = "Unclosed tag"
    case (HSD_STAT_UNCLOSED_ATTRIB)
      msg = "Unclosed attribute"
    case (HSD_STAT_UNCLOSED_QUOTE)
      msg = "Unclosed quotation"
    case (HSD_STAT_ORPHAN_TEXT)
      msg = "Orphan text outside of any tag"
    case (HSD_STAT_INCLUDE_CYCLE)
      msg = "Cyclic include detected"
    case (HSD_STAT_INCLUDE_DEPTH)
      msg = "Maximum include depth exceeded"
    case (HSD_STAT_FILE_NOT_FOUND)
      msg = "File not found"
    case (HSD_STAT_IO_ERROR)
      msg = "I/O error"
    case (HSD_STAT_TYPE_ERROR)
      msg = "Type conversion error"
    case (HSD_STAT_NOT_FOUND)
      msg = "Key not found"
    case (HSD_STAT_SCHEMA_ERROR)
      msg = "Schema validation error"
    case default
      msg = "Unknown error"
    end select

  end function error_message

  !> Print error to standard output
  subroutine error_print(self, unit)
    class(hsd_error_t), intent(in) :: self
    integer, intent(in), optional :: unit

    integer :: out_unit
    character(len=256) :: line_info, col_info

    if (present(unit)) then
      out_unit = unit
    else
      out_unit = 6  ! stdout
    end if

    ! Format location information
    if (self%line_start > 0) then
      if (self%line_end > self%line_start) then
        write(line_info, '(A,I0,A,I0)') "lines ", self%line_start, "-", self%line_end
      else
        write(line_info, '(A,I0)') "line ", self%line_start
      end if

      if (self%column > 0) then
        write(col_info, '(A,I0)') ", column ", self%column
      else
        col_info = ""
      end if

      write(out_unit, '(A,A,A,A,A,A,A)') &
        "Error in '", trim(self%filename), "' at ", trim(line_info), &
        trim(col_info), ": ", self%message
    else
      write(out_unit, '(A,A,A,A)') &
        "Error in '", trim(self%filename), "': ", self%message
    end if

    ! Print expected vs actual if available
    if (allocated(self%expected) .and. allocated(self%actual)) then
      write(out_unit, '(A,A)') "  Expected: ", self%expected
      write(out_unit, '(A,A)') "  Got:      ", self%actual
    else if (allocated(self%expected)) then
      write(out_unit, '(A,A)') "  Expected: ", self%expected
    else if (allocated(self%actual)) then
      write(out_unit, '(A,A)') "  Got:      ", self%actual
    end if

    ! Print hint if available
    if (allocated(self%hint)) then
      write(out_unit, '(A,A)') "  Hint: ", self%hint
    end if

  end subroutine error_print

  !> Create a syntax error with expected vs actual context
  subroutine make_syntax_error(error, message, filename, line, column, expected, actual, hint)
    type(hsd_error_t), allocatable, intent(out) :: error
    character(len=*), intent(in) :: message
    character(len=*), intent(in), optional :: filename
    integer, intent(in), optional :: line
    integer, intent(in), optional :: column
    character(len=*), intent(in), optional :: expected
    character(len=*), intent(in), optional :: actual
    character(len=*), intent(in), optional :: hint

    allocate(error)
    error%code = HSD_STAT_SYNTAX_ERROR
    error%message = message

    if (present(filename)) then
      error%filename = filename
    else
      error%filename = "<unknown>"
    end if

    if (present(line)) then
      error%line_start = line
      error%line_end = line
    end if

    if (present(column)) then
      error%column = column
    end if

    if (present(expected)) then
      error%expected = expected
    end if

    if (present(actual)) then
      error%actual = actual
    end if

    if (present(hint)) then
      error%hint = hint
    end if

  end subroutine make_syntax_error

  !> Create a type error with expected vs actual types
  subroutine make_type_error(error, message, filename, line, expected, actual, hint)
    type(hsd_error_t), allocatable, intent(out) :: error
    character(len=*), intent(in) :: message
    character(len=*), intent(in), optional :: filename
    integer, intent(in), optional :: line
    character(len=*), intent(in), optional :: expected
    character(len=*), intent(in), optional :: actual
    character(len=*), intent(in), optional :: hint

    allocate(error)
    error%code = HSD_STAT_TYPE_ERROR
    error%message = message

    if (present(filename)) then
      error%filename = filename
    else
      error%filename = "<unknown>"
    end if

    if (present(line)) then
      error%line_start = line
      error%line_end = line
    end if

    if (present(expected)) then
      error%expected = expected
    end if

    if (present(actual)) then
      error%actual = actual
    end if

    if (present(hint)) then
      error%hint = hint
    end if

  end subroutine make_type_error

end module hsd_error
