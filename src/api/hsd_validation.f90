!> HSD validation and verification
!>
!> This module provides utilities for validating HSD data, including
!> required field checking, range validation, and unit conversion support.
module hsd_validation
  use hsd_constants, only: dp
  use hsd_utils, only: to_lower
  use hsd_error, only: hsd_error_t, make_error, HSD_STAT_OK, HSD_STAT_NOT_FOUND, &
    HSD_STAT_TYPE_ERROR
  use hsd_types, only: hsd_node, hsd_table, hsd_value, &
    VALUE_TYPE_NONE, VALUE_TYPE_STRING, VALUE_TYPE_INTEGER, &
    VALUE_TYPE_REAL, VALUE_TYPE_LOGICAL, VALUE_TYPE_ARRAY, VALUE_TYPE_COMPLEX
  use hsd_query, only: hsd_get_child
  implicit none (type, external)
  private

  ! Public procedures
  public :: hsd_require
  public :: hsd_validate_range
  public :: hsd_validate_one_of
  public :: hsd_get_with_unit

contains

  !> Require that a path exists and optionally check its type
  !>
  !> If the path doesn't exist or type doesn't match, creates a descriptive error.
  !>
  !> Both optional arguments should be passed as keyword arguments for clarity:
  !>
  !>   call hsd_require(root, "Driver/MaxSteps", error, &
  !>     expected_type=FIELD_TYPE_INTEGER, context="load_config")
  subroutine hsd_require(table, path, error, expected_type, context)
    type(hsd_table), intent(in), target :: table
    character(len=*), intent(in) :: path
    type(hsd_error_t), allocatable, intent(out) :: error
    integer, intent(in), optional :: expected_type
    character(len=*), intent(in), optional :: context

    class(hsd_node), pointer :: child
    integer :: local_stat, actual_type
    character(len=:), allocatable :: ctx_prefix

    call hsd_get_child(table, path, child, local_stat)

    if (present(context)) then
      ctx_prefix = context // ": "
    else
      ctx_prefix = ""
    end if

    if (local_stat /= 0 .or. .not. associated(child)) then
      call make_error(error, HSD_STAT_NOT_FOUND, &
        ctx_prefix // "Required field '" // path // "' not found")
      return
    end if

    if (present(expected_type)) then
      select type (child)
      type is (hsd_value)
        actual_type = child%value_type
        if (expected_type /= actual_type) then
          call make_error(error, HSD_STAT_TYPE_ERROR, &
            ctx_prefix // "Field '" // path // "' has wrong type: expected " // &
            type_name(expected_type) // ", got " // type_name(actual_type))
        end if
      type is (hsd_table)
        if (expected_type /= VALUE_TYPE_NONE) then
          call make_error(error, HSD_STAT_TYPE_ERROR, &
            ctx_prefix // "Field '" // path // "' is a table, expected value of type " // &
            type_name(expected_type))
        end if
      end select
    end if

  end subroutine hsd_require

  !> Get a human-readable name for a value type
  pure function type_name(val_type) result(name)
    integer, intent(in) :: val_type
    character(len=:), allocatable :: name

    select case (val_type)
    case (VALUE_TYPE_NONE)
      name = "none"
    case (VALUE_TYPE_STRING)
      name = "string"
    case (VALUE_TYPE_INTEGER)
      name = "integer"
    case (VALUE_TYPE_REAL)
      name = "real"
    case (VALUE_TYPE_LOGICAL)
      name = "logical"
    case (VALUE_TYPE_ARRAY)
      name = "array"
    case (VALUE_TYPE_COMPLEX)
      name = "complex"
    case default
      name = "unknown"
    end select

  end function type_name

  !> Validate that a real value is within a specified range
  subroutine hsd_validate_range(table, path, min_val, max_val, error, context)
    type(hsd_table), intent(in), target :: table
    character(len=*), intent(in) :: path
    real(dp), intent(in) :: min_val, max_val
    type(hsd_error_t), allocatable, intent(out) :: error
    character(len=*), intent(in), optional :: context

    class(hsd_node), pointer :: child
    real(dp) :: val
    integer :: local_stat
    character(len=32) :: min_str, max_str, val_str
    character(len=:), allocatable :: ctx_prefix

    call hsd_get_child(table, path, child, local_stat)

    if (present(context)) then
      ctx_prefix = context // ": "
    else
      ctx_prefix = ""
    end if

    if (local_stat /= HSD_STAT_OK .or. .not. associated(child)) then
      call make_error(error, local_stat, &
        ctx_prefix // "Field '" // path // "' not found or invalid type for range validation")
      return
    end if

    select type (child)
    type is (hsd_value)
      call child%get_real(val, local_stat)
      if (local_stat /= HSD_STAT_OK) then
        call make_error(error, HSD_STAT_TYPE_ERROR, &
          ctx_prefix // "Field '" // path // "' is not a real number")
        return
      end if

      if (val < min_val .or. val > max_val) then
        write(min_str, '(G0)') min_val
        write(max_str, '(G0)') max_val
        write(val_str, '(G0)') val
        call make_error(error, HSD_STAT_TYPE_ERROR, &
          ctx_prefix // "Field '" // path // "' value " // trim(val_str) // &
          " is outside valid range [" // trim(min_str) // ", " // trim(max_str) // "]")
      end if
    class default
      call make_error(error, HSD_STAT_TYPE_ERROR, &
        ctx_prefix // "Field '" // path // "' is not a value node")
    end select

  end subroutine hsd_validate_range

  !> Validate that a string value is one of the allowed choices
  subroutine hsd_validate_one_of(table, path, choices, error, context)
    type(hsd_table), intent(in), target :: table
    character(len=*), intent(in) :: path
    character(len=*), intent(in) :: choices(:)
    type(hsd_error_t), allocatable, intent(out) :: error
    character(len=*), intent(in), optional :: context

    class(hsd_node), pointer :: child
    character(len=:), allocatable :: val, choices_str, ctx_prefix
    integer :: i, local_stat
    logical :: found

    call hsd_get_child(table, path, child, local_stat)

    if (present(context)) then
      ctx_prefix = context // ": "
    else
      ctx_prefix = ""
    end if

    if (local_stat /= HSD_STAT_OK .or. .not. associated(child)) then
      call make_error(error, local_stat, &
        ctx_prefix // "Field '" // path // "' not found")
      return
    end if

    select type (child)
    type is (hsd_value)
      call child%get_string(val, local_stat)
      if (local_stat /= HSD_STAT_OK) then
        call make_error(error, HSD_STAT_TYPE_ERROR, &
          ctx_prefix // "Field '" // path // "' is not a string")
        return
      end if

      found = .false.
      do i = 1, size(choices)
        if (to_lower(val) == to_lower(choices(i))) then
          found = .true.
          exit
        end if
      end do

      if (.not. found) then
        choices_str = ""
        do i = 1, size(choices)
          if (i > 1) choices_str = choices_str // ", "
          choices_str = choices_str // "'" // trim(choices(i)) // "'"
        end do
        call make_error(error, HSD_STAT_TYPE_ERROR, &
          ctx_prefix // "Field '" // path // "' value '" // val // &
          "' is not one of: " // choices_str)
      end if
    class default
      call make_error(error, HSD_STAT_TYPE_ERROR, &
        ctx_prefix // "Field '" // path // "' is not a value node")
    end select

  end subroutine hsd_validate_one_of

  !> Get a real value with automatic unit conversion
  !>
  !> The unit is read from the node's attribute and converted to the target unit.
  !> The converter function takes (value, from_unit, to_unit) and returns the converted value.
  !>
  !> Example:
  !>   For input `Temperature [Kelvin] = 300`, calling
  !>   `hsd_get_with_unit(root, "Temperature", val, "Celsius", converter)`
  !>   would call `converter(300.0, "Kelvin", "Celsius")` to get the result.
  subroutine hsd_get_with_unit(table, path, val, target_unit, converter, stat)
    type(hsd_table), intent(in), target :: table
    character(len=*), intent(in) :: path
    real(dp), intent(out) :: val
    character(len=*), intent(in) :: target_unit
    interface
      pure function converter(value, from_unit, to_unit) result(converted)
        import :: dp
        implicit none (type, external)
        real(dp), intent(in) :: value
        character(len=*), intent(in) :: from_unit, to_unit
        real(dp) :: converted
      end function converter
    end interface
    integer, intent(out), optional :: stat

    class(hsd_node), pointer :: child
    character(len=:), allocatable :: source_unit
    real(dp) :: raw_val
    integer :: local_stat

    val = 0.0_dp

    call hsd_get_child(table, path, child, local_stat)
    if (local_stat /= 0 .or. .not. associated(child)) then
      if (present(stat)) stat = HSD_STAT_NOT_FOUND
      return
    end if

    select type (child)
    type is (hsd_value)
      call child%get_real(raw_val, local_stat)
      if (local_stat /= HSD_STAT_OK) then
        if (present(stat)) stat = local_stat
        return
      end if

      if (allocated(child%attrib)) then
        source_unit = child%attrib
      else
        source_unit = target_unit  ! No conversion needed
      end if

      val = converter(raw_val, source_unit, target_unit)
      if (present(stat)) stat = HSD_STAT_OK

    class default
      if (present(stat)) stat = HSD_STAT_TYPE_ERROR
    end select

  end subroutine hsd_get_with_unit

end module hsd_validation
