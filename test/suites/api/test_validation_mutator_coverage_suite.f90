!> Validation and mutator coverage tests
module test_validation_mutator_coverage_suite
  use hsd
  use build_env, only: build_dir
  use fortuno_serial, only: test => serial_case_item, check => serial_check, &
      & suite => serial_suite_item, test_list
  implicit none (type, external)

  private
  public :: validation_mutator_coverage_tests

contains

  function validation_mutator_coverage_tests() result(test_items)
    type(test_list) :: test_items

    test_items = test_list([&
            test("require_missing_field", test_require_missing_field), &
            test("require_wrong_type", test_require_wrong_type), &
            test("require_table_vs_value", test_require_table_vs_value), &
            test("validate_range_violation", test_validate_range_violation), &
            test("validate_one_of_violation", test_validate_one_of_violation), &
            test("mutator_set_operations", test_mutator_set_operations), &
            test("require_with_context", test_require_with_context), &
            test("get_with_unit_conversion", test_get_with_unit_conversion), &
            test("mutator_error_paths", test_mutator_error_paths) &
        ])
  end function validation_mutator_coverage_tests

  !> Test hsd_require with missing field
  subroutine test_require_missing_field()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error

    call hsd_load_string("x = 1", root, error)
    call check(.not. allocated(error), msg="Parse OK")

    ! Require a field that doesn't exist
    call hsd_require(root, "missing_field", error)
    call check(allocated(error), msg="Error for missing field")

    call root%destroy()
  end subroutine test_require_missing_field

  !> Test hsd_require with wrong type
  subroutine test_require_wrong_type()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error

    call hsd_load_string("x = hello", root, error)
    call check(.not. allocated(error), msg="Parse OK")

    ! Require integer type but x is string
    call hsd_require(root, "x", error, expected_type=VALUE_TYPE_INTEGER)
    call check(allocated(error), msg="Error for wrong type")

    call root%destroy()
  end subroutine test_require_wrong_type

  !> Test hsd_require expecting value but got table
  subroutine test_require_table_vs_value()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error

    call hsd_load_string("block { x = 1 }", root, error)
    call check(.not. allocated(error), msg="Parse OK")

    ! Require integer type but block is table
    call hsd_require(root, "block", error, expected_type=VALUE_TYPE_INTEGER)
    call check(allocated(error), msg="Error for table when expecting value")

    call root%destroy()
  end subroutine test_require_table_vs_value

  !> Test hsd_validate_range with violation
  subroutine test_validate_range_violation()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error

    call hsd_load_string("val = 150.0", root, error)
    call check(.not. allocated(error), msg="Parse OK")

    ! Validate range 0-100, value is 150
    call hsd_validate_range(root, "val", 0.0_dp, 100.0_dp, error)
    call check(allocated(error), msg="Error for out of range")

    call root%destroy()
  end subroutine test_validate_range_violation

  !> Test hsd_validate_one_of with violation
  subroutine test_validate_one_of_violation()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error

    call hsd_load_string('mode = "invalid"', root, error)
    call check(.not. allocated(error), msg="Parse OK")

    ! Validate against allowed values
    call hsd_validate_one_of(root, "mode", ["fast  ", "slow  ", "medium"], error)
    call check(allocated(error), msg="Error for invalid choice")

    call root%destroy()
  end subroutine test_validate_one_of_violation

  !> Test hsd_set operations (mutators)
  subroutine test_mutator_set_operations()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    integer :: val_int
    real(dp) :: val_real
    logical :: val_log
    character(len=:), allocatable :: val_str

    call new_table(root)

    ! Set various types
    call hsd_set(root, "int_val", 42)
    call hsd_set(root, "real_val", 3.14_dp)
    call hsd_set(root, "log_val", .true.)
    call hsd_set(root, "str_val", "hello")

    ! Get them back
    call hsd_get(root, "int_val", val_int)
    call check(val_int == 42, msg="Int set/get")

    call hsd_get(root, "real_val", val_real)
    call check(abs(val_real - 3.14_dp) < 1e-10_dp, msg="Real set/get")

    call hsd_get(root, "log_val", val_log)
    call check(val_log, msg="Logical set/get")

    call hsd_get(root, "str_val", val_str)
    call check(val_str == "hello", msg="String set/get")

    call root%destroy()
  end subroutine test_mutator_set_operations

  !> Test hsd_require with context string
  subroutine test_require_with_context()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error

    call new_table(root)

    ! Require field with context
    call hsd_require(root, "missing", error, context="Config validation")
    call check(allocated(error), msg="Error with context")

    call root%destroy()
  end subroutine test_require_with_context

  !> Test hsd_get_with_unit for unit conversion
  subroutine test_get_with_unit_conversion()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    real(dp) :: val
    integer :: stat

    ! Create a value with unit attribute
    call hsd_load_string("distance [m] = 100.0", root, error)
    call check(.not. allocated(error), msg="Parse OK")

    ! Test unit conversion
    call hsd_get_with_unit(root, "distance", val, "cm", simple_length_converter, stat)
    call check(stat == HSD_STAT_OK, msg="Conversion successful")
    call check(abs(val - 10000.0_dp) < 1e-10_dp, msg="Converted m to cm")

    call root%destroy()
  end subroutine test_get_with_unit_conversion

  !> Simple length unit converter for testing
  pure function simple_length_converter(value, from_unit, to_unit) result(converted)
    real(dp), intent(in) :: value
    character(len=*), intent(in) :: from_unit, to_unit
    real(dp) :: converted
    real(dp) :: to_meters, from_meters

    ! Convert input to meters
    select case (trim(from_unit))
    case ('m')
      from_meters = 1.0_dp
    case ('cm')
      from_meters = 0.01_dp
    case ('km')
      from_meters = 1000.0_dp
    case default
      from_meters = 1.0_dp
    end select

    ! Convert from meters to target
    select case (trim(to_unit))
    case ('m')
      to_meters = 1.0_dp
    case ('cm')
      to_meters = 0.01_dp
    case ('km')
      to_meters = 1000.0_dp
    case default
      to_meters = 1.0_dp
    end select

    converted = value * from_meters / to_meters
  end function simple_length_converter

  !> Test mutator error paths (trying to set through a value node)
  subroutine test_mutator_error_paths()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    integer :: stat

    ! Create a value node
    call hsd_load_string("x = 42", root, error)
    call check(.not. allocated(error), msg="Parse OK")

    ! Try to set a child through the value node (should fail)
    call hsd_set(root, "x/y", 100, stat=stat)
    call check(stat /= HSD_STAT_OK, msg="Should fail when navigating through value")

    call root%destroy()
  end subroutine test_mutator_error_paths

end module test_validation_mutator_coverage_suite
