!> Tests for HSD schema validation
module test_schema_suite
  use hsd, only : &
  & hsd_table, hsd_value, hsd_node, hsd_error_t, hsd_iterator, hsd_node_ptr, &
  & hsd_load, hsd_load_string, hsd_dump, hsd_dump_to_string, hsd_get, &
  & hsd_get_or, hsd_get_matrix, hsd_get_child, hsd_get_table, hsd_get_attrib, &
  & hsd_get_keys, hsd_get_type, hsd_set, hsd_has_child, hsd_has_attrib, &
  & hsd_is_table, hsd_is_value, hsd_is_array, hsd_child_count, hsd_require, &
  & hsd_validate_range, hsd_validate_one_of, hsd_visitor_t, hsd_accept, &
  & hsd_merge, hsd_clone, hsd_remove_child, dp, sp, VALUE_TYPE_NONE, &
  & VALUE_TYPE_ARRAY, VALUE_TYPE_STRING, VALUE_TYPE_INTEGER, VALUE_TYPE_REAL, &
  & VALUE_TYPE_LOGICAL, VALUE_TYPE_COMPLEX, HSD_STAT_OK, HSD_STAT_NOT_FOUND, &
  & HSD_STAT_SYNTAX_ERROR, HSD_STAT_UNCLOSED_TAG, HSD_STAT_UNCLOSED_ATTRIB, &
  & HSD_STAT_UNCLOSED_QUOTE, HSD_STAT_ORPHAN_TEXT, HSD_STAT_INCLUDE_CYCLE, &
  & HSD_STAT_INCLUDE_DEPTH, HSD_STAT_FILE_NOT_FOUND, HSD_STAT_IO_ERROR, &
  & HSD_STAT_TYPE_ERROR, hsd_schema_t, schema_init, schema_destroy, &
  & schema_add_field, schema_add_field_enum, schema_validate, &
  & schema_validate_strict, FIELD_REQUIRED, FIELD_OPTIONAL, FIELD_TYPE_STRING, &
  & FIELD_TYPE_INTEGER, FIELD_TYPE_REAL, FIELD_TYPE_LOGICAL, FIELD_TYPE_ARRAY, &
  & FIELD_TYPE_TABLE, new_table, new_value
  use fortuno_serial, only: is_equal, test => serial_case_item, &
      & check => serial_check, suite => serial_suite_item, test_list
  implicit none (type, external)
  private
  public :: tests

contains

  function tests()
    type(test_list) :: tests
    tests = test_list([&
        suite("schema", test_list([&
            test("required_field_present", test_required_field_present), &
            test("required_field_missing", test_required_field_missing), &
            test("optional_field_missing", test_optional_field_missing), &
            test("type_validation", test_type_validation), &
            test("integer_range", test_integer_range), &
            test("real_range", test_real_range), &
            test("enum_validation", test_enum_validation), &
            test("table_type", test_table_type), &
            test("multiple_errors", test_multiple_errors), &
            test("schema_lifecycle", test_schema_lifecycle), &
            test("nested_paths", test_nested_paths), &
            test("case_insensitive_enum", test_case_insensitive_enum), &
            test("table_vs_value_error", test_table_vs_value_error), &
            test("schema_strict_and_methods", test_schema_strict_and_methods) &
        ]))&
    ])
  end function tests

  !> Test that required field validation passes when present
  subroutine test_required_field_present()
    type(hsd_schema_t) :: schema
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: errors(:)
    type(hsd_error_t), allocatable :: parse_error

    call schema_init(schema)
    call schema_add_field(schema, "Name", FIELD_REQUIRED, FIELD_TYPE_STRING)

    call hsd_load_string('Name = "test"', root, parse_error)
    call check(.not. allocated(parse_error), msg="Parse should succeed")

    call schema_validate(schema, root, errors)
    call check(size(errors) == 0, msg="No errors expected for valid input")

    call schema_destroy(schema)
    call root%destroy()

  end subroutine test_required_field_present

  !> Test that required field validation fails when missing
  subroutine test_required_field_missing()
    type(hsd_schema_t) :: schema
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: errors(:)
    type(hsd_error_t), allocatable :: parse_error

    call schema_init(schema)
    call schema_add_field(schema, "Required", FIELD_REQUIRED, FIELD_TYPE_STRING)

    call hsd_load_string('Other = "value"', root, parse_error)

    call schema_validate(schema, root, errors)
    call check(size(errors) == 1, msg="One error expected for missing required field")

    call schema_destroy(schema)
    call root%destroy()

  end subroutine test_required_field_missing

  !> Test that optional field missing is OK
  subroutine test_optional_field_missing()
    type(hsd_schema_t) :: schema
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: errors(:)
    type(hsd_error_t), allocatable :: parse_error

    call schema_init(schema)
    call schema_add_field(schema, "Optional", FIELD_OPTIONAL, FIELD_TYPE_STRING)

    call hsd_load_string('Other = "value"', root, parse_error)

    call schema_validate(schema, root, errors)
    call check(size(errors) == 0, msg="No errors expected for missing optional field")

    call schema_destroy(schema)
    call root%destroy()

  end subroutine test_optional_field_missing

  !> Test type validation
  subroutine test_type_validation()
    type(hsd_schema_t) :: schema
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: errors(:)
    type(hsd_error_t), allocatable :: parse_error

    call schema_init(schema)
    call schema_add_field(schema, "Count", FIELD_REQUIRED, FIELD_TYPE_INTEGER)

    ! Valid integer
    call hsd_load_string('Count = 42', root, parse_error)
    call schema_validate(schema, root, errors)
    call check(size(errors) == 0, msg="Integer value should validate")
    call root%destroy()

    ! Invalid: string instead of integer
    call hsd_load_string('Count = "not a number"', root, parse_error)
    call schema_validate(schema, root, errors)
    call check(size(errors) == 1, msg="String should fail integer validation")
    call root%destroy()

    call schema_destroy(schema)

  end subroutine test_type_validation

  !> Test integer range validation
  subroutine test_integer_range()
    type(hsd_schema_t) :: schema
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: errors(:)
    type(hsd_error_t), allocatable :: parse_error

    call schema_init(schema)
    call schema_add_field(schema, "Value", FIELD_REQUIRED, FIELD_TYPE_INTEGER, &
                          min_int=1, max_int=100)

    ! Valid: in range
    call hsd_load_string('Value = 50', root, parse_error)
    call schema_validate(schema, root, errors)
    call check(size(errors) == 0, msg="Value in range should validate")
    call root%destroy()

    ! Invalid: below range
    call hsd_load_string('Value = 0', root, parse_error)
    call schema_validate(schema, root, errors)
    call check(size(errors) == 1, msg="Value below range should fail")
    call root%destroy()

    ! Invalid: above range
    call hsd_load_string('Value = 101', root, parse_error)
    call schema_validate(schema, root, errors)
    call check(size(errors) == 1, msg="Value above range should fail")
    call root%destroy()

    ! Edge: at minimum
    call hsd_load_string('Value = 1', root, parse_error)
    call schema_validate(schema, root, errors)
    call check(size(errors) == 0, msg="Value at minimum should validate")
    call root%destroy()

    ! Edge: at maximum
    call hsd_load_string('Value = 100', root, parse_error)
    call schema_validate(schema, root, errors)
    call check(size(errors) == 0, msg="Value at maximum should validate")
    call root%destroy()

    call schema_destroy(schema)

  end subroutine test_integer_range

  !> Test real range validation
  subroutine test_real_range()
    type(hsd_schema_t) :: schema
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: errors(:)
    type(hsd_error_t), allocatable :: parse_error

    call schema_init(schema)
    call schema_add_field(schema, "Temperature", FIELD_REQUIRED, FIELD_TYPE_REAL, &
                          min_real=0.0_dp, max_real=1000.0_dp)

    ! Valid: in range
    call hsd_load_string('Temperature = 300.0', root, parse_error)
    call schema_validate(schema, root, errors)
    call check(size(errors) == 0, msg="Real in range should validate")
    call root%destroy()

    ! Invalid: below range
    call hsd_load_string('Temperature = -1.0', root, parse_error)
    call schema_validate(schema, root, errors)
    call check(size(errors) == 1, msg="Real below range should fail")
    call root%destroy()

    call schema_destroy(schema)

  end subroutine test_real_range

  !> Test enumerated values validation
  subroutine test_enum_validation()
    type(hsd_schema_t) :: schema
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: errors(:)
    type(hsd_error_t), allocatable :: parse_error
    character(len=16) :: allowed(3)

    allowed(1) = "Red"
    allowed(2) = "Green"
    allowed(3) = "Blue"

    call schema_init(schema)
    call schema_add_field_enum(schema, "Color", FIELD_REQUIRED, allowed)

    ! Valid: in list
    call hsd_load_string('Color = "Red"', root, parse_error)
    call schema_validate(schema, root, errors)
    call check(size(errors) == 0, msg="Enum value in list should validate")
    call root%destroy()

    ! Invalid: not in list
    call hsd_load_string('Color = "Yellow"', root, parse_error)
    call schema_validate(schema, root, errors)
    call check(size(errors) == 1, msg="Enum value not in list should fail")
    call root%destroy()

    call schema_destroy(schema)

  end subroutine test_enum_validation

  !> Test table type validation
  subroutine test_table_type()
    type(hsd_schema_t) :: schema
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: errors(:)
    type(hsd_error_t), allocatable :: parse_error

    call schema_init(schema)
    call schema_add_field(schema, "Geometry", FIELD_REQUIRED, FIELD_TYPE_TABLE)

    ! Valid: table
    call hsd_load_string('Geometry { Type = Cluster }', root, parse_error)
    call schema_validate(schema, root, errors)
    call check(size(errors) == 0, msg="Table node should validate")
    call root%destroy()

    ! Invalid: value instead of table
    call hsd_load_string('Geometry = "not a table"', root, parse_error)
    call schema_validate(schema, root, errors)
    call check(size(errors) == 1, msg="Value instead of table should fail")
    call root%destroy()

    call schema_destroy(schema)

  end subroutine test_table_type

  !> Test multiple errors are collected
  subroutine test_multiple_errors()
    type(hsd_schema_t) :: schema
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: errors(:)
    type(hsd_error_t), allocatable :: parse_error

    call schema_init(schema)
    call schema_add_field(schema, "Required1", FIELD_REQUIRED, FIELD_TYPE_STRING)
    call schema_add_field(schema, "Required2", FIELD_REQUIRED, FIELD_TYPE_INTEGER)
    call schema_add_field(schema, "Required3", FIELD_REQUIRED, FIELD_TYPE_LOGICAL)

    ! All missing
    call hsd_load_string('Other = 1', root, parse_error)
    call schema_validate(schema, root, errors)
    call check(size(errors) == 3, msg="Should report all three missing fields")

    call schema_destroy(schema)
    call root%destroy()

  end subroutine test_multiple_errors

  !> Test schema lifecycle (init/destroy)
  subroutine test_schema_lifecycle()
    type(hsd_schema_t) :: schema

    ! Initialize
    call schema_init(schema, name="TestSchema")
    call check(schema%num_fields == 0, msg="New schema should have no fields")

    ! Add some fields
    call schema_add_field(schema, "Field1", FIELD_REQUIRED)
    call schema_add_field(schema, "Field2", FIELD_OPTIONAL)
    call check(schema%num_fields == 2, msg="Should have two fields")

    ! Destroy
    call schema_destroy(schema)
    call check(schema%num_fields == 0, msg="Destroyed schema should have no fields")

  end subroutine test_schema_lifecycle

  !> Test nested path validation
  subroutine test_nested_paths()
    type(hsd_schema_t) :: schema
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: errors(:)
    type(hsd_error_t), allocatable :: parse_error

    call schema_init(schema)
    call schema_add_field(schema, "Section/SubSection/Value", FIELD_REQUIRED, &
                          FIELD_TYPE_INTEGER)

    ! Valid nested path
    call hsd_load_string('Section { SubSection { Value = 42 } }', root, parse_error)
    call check(.not. allocated(parse_error), msg="Parse should succeed")
    call schema_validate(schema, root, errors)
    call check(size(errors) == 0, msg="Nested path should validate")

    call schema_destroy(schema)
    call root%destroy()

  end subroutine test_nested_paths

  !> Test case-insensitive enum matching
  subroutine test_case_insensitive_enum()
    type(hsd_schema_t) :: schema
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: errors(:)
    type(hsd_error_t), allocatable :: parse_error
    character(len=16) :: allowed(2)

    allowed(1) = "Yes"
    allowed(2) = "No"

    call schema_init(schema)
    call schema_add_field_enum(schema, "Enabled", FIELD_REQUIRED, allowed)

    ! Should match case-insensitively
    call hsd_load_string('Enabled = "yes"', root, parse_error)
    call schema_validate(schema, root, errors)
    call check(size(errors) == 0, msg="Lowercase should match uppercase enum")
    call root%destroy()

    call hsd_load_string('Enabled = "YES"', root, parse_error)
    call schema_validate(schema, root, errors)
    call check(size(errors) == 0, msg="Uppercase should match mixed case enum")
    call root%destroy()

    call schema_destroy(schema)

  end subroutine test_case_insensitive_enum


  !> Test table vs value mismatch errors
  subroutine test_table_vs_value_error()
    type(hsd_schema_t) :: schema
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: errors(:)
    type(hsd_error_t), allocatable :: parse_error
    type(hsd_table) :: subtable

    call schema_init(schema)
    call schema_add_field(schema, "Scalar", FIELD_REQUIRED, FIELD_TYPE_INTEGER)
    call schema_add_field(schema, "Group", FIELD_REQUIRED, FIELD_TYPE_TABLE)

    ! 1. Providing a table where a scalar is expected
    call hsd_load_string("Scalar { Value = 42 }" // char(10) // "Group = 42", root, parse_error)
    call check(.not. allocated(parse_error), msg="Parse should succeed in table_vs_value")

    if (.not. allocated(parse_error)) then
      call schema_validate(schema, root, errors)
      ! Should have 2 errors:
      ! - Scalar is a table, expected integer
      ! - Group is a value, expected table
      call check(size(errors) == 2, msg="Should report both table-vs-value mismatches")
    end if

    call schema_destroy(schema)
    call root%destroy()

  end subroutine test_table_vs_value_error


  !> Test strict validation and OO methods
  subroutine test_schema_strict_and_methods()
    type(hsd_schema_t) :: schema
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: errors(:)
    type(hsd_error_t), allocatable :: parse_error

    ! Use OO method for initialization
    call schema%init(name="OO_Schema", allow_unknown=.false.)

    ! Use OO method for adding fields
    call schema%add_field("Name", FIELD_REQUIRED, FIELD_TYPE_STRING)

    call hsd_load_string('Name = "Test"', root, parse_error)

    ! Use OO method for validation
    call schema%validate(root, errors)
    call check(size(errors) == 0, msg="OO validation should work")

    ! Call strict validation
    call schema_validate_strict(schema, root, errors)
    call check(size(errors) == 0, msg="Strict validation should work")

    call schema%destroy()
    call root%destroy()

  end subroutine test_schema_strict_and_methods


end module test_schema_suite
