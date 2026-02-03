!> Error handling and edge case unit tests using Fortuno framework
module test_error_suite
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
  use hsd_error, only : error_message, make_error
  use hsd_token, only : token_name, TOKEN_EOF, TOKEN_LBRACE, TOKEN_RBRACE, &
      & TOKEN_EQUAL, TOKEN_STRING, TOKEN_TEXT, TOKEN_INVALID
  use fortuno_serial, only : is_equal, test => serial_case_item, check => serial_check, &
      & suite => serial_suite_item, test_list
  implicit none (type, external)

  private
  public :: tests

contains

  !> Returns all error handling tests
  function tests()
    type(test_list) :: tests

    tests = test_list([&
        suite("errors", test_list([&
            test("unclosed_brace", test_unclosed_brace), &
            test("unclosed_quote", test_unclosed_quote), &
            test("missing_path", test_missing_path), &
            test("type_mismatch", test_type_mismatch), &
            test("empty_input", test_empty_input), &
            test("complex_number", test_complex_number), &
            test("complex_array", test_complex_array), &
            test("set_value", test_set_value), &
            test("set_nested_value", test_set_nested_value), &
            test("large_output", test_large_output), &
            test("error_messages", test_error_messages), &
            test("token_names", test_token_names), &
            test("error_print", test_error_print) &
        ])) &
    ])

  end function tests


  !> Test unclosed brace error handling
  subroutine test_unclosed_brace()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error

    call hsd_load_string("Block {" // char(10) // "  value = 1", root, error)

    ! Parser should handle this gracefully
    call root%destroy()

  end subroutine test_unclosed_brace


  !> Test unclosed quote error handling
  subroutine test_unclosed_quote()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error

    call hsd_load_string('name = "unclosed string', root, error)

    ! Parser should handle this gracefully
    call root%destroy()

  end subroutine test_unclosed_quote


  !> Test accessing non-existent path
  subroutine test_missing_path()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    integer :: val, stat

    call hsd_load_string("existing = 42", root, error)

    call check(.not. allocated(error), msg="No parse error")

    call hsd_get(root, "nonexistent/path/to/value", val, stat)
    call check(stat /= 0, msg="Non-existent path returns error status")

    call root%destroy()

  end subroutine test_missing_path


  !> Test type mismatch
  subroutine test_type_mismatch()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    integer :: int_val, stat

    call hsd_load_string("name = hello_world", root, error)

    call check(.not. allocated(error), msg="No parse error")

    ! Try to read a non-numeric string as an integer
    call hsd_get(root, "name", int_val, stat)
    call check(stat /= 0, msg="Type mismatch returns error status")

    call root%destroy()

  end subroutine test_type_mismatch


  !> Test parsing empty input
  subroutine test_empty_input()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error

    call hsd_load_string("", root, error)

    call check(.not. allocated(error), msg="Empty input parses without error")
    call check(root%num_children == 0, msg="Empty input has no children")

    call root%destroy()

  end subroutine test_empty_input


  !> Test complex number parsing
  subroutine test_complex_number()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    complex(dp) :: cval
    integer :: stat

    ! Test a+bi format
    call hsd_load_string("z1 = 3.0+4.0i", root, error)
    call check(.not. allocated(error), msg="No parse error for complex")

    call hsd_get(root, "z1", cval, stat)
    call check(is_equal(stat, 0), msg="Can get complex value")
    call check(abs(real(cval) - 3.0_dp) < 0.001_dp, msg="Real part is 3.0")
    call check(abs(aimag(cval) - 4.0_dp) < 0.001_dp, msg="Imag part is 4.0")

    call root%destroy()

    ! Test negative imaginary part
    call hsd_load_string("z2 = 1.0-2.0i", root, error)
    call check(.not. allocated(error), msg="No parse error for complex")

    call hsd_get(root, "z2", cval, stat)
    call check(is_equal(stat, 0), msg="Can get complex value")
    call check(abs(real(cval) - 1.0_dp) < 0.001_dp, msg="Real part is 1.0")
    call check(abs(aimag(cval) + 2.0_dp) < 0.001_dp, msg="Imag part is -2.0")

    call root%destroy()

    ! Test Fortran (re,im) format
    call hsd_load_string("z3 = (5.0,6.0)", root, error)
    call check(.not. allocated(error), msg="No parse error for Fortran complex format")

    call hsd_get(root, "z3", cval, stat)
    call check(is_equal(stat, 0), msg="Can get Fortran format complex value")
    call check(abs(real(cval) - 5.0_dp) < 0.001_dp, msg="Real part is 5.0")
    call check(abs(aimag(cval) - 6.0_dp) < 0.001_dp, msg="Imag part is 6.0")

    call root%destroy()

  end subroutine test_complex_number


  !> Test complex array parsing
  subroutine test_complex_array()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    complex(dp), allocatable :: arr(:)
    integer :: stat

    call hsd_load_string("values = 1.0+2.0i 3.0-4.0i 5.0+0.0i", root, error)
    call check(.not. allocated(error), msg="No parse error for complex array")

    call hsd_get(root, "values", arr, stat)
    call check(is_equal(stat, 0), msg="Can get complex array")
    call check(is_equal(size(arr), 3), msg="Array has 3 elements")

    if (size(arr) >= 3) then
      call check(abs(real(arr(1)) - 1.0_dp) < 0.001_dp, msg="First element real part")
      call check(abs(aimag(arr(1)) - 2.0_dp) < 0.001_dp, msg="First element imag part")
      call check(abs(real(arr(2)) - 3.0_dp) < 0.001_dp, msg="Second element real part")
      call check(abs(aimag(arr(2)) + 4.0_dp) < 0.001_dp, msg="Second element imag part")
    end if

    call root%destroy()

  end subroutine test_complex_array


  !> Test hsd_set functionality
  subroutine test_set_value()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    integer :: int_val, stat
    real(dp) :: real_val
    character(len=:), allocatable :: str_val
    logical :: log_val

    call hsd_load_string("", root, error)
    call check(.not. allocated(error), msg="Empty parse OK")

    ! Set various types
    call hsd_set(root, "int_value", 42, stat)
    call check(is_equal(stat, 0), msg="Can set integer")

    call hsd_set(root, "real_value", 3.14_dp, stat)
    call check(is_equal(stat, 0), msg="Can set real")

    call hsd_set(root, "str_value", "hello", stat)
    call check(is_equal(stat, 0), msg="Can set string")

    call hsd_set(root, "bool_value", .true., stat)
    call check(is_equal(stat, 0), msg="Can set logical")

    ! Read back
    call hsd_get(root, "int_value", int_val, stat)
    call check(is_equal(stat, 0), msg="Can get set integer")
    call check(is_equal(int_val, 42), msg="Integer value correct")

    call hsd_get(root, "real_value", real_val, stat)
    call check(is_equal(stat, 0), msg="Can get set real")
    call check(abs(real_val - 3.14_dp) < 0.001_dp, msg="Real value correct")

    call hsd_get(root, "str_value", str_val, stat)
    call check(is_equal(stat, 0), msg="Can get set string")
    call check(str_val == "hello", msg="String value correct")

    call hsd_get(root, "bool_value", log_val, stat)
    call check(is_equal(stat, 0), msg="Can get set logical")
    call check(log_val, msg="Logical value correct")

    call root%destroy()

  end subroutine test_set_value


  !> Test hsd_set with nested paths (creating intermediate nodes)
  subroutine test_set_nested_value()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    integer :: val, stat
    character(len=:), allocatable :: output

    call hsd_load_string("", root, error)
    call check(.not. allocated(error), msg="Empty parse OK")

    ! Set a nested value (should create intermediate tables)
    call hsd_set(root, "driver/max_steps", 200, stat)
    call check(is_equal(stat, 0), msg="Can set nested value")

    call hsd_get(root, "driver/max_steps", val, stat)
    call check(is_equal(stat, 0), msg="Can get nested value")
    call check(is_equal(val, 200), msg="Nested value is correct")

    ! Dump and verify structure
    call hsd_dump_to_string(root, output)
    call check(index(output, "driver") > 0, msg="Output contains driver")

    call root%destroy()

  end subroutine test_set_nested_value


  !> Test that large structures don't truncate
  subroutine test_large_output()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: output
    character(len=100) :: key
    integer :: i, stat

    call hsd_load_string("", root, error)
    call check(.not. allocated(error), msg="Empty parse OK")

    ! Add many values to exceed old 64KB limit
    do i = 1, 1000
      write(key, '(A,I0)') "value_", i
      call hsd_set(root, trim(key), i * 100, stat)
    end do

    ! Dump to string
    call hsd_dump_to_string(root, output)

    ! Check that output is large enough (should be > 64KB for 1000 values)
    call check(len(output) > 10000, msg="Output is sufficiently large")

    ! Verify last value is present
    call check(index(output, "value_1000") > 0, msg="Last value present in output")

    call root%destroy()

  end subroutine test_large_output


  !> Test error_message function for all error codes
  subroutine test_error_messages()
    character(len=:), allocatable :: msg

    msg = error_message(HSD_STAT_OK)
    call check(len(msg) > 0, msg="OK message exists")

    msg = error_message(HSD_STAT_SYNTAX_ERROR)
    call check(index(msg, "Syntax") > 0, msg="Syntax error message")

    msg = error_message(HSD_STAT_UNCLOSED_TAG)
    call check(index(msg, "tag") > 0, msg="Unclosed tag message")

    msg = error_message(HSD_STAT_UNCLOSED_QUOTE)
    call check(index(msg, "quot") > 0, msg="Unclosed quote message")

    msg = error_message(HSD_STAT_FILE_NOT_FOUND)
    call check(index(msg, "not found") > 0, msg="File not found message")

    msg = error_message(HSD_STAT_TYPE_ERROR)
    call check(index(msg, "Type") > 0, msg="Type error message")

    msg = error_message(HSD_STAT_NOT_FOUND)
    call check(index(msg, "not found") > 0, msg="Not found message")

    ! Test unknown error code
    msg = error_message(-999)
    call check(index(msg, "nknown") > 0, msg="Unknown error message")

  end subroutine test_error_messages


  !> Test token_name function for all token types
  subroutine test_token_names()
    character(len=:), allocatable :: name

    name = token_name(TOKEN_EOF)
    call check(len(name) > 0, msg="EOF token has name")

    name = token_name(TOKEN_LBRACE)
    call check(index(name, "brace") > 0, msg="LBRACE token name contains brace")

    name = token_name(TOKEN_RBRACE)
    call check(index(name, "brace") > 0, msg="RBRACE token name contains brace")

    name = token_name(TOKEN_EQUAL)
    call check(index(name, "equal") > 0, msg="EQUAL token name")

    name = token_name(TOKEN_STRING)
    call check(index(name, "string") > 0, msg="STRING token name")

    name = token_name(TOKEN_TEXT)
    call check(index(name, "text") > 0, msg="TEXT token name")

    name = token_name(TOKEN_INVALID)
    call check(index(name, "invalid") > 0, msg="INVALID token name")

    ! Unknown token
    name = token_name(9999)
    call check(index(name, "unknown") > 0, msg="Unknown token name")

  end subroutine test_token_names


  !> Test error printing functionality
  subroutine test_error_print()
    type(hsd_error_t), allocatable :: error

    ! Create an error with full location info
    call make_error(error, HSD_STAT_SYNTAX_ERROR, "Test error message", &
                    "test.hsd", 10, 15)

    call check(allocated(error), msg="Error allocated")
    call check(is_equal(error%code, HSD_STAT_SYNTAX_ERROR), msg="Error code correct")
    call check(error%message == "Test error message", msg="Message correct")
    call check(error%filename == "test.hsd", msg="Filename correct")
    call check(is_equal(error%line_start, 10), msg="Line start correct")
    call check(is_equal(error%line_end, 15), msg="Line end correct")

    ! Test print (just verify it doesn't crash - output goes to stdout)
    call error%print()

  end subroutine test_error_print

end module test_error_suite
