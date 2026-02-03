!> Formatter unit tests using Fortuno framework
module test_formatter_suite
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
  use build_env, only : build_dir
  use fortuno_serial, only : is_equal, test => serial_case_item, check => serial_check, &
      & suite => serial_suite_item, test_list
  implicit none (type, external)

  private
  public :: tests

contains

  !> Returns all formatter tests
  function tests()
    type(test_list) :: tests

    tests = test_list([&
        suite("formatter", test_list([&
            test("dump_integer", test_dump_integer), &
            test("dump_real", test_dump_real), &
            test("dump_boolean", test_dump_boolean), &
            test("dump_string", test_dump_string), &
            test("dump_nested", test_dump_nested), &
            test("dump_attribute", test_dump_attribute), &
            test("roundtrip_simple", test_roundtrip_simple), &
            test("roundtrip_nested", test_roundtrip_nested), &
            test("dump_to_file", test_dump_to_file), &
            test("multiline_value", test_multiline_value), &
            test("equal_syntax_output", test_equal_syntax_output) &
        ])) &
    ])

  end function tests


  !> Test dumping integer values
  subroutine test_dump_integer()
    type(hsd_table) :: root
    type(hsd_value) :: val
    character(len=:), allocatable :: output

    call new_table(root)

    call new_value(val, "count")
    call val%set_integer(42)
    call root%add_child(val)

    call hsd_dump_to_string(root, output)

    call check(len(output) > 0, msg="Output is not empty")
    call check(index(output, "count") > 0, msg="Output contains 'count'")
    call check(index(output, "42") > 0, msg="Output contains '42'")

    call root%destroy()

  end subroutine test_dump_integer


  !> Test dumping real values
  subroutine test_dump_real()
    type(hsd_table) :: root
    type(hsd_value) :: val
    character(len=:), allocatable :: output

    call new_table(root)

    call new_value(val, "pi")
    call val%set_real(3.14159_dp)
    call root%add_child(val)

    call hsd_dump_to_string(root, output)

    call check(len(output) > 0, msg="Output is not empty")
    call check(index(output, "pi") > 0, msg="Output contains 'pi'")
    call check(index(output, "3.14") > 0, msg="Output contains real value")

    call root%destroy()

  end subroutine test_dump_real


  !> Test dumping boolean values (should be Yes/No)
  subroutine test_dump_boolean()
    type(hsd_table) :: root
    type(hsd_value) :: val
    character(len=:), allocatable :: output

    call new_table(root)

    call new_value(val, "enabled")
    call val%set_logical(.true.)
    call root%add_child(val)

    call new_value(val, "disabled")
    call val%set_logical(.false.)
    call root%add_child(val)

    call hsd_dump_to_string(root, output)

    call check(index(output, "Yes") > 0, msg="True is dumped as 'Yes'")
    call check(index(output, "No") > 0, msg="False is dumped as 'No'")

    call root%destroy()

  end subroutine test_dump_boolean


  !> Test dumping string values with proper quoting
  subroutine test_dump_string()
    type(hsd_table) :: root
    type(hsd_value) :: val
    character(len=:), allocatable :: output

    call new_table(root)

    call new_value(val, "message")
    call val%set_string("hello world")
    call root%add_child(val)

    call hsd_dump_to_string(root, output)

    call check(index(output, "message") > 0, msg="Output contains 'message'")
    ! String with spaces should be quoted
    call check(index(output, '"hello world"') > 0 .or. &
               index(output, "'hello world'") > 0, msg="String with spaces is quoted")

    call root%destroy()

  end subroutine test_dump_string


  !> Test dumping nested structures
  subroutine test_dump_nested()
    type(hsd_table) :: root, child_table
    type(hsd_value) :: val
    character(len=:), allocatable :: output

    call new_table(root)
    call new_table(child_table)
    child_table%name = "options"

    call new_value(val, "value")
    call val%set_integer(123)
    call child_table%add_child(val)

    call root%add_child(child_table)

    call hsd_dump_to_string(root, output)

    call check(index(output, "options") > 0, msg="Output contains 'options'")
    call check(index(output, "{") > 0, msg="Output contains opening brace")
    call check(index(output, "}") > 0, msg="Output contains closing brace")
    call check(index(output, "value") > 0, msg="Output contains nested 'value'")

    call root%destroy()

  end subroutine test_dump_nested


  !> Test dumping with attributes
  subroutine test_dump_attribute()
    type(hsd_table) :: root
    type(hsd_value) :: val
    character(len=:), allocatable :: output

    call new_table(root)

    call new_value(val, "temperature")
    call val%set_real(300.0_dp)
    val%attrib = "Kelvin"
    call root%add_child(val)

    call hsd_dump_to_string(root, output)

    call check(index(output, "temperature") > 0, msg="Output contains 'temperature'")
    call check(index(output, "[Kelvin]") > 0, msg="Output contains '[Kelvin]' attribute")
    call check(index(output, "300") > 0, msg="Output contains '300'")

    call root%destroy()

  end subroutine test_dump_attribute


  !> Test round-trip: parse -> dump -> parse
  subroutine test_roundtrip_simple()
    type(hsd_table) :: root1, root2
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: output
    integer :: val1, val2, stat

    call hsd_load_string("max_steps = 200", root1, error)
    call check(.not. allocated(error), msg="First parse succeeds")

    call hsd_dump_to_string(root1, output)
    call check(len(output) > 0, msg="Dump produces output")

    call hsd_load_string(output, root2, error)
    call check(.not. allocated(error), msg="Second parse succeeds")

    call hsd_get(root1, "max_steps", val1, stat)
    call hsd_get(root2, "max_steps", val2, stat)

    call check(is_equal(val1, val2), msg="Values match after round-trip")
    call check(is_equal(val1, 200), msg="Value is still 200")

    call root1%destroy()
    call root2%destroy()

  end subroutine test_roundtrip_simple


  !> Test round-trip with nested structure
  subroutine test_roundtrip_nested()
    type(hsd_table) :: root1, root2
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: input, output
    logical :: val1, val2
    integer :: stat

    input = &
      "driver {" // char(10) // &
      "  optimizer {" // char(10) // &
      "    enabled = yes" // char(10) // &
      "  }" // char(10) // &
      "}"

    call hsd_load_string(input, root1, error)
    call check(.not. allocated(error), msg="First parse succeeds")

    call hsd_dump_to_string(root1, output)

    call hsd_load_string(output, root2, error)
    call check(.not. allocated(error), msg="Second parse succeeds")

    call hsd_get(root1, "driver/optimizer/enabled", val1, stat)
    call hsd_get(root2, "driver/optimizer/enabled", val2, stat)

    call check(val1 .eqv. val2, msg="Nested values match after round-trip")
    call check(val1 .eqv. .true., msg="Value is still true")

    call root1%destroy()
    call root2%destroy()

  end subroutine test_roundtrip_nested


  !> Test dumping to file (covers hsd_dump file I/O path)
  subroutine test_dump_to_file()
    use build_env, only : build_dir
    type(hsd_table) :: root, root2
    type(hsd_error_t), allocatable :: error
    type(hsd_value) :: val
    character(len=512) :: filepath
    integer :: int_val, stat

    call new_table(root)
    call new_value(val, "test_key")
    call val%set_integer(12345)
    call root%add_child(val)

    ! Write to file in build directory
    filepath = build_dir // "/formatter_output.hsd"
    call hsd_dump(root, trim(filepath), error)
    call check(.not. allocated(error), msg="File dump succeeds")

    ! Read back and verify
    call hsd_load(trim(filepath), root2, error)
    call check(.not. allocated(error), msg="Can read dumped file")

    call hsd_get(root2, "test_key", int_val, stat)
    call check(is_equal(int_val, 12345), msg="Value preserved through file dump")

    call root%destroy()
    call root2%destroy()

  end subroutine test_dump_to_file


  !> Test multiline value formatting
  subroutine test_multiline_value()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: output, hsd_input

    ! Create input with multiline data block
    hsd_input = &
      "data {" // char(10) // &
      "  1.0 2.0 3.0" // char(10) // &
      "  4.0 5.0 6.0" // char(10) // &
      "  7.0 8.0 9.0" // char(10) // &
      "}"

    call hsd_load_string(hsd_input, root, error)
    call check(.not. allocated(error), msg="Multiline data parses OK")

    call hsd_dump_to_string(root, output)
    call check(len(output) > 0, msg="Output generated")
    call check(index(output, "data") > 0, msg="Contains data key")

    call root%destroy()

  end subroutine test_multiline_value


  !> Test equal-syntax output (Tag = ChildTag { ... })
  subroutine test_equal_syntax_output()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: output, hsd_input

    hsd_input = &
      "hamiltonian = dftb {" // char(10) // &
      "  scc = yes" // char(10) // &
      "}"

    call hsd_load_string(hsd_input, root, error)
    call check(.not. allocated(error), msg="Parse OK")

    call hsd_dump_to_string(root, output)
    call check(index(output, "hamiltonian") > 0, msg="Contains hamiltonian")
    call check(index(output, "dftb") > 0, msg="Contains dftb")

    call root%destroy()

  end subroutine test_equal_syntax_output

end module test_formatter_suite
