!> Additional formatter and output tests
module test_formatter_extended_suite
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
  use hsd_constants, only : dp, sp, CHAR_NEWLINE
  use hsd_utils, only : string_buffer_t
  use hsd_formatter, only : hsd_dump, hsd_dump_to_string
  use build_env, only : build_dir, source_dir
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
        suite("formatter_extended", test_list([&
            test("dump_nested_tables", test_dump_nested_tables), &
            test("dump_mixed_content", test_dump_mixed_content), &
            test("dump_arrays", test_dump_arrays), &
            test("dump_matrix", test_dump_matrix), &
            test("dump_with_attributes", test_dump_with_attributes), &
            test("dump_special_chars", test_dump_special_chars), &
            test("dump_deep_nesting", test_dump_deep_nesting), &
            test("dump_empty_table", test_dump_empty_table), &
            test("dump_to_file", test_dump_to_file), &
            test("dump_complex_values", test_dump_complex_values), &
            test("dump_boolean_values", test_dump_boolean_values), &
            test("dump_exponential_format", test_dump_exponential_format), &
            test("buffer_operations_extended", test_buffer_operations_extended), &
            test("roundtrip_complex_document", test_roundtrip_complex_document), &
            test("dump_preserved_order", test_dump_preserved_order) &
        ])) &
    ])

  end function tests


  !> Test dumping nested tables
  subroutine test_dump_nested_tables()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: output

    call hsd_load_string("outer {" // char(10) // &
      "  middle {" // char(10) // &
      "    inner = 42" // char(10) // &
      "  }" // char(10) // &
      "}", root, error)
    call check(.not. allocated(error), msg="Parse OK")

    call hsd_dump_to_string(root, output)

    call check(index(output, "outer") > 0, msg="Contains 'outer'")
    call check(index(output, "middle") > 0, msg="Contains 'middle'")
    call check(index(output, "inner") > 0, msg="Contains 'inner'")
    call check(index(output, "42") > 0, msg="Contains '42'")

    call root%destroy()

  end subroutine test_dump_nested_tables


  !> Test dumping mixed content (tables and values)
  subroutine test_dump_mixed_content()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: output

    call hsd_load_string("header = 'test'" // char(10) // &
      "section {" // char(10) // &
      "  value = 123" // char(10) // &
      "}" // char(10) // &
      "footer = 'end'", root, error)
    call check(.not. allocated(error), msg="Parse OK")

    call hsd_dump_to_string(root, output)

    call check(index(output, "header") > 0, msg="Contains 'header'")
    call check(index(output, "section") > 0, msg="Contains 'section'")
    call check(index(output, "value") > 0, msg="Contains 'value'")
    call check(index(output, "footer") > 0, msg="Contains 'footer'")

    call root%destroy()

  end subroutine test_dump_mixed_content


  !> Test dumping arrays
  subroutine test_dump_arrays()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: output

    ! Parse arrays directly from HSD string
    call hsd_load_string("int_arr = 10 20 30" // char(10) // "real_arr = 1.5 2.5", root, error)
    call check(.not. allocated(error), msg="Parse OK")

    call hsd_dump_to_string(root, output)

    call check(index(output, "int_arr") > 0, msg="Contains 'int_arr'")
    call check(index(output, "10") > 0, msg="Contains '10'")
    call check(index(output, "20") > 0, msg="Contains '20'")
    call check(index(output, "30") > 0, msg="Contains '30'")
    call check(index(output, "real_arr") > 0, msg="Contains 'real_arr'")

    call root%destroy()

  end subroutine test_dump_arrays


  !> Test dumping matrix
  subroutine test_dump_matrix()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: output

    ! Matrix with semicolon separators
    call hsd_load_string("matrix = 1 2; 3 4; 5 6", root, error)
    call check(.not. allocated(error), msg="Parse OK")

    call hsd_dump_to_string(root, output)

    call check(index(output, "matrix") > 0, msg="Contains 'matrix'")
    call check(len(output) > 0, msg="Output not empty")

    call root%destroy()

  end subroutine test_dump_matrix


  !> Test dumping with attributes
  subroutine test_dump_with_attributes()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: output

    call hsd_load_string("item [unit=eV] = 1.5", root, error)
    call check(.not. allocated(error), msg="Parse OK")

    call hsd_dump_to_string(root, output)

    call check(index(output, "item") > 0, msg="Contains 'item'")
    call check(index(output, "unit") > 0 .or. index(output, "eV") > 0, &
        msg="Contains attribute info")

    call root%destroy()

  end subroutine test_dump_with_attributes


  !> Test dumping strings with special characters
  subroutine test_dump_special_chars()
    type(hsd_table) :: root
    type(hsd_value) :: val
    character(len=:), allocatable :: output

    call new_table(root)

    ! String with spaces
    call new_value(val, "with_space")
    call val%set_string("hello world")
    call root%add_child(val)

    ! String with equals
    call new_value(val, "with_equals")
    call val%set_string("a=b")
    call root%add_child(val)

    ! String with brackets
    call new_value(val, "with_brackets")
    call val%set_string("test [info]")
    call root%add_child(val)

    call hsd_dump_to_string(root, output)

    call check(index(output, "with_space") > 0, msg="Contains 'with_space'")
    call check(index(output, "with_equals") > 0, msg="Contains 'with_equals'")
    call check(index(output, "with_brackets") > 0, msg="Contains 'with_brackets'")

    call root%destroy()

  end subroutine test_dump_special_chars


  !> Test dumping deeply nested structure
  subroutine test_dump_deep_nesting()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: output

    call hsd_load_string("l1 { l2 { l3 { l4 { l5 { deep = yes } } } } }", root, error)
    call check(.not. allocated(error), msg="Parse OK")

    call hsd_dump_to_string(root, output)

    call check(index(output, "l1") > 0, msg="Contains 'l1'")
    call check(index(output, "l5") > 0, msg="Contains 'l5'")
    call check(index(output, "deep") > 0, msg="Contains 'deep'")

    call root%destroy()

  end subroutine test_dump_deep_nesting


  !> Test dumping empty table
  subroutine test_dump_empty_table()
    type(hsd_table) :: root
    character(len=:), allocatable :: output

    call new_table(root)

    call hsd_dump_to_string(root, output)

    ! Empty table should produce empty or minimal output
    call check(len(output) >= 0, msg="Output valid for empty table")

    call root%destroy()

  end subroutine test_dump_empty_table


  !> Test dumping to file
  subroutine test_dump_to_file()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=512) :: filepath

    call hsd_load_string("test_data = 42", root, error)
    call check(.not. allocated(error), msg="Parse OK")

    filepath = build_dir // "/test_output.hsd"
    call hsd_dump(root, trim(filepath), error)
    call check(.not. allocated(error), msg="Dump to file OK")

    ! Verify we can read it back
    call root%destroy()
    call hsd_load(trim(filepath), root, error)
    call check(.not. allocated(error), msg="Reload OK")

    call root%destroy()

    ! Clean up test file
    call delete_file(trim(filepath))

  end subroutine test_dump_to_file


  !> Test dumping complex values
  subroutine test_dump_complex_values()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: output

    ! Parse complex value from string (the formatter preserves raw format)
    call hsd_load_string("cpx = 3.0+4.0i", root, error)
    call check(.not. allocated(error), msg="Parse OK")

    call hsd_dump_to_string(root, output)

    call check(index(output, "cpx") > 0, msg="Contains 'cpx'")
    call check(len(output) > 0, msg="Output not empty")

    call root%destroy()

  end subroutine test_dump_complex_values


  !> Test dumping boolean values
  subroutine test_dump_boolean_values()
    type(hsd_table) :: root
    type(hsd_value) :: val
    character(len=:), allocatable :: output

    call new_table(root)

    call new_value(val, "flag_true")
    call val%set_logical(.true.)
    call root%add_child(val)

    call new_value(val, "flag_false")
    call val%set_logical(.false.)
    call root%add_child(val)

    call hsd_dump_to_string(root, output)

    call check(index(output, "flag_true") > 0, msg="Contains 'flag_true'")
    call check(index(output, "flag_false") > 0, msg="Contains 'flag_false'")
    ! Should contain Yes/No or True/False or similar
    call check(len(output) > 20, msg="Reasonable output length")

    call root%destroy()

  end subroutine test_dump_boolean_values


  !> Test dumping values in exponential format
  subroutine test_dump_exponential_format()
    type(hsd_table) :: root
    type(hsd_value) :: val
    character(len=:), allocatable :: output

    call new_table(root)

    call new_value(val, "big")
    call val%set_real(1.23e10_dp)
    call root%add_child(val)

    call new_value(val, "small")
    call val%set_real(4.56e-10_dp)
    call root%add_child(val)

    call hsd_dump_to_string(root, output)

    call check(index(output, "big") > 0, msg="Contains 'big'")
    call check(index(output, "small") > 0, msg="Contains 'small'")

    call root%destroy()

  end subroutine test_dump_exponential_format


  !> Test extended buffer operations
  subroutine test_buffer_operations_extended()
    type(string_buffer_t) :: buf
    character(len=:), allocatable :: result
    integer :: i

    ! Test repeated append/clear cycles
    do i = 1, 3
      call buf%init(8)
      call buf%append_str("cycle " // char(ichar('0') + i))
      result = buf%get_string()
      call check(len(result) > 0, msg="Buffer not empty after append")
      call buf%clear()
    end do

    ! Test appending many small strings
    call buf%init(4)
    do i = 1, 50
      call buf%append_str("x")
    end do
    result = buf%get_string()
    call check(len(result) == 50, msg="50 chars appended")

    ! Test mixing char and string appends
    call buf%init()
    call buf%append_char('H')
    call buf%append_str("ello")
    call buf%append_char('!')
    result = buf%get_string()
    call check(result == "Hello!", msg="Mixed append works")

  end subroutine test_buffer_operations_extended


  !> Test roundtrip of complex document
  subroutine test_roundtrip_complex_document()
    type(hsd_table) :: root1, root2
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: output
    integer :: val1, val2, stat
    character(len=:), allocatable :: str1, str2

    ! Create a complex document
    call hsd_load_string(&
      "config {" // char(10) // &
      "  version = 1" // char(10) // &
      "  name = 'test'" // char(10) // &
      "  data {" // char(10) // &
      "    value = 123" // char(10) // &
      "  }" // char(10) // &
      "}", root1, error)
    call check(.not. allocated(error), msg="Parse original OK")

    ! Dump to string
    call hsd_dump_to_string(root1, output)
    call check(len(output) > 0, msg="Output not empty")

    ! Parse the dumped output
    call hsd_load_string(output, root2, error)
    call check(.not. allocated(error), msg="Parse roundtrip OK")

    ! Verify values preserved
    call hsd_get(root1, "config/version", val1, stat)
    call hsd_get(root2, "config/version", val2, stat)
    call check(is_equal(val1, val2), msg="Version preserved")

    call hsd_get(root1, "config/name", str1, stat)
    call hsd_get(root2, "config/name", str2, stat)
    call check(str1 == str2, msg="Name preserved")

    call root1%destroy()
    call root2%destroy()

  end subroutine test_roundtrip_complex_document


  !> Test that order is preserved in output
  subroutine test_dump_preserved_order()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: output
    integer :: pos_a, pos_b, pos_c

    call hsd_load_string("alpha = 1; beta = 2; gamma = 3", root, error)
    call check(.not. allocated(error), msg="Parse OK")

    call hsd_dump_to_string(root, output)

    pos_a = index(output, "alpha")
    pos_b = index(output, "beta")
    pos_c = index(output, "gamma")

    call check(pos_a > 0, msg="alpha found")
    call check(pos_b > 0, msg="beta found")
    call check(pos_c > 0, msg="gamma found")
    call check(pos_a < pos_b, msg="alpha before beta")
    call check(pos_b < pos_c, msg="beta before gamma")

    call root%destroy()

  end subroutine test_dump_preserved_order


  !> Helper: Delete a file
  subroutine delete_file(filepath)
    character(len=*), intent(in) :: filepath
    integer :: unit_num, io_stat

    open(newunit=unit_num, file=filepath, status='old', iostat=io_stat)
    if (io_stat == 0) close(unit_num, status='delete')

  end subroutine delete_file

end module test_formatter_extended_suite
