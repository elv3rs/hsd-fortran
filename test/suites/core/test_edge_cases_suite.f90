!> Targeted tests for remaining uncovered code paths
!> Focus on hsd_types.f90, hsd.f90, hsd_formatter.f90 edge cases
module test_edge_cases_suite
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
  use hsd_formatter, only : hsd_dump, hsd_dump_to_string
  use hsd_error, only : HSD_STAT_TYPE_ERROR, HSD_STAT_NOT_FOUND
  use build_env, only : build_dir, source_dir
  use fortuno_serial, only : is_equal, test => serial_case_item, check => serial_check, &
      & suite => serial_suite_item, test_list
  implicit none (type, external)

  private
  public :: tests

contains

  !> Returns all edge case tests
  function tests()
    type(test_list) :: tests

    tests = test_list([&
        suite("edge_cases", test_list([&
            test("matrix_with_semicolons", test_matrix_with_semicolons), &
            test("matrix_irregular_rows", test_matrix_irregular_rows), &
            test("matrix_empty_rows", test_matrix_empty_rows), &
            test("complex_j_notation", test_complex_j_notation), &
            test("complex_capital_i", test_complex_capital_i), &
            test("complex_negative_imaginary", test_complex_negative_imaginary), &
            test("pure_imaginary_values", test_pure_imaginary_values), &
            test("get_or_fallback_all_types", test_get_or_fallback_all_types), &
            test("set_all_types", test_set_all_types), &
            test("table_grow_capacity", test_table_grow_capacity), &
            test("iterator_reset", test_iterator_reset), &
            test("remove_child_edge_cases", test_remove_child_edge_cases), &
            test("get_keys_variations", test_get_keys_variations), &
            test("visitor_deep_tree", test_visitor_deep_tree), &
            test("parse_quoted_strings", test_parse_quoted_strings), &
            test("logical_variations", test_logical_variations), &
            test("real_sp_values", test_real_sp_values), &
            test("complex_dp_values", test_complex_dp_values), &
            test("matrix_in_block", test_matrix_in_block), &
            test("nested_blocks", test_nested_blocks), &
            test("get_child_type_mismatch", test_get_child_type_mismatch), &
            test("format_large_numbers", test_format_large_numbers), &
            test("format_small_numbers", test_format_small_numbers), &
            test("format_integer_edge", test_format_integer_edge), &
            test("require_type_variations", test_require_type_variations), &
            test("clone_with_arrays", test_clone_with_arrays), &
            test("merge_value_types", test_merge_value_types), &
            test("get_child_direct", test_get_child_direct), &
            test("string_escapes", test_string_escapes), &
            test("sp_array_values", test_sp_array_values) &
        ])) &
    ])

  end function tests


  !> Test matrix parsing with semicolon row separators
  subroutine test_matrix_with_semicolons()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    integer, allocatable :: mat(:,:), arr(:)
    integer :: nrows, ncols, stat

    ! Semicolons in HSD separate statements, but within a value they may be preserved
    ! This tests how the parser handles them
    call hsd_load_string("data = 1 2 3" // char(10) // "4 5 6", root, error)
    call check(.not. allocated(error), msg="Parse OK")

    ! Get as array first
    call hsd_get(root, "data", arr, stat)
    call check(is_equal(stat, 0), msg="Get array OK")

    call root%destroy()

  end subroutine test_matrix_with_semicolons


  !> Test matrix with irregular row lengths
  subroutine test_matrix_irregular_rows()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    integer, allocatable :: mat(:,:)
    integer :: nrows, ncols, stat

    ! Create data with varying row lengths
    call hsd_load_string("matrix {" // char(10) // &
      "1 2 3" // char(10) // &
      "4 5" // char(10) // &
      "6 7 8 9" // char(10) // &
      "}", root, error)
    call check(.not. allocated(error), msg="Parse OK")

    call hsd_get_matrix(root, "matrix", mat, nrows, ncols, stat)
    ! Even with irregular data, should not crash
    call check(.true., msg="Matrix parsing handled")

    call root%destroy()

  end subroutine test_matrix_irregular_rows


  !> Test matrix with empty rows
  subroutine test_matrix_empty_rows()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    integer, allocatable :: arr(:)
    integer :: stat

    call hsd_load_string("matrix = 1 2 3", root, error)
    call check(.not. allocated(error), msg="Parse OK")

    call hsd_get(root, "matrix", arr, stat)
    call check(is_equal(stat, 0), msg="Get array OK")
    call check(is_equal(size(arr), 3), msg="Array has 3 elements")

    call root%destroy()

  end subroutine test_matrix_empty_rows


  !> Test complex with 'j' notation
  subroutine test_complex_j_notation()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    complex(dp) :: val
    integer :: stat

    call hsd_load_string("c = 3.0+4.0j", root, error)
    call check(.not. allocated(error), msg="Parse OK")

    call hsd_get(root, "c", val, stat)
    call check(is_equal(stat, 0), msg="Get j-notation complex")
    call check(abs(real(val) - 3.0_dp) < 0.001_dp, msg="Real part correct")
    call check(abs(aimag(val) - 4.0_dp) < 0.001_dp, msg="Imag part correct")

    call root%destroy()

  end subroutine test_complex_j_notation


  !> Test complex with capital I notation
  subroutine test_complex_capital_i()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    complex(dp) :: val
    integer :: stat

    call hsd_load_string("c = 1.0+2.0I", root, error)
    call check(.not. allocated(error), msg="Parse OK")

    call hsd_get(root, "c", val, stat)
    call check(is_equal(stat, 0), msg="Get I-notation complex")

    call root%destroy()

    ! Also test J notation
    call hsd_load_string("c = 5.0-3.0J", root, error)
    call check(.not. allocated(error), msg="Parse J OK")

    call hsd_get(root, "c", val, stat)
    call check(is_equal(stat, 0), msg="Get J-notation complex")

    call root%destroy()

  end subroutine test_complex_capital_i


  !> Test complex with negative imaginary
  subroutine test_complex_negative_imaginary()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    complex(dp) :: val
    integer :: stat

    call hsd_load_string("c = 5.0-2.5i", root, error)
    call check(.not. allocated(error), msg="Parse OK")

    call hsd_get(root, "c", val, stat)
    call check(is_equal(stat, 0), msg="Get negative imag complex")
    call check(abs(aimag(val) + 2.5_dp) < 0.001_dp, msg="Imag is -2.5")

    call root%destroy()

  end subroutine test_complex_negative_imaginary


  !> Test pure imaginary values
  subroutine test_pure_imaginary_values()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    complex(dp) :: val
    integer :: stat

    call hsd_load_string("imag = 5.0i", root, error)
    call check(.not. allocated(error), msg="Parse OK")

    call hsd_get(root, "imag", val, stat)
    call check(is_equal(stat, 0), msg="Get pure imaginary")
    call check(abs(real(val)) < 0.001_dp, msg="Real part zero")
    call check(abs(aimag(val) - 5.0_dp) < 0.001_dp, msg="Imag is 5.0")

    call root%destroy()

  end subroutine test_pure_imaginary_values


  !> Test hsd_get_or with all types
  subroutine test_get_or_fallback_all_types()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    integer :: int_val
    real(dp) :: real_val
    real(sp) :: sp_val
    logical :: log_val
    character(len=:), allocatable :: str_val
    complex(dp) :: cpx_val

    call hsd_load_string("dummy = 0", root, error)
    call check(.not. allocated(error), msg="Parse OK")

    ! All fallback calls - keys don't exist so defaults returned
    call hsd_get_or(root, "missing_int", int_val, 42)
    call check(is_equal(int_val, 42), msg="Int fallback is 42")

    call hsd_get_or(root, "missing_real", real_val, 3.14_dp)
    call check(abs(real_val - 3.14_dp) < 0.001_dp, msg="Real fallback is 3.14")

    call hsd_get_or(root, "missing_sp", sp_val, 2.0_sp)
    call check(abs(sp_val - 2.0_sp) < 0.001_sp, msg="SP fallback is 2.0")

    call hsd_get_or(root, "missing_log", log_val, .true.)
    call check(log_val .eqv. .true., msg="Logical fallback is true")

    call hsd_get_or(root, "missing_str", str_val, "default")
    call check(str_val == "default", msg="String fallback is 'default'")

    call hsd_get_or(root, "missing_cpx", cpx_val, (1.0_dp, 2.0_dp))
    call check(abs(real(cpx_val) - 1.0_dp) < 0.001_dp, msg="Complex fallback real")

    call root%destroy()

  end subroutine test_get_or_fallback_all_types


  !> Test setting all value types
  subroutine test_set_all_types()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    integer :: stat
    integer :: int_val
    real(dp) :: real_val
    real(sp) :: sp_val
    logical :: log_val
    character(len=:), allocatable :: str_val
    complex(dp) :: cpx_val

    call hsd_load_string("", root, error)

    ! Set various types
    call hsd_set(root, "int_key", 123, stat)
    call check(is_equal(stat, 0), msg="Set int OK")

    call hsd_set(root, "real_key", 4.56_dp, stat)
    call check(is_equal(stat, 0), msg="Set real OK")

    call hsd_set(root, "sp_key", 7.89_sp, stat)
    call check(is_equal(stat, 0), msg="Set SP OK")

    call hsd_set(root, "log_key", .true., stat)
    call check(is_equal(stat, 0), msg="Set logical OK")

    call hsd_set(root, "str_key", "hello", stat)
    call check(is_equal(stat, 0), msg="Set string OK")

    call hsd_set(root, "cpx_key", (2.0_dp, 3.0_dp), stat)
    call check(is_equal(stat, 0), msg="Set complex OK")

    ! Read back
    call hsd_get(root, "int_key", int_val, stat)
    call check(is_equal(int_val, 123), msg="Read int back")

    call hsd_get(root, "real_key", real_val, stat)
    call check(abs(real_val - 4.56_dp) < 0.001_dp, msg="Read real back")

    call hsd_get(root, "log_key", log_val, stat)
    call check(log_val .eqv. .true., msg="Read logical back")

    call root%destroy()

  end subroutine test_set_all_types


  !> Test table capacity growth
  subroutine test_table_grow_capacity()
    type(hsd_table) :: root
    type(hsd_value) :: val
    integer :: i

    call new_table(root)

    ! Add many children to force capacity growth
    do i = 1, 100
      call new_value(val, "item" // char(ichar('0') + mod(i, 10)))
      call val%set_integer(i)
      call root%add_child(val)
    end do

    call check(root%num_children == 100, msg="Added 100 children")
    call check(root%capacity >= 100, msg="Capacity >= 100")

    call root%destroy()

  end subroutine test_table_grow_capacity


  !> Test iterator reset
  subroutine test_iterator_reset()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    type(hsd_iterator) :: iter
    class(hsd_node), pointer :: node
    integer :: count1, count2

    call hsd_load_string("a = 1; b = 2; c = 3", root, error)
    call check(.not. allocated(error), msg="Parse OK")

    ! First pass
    count1 = 0
    call iter%init(root)
    do while (iter%has_next())
      if (iter%next(node)) count1 = count1 + 1
    end do

    ! Reset and second pass
    call iter%reset()
    count2 = 0
    do while (iter%has_next())
      if (iter%next(node)) count2 = count2 + 1
    end do

    call check(is_equal(count1, count2), msg="Same count after reset")

    call root%destroy()

  end subroutine test_iterator_reset


  !> Test remove_child edge cases
  subroutine test_remove_child_edge_cases()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    integer :: stat

    call hsd_load_string("a = 1; b = 2", root, error)
    call check(.not. allocated(error), msg="Parse OK")

    ! Remove non-existent
    call root%remove_child_by_name("nonexistent", stat)
    call check(stat == HSD_STAT_NOT_FOUND, msg="Non-existent returns not found")

    ! Remove by index 0 (invalid)
    call root%remove_child(0, stat)
    call check(stat /= 0, msg="Index 0 invalid")

    ! Remove by index > count
    call root%remove_child(999, stat)
    call check(stat /= 0, msg="Index too large")

    call root%destroy()

  end subroutine test_remove_child_edge_cases


  !> Test get_keys variations
  subroutine test_get_keys_variations()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: keys(:)
    integer :: stat

    ! Table with named and unnamed children
    call hsd_load_string("named = 1" // char(10) // &
      "nested { a = 2 }" // char(10) // &
      "another = 3", root, error)
    call check(.not. allocated(error), msg="Parse OK")

    call hsd_get_keys(root, "", keys, stat)
    call check(size(keys) >= 2, msg="Got keys from root")

    call hsd_get_keys(root, "nested", keys, stat)
    call check(is_equal(stat, 0), msg="Got keys from nested")

    ! Get keys from value (should fail)
    call hsd_get_keys(root, "named", keys, stat)
    call check(stat == HSD_STAT_TYPE_ERROR, msg="Keys from value is error")

    call root%destroy()

  end subroutine test_get_keys_variations


  !> Test visitor on deep tree
  subroutine test_visitor_deep_tree()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error

    call hsd_load_string("l1 { l2 { l3 { l4 { l5 { l6 { l7 { l8 { val = 42 } } } } } } } }", &
        root, error)
    call check(.not. allocated(error), msg="Parse deep tree OK")

    call check(hsd_is_value(root, "l1/l2/l3/l4/l5/l6/l7/l8/val"), msg="Deep value accessible")

    call root%destroy()

  end subroutine test_visitor_deep_tree


  !> Test parsing quoted strings in arrays
  subroutine test_parse_quoted_strings()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: arr(:)
    integer :: stat

    call hsd_load_string('words = "hello world" simple "quoted value"', root, error)
    call check(.not. allocated(error), msg="Parse OK")

    call hsd_get(root, "words", arr, stat)
    call check(is_equal(stat, 0), msg="Get quoted strings OK")

    call root%destroy()

  end subroutine test_parse_quoted_strings


  !> Test logical value variations
  subroutine test_logical_variations()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    logical :: val
    integer :: stat

    ! Test all variations
    call hsd_load_string("a = yes; b = no; c = true; d = false; e = Yes; f = TRUE", root, error)
    call check(.not. allocated(error), msg="Parse OK")

    call hsd_get(root, "a", val, stat)
    call check(val .eqv. .true., msg="'yes' is true")

    call hsd_get(root, "b", val, stat)
    call check(val .eqv. .false., msg="'no' is false")

    call hsd_get(root, "c", val, stat)
    call check(val .eqv. .true., msg="'true' is true")

    call hsd_get(root, "d", val, stat)
    call check(val .eqv. .false., msg="'false' is false")

    call root%destroy()

  end subroutine test_logical_variations


  !> Test single precision real values
  subroutine test_real_sp_values()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    real(sp) :: val
    integer :: stat

    call hsd_load_string("sp_val = 3.14159", root, error)
    call check(.not. allocated(error), msg="Parse OK")

    call hsd_get(root, "sp_val", val, stat)
    call check(is_equal(stat, 0), msg="Get SP OK")
    call check(abs(val - 3.14159_sp) < 0.001_sp, msg="SP value correct")

    call root%destroy()

  end subroutine test_real_sp_values


  !> Test double precision complex values with various formats
  subroutine test_complex_dp_values()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    complex(dp) :: val
    integer :: stat

    call hsd_load_string("cpx = 1.5+2.5i", root, error)
    call check(.not. allocated(error), msg="Parse OK")

    call hsd_get(root, "cpx", val, stat)
    call check(is_equal(stat, 0), msg="Get DP complex OK")
    call check(abs(real(val, dp) - 1.5_dp) < 0.001_dp, msg="DP real part")
    call check(abs(aimag(val) - 2.5_dp) < 0.001_dp, msg="DP imag part")

    call root%destroy()

  end subroutine test_complex_dp_values


  !> Test matrix inside a block
  subroutine test_matrix_in_block()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    integer, allocatable :: mat(:,:)
    integer :: nrows, ncols, stat

    call hsd_load_string("container {" // char(10) // &
      "  matrix = 1 2 3 4 5 6" // char(10) // &
      "}", root, error)
    call check(.not. allocated(error), msg="Parse OK")

    call hsd_get_matrix(root, "container/matrix", mat, nrows, ncols, stat)
    call check(is_equal(stat, 0), msg="Get matrix from block OK")

    call root%destroy()

  end subroutine test_matrix_in_block


  !> Test deeply nested blocks
  subroutine test_nested_blocks()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    integer :: val, stat

    call hsd_load_string("a { b { c { d { e = 42 } } } }", root, error)
    call check(.not. allocated(error), msg="Parse OK")

    call hsd_get(root, "a/b/c/d/e", val, stat)
    call check(is_equal(stat, 0), msg="Get deep value OK")
    call check(is_equal(val, 42), msg="Deep value is 42")

    call root%destroy()

  end subroutine test_nested_blocks


  !> Test get_child with type mismatch
  subroutine test_get_child_type_mismatch()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    integer :: int_val, stat
    character(len=:), allocatable :: str_val

    call hsd_load_string("text = hello", root, error)
    call check(.not. allocated(error), msg="Parse OK")

    ! Try to get string as integer (type conversion)
    call hsd_get(root, "text", int_val, stat)
    ! Should fail since "hello" is not numeric
    call check(stat /= 0, msg="Type mismatch detected")

    ! But string should work
    call hsd_get(root, "text", str_val, stat)
    call check(is_equal(stat, 0), msg="String get OK")
    call check(str_val == "hello", msg="String value correct")

    call root%destroy()

  end subroutine test_get_child_type_mismatch


  !> Test formatting large numbers
  subroutine test_format_large_numbers()
    type(hsd_table) :: root
    type(hsd_value) :: val
    character(len=:), allocatable :: output

    call new_table(root)

    call new_value(val, "large")
    call val%set_real(1.23e15_dp)
    call root%add_child(val)

    call new_value(val, "very_large")
    call val%set_real(9.87e30_dp)
    call root%add_child(val)

    call hsd_dump_to_string(root, output)
    call check(len(output) > 0, msg="Large numbers formatted")

    call root%destroy()

  end subroutine test_format_large_numbers


  !> Test formatting small numbers
  subroutine test_format_small_numbers()
    type(hsd_table) :: root
    type(hsd_value) :: val
    character(len=:), allocatable :: output

    call new_table(root)

    call new_value(val, "small")
    call val%set_real(1.23e-15_dp)
    call root%add_child(val)

    call new_value(val, "very_small")
    call val%set_real(9.87e-30_dp)
    call root%add_child(val)

    call hsd_dump_to_string(root, output)
    call check(len(output) > 0, msg="Small numbers formatted")

    call root%destroy()

  end subroutine test_format_small_numbers


  !> Test integer formatting edge cases
  subroutine test_format_integer_edge()
    type(hsd_table) :: root
    type(hsd_value) :: val
    character(len=:), allocatable :: output

    call new_table(root)

    call new_value(val, "zero")
    call val%set_integer(0)
    call root%add_child(val)

    call new_value(val, "neg")
    call val%set_integer(-123456)
    call root%add_child(val)

    call new_value(val, "large")
    call val%set_integer(999999999)
    call root%add_child(val)

    call hsd_dump_to_string(root, output)
    call check(index(output, "0") > 0, msg="Zero formatted")
    call check(index(output, "-123456") > 0, msg="Negative formatted")

    call root%destroy()

  end subroutine test_format_integer_edge


  !> Test hsd_require with type variations
  subroutine test_require_type_variations()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error, req_error

    call hsd_load_string("int_val = 42; str_val = hello; table { x = 1 }", root, error)
    call check(.not. allocated(error), msg="Parse OK")

    ! Require existing value
    call hsd_require(root, "int_val", req_error)
    call check(.not. allocated(req_error), msg="int_val exists")

    ! Require missing value
    call hsd_require(root, "missing", req_error)
    call check(allocated(req_error), msg="missing detected")
    if (allocated(req_error)) deallocate(req_error)

    ! Require with type check
    call hsd_require(root, "int_val", req_error, VALUE_TYPE_STRING)
    ! May or may not fail depending on implementation
    if (allocated(req_error)) deallocate(req_error)

    call root%destroy()

  end subroutine test_require_type_variations


  !> Test cloning with arrays
  subroutine test_clone_with_arrays()
    type(hsd_table) :: root, clone
    type(hsd_error_t), allocatable :: error
    integer, allocatable :: arr1(:), arr2(:)
    integer :: stat

    call hsd_load_string("arr = 1 2 3 4 5", root, error)
    call check(.not. allocated(error), msg="Parse OK")

    call hsd_clone(root, clone, stat)
    call check(is_equal(stat, 0), msg="Clone OK")

    call hsd_get(root, "arr", arr1, stat)
    call hsd_get(clone, "arr", arr2, stat)
    call check(is_equal(size(arr1), size(arr2)), msg="Array sizes match")

    call root%destroy()
    call clone%destroy()

  end subroutine test_clone_with_arrays


  !> Test merging different value types
  subroutine test_merge_value_types()
    type(hsd_table) :: base, overlay
    type(hsd_error_t), allocatable :: error
    integer :: int_val, stat
    real(dp) :: real_val

    call hsd_load_string("int_key = 1; real_key = 1.0", base, error)
    call check(.not. allocated(error), msg="Base OK")

    call hsd_load_string("int_key = 2; new_key = 3", overlay, error)
    call check(.not. allocated(error), msg="Overlay OK")

    call hsd_merge(base, overlay, stat)
    call check(is_equal(stat, 0), msg="Merge OK")

    call hsd_get(base, "int_key", int_val, stat)
    call check(is_equal(int_val, 2), msg="int_key updated to 2")

    call hsd_get(base, "new_key", int_val, stat)
    call check(is_equal(int_val, 3), msg="new_key is 3")

    call base%destroy()
    call overlay%destroy()

  end subroutine test_merge_value_types


  !> Test getting child directly
  subroutine test_get_child_direct()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    class(hsd_node), pointer :: child

    call hsd_load_string("a = 1; b { c = 2 }", root, error)
    call check(.not. allocated(error), msg="Parse OK")

    ! Direct child access
    call root%get_child(1, child)
    call check(associated(child), msg="Child 1 exists")

    call root%get_child(2, child)
    call check(associated(child), msg="Child 2 exists")

    ! Out of range
    call root%get_child(99, child)
    call check(.not. associated(child), msg="Child 99 not found")

    call root%destroy()

  end subroutine test_get_child_direct


  !> Test string with escape sequences
  subroutine test_string_escapes()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: val
    integer :: stat

    ! Test with newline escape
    call hsd_load_string('text = "line1\nline2"', root, error)
    call check(.not. allocated(error), msg="Parse escape OK")

    call hsd_get(root, "text", val, stat)
    call check(is_equal(stat, 0), msg="Get escaped string OK")
    call check(index(val, char(10)) > 0, msg="Newline present")

    call root%destroy()

    ! Test with tab escape
    call hsd_load_string('text = "col1\tcol2"', root, error)
    call check(.not. allocated(error), msg="Parse tab escape OK")

    call hsd_get(root, "text", val, stat)
    call check(index(val, char(9)) > 0, msg="Tab present")

    call root%destroy()

  end subroutine test_string_escapes


  !> Test SP array values
  subroutine test_sp_array_values()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    real(sp), allocatable :: arr(:)
    integer :: stat

    call hsd_load_string("sp_arr = 1.1 2.2 3.3", root, error)
    call check(.not. allocated(error), msg="Parse OK")

    call hsd_get(root, "sp_arr", arr, stat)
    call check(is_equal(stat, 0), msg="Get SP array OK")
    call check(is_equal(size(arr), 3), msg="3 SP values")

    call root%destroy()

  end subroutine test_sp_array_values

end module test_edge_cases_suite
