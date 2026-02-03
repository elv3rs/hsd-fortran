!> Edge case coverage tests for hsd_types and other modules
module test_types_edge_coverage_suite
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
  use hsd_constants, only: dp
  use hsd_types, only: hsd_table, hsd_value, new_value, new_table, &
    VALUE_TYPE_INTEGER, VALUE_TYPE_STRING
  use hsd_error, only: HSD_STAT_OK, HSD_STAT_NOT_FOUND, HSD_STAT_TYPE_ERROR
  use fortuno_serial, only: test => serial_case_item, check => serial_check, &
      & suite => serial_suite_item, test_list
  implicit none (type, external)

  private
  public :: types_edge_coverage_tests

contains

  function types_edge_coverage_tests() result(test_items)
    type(test_list) :: test_items

    test_items = test_list([&
            test("table_num_children_direct", test_table_num_children_direct), &
            test("value_get_attrib_unallocated", test_value_get_attrib_unallocated), &
            test("int_array_on_missing_value", test_int_array_on_missing_value), &
            test("real_array_on_missing_value", test_real_array_on_missing_value), &
            test("logical_array_on_missing_value", test_logical_array_on_missing_value), &
            test("complex_array_on_missing_value", test_complex_array_on_missing_value), &
            test("int_matrix_on_missing_value", test_int_matrix_on_missing_value), &
            test("real_matrix_on_missing_value", test_real_matrix_on_missing_value), &
            test("string_array_on_missing_value", test_string_array_on_missing_value), &
            test("complex_get_direct_value", test_complex_get_direct_value), &
            test("logical_default_case", test_logical_default_case), &
            test("integer_type_error_parse", test_integer_type_error_parse), &
            test("table_get_keys_unnamed_children", test_table_get_keys_unnamed_children) &
        ])
  end function types_edge_coverage_tests


  !> Test table%num_children() pure function directly (lines 288-292)
  subroutine test_table_num_children_direct()
    type(hsd_table) :: table
    type(hsd_value), allocatable :: val
    integer :: n

    call new_table(table)

    ! Test empty table - call the method directly
    n = table%num_children_func()
    call check(n == 0, msg="Empty table has 0 children")

    ! Add one child
    allocate(val)
    call new_value(val, "child")
    call table%add_child(val)

    n = table%num_children_func()
    call check(n == 1, msg="Table has 1 child")

    call table%destroy()
  end subroutine test_table_num_children_direct


  !> Test value%get_attrib() when attrib is not allocated (line 157)
  subroutine test_value_get_attrib_unallocated()
    type(hsd_value), allocatable :: val
    character(len=:), allocatable :: attrib

    allocate(val)
    call new_value(val, "test")
    ! Don't set attrib

    attrib = val%get_attrib()
    call check(len(attrib) == 0 .or. attrib == "", msg="Empty attrib")

    deallocate(val)
  end subroutine test_value_get_attrib_unallocated


  !> Test get_int_array on value with no data (lines 665-667)
  subroutine test_int_array_on_missing_value()
    type(hsd_value), allocatable :: val
    integer, allocatable :: arr(:)
    integer :: stat

    allocate(val)
    call new_value(val, "empty")
    ! Don't set any value

    call val%get_int_array(arr, stat)
    call check(stat == HSD_STAT_NOT_FOUND .or. size(arr) == 0, &
               msg="Int array not found")

    deallocate(val)
  end subroutine test_int_array_on_missing_value


  !> Test get_real_array on value with no data (lines 704-706)
  subroutine test_real_array_on_missing_value()
    type(hsd_value), allocatable :: val
    real(dp), allocatable :: arr(:)
    integer :: stat

    allocate(val)
    call new_value(val, "empty")

    call val%get_real_array(arr, stat)
    call check(stat == HSD_STAT_NOT_FOUND .or. size(arr) == 0, &
               msg="Real array not found")

    deallocate(val)
  end subroutine test_real_array_on_missing_value


  !> Test get_logical_array on value with no data (lines 742-744)
  subroutine test_logical_array_on_missing_value()
    type(hsd_value), allocatable :: val
    logical, allocatable :: arr(:)
    integer :: stat

    allocate(val)
    call new_value(val, "empty")

    call val%get_logical_array(arr, stat)
    call check(stat == HSD_STAT_NOT_FOUND .or. size(arr) == 0, &
               msg="Logical array not found")

    deallocate(val)
  end subroutine test_logical_array_on_missing_value


  !> Test get_complex_array on value with no data (lines 798-800)
  subroutine test_complex_array_on_missing_value()
    type(hsd_value), allocatable :: val
    complex(dp), allocatable :: arr(:)
    integer :: stat

    allocate(val)
    call new_value(val, "empty")

    call val%get_complex_array(arr, stat)
    call check(stat == HSD_STAT_NOT_FOUND .or. size(arr) == 0, &
               msg="Complex array not found")

    deallocate(val)
  end subroutine test_complex_array_on_missing_value


  !> Test get_int_matrix on value with no data (lines 871-875)
  subroutine test_int_matrix_on_missing_value()
    type(hsd_value), allocatable :: val
    integer, allocatable :: mat(:,:)
    integer :: nrows, ncols, stat

    allocate(val)
    call new_value(val, "empty")

    call val%get_int_matrix(mat, nrows, ncols, stat)
    call check(stat == HSD_STAT_NOT_FOUND .or. nrows == 0, &
               msg="Int matrix not found")

    deallocate(val)
  end subroutine test_int_matrix_on_missing_value


  !> Test get_real_matrix on value with no data (lines 914-918)
  subroutine test_real_matrix_on_missing_value()
    type(hsd_value), allocatable :: val
    real(dp), allocatable :: mat(:,:)
    integer :: nrows, ncols, stat

    allocate(val)
    call new_value(val, "empty")

    call val%get_real_matrix(mat, nrows, ncols, stat)
    call check(stat == HSD_STAT_NOT_FOUND .or. nrows == 0, &
               msg="Real matrix not found")

    deallocate(val)
  end subroutine test_real_matrix_on_missing_value


  !> Test get_string_array on value with no raw_text (lines 834-836)
  subroutine test_string_array_on_missing_value()
    type(hsd_value), allocatable :: val
    character(len=:), allocatable :: arr(:)
    integer :: stat

    allocate(val)
    call new_value(val, "empty")

    call val%get_string_array(arr, stat)
    call check(stat == HSD_STAT_NOT_FOUND .or. size(arr) == 0, &
               msg="String array not found")

    deallocate(val)
  end subroutine test_string_array_on_missing_value


  !> Test get_complex on value with complex type (lines 631-632)
  subroutine test_complex_get_direct_value()
    type(hsd_value), allocatable :: val
    complex(dp) :: c
    integer :: stat

    allocate(val)
    call new_value(val, "z")
    val%complex_value = cmplx(3.0_dp, 4.0_dp, dp)
    call val%set_complex(cmplx(3.0_dp, 4.0_dp, dp))

    call val%get_complex(c, stat)
    call check(stat == HSD_STAT_OK, msg="Got complex value")
    call check(abs(real(c) - 3.0_dp) < 1e-10_dp, msg="Real part OK")

    deallocate(val)
  end subroutine test_complex_get_direct_value


  !> Test get_logical with invalid string (line 613)
  subroutine test_logical_default_case()
    type(hsd_value), allocatable :: val
    logical :: result
    integer :: stat

    allocate(val)
    call new_value(val, "flag")
    val%string_value = "invalid_boolean"
    val%value_type = VALUE_TYPE_STRING

    call val%get_logical(result, stat)
    call check(stat == HSD_STAT_TYPE_ERROR, msg="Type error for invalid logical")
    call check(result .eqv. .false., msg="Default false")

    deallocate(val)
  end subroutine test_logical_default_case


  !> Test get_integer with unparseable string (line 581)
  subroutine test_integer_type_error_parse()
    type(hsd_value), allocatable :: val
    integer :: result
    integer :: stat

    allocate(val)
    call new_value(val, "num")
    val%string_value = "not_a_number"
    val%value_type = VALUE_TYPE_STRING

    call val%get_integer(result, stat)
    call check(stat == HSD_STAT_TYPE_ERROR, msg="Type error for unparseable int")

    deallocate(val)
  end subroutine test_integer_type_error_parse


  !> Test get_keys with unnamed children (line 319)
  subroutine test_table_get_keys_unnamed_children()
    type(hsd_table) :: table
    type(hsd_value), allocatable :: val1, val2
    character(len=:), allocatable :: keys(:)

    call new_table(table)

    ! Add unnamed child
    allocate(val1)
    call new_value(val1)
    val1%string_value = "anon"
    val1%value_type = VALUE_TYPE_STRING
    call table%add_child(val1)

    ! Add named child
    allocate(val2)
    call new_value(val2, "named")
    val2%string_value = "value"
    val2%value_type = VALUE_TYPE_STRING
    call table%add_child(val2)

    call table%get_keys(keys)
    call check(size(keys) == 2, msg="Got 2 keys")
    ! First key should be empty string
    call check(len_trim(keys(1)) == 0, msg="First key empty")

    call table%destroy()
  end subroutine test_table_get_keys_unnamed_children

end module test_types_edge_coverage_suite
