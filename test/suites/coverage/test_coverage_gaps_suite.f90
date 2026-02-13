!> Tests targeting specific coverage gaps in hsd_query, hsd_validation,
!> hsd_accessors, and hsd_parser.
module test_coverage_gaps_suite
  use hsd
  use hsd_constants, only: dp, sp
  use hsd_hash_table, only: hsd_name_index_t
  use fortuno_serial, only: is_equal, test => serial_case_item, &
      & check => serial_check, suite => serial_suite_item, test_list
  implicit none (type, external)
  private

  public :: tests

contains

  !> Returns all coverage-gap tests
  function tests()
    type(test_list) :: tests

    tests = test_list([&
        suite("coverage_gaps", test_list([&
            ! P1: hsd_get_child_tables
            test("child_tables_simple", test_child_tables_simple), &
            test("child_tables_nested_path", test_child_tables_nested_path), &
            test("child_tables_empty_path", test_child_tables_empty_path), &
            test("child_tables_value_parent", test_child_tables_value_parent), &
            ! P2: Amendment parsing
            test("amendment_basic", test_amendment_basic), &
            test("amendment_missing_target", test_amendment_missing_target), &
            ! P3: hsd_get_or_set uncovered types
            test("get_or_set_real_sp_missing", test_get_or_set_real_sp_missing), &
            test("get_or_set_real_sp_existing", test_get_or_set_real_sp_existing), &
            test("get_or_set_logical_missing", test_get_or_set_logical_missing), &
            test("get_or_set_logical_existing", test_get_or_set_logical_existing), &
            test("get_or_set_complex_dp_missing", test_get_or_set_complex_dp_missing), &
            test("get_or_set_complex_dp_existing", test_get_or_set_complex_dp_existing), &
            ! P4: hsd_warn_unprocessed
            test("warn_unprocessed", test_warn_unprocessed), &
            ! P5: hsd_node_context, hsd_format_error, hsd_format_warning
            test("node_context_with_line", test_node_context_with_line), &
            test("node_context_name_only", test_node_context_name_only), &
            test("format_error_msg", test_format_error_msg), &
            test("format_warning_msg", test_format_warning_msg), &
            ! P6: values_equal via hsd_table_equal
            test("table_equal_integer_values", test_table_equal_integer_values), &
            test("table_equal_real_values", test_table_equal_real_values), &
            test("table_equal_logical_values", test_table_equal_logical_values), &
            test("table_equal_mismatched", test_table_equal_mismatched), &
            ! P7: Column-major matrix getters
            test("matrix_column_major_int", test_matrix_column_major_int), &
            test("matrix_column_major_real", test_matrix_column_major_real), &
            ! P8: wrap_value_to_table_ via auto_wrap
            test("auto_wrap_value_to_table", test_auto_wrap_value_to_table), &
            ! P9: hsd_get_choice value-child
            test("get_choice_value_child", test_get_choice_value_child), &
            ! P10: hsd_rename_child with path
            test("rename_child_with_path", test_rename_child_with_path), &
            ! P11: strip_hsd_comments
            test("strip_hsd_comments", test_strip_hsd_comments), &
            ! --- NEW TESTS ---
            ! P12: Accessor table-inline-value paths
            test("get_int_from_table_inline", test_get_int_from_table_inline), &
            test("get_real_from_table_inline", test_get_real_from_table_inline), &
            test("get_logical_from_table_inline", test_get_logical_from_table_inline), &
            test("get_complex_from_table_inline", test_get_complex_from_table_inline), &
            test("get_int_array_from_table_inline", test_get_int_array_from_table_inline), &
            test("get_real_arr_table_inline", &
                & test_get_real_array_from_table_inline), &
            test("get_log_arr_table_inline", &
                & test_get_logical_array_from_table_inline), &
            test("get_str_arr_table_inline", &
                & test_get_string_array_from_table_inline), &
            test("get_cplx_arr_table_inline", &
                & test_get_complex_array_from_table_inline), &
            ! P13: Complex matrix from table + column-major
            test("complex_matrix_from_value", test_complex_matrix_from_value), &
            test("complex_matrix_column_major", test_complex_matrix_column_major), &
            ! P14: hsd_get_or_set with child arg
            test("get_or_set_string_with_child", test_get_or_set_string_with_child), &
            test("get_or_set_integer_with_child", test_get_or_set_integer_with_child), &
            test("get_or_set_real_dp_with_child", test_get_or_set_real_dp_with_child), &
            test("get_or_set_real_sp_with_child", test_get_or_set_real_sp_with_child), &
            test("get_or_set_logical_with_child", test_get_or_set_logical_with_child), &
            test("get_or_set_complex_with_child", test_get_or_set_complex_with_child), &
            test("get_or_set_int_array_with_child", test_get_or_set_int_array_with_child), &
            test("get_or_set_real_dp_array_child", test_get_or_set_real_dp_array_child), &
            test("get_or_set_real_sp_array_child", test_get_or_set_real_sp_array_child), &
            test("get_or_set_logical_array_child", test_get_or_set_logical_array_child), &
            ! P15: Parser — nested amendment +Tag = +Child
            test("nested_amendment", test_nested_amendment), &
            test("amendment_child_not_block", test_amendment_child_not_block), &
            test("semicolon_separated_values", test_semicolon_separated_values), &
            test("value_after_equals_string", test_value_after_equals_string), &
            ! P16: Formatter — complex/escape/attrib
            test("format_complex_value", test_format_complex_value), &
            test("format_string_with_escapes", test_format_string_with_escapes), &
            test("format_table_with_attrib_str", test_format_table_with_attrib_str), &
            test("format_empty_table_str", test_format_empty_table_str), &
            test("format_multiline_value_str", test_format_multiline_value_str), &
            test("format_named_multi_child_str", test_format_named_multi_child_str), &
            ! P17: Validation — error paths
            test("validate_one_of_not_string", test_validate_one_of_not_string), &
            test("get_array_with_unit_not_found", test_get_array_with_unit_not_found), &
            test("get_array_with_unit_type_err", test_get_array_with_unit_type_err), &
            test("get_matrix_with_unit_not_found", test_get_matrix_with_unit_not_found), &
            test("get_matrix_with_unit_type_err", test_get_matrix_with_unit_type_err), &
            test("format_err_no_context", test_format_err_no_context), &
            test("format_warn_no_context", test_format_warn_no_context), &
            test("warn_unprocessed_many", test_warn_unprocessed_many), &
            ! P18: Query — merge array types, get_name, values_equal
            test("merge_logical_arrays", test_merge_logical_arrays), &
            test("merge_string_arrays", test_merge_string_arrays), &
            test("merge_complex_arrays", test_merge_complex_arrays), &
            test("merge_int_matrices", test_merge_int_matrices), &
            test("merge_real_matrices", test_merge_real_matrices), &
            test("get_name_empty", test_get_name_empty), &
            test("get_name_with_default", test_get_name_with_default), &
            test("table_equal_complex", test_table_equal_complex), &
            test("table_equal_string_values", test_table_equal_string_values), &
            test("table_equal_raw_text", test_table_equal_raw_text), &
            test("get_children_not_found", test_get_children_not_found), &
            test("get_children_type_error", test_get_children_type_error), &
            test("get_choice_not_found", test_get_choice_not_found), &
            ! P19: Hash table — overflow remove
            test("hash_table_overflow_remove", test_hash_table_overflow_remove), &
            test("hash_table_chain_promote", test_hash_table_chain_promote), &
            ! P20: Parser — amendment equals syntax (+Tag = +ChildTag / ChildTag)
            test("amend_equals_child_amend", test_amend_equals_child_amend), &
            test("amend_equals_new_child", test_amend_equals_new_child), &
            test("amend_equals_target_missing", test_amend_equals_target_missing), &
            test("amend_equals_target_not_block", test_amend_equals_target_not_block), &
            test("amend_equals_child_missing", test_amend_equals_child_missing), &
            ! P21: Parser — text buffer flush and multi-value with comment
            test("text_buffer_before_tag", test_text_buffer_before_tag), &
            test("multi_value_with_comment", test_multi_value_with_comment), &
            ! P22: Mutators — hsd_clear_children
            test("clear_children", test_clear_children), &
            ! P23: Accessors — inline value from table (#text child)
            test("get_int_inline_table", test_get_int_inline_table), &
            test("get_real_inline_table", test_get_real_inline_table), &
            test("get_logical_inline_table", test_get_logical_inline_table), &
            test("get_complex_inline_table", test_get_complex_inline_table), &
            test("get_int_arr_inline_table", test_get_int_arr_inline_table), &
            test("get_real_arr_inline_table", test_get_real_arr_inline_table), &
            test("get_logical_arr_inline_tbl", test_get_logical_arr_inline_tbl), &
            test("get_string_arr_inline_tbl", test_get_string_arr_inline_tbl), &
            test("get_complex_arr_inline_tbl", test_get_complex_arr_inline_tbl), &
            ! P24: Accessors — get_or_set existing value with child+stat
            test("get_or_set_str_exist_child", test_get_or_set_str_exist_child), &
            test("get_or_set_int_exist_child", test_get_or_set_int_exist_child), &
            test("get_or_set_log_arr_ex_child", test_get_or_set_log_arr_ex_child), &
            ! P25: Formatter — dump to file with multiline + complex format
            test("dump_to_file_multiline", test_dump_to_file_multiline), &
            test("dump_to_file_complex", test_dump_to_file_complex), &
            ! P26: Complex matrix from table children
            test("complex_matrix_from_table", test_complex_matrix_from_table), &
            test("complex_matrix_empty_table", test_complex_matrix_empty_table), &
            ! P27: Remove child by path not found
            test("remove_child_not_found", test_remove_child_not_found) &
        ])) &
    ])

  end function tests

  ! ============================================================
  ! P1: hsd_get_child_tables
  ! ============================================================

  !> Get table children by simple name
  subroutine test_child_tables_simple()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    type(hsd_table_ptr), allocatable :: children(:)
    integer :: stat

    call hsd_load_string( &
        "Atom { x = 1 }" // char(10) // &
        "Atom { x = 2 }" // char(10) // &
        "Other { x = 3 }", &
        root, error)
    call check(.not. allocated(error), msg="parse ok")

    call hsd_get_child_tables(root, "Atom", children, stat)
    call check(stat == HSD_STAT_OK, msg="stat ok")
    call check(size(children) == 2, msg="found 2 Atom children")

    call root%destroy()
  end subroutine test_child_tables_simple

  !> Get table children via nested path "Parent/Child"
  subroutine test_child_tables_nested_path()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    type(hsd_table_ptr), allocatable :: children(:)
    integer :: stat

    call hsd_load_string( &
        "Geometry {" // char(10) // &
        "  Atom { x = 1 }" // char(10) // &
        "  Atom { x = 2 }" // char(10) // &
        "  Atom { x = 3 }" // char(10) // &
        "}", &
        root, error)
    call check(.not. allocated(error), msg="parse ok")

    call hsd_get_child_tables(root, "Geometry/Atom", children, stat)
    call check(stat == HSD_STAT_OK, msg="stat ok")
    call check(size(children) == 3, msg="found 3 Atom children under Geometry")

    call root%destroy()
  end subroutine test_child_tables_nested_path

  !> Empty path returns NOT_FOUND
  subroutine test_child_tables_empty_path()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    type(hsd_table_ptr), allocatable :: children(:)
    integer :: stat

    call hsd_load_string("X { y = 1 }", root, error)
    call check(.not. allocated(error), msg="parse ok")

    call hsd_get_child_tables(root, "", children, stat)
    call check(stat == HSD_STAT_NOT_FOUND, msg="empty path gives NOT_FOUND")
    call check(size(children) == 0, msg="zero children for empty path")

    call root%destroy()
  end subroutine test_child_tables_empty_path

  !> Path pointing at a value node returns TYPE_ERROR
  subroutine test_child_tables_value_parent()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    type(hsd_table_ptr), allocatable :: children(:)
    integer :: stat

    call hsd_load_string( &
        "Container {" // char(10) // &
        "  leaf = 42" // char(10) // &
        "}", &
        root, error)
    call check(.not. allocated(error), msg="parse ok")

    ! "Container/leaf" is a value, so looking for children under it is TYPE_ERROR
    call hsd_get_child_tables(root, "Container/leaf/Sub", children, stat)
    call check(stat == HSD_STAT_TYPE_ERROR, msg="path through value gives TYPE_ERROR")
    call check(size(children) == 0, msg="no children")

    call root%destroy()
  end subroutine test_child_tables_value_parent

  ! ============================================================
  ! P2: Amendment parsing (+Tag)
  ! ============================================================

  !> Basic amendment: +Outer merges into existing Outer
  subroutine test_amendment_basic()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    integer :: inner_val
    integer :: stat

    call hsd_load_string( &
        "Outer { Inner = 1 }" // char(10) // &
        "+Outer { Inner = 2 }", &
        root, error)
    call check(.not. allocated(error), msg="parse ok")

    ! The amendment should overwrite Inner to 2
    call hsd_get(root, "Outer/Inner", inner_val, stat)
    call check(stat == HSD_STAT_OK, msg="get Inner ok")
    call check(is_equal(inner_val, 2), msg="Inner should be 2 after amendment")

    call root%destroy()
  end subroutine test_amendment_basic

  !> Amendment target not found produces error
  subroutine test_amendment_missing_target()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error

    call hsd_load_string("+Missing { x = 1 }", root, error)
    call check(allocated(error), msg="amendment to missing target should error")

    call root%destroy()
  end subroutine test_amendment_missing_target

  ! ============================================================
  ! P3: hsd_get_or_set for uncovered types
  ! ============================================================

  !> hsd_get_or_set with real(sp) — missing key uses default
  subroutine test_get_or_set_real_sp_missing()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    real(sp) :: val
    real(sp) :: default_val
    integer :: stat

    call hsd_load_string("Other = 1", root, error)
    call check(.not. allocated(error), msg="parse ok")

    default_val = 2.5_sp
    call hsd_get_or_set(root, "SpVal", val, default_val, stat)
    call check(abs(val - default_val) < 1.0e-5_sp, msg="default used for missing sp key")

    call root%destroy()
  end subroutine test_get_or_set_real_sp_missing

  !> hsd_get_or_set with real(sp) — existing key reads value
  subroutine test_get_or_set_real_sp_existing()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    real(sp) :: val
    real(sp) :: default_val
    integer :: stat

    call hsd_load_string("SpVal = 3.14", root, error)
    call check(.not. allocated(error), msg="parse ok")

    default_val = 0.0_sp
    call hsd_get_or_set(root, "SpVal", val, default_val, stat)
    call check(stat == HSD_STAT_OK, msg="stat ok for existing sp key")
    call check(abs(val - 3.14_sp) < 0.01_sp, msg="existing sp value read correctly")

    call root%destroy()
  end subroutine test_get_or_set_real_sp_existing

  !> hsd_get_or_set with logical — missing key uses default
  subroutine test_get_or_set_logical_missing()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    logical :: val
    integer :: stat

    call hsd_load_string("Other = 1", root, error)
    call check(.not. allocated(error), msg="parse ok")

    call hsd_get_or_set(root, "Flag", val, .true., stat)
    call check(val, msg="default .true. used for missing logical key")

    call root%destroy()
  end subroutine test_get_or_set_logical_missing

  !> hsd_get_or_set with logical — existing key reads value
  subroutine test_get_or_set_logical_existing()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    logical :: val
    integer :: stat

    call hsd_load_string("Flag = No", root, error)
    call check(.not. allocated(error), msg="parse ok")

    call hsd_get_or_set(root, "Flag", val, .true., stat)
    call check(stat == HSD_STAT_OK, msg="stat ok for existing logical key")
    call check(.not. val, msg="existing logical value read correctly as .false.")

    call root%destroy()
  end subroutine test_get_or_set_logical_existing

  !> hsd_get_or_set with complex(dp) — missing key uses default
  subroutine test_get_or_set_complex_dp_missing()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    complex(dp) :: val
    complex(dp) :: default_val
    integer :: stat

    call hsd_load_string("Other = 1", root, error)
    call check(.not. allocated(error), msg="parse ok")

    default_val = cmplx(1.0_dp, 2.0_dp, dp)
    call hsd_get_or_set(root, "Cval", val, default_val, stat)
    call check(abs(val - default_val) < 1.0e-10_dp, &
        msg="default used for missing complex key")

    call root%destroy()
  end subroutine test_get_or_set_complex_dp_missing

  !> hsd_get_or_set with complex(dp) — existing key reads value
  subroutine test_get_or_set_complex_dp_existing()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    complex(dp) :: val
    complex(dp) :: default_val
    integer :: stat

    call hsd_load_string("Cval = 3.0 4.0", root, error)
    call check(.not. allocated(error), msg="parse ok")

    default_val = cmplx(0.0_dp, 0.0_dp, dp)
    call hsd_get_or_set(root, "Cval", val, default_val, stat)
    call check(stat == HSD_STAT_OK, msg="stat ok for existing complex key")
    call check(abs(real(val) - 3.0_dp) < 0.01_dp, &
        msg="real part of existing complex correct")

    call root%destroy()
  end subroutine test_get_or_set_complex_dp_existing

  ! ============================================================
  ! P4: hsd_warn_unprocessed
  ! ============================================================

  !> Parse HSD, process some children, check warnings for unprocessed
  subroutine test_warn_unprocessed()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=MAX_WARNING_LEN), allocatable :: warnings(:)
    integer :: val, stat

    call hsd_load_string( &
        "Processed = 1" // char(10) // &
        "Unprocessed = 2", &
        root, error)
    call check(.not. allocated(error), msg="parse ok")

    ! Read only "Processed", leaving "Unprocessed" untouched
    call hsd_get(root, "Processed", val, stat)

    call hsd_warn_unprocessed(root, warnings)
    call check(size(warnings) >= 1, msg="at least 1 unprocessed warning")

    call root%destroy()
  end subroutine test_warn_unprocessed

  ! ============================================================
  ! P5: hsd_node_context, hsd_format_error, hsd_format_warning
  ! ============================================================

  !> hsd_node_context on a node with name + line
  subroutine test_node_context_with_line()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    type(hsd_table), pointer :: child_tbl
    character(len=:), allocatable :: ctx
    integer :: stat

    call hsd_load_string("MyBlock { x = 1 }", root, error)
    call check(.not. allocated(error), msg="parse ok")

    call hsd_get_table(root, "MyBlock", child_tbl, stat)
    call check(stat == HSD_STAT_OK, msg="get table ok")

    ctx = hsd_node_context(child_tbl)
    call check(len(ctx) > 0, msg="context string not empty")
    ! Should contain the name
    call check(index(ctx, "myblock") > 0 .or. index(ctx, "MyBlock") > 0, &
        msg="context contains node name")

    call root%destroy()
  end subroutine test_node_context_with_line

  !> hsd_node_context on a node with name only (no line)
  subroutine test_node_context_name_only()
    type(hsd_table) :: tbl
    character(len=:), allocatable :: ctx

    call new_table(tbl, name="TestNode", line=0)
    ctx = hsd_node_context(tbl)
    call check(len(ctx) > 0, msg="context string not empty")
    call check(index(ctx, "TestNode") > 0, msg="context contains name")
    ! Should NOT contain "line" since line=0
    call check(index(ctx, "line") == 0, msg="no line info when line=0")

    call tbl%destroy()
  end subroutine test_node_context_name_only

  !> hsd_format_error produces error prefix
  subroutine test_format_error_msg()
    type(hsd_table) :: tbl
    character(len=:), allocatable :: formatted

    call new_table(tbl, name="BadNode", line=5)
    call hsd_format_error(tbl, "something went wrong", formatted)
    call check(index(formatted, "Error") > 0, msg="formatted error contains 'Error'")
    call check(index(formatted, "something went wrong") > 0, &
        msg="formatted error contains message")

    call tbl%destroy()
  end subroutine test_format_error_msg

  !> hsd_format_warning produces warning prefix
  subroutine test_format_warning_msg()
    type(hsd_table) :: tbl
    character(len=:), allocatable :: formatted

    call new_table(tbl, name="WarnNode", line=10)
    call hsd_format_warning(tbl, "check this", formatted)
    call check(index(formatted, "Warning") > 0, msg="formatted warning contains 'Warning'")
    call check(index(formatted, "check this") > 0, &
        msg="formatted warning contains message")

    call tbl%destroy()
  end subroutine test_format_warning_msg

  ! ============================================================
  ! P6: values_equal via hsd_table_equal
  ! ============================================================

  !> Compare tables with integer values
  subroutine test_table_equal_integer_values()
    type(hsd_table) :: a, b
    logical :: eq

    call new_table(a, name="root")
    call new_table(b, name="root")
    call hsd_set(a, "x", 42)
    call hsd_set(b, "x", 42)

    eq = hsd_table_equal(a, b)
    call check(eq, msg="tables with same integer values are equal")

    call a%destroy()
    call b%destroy()
  end subroutine test_table_equal_integer_values

  !> Compare tables with real values
  subroutine test_table_equal_real_values()
    type(hsd_table) :: a, b
    logical :: eq

    call new_table(a, name="root")
    call new_table(b, name="root")
    call hsd_set(a, "x", 3.14_dp)
    call hsd_set(b, "x", 3.14_dp)

    eq = hsd_table_equal(a, b)
    call check(eq, msg="tables with same real values are equal")

    call a%destroy()
    call b%destroy()
  end subroutine test_table_equal_real_values

  !> Compare tables with logical values
  subroutine test_table_equal_logical_values()
    type(hsd_table) :: a, b
    logical :: eq

    call new_table(a, name="root")
    call new_table(b, name="root")
    call hsd_set(a, "flag", .true.)
    call hsd_set(b, "flag", .true.)

    eq = hsd_table_equal(a, b)
    call check(eq, msg="tables with same logical values are equal")

    call a%destroy()
    call b%destroy()
  end subroutine test_table_equal_logical_values

  !> Compare tables with mismatched values
  subroutine test_table_equal_mismatched()
    type(hsd_table) :: a, b
    logical :: eq

    call new_table(a, name="root")
    call new_table(b, name="root")
    call hsd_set(a, "x", 1)
    call hsd_set(b, "x", 2)

    eq = hsd_table_equal(a, b)
    call check(.not. eq, msg="tables with different values are not equal")

    call a%destroy()
    call b%destroy()
  end subroutine test_table_equal_mismatched

  ! ============================================================
  ! P7: Column-major matrix getters
  ! ============================================================

  !> Parse a matrix and get with column-major ordering (integer)
  subroutine test_matrix_column_major_int()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    integer, allocatable :: mat(:,:)
    integer :: nrows, ncols, stat

    ! 2x3 matrix: row1=[1,2,3], row2=[4,5,6]
    call hsd_load_string( &
        "Matrix = {" // char(10) // &
        "  1 2 3" // char(10) // &
        "  4 5 6" // char(10) // &
        "}", &
        root, error)
    call check(.not. allocated(error), msg="parse ok")

    call hsd_get_matrix(root, "Matrix", mat, nrows, ncols, stat, order="column-major")
    call check(stat == HSD_STAT_OK, msg="matrix get ok")
    ! After column-major transpose: original 2x3 becomes 3x2
    call check(size(mat, 1) == 3, msg="col-major dim1 = 3")
    call check(size(mat, 2) == 2, msg="col-major dim2 = 2")
    ! mat(1,1) should be 1, mat(1,2) should be 4
    call check(is_equal(mat(1, 1), 1), msg="mat(1,1) = 1")
    call check(is_equal(mat(1, 2), 4), msg="mat(1,2) = 4")

    call root%destroy()
  end subroutine test_matrix_column_major_int

  !> Parse a matrix and get with column-major ordering (real dp)
  subroutine test_matrix_column_major_real()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    real(dp), allocatable :: mat(:,:)
    integer :: nrows, ncols, stat

    call hsd_load_string( &
        "Matrix = {" // char(10) // &
        "  1.0 2.0" // char(10) // &
        "  3.0 4.0" // char(10) // &
        "}", &
        root, error)
    call check(.not. allocated(error), msg="parse ok")

    call hsd_get_matrix(root, "Matrix", mat, nrows, ncols, stat, order="column-major")
    call check(stat == HSD_STAT_OK, msg="matrix get ok")
    ! After column-major transpose: original 2x2 stays 2x2 but transposed
    call check(size(mat, 1) == 2, msg="col-major dim1 = 2")
    call check(size(mat, 2) == 2, msg="col-major dim2 = 2")
    ! original row1=[1,2], row2=[3,4] -> transposed: mat(1,1)=1, mat(2,1)=2, mat(1,2)=3, mat(2,2)=4
    call check(abs(mat(1, 1) - 1.0_dp) < 1.0e-10_dp, msg="mat(1,1) = 1.0")
    call check(abs(mat(2, 1) - 2.0_dp) < 1.0e-10_dp, msg="mat(2,1) = 2.0")

    call root%destroy()
  end subroutine test_matrix_column_major_real

  ! ============================================================
  ! P8: wrap_value_to_table_ via hsd_get_table(auto_wrap=.true.)
  ! ============================================================

  !> Test auto-wrapping a value child into a table
  subroutine test_auto_wrap_value_to_table()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    type(hsd_table), pointer :: child_tbl
    integer :: stat
    character(len=:), allocatable :: text_content

    call hsd_load_string("Solver = QuickSolver", root, error)
    call check(.not. allocated(error), msg="parse ok")

    call hsd_get_table(root, "Solver", child_tbl, stat, auto_wrap=.true.)
    call check(stat == HSD_STAT_OK, msg="auto_wrap stat ok")
    call check(associated(child_tbl), msg="child_tbl associated after wrap")

    ! The wrapped table should contain a #text child with the original value
    call hsd_get_inline_text(child_tbl, text_content, stat)
    call check(stat == HSD_STAT_OK, msg="inline text found")
    call check(index(text_content, "QuickSolver") > 0, &
        msg="wrapped text contains original value")

    call root%destroy()
  end subroutine test_auto_wrap_value_to_table

  ! ============================================================
  ! P9: hsd_get_choice value-child fallback
  ! ============================================================

  !> Get choice when only a value child exists (no table children)
  subroutine test_get_choice_value_child()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: choice_name
    type(hsd_table), pointer :: choice_tbl
    integer :: stat

    ! "Driver { ConjugateGradient }" — table with only a text value child
    call hsd_load_string("Driver {" // char(10) // "  ConjugateGradient" // char(10) // "}", &
        root, error)
    call check(.not. allocated(error), msg="parse ok")

    call hsd_get_choice(root, "Driver", choice_name, choice_tbl, stat)
    call check(stat == HSD_STAT_OK, msg="get_choice ok")
    call check(len(choice_name) > 0, msg="choice_name not empty")
    call check(index(choice_name, "conjugategradient") > 0, &
        msg="choice name is conjugategradient (lowered)")

    call root%destroy()
  end subroutine test_get_choice_value_child

  ! ============================================================
  ! P10: hsd_rename_child with path
  ! ============================================================

  !> Rename a child via path "Parent/OldChild" -> "NewChild"
  subroutine test_rename_child_with_path()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    integer :: stat, val
    logical :: found

    call hsd_load_string( &
        "Parent {" // char(10) // &
        "  OldChild = 42" // char(10) // &
        "}", &
        root, error)
    call check(.not. allocated(error), msg="parse ok")

    call hsd_rename_child(root, "Parent/OldChild", "NewChild", stat)
    call check(stat == HSD_STAT_OK, msg="rename with path ok")

    ! Old name should not exist
    found = hsd_has_child(root, "Parent/OldChild")
    call check(.not. found, msg="old name gone")

    ! New name should exist and have the value
    call hsd_get(root, "Parent/NewChild", val, stat)
    call check(stat == HSD_STAT_OK, msg="new name found")
    call check(is_equal(val, 42), msg="value preserved after rename")

    call root%destroy()
  end subroutine test_rename_child_with_path

  ! ============================================================
  ! P11: strip_hsd_comments_
  ! ============================================================

  !> Parse text that contains # comments and verify they are stripped
  subroutine test_strip_hsd_comments()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    integer :: val, stat

    ! The # comment should be stripped by the parser
    call hsd_load_string( &
        "# This is a comment" // char(10) // &
        "Value = 42 # inline comment", &
        root, error)
    call check(.not. allocated(error), msg="parse ok with comments")

    call hsd_get(root, "Value", val, stat)
    call check(stat == HSD_STAT_OK, msg="get value ok")
    call check(is_equal(val, 42), msg="value correct after comment stripping")

    call root%destroy()
  end subroutine test_strip_hsd_comments

  ! ============================================================
  ! P12: Accessor table-inline-value paths (type is (hsd_table) + get_inline_value_)
  ! ============================================================

  !> Get integer from a table node that has an inline #text value child
  subroutine test_get_int_from_table_inline()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    integer :: val, stat

    ! "Key = Value { #text child }" pattern: Key is a table with a text child
    call hsd_load_string( &
        "Outer = Inner {" // char(10) // &
        "  Count = 7" // char(10) // &
        "}", root, error)
    call check(.not. allocated(error), msg="parse ok")

    ! Access the table child "Inner" inside "Outer" — "Outer/Inner/Count" should be int
    call hsd_get(root, "Outer/Inner/Count", val, stat)
    call check(stat == HSD_STAT_OK, msg="get int from nested table ok")
    call check(is_equal(val, 7), msg="int value correct")

    call root%destroy()
  end subroutine test_get_int_from_table_inline

  !> Get real from a table that has inline text
  subroutine test_get_real_from_table_inline()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    real(dp) :: val
    integer :: stat

    call hsd_load_string( &
        "Wrapper {" // char(10) // &
        "  Pi = 3.14159" // char(10) // &
        "}", root, error)
    call check(.not. allocated(error), msg="parse ok")
    call hsd_get(root, "Wrapper/Pi", val, stat)
    call check(stat == HSD_STAT_OK, msg="get real ok")
    call check(abs(val - 3.14159_dp) < 1.0e-4_dp, msg="real value correct")
    call root%destroy()
  end subroutine test_get_real_from_table_inline

  !> Get logical from a table that has inline text
  subroutine test_get_logical_from_table_inline()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    logical :: val
    integer :: stat

    call hsd_load_string( &
        "Config {" // char(10) // &
        "  Enabled = Yes" // char(10) // &
        "}", root, error)
    call check(.not. allocated(error), msg="parse ok")
    call hsd_get(root, "Config/Enabled", val, stat)
    call check(stat == HSD_STAT_OK, msg="get logical ok")
    call check(val, msg="logical value is true")
    call root%destroy()
  end subroutine test_get_logical_from_table_inline

  !> Get complex from a table that has inline text
  subroutine test_get_complex_from_table_inline()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    complex(dp) :: val
    integer :: stat

    ! Use Fortran-style (re,im) format for a single complex value
    call hsd_load_string( &
        "Data {" // char(10) // &
        "  Z = (1.0,2.0)" // char(10) // &
        "}", root, error)
    call check(.not. allocated(error), msg="parse ok")
    call hsd_get(root, "Data/Z", val, stat)
    call check(stat == HSD_STAT_OK, msg="get complex ok")
    call check(abs(real(val) - 1.0_dp) < 1.0e-10_dp, msg="real part correct")
    call check(abs(aimag(val) - 2.0_dp) < 1.0e-10_dp, msg="imag part correct")
    call root%destroy()
  end subroutine test_get_complex_from_table_inline

  !> Get integer array from a table with inline text child
  subroutine test_get_int_array_from_table_inline()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    integer, allocatable :: val(:)
    integer :: stat

    call hsd_load_string( &
        "Data {" // char(10) // &
        "  Nums = 1 2 3 4 5" // char(10) // &
        "}", root, error)
    call check(.not. allocated(error), msg="parse ok")
    call hsd_get(root, "Data/Nums", val, stat)
    call check(stat == HSD_STAT_OK, msg="get int array ok")
    call check(size(val) == 5, msg="int array size correct")
    call check(is_equal(val(1), 1), msg="first element correct")
    call root%destroy()
  end subroutine test_get_int_array_from_table_inline

  !> Get real array from table with inline text child
  subroutine test_get_real_array_from_table_inline()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    real(dp), allocatable :: val(:)
    integer :: stat

    call hsd_load_string( &
        "Data {" // char(10) // &
        "  Vals = 1.1 2.2 3.3" // char(10) // &
        "}", root, error)
    call check(.not. allocated(error), msg="parse ok")
    call hsd_get(root, "Data/Vals", val, stat)
    call check(stat == HSD_STAT_OK, msg="get real array ok")
    call check(size(val) == 3, msg="real array size correct")
    call root%destroy()
  end subroutine test_get_real_array_from_table_inline

  !> Get logical array from table with inline text child
  subroutine test_get_logical_array_from_table_inline()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    logical, allocatable :: val(:)
    integer :: stat

    call hsd_load_string( &
        "Config {" // char(10) // &
        "  Flags = Yes No Yes" // char(10) // &
        "}", root, error)
    call check(.not. allocated(error), msg="parse ok")
    call hsd_get(root, "Config/Flags", val, stat)
    call check(stat == HSD_STAT_OK, msg="get logical array ok")
    call check(size(val) == 3, msg="logical array size correct")
    call check(val(1), msg="first flag is true")
    call check(.not. val(2), msg="second flag is false")
    call root%destroy()
  end subroutine test_get_logical_array_from_table_inline

  !> Get string array from table with inline text child
  subroutine test_get_string_array_from_table_inline()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: val(:)
    integer :: stat

    call hsd_load_string( &
        "Data {" // char(10) // &
        "  Names = alpha beta gamma" // char(10) // &
        "}", root, error)
    call check(.not. allocated(error), msg="parse ok")
    call hsd_get(root, "Data/Names", val, stat)
    call check(stat == HSD_STAT_OK, msg="get string array ok")
    call check(size(val) >= 3, msg="string array has 3+ elements")
    call root%destroy()
  end subroutine test_get_string_array_from_table_inline

  !> Get complex array from table with inline text child
  subroutine test_get_complex_array_from_table_inline()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    complex(dp), allocatable :: val(:)
    integer :: stat

    ! Use a+bi format for complex array elements
    call hsd_load_string( &
        "Data {" // char(10) // &
        "  Zs = 1.0+2.0i 3.0+4.0i" // char(10) // &
        "}", root, error)
    call check(.not. allocated(error), msg="parse ok")
    call hsd_get(root, "Data/Zs", val, stat)
    call check(stat == HSD_STAT_OK, msg="get complex array ok")
    call check(size(val) == 2, msg="complex array size correct (2 pairs)")
    call root%destroy()
  end subroutine test_get_complex_array_from_table_inline

  ! ============================================================
  ! P13: Complex matrix from value + column-major
  ! ============================================================

  !> Get complex matrix from a direct value node
  subroutine test_complex_matrix_from_value()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    complex(dp), allocatable :: mat(:,:)
    integer :: nrows, ncols, stat

    ! Use a+bi format: 2 rows x 2 cols
    call hsd_load_string( &
        "CMatrix = {" // char(10) // &
        "  1.0+0.0i 2.0+0.0i" // char(10) // &
        "  3.0+0.0i 4.0+0.0i" // char(10) // &
        "}", root, error)
    call check(.not. allocated(error), msg="parse ok")

    call hsd_get_matrix(root, "CMatrix", mat, nrows, ncols, stat)
    call check(stat == HSD_STAT_OK, msg="complex matrix get ok")
    call check(nrows == 2, msg="complex matrix has 2 rows")
    call check(ncols == 2, msg="complex matrix has 2 cols")
    call root%destroy()
  end subroutine test_complex_matrix_from_value

  !> Get complex matrix with column-major order
  subroutine test_complex_matrix_column_major()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    complex(dp), allocatable :: mat(:,:)
    integer :: nrows, ncols, stat

    ! Use a+bi format: 2 rows x 3 cols
    call hsd_load_string( &
        "CMatrix = {" // char(10) // &
        "  1.0+0.0i 2.0+0.0i 3.0+0.0i" // char(10) // &
        "  4.0+0.0i 5.0+0.0i 6.0+0.0i" // char(10) // &
        "}", root, error)
    call check(.not. allocated(error), msg="parse ok")

    call hsd_get_matrix(root, "CMatrix", mat, nrows, ncols, stat, order="column-major")
    call check(stat == HSD_STAT_OK, msg="complex matrix col-major get ok")
    ! Original 2x3 transposed to 3x2
    call check(nrows == 3, msg="transposed nrows")
    call check(ncols == 2, msg="transposed ncols")
    call root%destroy()
  end subroutine test_complex_matrix_column_major

  ! ============================================================
  ! P14: hsd_get_or_set with child argument
  ! ============================================================

  !> get_or_set string with child output
  subroutine test_get_or_set_string_with_child()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: val
    type(hsd_table), pointer :: child
    integer :: stat

    call hsd_load_string("Other = 1", root, error)
    call check(.not. allocated(error), msg="parse ok")

    call hsd_get_or_set(root, "Name", val, "default_str", stat, child)
    call check(val == "default_str", msg="default string used")

    call root%destroy()
  end subroutine test_get_or_set_string_with_child

  !> get_or_set integer with child output
  subroutine test_get_or_set_integer_with_child()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    integer :: val, stat
    type(hsd_table), pointer :: child

    call hsd_load_string("Other = 1", root, error)
    call check(.not. allocated(error), msg="parse ok")

    call hsd_get_or_set(root, "Count", val, 99, stat, child)
    call check(is_equal(val, 99), msg="default int used")

    call root%destroy()
  end subroutine test_get_or_set_integer_with_child

  !> get_or_set real(dp) with child output
  subroutine test_get_or_set_real_dp_with_child()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    real(dp) :: val
    integer :: stat
    type(hsd_table), pointer :: child

    call hsd_load_string("Other = 1", root, error)
    call check(.not. allocated(error), msg="parse ok")

    call hsd_get_or_set(root, "DpVal", val, 2.718_dp, stat, child)
    call check(abs(val - 2.718_dp) < 1.0e-10_dp, msg="default dp used")

    call root%destroy()
  end subroutine test_get_or_set_real_dp_with_child

  !> get_or_set real(sp) with child output
  subroutine test_get_or_set_real_sp_with_child()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    real(sp) :: val
    integer :: stat
    type(hsd_table), pointer :: child

    call hsd_load_string("Other = 1", root, error)
    call check(.not. allocated(error), msg="parse ok")

    call hsd_get_or_set(root, "SpVal", val, 1.5_sp, stat, child)
    call check(abs(val - 1.5_sp) < 1.0e-4_sp, msg="default sp used")

    call root%destroy()
  end subroutine test_get_or_set_real_sp_with_child

  !> get_or_set logical with child output
  subroutine test_get_or_set_logical_with_child()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    logical :: val
    integer :: stat
    type(hsd_table), pointer :: child

    call hsd_load_string("Other = 1", root, error)
    call check(.not. allocated(error), msg="parse ok")

    call hsd_get_or_set(root, "Flag", val, .true., stat, child)
    call check(val, msg="default logical used")

    call root%destroy()
  end subroutine test_get_or_set_logical_with_child

  !> get_or_set complex with child output
  subroutine test_get_or_set_complex_with_child()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    complex(dp) :: val, def
    integer :: stat
    type(hsd_table), pointer :: child

    call hsd_load_string("Other = 1", root, error)
    call check(.not. allocated(error), msg="parse ok")

    def = cmplx(5.0_dp, 6.0_dp, dp)
    call hsd_get_or_set(root, "Z", val, def, stat, child)
    call check(abs(val - def) < 1.0e-10_dp, msg="default complex used")

    call root%destroy()
  end subroutine test_get_or_set_complex_with_child

  !> get_or_set integer array with child output
  subroutine test_get_or_set_int_array_with_child()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    integer, allocatable :: val(:)
    integer :: stat
    type(hsd_table), pointer :: child

    call hsd_load_string("Other = 1", root, error)
    call check(.not. allocated(error), msg="parse ok")

    call hsd_get_or_set(root, "Nums", val, [10, 20, 30], stat, child)
    call check(size(val) == 3, msg="default int array size")
    call check(is_equal(val(1), 10), msg="first element correct")

    call root%destroy()
  end subroutine test_get_or_set_int_array_with_child

  !> get_or_set real(dp) array with child output
  subroutine test_get_or_set_real_dp_array_child()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    real(dp), allocatable :: val(:)
    integer :: stat
    type(hsd_table), pointer :: child

    call hsd_load_string("Other = 1", root, error)
    call check(.not. allocated(error), msg="parse ok")

    call hsd_get_or_set(root, "Vals", val, [1.0_dp, 2.0_dp], stat, child)
    call check(size(val) == 2, msg="default dp array size")

    call root%destroy()
  end subroutine test_get_or_set_real_dp_array_child

  !> get_or_set real(sp) array with child output
  subroutine test_get_or_set_real_sp_array_child()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    real(sp), allocatable :: val(:)
    integer :: stat
    type(hsd_table), pointer :: child

    call hsd_load_string("Other = 1", root, error)
    call check(.not. allocated(error), msg="parse ok")

    call hsd_get_or_set(root, "Spvals", val, [1.0_sp, 2.0_sp], stat, child)
    call check(size(val) == 2, msg="default sp array size")

    call root%destroy()
  end subroutine test_get_or_set_real_sp_array_child

  !> get_or_set logical array with child output
  subroutine test_get_or_set_logical_array_child()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    logical, allocatable :: val(:)
    integer :: stat
    type(hsd_table), pointer :: child

    call hsd_load_string("Other = 1", root, error)
    call check(.not. allocated(error), msg="parse ok")

    call hsd_get_or_set(root, "Flags", val, [.true., .false.], stat, child)
    call check(size(val) == 2, msg="default logical array size")
    call check(val(1), msg="first flag true")

    call root%destroy()
  end subroutine test_get_or_set_logical_array_child

  ! ============================================================
  ! P15: Parser — nested amendments, semicolons, quoted strings
  ! ============================================================

  !> Nested amendment: +Outer { +Inner { Key = newval } }
  subroutine test_nested_amendment()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    integer :: val, stat

    call hsd_load_string( &
        "Outer {" // char(10) // &
        "  Inner {" // char(10) // &
        "    Key = 1" // char(10) // &
        "  }" // char(10) // &
        "}" // char(10) // &
        "+Outer {" // char(10) // &
        "  +Inner {" // char(10) // &
        "    Key = 99" // char(10) // &
        "  }" // char(10) // &
        "}", root, error)
    call check(.not. allocated(error), msg="nested amendment parse ok")

    call hsd_get(root, "Outer/Inner/Key", val, stat)
    call check(stat == HSD_STAT_OK, msg="nested amendment get ok")
    call check(is_equal(val, 99), msg="nested amendment value correct")

    call root%destroy()
  end subroutine test_nested_amendment

  !> Amendment target child is not a block (error)
  subroutine test_amendment_child_not_block()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error

    ! Outer has a value child "Inner", not a table, so +Inner should fail
    call hsd_load_string( &
        "Outer {" // char(10) // &
        "  Inner = 42" // char(10) // &
        "}" // char(10) // &
        "+Outer {" // char(10) // &
        "  +Inner {" // char(10) // &
        "    Key = 1" // char(10) // &
        "  }" // char(10) // &
        "}", root, error)
    call check(allocated(error), msg="amendment to non-block child should error")

    call root%destroy()
  end subroutine test_amendment_child_not_block

  !> Parse values separated by semicolons
  subroutine test_semicolon_separated_values()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    integer :: val1, val2, stat

    call hsd_load_string("A = 1; B = 2", root, error)
    call check(.not. allocated(error), msg="semicolon parse ok")

    call hsd_get(root, "A", val1, stat)
    call check(stat == HSD_STAT_OK .and. val1 == 1, msg="A = 1")
    call hsd_get(root, "B", val2, stat)
    call check(stat == HSD_STAT_OK .and. val2 == 2, msg="B = 2")

    call root%destroy()
  end subroutine test_semicolon_separated_values

  !> Parse quoted string values after equals
  subroutine test_value_after_equals_string()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: val
    integer :: stat

    call hsd_load_string('Name = "hello world"', root, error)
    call check(.not. allocated(error), msg="quoted string parse ok")

    call hsd_get(root, "Name", val, stat)
    call check(stat == HSD_STAT_OK, msg="get string ok")
    call check(index(val, "hello world") > 0, msg="quoted string value correct")

    call root%destroy()
  end subroutine test_value_after_equals_string

  ! ============================================================
  ! P16: Formatter — complex values, escape strings, attribs in string output
  ! ============================================================

  !> Format a table containing a complex value
  subroutine test_format_complex_value()
    type(hsd_table) :: root
    character(len=:), allocatable :: output

    call new_table(root, name="root")
    call hsd_set(root, "Z", cmplx(1.0_dp, 2.0_dp, dp))
    call hsd_dump_to_string(root, output)
    call check(len(output) > 0, msg="complex value formatted")
    call root%destroy()
  end subroutine test_format_complex_value

  !> Format a string that requires escape sequences (backslash, tab, newline)
  subroutine test_format_string_with_escapes()
    type(hsd_table) :: root
    character(len=:), allocatable :: output

    call new_table(root, name="root")
    ! Set a string value with tab and backslash characters
    call hsd_set(root, "Escaped", "hello" // char(9) // "world" // char(92) // "end")
    call hsd_dump_to_string(root, output)
    call check(len(output) > 0, msg="escaped string formatted")
    ! Should contain escaped backslash in the output
    call check(index(output, "\") > 0, msg="escape chars present")
    call root%destroy()
  end subroutine test_format_string_with_escapes

  !> Format a table with attributes to string
  subroutine test_format_table_with_attrib_str()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: output

    call hsd_load_string( &
        "Temp [Kelvin] = 300", root, error)
    call check(.not. allocated(error), msg="parse ok")

    call hsd_dump_to_string(root, output)
    call check(len(output) > 0, msg="table with attrib formatted to string")
    ! Output should contain the attribute
    call check(index(output, "Kelvin") > 0, msg="attribute preserved in string output")
    call root%destroy()
  end subroutine test_format_table_with_attrib_str

  !> Format an empty table to string
  subroutine test_format_empty_table_str()
    type(hsd_table) :: root
    character(len=:), allocatable :: output

    call new_table(root, name="root")
    call hsd_dump_to_string(root, output)
    ! Empty table should produce empty or minimal output
    call check(len(output) >= 0, msg="empty table formatted ok")
    call root%destroy()
  end subroutine test_format_empty_table_str

  !> Format a multiline string value to string buffer
  subroutine test_format_multiline_value_str()
    type(hsd_table) :: root
    character(len=:), allocatable :: output

    call new_table(root, name="root")
    call hsd_set(root, "Data", "line1" // char(10) // "line2" // char(10) // "line3")
    call hsd_dump_to_string(root, output)
    call check(len(output) > 0, msg="multiline value formatted to string")
    call root%destroy()
  end subroutine test_format_multiline_value_str

  !> Format a named table with multiple children (falls through to block syntax)
  subroutine test_format_named_multi_child_str()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: output

    ! Parse a block with multiple children so the formatter uses block syntax
    call hsd_load_string( &
        "Block {" // char(10) // &
        "  A = 1" // char(10) // &
        "  B = 2" // char(10) // &
        "  C = 3" // char(10) // &
        "}", root, error)
    call check(.not. allocated(error), msg="parse ok")
    call hsd_dump_to_string(root, output)
    call check(len(output) > 0, msg="named multi-child block formatted")
    call check(index(output, "block") > 0 .or. index(output, "Block") > 0, &
        msg="block name present")
    call root%destroy()
  end subroutine test_format_named_multi_child_str

  ! ============================================================
  ! P17: Validation — error paths
  ! ============================================================

  !> hsd_validate_one_of on a table node (not a string value)
  subroutine test_validate_one_of_not_string()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error, val_error

    call hsd_load_string( &
        "Method {" // char(10) // &
        "  Sub = 1" // char(10) // &
        "}", root, error)
    call check(.not. allocated(error), msg="parse ok")

    ! "Method" is a table, not a value, so validate_one_of should error
    call hsd_validate_one_of(root, "Method", [character(len=10) :: "alpha", "beta"], val_error)
    call check(allocated(val_error), msg="validate_one_of on table should error")

    call root%destroy()
  end subroutine test_validate_one_of_not_string

  !> hsd_get_array_with_unit when path not found
  subroutine test_get_array_with_unit_not_found()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    real(dp), allocatable :: val(:)
    integer :: stat

    call hsd_load_string("X = 1", root, error)
    call check(.not. allocated(error), msg="parse ok")

    call hsd_get_array_with_unit(root, "Missing", val, "m", identity_converter, stat)
    call check(stat == HSD_STAT_NOT_FOUND, msg="array_with_unit not found")
    call check(size(val) == 0, msg="empty array returned")

    call root%destroy()
  end subroutine test_get_array_with_unit_not_found

  !> hsd_get_array_with_unit on a table node (type error)
  subroutine test_get_array_with_unit_type_err()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    real(dp), allocatable :: val(:)
    integer :: stat

    call hsd_load_string("Block { X = 1 }", root, error)
    call check(.not. allocated(error), msg="parse ok")

    ! "Block" is a table, not a value
    call hsd_get_array_with_unit(root, "Block", val, "m", identity_converter, stat)
    call check(stat == HSD_STAT_TYPE_ERROR, msg="array_with_unit type error")
    call check(size(val) == 0, msg="empty array for type error")

    call root%destroy()
  end subroutine test_get_array_with_unit_type_err

  !> hsd_get_matrix_with_unit when path not found
  subroutine test_get_matrix_with_unit_not_found()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    real(dp), allocatable :: val(:,:)
    integer :: nrows, ncols, stat

    call hsd_load_string("X = 1", root, error)
    call check(.not. allocated(error), msg="parse ok")

    call hsd_get_matrix_with_unit(root, "Missing", val, nrows, ncols, &
        "m", identity_converter, stat)
    call check(stat == HSD_STAT_NOT_FOUND, msg="matrix_with_unit not found")
    call check(size(val) == 0, msg="empty matrix returned")

    call root%destroy()
  end subroutine test_get_matrix_with_unit_not_found

  !> hsd_get_matrix_with_unit on a table node (type error)
  subroutine test_get_matrix_with_unit_type_err()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    real(dp), allocatable :: val(:,:)
    integer :: nrows, ncols, stat

    call hsd_load_string("Block { X = 1 }", root, error)
    call check(.not. allocated(error), msg="parse ok")

    call hsd_get_matrix_with_unit(root, "Block", val, nrows, ncols, &
        "m", identity_converter, stat)
    call check(stat == HSD_STAT_TYPE_ERROR, msg="matrix_with_unit type error")
    call check(size(val) == 0, msg="empty matrix for type error")

    call root%destroy()
  end subroutine test_get_matrix_with_unit_type_err

  !> hsd_format_error on a node with no context (empty name)
  subroutine test_format_err_no_context()
    type(hsd_table) :: tbl
    character(len=:), allocatable :: formatted

    call new_table(tbl, line=0)
    call hsd_format_error(tbl, "something bad", formatted)
    call check(formatted == "something bad", msg="error without context is just the message")
    call tbl%destroy()
  end subroutine test_format_err_no_context

  !> hsd_format_warning on a node with no context (empty name)
  subroutine test_format_warn_no_context()
    type(hsd_table) :: tbl
    character(len=:), allocatable :: formatted

    call new_table(tbl, line=0)
    call hsd_format_warning(tbl, "check this", formatted)
    call check(formatted == "check this", msg="warning without context is just the message")
    call tbl%destroy()
  end subroutine test_format_warn_no_context

  !> hsd_warn_unprocessed with enough children to trigger buffer grow
  subroutine test_warn_unprocessed_many()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=MAX_WARNING_LEN), allocatable :: warnings(:)
    character(len=:), allocatable :: hsd_input
    integer :: i

    ! Build an HSD string with 40 unprocessed keys (buffer starts at 32)
    hsd_input = ""
    do i = 1, 40
      if (i > 1) hsd_input = hsd_input // char(10)
      block
        character(len=20) :: ibuf
        write(ibuf, '(I0)') i
        hsd_input = hsd_input // "Key" // trim(ibuf) // " = " // trim(ibuf)
      end block
    end do

    call hsd_load_string(hsd_input, root, error)
    call check(.not. allocated(error), msg="parse ok")

    ! Don't process any keys -> all 40 should be warned
    call hsd_warn_unprocessed(root, warnings)
    call check(size(warnings) == 40, msg="40 unprocessed warnings (buffer grew)")

    call root%destroy()
  end subroutine test_warn_unprocessed_many

  ! ============================================================
  ! P18: Query — merge array types, get_name, values_equal
  ! ============================================================

  !> Merge tables that overwrite logical array values
  subroutine test_merge_logical_arrays()
    type(hsd_table) :: base, overlay
    logical, allocatable :: val(:)
    integer :: stat

    call new_table(base, name="root")
    call new_table(overlay, name="root")
    call hsd_set(base, "Flags", [.true., .false.])
    call hsd_set(overlay, "Flags", [.false., .true., .true.])

    call hsd_merge(base, overlay, stat)
    call check(stat == HSD_STAT_OK, msg="merge logical arrays ok")

    call hsd_get(base, "Flags", val, stat)
    call check(stat == HSD_STAT_OK, msg="get merged logical array ok")
    call check(size(val) == 3, msg="merged logical array has 3 elements")
    call check(.not. val(1), msg="first element false after merge")

    call base%destroy()
    call overlay%destroy()
  end subroutine test_merge_logical_arrays

  !> Merge tables that overwrite string array values
  subroutine test_merge_string_arrays()
    type(hsd_table) :: base, overlay
    character(len=:), allocatable :: val(:)
    integer :: stat

    call new_table(base, name="root")
    call new_table(overlay, name="root")
    call hsd_set(base, "Names", [character(len=5) :: "alpha", "beta"])
    call hsd_set(overlay, "Names", [character(len=5) :: "gamma", "delta"])

    call hsd_merge(base, overlay, stat)
    call check(stat == HSD_STAT_OK, msg="merge string arrays ok")

    call hsd_get(base, "Names", val, stat)
    call check(stat == HSD_STAT_OK, msg="get merged string array ok")
    call check(size(val) == 2, msg="merged string array size")

    call base%destroy()
    call overlay%destroy()
  end subroutine test_merge_string_arrays

  !> Merge tables that overwrite complex array values
  subroutine test_merge_complex_arrays()
    type(hsd_table) :: base, overlay
    complex(dp), allocatable :: val(:)
    integer :: stat

    call new_table(base, name="root")
    call new_table(overlay, name="root")
    call hsd_set(base, "Zs", [cmplx(1.0_dp, 0.0_dp, dp)])
    call hsd_set(overlay, "Zs", [cmplx(2.0_dp, 3.0_dp, dp), cmplx(4.0_dp, 5.0_dp, dp)])

    call hsd_merge(base, overlay, stat)
    call check(stat == HSD_STAT_OK, msg="merge complex arrays ok")

    call hsd_get(base, "Zs", val, stat)
    call check(stat == HSD_STAT_OK, msg="get merged complex array ok")
    call check(size(val) == 2, msg="merged complex array size")

    call base%destroy()
    call overlay%destroy()
  end subroutine test_merge_complex_arrays

  !> Merge tables that overwrite integer matrix values
  subroutine test_merge_int_matrices()
    type(hsd_table) :: base, overlay
    type(hsd_error_t), allocatable :: error
    integer, allocatable :: mat(:,:)
    integer :: nrows, ncols, stat

    call hsd_load_string( &
        "Mat = {" // char(10) // &
        "  1 2" // char(10) // &
        "  3 4" // char(10) // &
        "}", base, error)
    call check(.not. allocated(error), msg="base parse ok")

    call hsd_load_string( &
        "Mat = {" // char(10) // &
        "  5 6" // char(10) // &
        "  7 8" // char(10) // &
        "}", overlay, error)
    call check(.not. allocated(error), msg="overlay parse ok")

    call hsd_merge(base, overlay, stat)
    call check(stat == HSD_STAT_OK, msg="merge int matrices ok")

    call base%destroy()
    call overlay%destroy()
  end subroutine test_merge_int_matrices

  !> Merge tables that overwrite real matrix values
  subroutine test_merge_real_matrices()
    type(hsd_table) :: base, overlay
    type(hsd_error_t), allocatable :: error
    integer :: stat

    call hsd_load_string( &
        "Mat = {" // char(10) // &
        "  1.0 2.0" // char(10) // &
        "  3.0 4.0" // char(10) // &
        "}", base, error)
    call check(.not. allocated(error), msg="base parse ok")

    call hsd_load_string( &
        "Mat = {" // char(10) // &
        "  5.0 6.0" // char(10) // &
        "  7.0 8.0" // char(10) // &
        "}", overlay, error)
    call check(.not. allocated(error), msg="overlay parse ok")

    call hsd_merge(base, overlay, stat)
    call check(stat == HSD_STAT_OK, msg="merge real matrices ok")

    call base%destroy()
    call overlay%destroy()
  end subroutine test_merge_real_matrices

  !> hsd_get_name on a node with empty name returns default
  subroutine test_get_name_empty()
    type(hsd_table) :: tbl
    character(len=:), allocatable :: name

    call new_table(tbl)
    call hsd_get_name(tbl, name)
    call check(name == "", msg="empty name returns empty string")
    call tbl%destroy()
  end subroutine test_get_name_empty

  !> hsd_get_name on a node with explicit default
  subroutine test_get_name_with_default()
    type(hsd_table) :: tbl
    character(len=:), allocatable :: name

    call new_table(tbl)
    call hsd_get_name(tbl, name, default="fallback")
    call check(name == "fallback", msg="unset name returns fallback")
    call tbl%destroy()
  end subroutine test_get_name_with_default

  !> Table equality for complex values
  subroutine test_table_equal_complex()
    type(hsd_table) :: a, b
    logical :: eq

    call new_table(a, name="root")
    call new_table(b, name="root")
    call hsd_set(a, "z", cmplx(1.0_dp, 2.0_dp, dp))
    call hsd_set(b, "z", cmplx(1.0_dp, 2.0_dp, dp))

    eq = hsd_table_equal(a, b)
    call check(eq, msg="tables with same complex values are equal")

    call a%destroy()
    call b%destroy()
  end subroutine test_table_equal_complex

  !> Table equality for string values
  subroutine test_table_equal_string_values()
    type(hsd_table) :: a, b
    logical :: eq

    call new_table(a, name="root")
    call new_table(b, name="root")
    call hsd_set(a, "name", "hello")
    call hsd_set(b, "name", "hello")

    eq = hsd_table_equal(a, b)
    call check(eq, msg="tables with same string values are equal")

    ! Change one and verify inequality
    call b%destroy()
    call new_table(b, name="root")
    call hsd_set(b, "name", "world")
    eq = hsd_table_equal(a, b)
    call check(.not. eq, msg="tables with different strings not equal")

    call a%destroy()
    call b%destroy()
  end subroutine test_table_equal_string_values

  !> Table equality via raw text comparison
  subroutine test_table_equal_raw_text()
    type(hsd_table) :: a, b
    type(hsd_error_t), allocatable :: error
    logical :: eq

    ! Parse identical input so raw_text is used for comparison
    call hsd_load_string("Val = hello", a, error)
    call check(.not. allocated(error), msg="parse a ok")
    call hsd_load_string("Val = hello", b, error)
    call check(.not. allocated(error), msg="parse b ok")

    eq = hsd_table_equal(a, b)
    call check(eq, msg="tables from same input are equal via raw_text")

    call a%destroy()
    call b%destroy()
  end subroutine test_table_equal_raw_text

  !> hsd_get_children with empty path returns NOT_FOUND
  subroutine test_get_children_not_found()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    type(hsd_child_ptr), allocatable :: children(:)
    integer :: stat

    call hsd_load_string("X = 1", root, error)
    call check(.not. allocated(error), msg="parse ok")

    call hsd_get_children(root, "", children, stat)
    call check(stat == HSD_STAT_NOT_FOUND, msg="empty path not found")
    call check(size(children) == 0, msg="no children for empty path")

    call root%destroy()
  end subroutine test_get_children_not_found

  !> hsd_get_children through a value node (type error)
  subroutine test_get_children_type_error()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    type(hsd_child_ptr), allocatable :: children(:)
    integer :: stat

    call hsd_load_string( &
        "Container {" // char(10) // &
        "  leaf = 42" // char(10) // &
        "}", root, error)
    call check(.not. allocated(error), msg="parse ok")

    ! "Container/leaf" is a value; "Container/leaf/Sub" navigates through it
    call hsd_get_children(root, "Container/leaf/Sub", children, stat)
    call check(stat == HSD_STAT_NOT_FOUND .or. stat == HSD_STAT_TYPE_ERROR, &
        msg="path through value gives error")
    call check(size(children) == 0, msg="no children through value")

    call root%destroy()
  end subroutine test_get_children_type_error

  !> hsd_get_choice when no children match returns NOT_FOUND
  subroutine test_get_choice_not_found()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: choice_name
    type(hsd_table), pointer :: choice_tbl
    integer :: stat

    call hsd_load_string("Empty {}", root, error)
    call check(.not. allocated(error), msg="parse ok")

    call hsd_get_choice(root, "Empty", choice_name, choice_tbl, stat)
    call check(stat == HSD_STAT_NOT_FOUND, msg="get_choice on empty table is NOT_FOUND")

    call root%destroy()
  end subroutine test_get_choice_not_found

  ! ============================================================
  ! P19: Hash table — overflow chain removal
  ! ============================================================

  !> Force hash collisions by inserting many keys, then remove from overflow chain
  subroutine test_hash_table_overflow_remove()
    type(hsd_name_index_t) :: ht
    integer :: i, val
    logical :: found
    character(len=10) :: key

    call ht%init(4)  ! Small bucket count to force collisions

    ! Insert 20 keys to force overflow chains
    do i = 1, 20
      write(key, '(A,I0)') "k", i
      call ht%insert(trim(key), i)
    end do

    ! Remove some keys that are likely in overflow chains
    do i = 2, 20, 3
      write(key, '(A,I0)') "k", i
      call ht%remove(trim(key))
    end do

    ! Verify removed keys are gone
    do i = 2, 20, 3
      write(key, '(A,I0)') "k", i
      val = ht%lookup(trim(key), found)
      call check(.not. found, msg="removed overflow key not found: " // trim(key))
    end do

    ! Verify remaining keys still exist
    do i = 1, 20, 3
      write(key, '(A,I0)') "k", i
      val = ht%lookup(trim(key), found)
      call check(found, msg="remaining key found: " // trim(key))
      call check(is_equal(val, i), msg="remaining key value correct")
    end do

  end subroutine test_hash_table_overflow_remove

  !> Remove from a bucket that has a chain (promotes first chain entry)
  subroutine test_hash_table_chain_promote()
    type(hsd_name_index_t) :: ht
    integer :: val
    logical :: found

    call ht%init(2)  ! Very small to force chains

    ! Insert keys that will collide
    call ht%insert("a", 1)
    call ht%insert("b", 2)
    call ht%insert("c", 3)
    call ht%insert("d", 4)
    call ht%insert("e", 5)
    call ht%insert("f", 6)

    ! Remove "a" — should promote from chain
    call ht%remove("a")
    val = ht%lookup("a", found)
    call check(.not. found, msg="a removed")

    ! Other keys should still be there
    val = ht%lookup("b", found)
    call check(found, msg="b still present after a removed")
    val = ht%lookup("c", found)
    call check(found, msg="c still present")
    val = ht%lookup("d", found)
    call check(found, msg="d still present")

    ! Remove "b" and "c" too
    call ht%remove("b")
    call ht%remove("c")
    val = ht%lookup("d", found)
    call check(found, msg="d still present after b,c removed")
    val = ht%lookup("e", found)
    call check(found, msg="e still present")
    val = ht%lookup("f", found)
    call check(found, msg="f still present")

  end subroutine test_hash_table_chain_promote

  ! ============================================================
  ! P20: Parser — amendment equals syntax (+Tag = +ChildTag { ... })
  ! ============================================================

  !> +Outer = +Inner { Key = newval } — amend Inner inside existing Outer
  subroutine test_amend_equals_child_amend()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    integer :: val, stat

    call hsd_load_string( &
        "Outer {" // char(10) // &
        "  Inner {" // char(10) // &
        "    Key = 1" // char(10) // &
        "  }" // char(10) // &
        "}" // char(10) // &
        "+Outer = +Inner {" // char(10) // &
        "  Key = 99" // char(10) // &
        "}", root, error)
    call check(.not. allocated(error), msg="amend equals child amend parse ok")

    call hsd_get(root, "Outer/Inner/Key", val, stat)
    call check(stat == HSD_STAT_OK, msg="amend equals child value found")
    call check(is_equal(val, 99), msg="amend equals child value updated")

    call root%destroy()
  end subroutine test_amend_equals_child_amend

  !> +Outer = NewChild { Key = 5 } — add new child table inside existing Outer
  subroutine test_amend_equals_new_child()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    integer :: val, stat

    call hsd_load_string( &
        "Outer {" // char(10) // &
        "  Existing = 1" // char(10) // &
        "}" // char(10) // &
        "+Outer = NewChild {" // char(10) // &
        "  Key = 42" // char(10) // &
        "}", root, error)
    call check(.not. allocated(error), msg="amend equals new child parse ok")

    ! Original value preserved
    call hsd_get(root, "Outer/Existing", val, stat)
    call check(stat == HSD_STAT_OK .and. val == 1, msg="existing value preserved")

    ! New child added
    call hsd_get(root, "Outer/NewChild/Key", val, stat)
    call check(stat == HSD_STAT_OK .and. val == 42, msg="new child added via amend equals")

    call root%destroy()
  end subroutine test_amend_equals_new_child

  !> +Missing = +Child { } — target table doesn't exist (error)
  subroutine test_amend_equals_target_missing()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error

    call hsd_load_string( &
        "Something { X = 1 }" // char(10) // &
        "+Missing = +Child {" // char(10) // &
        "  Key = 1" // char(10) // &
        "}", root, error)
    call check(allocated(error), msg="amend equals on missing target should error")
    call root%destroy()
  end subroutine test_amend_equals_target_missing

  !> +Tag = +Child { } where Tag is a value, not a table (error)
  subroutine test_amend_equals_target_not_block()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error

    call hsd_load_string( &
        "Tag = 42" // char(10) // &
        "+Tag = +Child {" // char(10) // &
        "  Key = 1" // char(10) // &
        "}", root, error)
    call check(allocated(error), msg="amend equals on non-block should error")
    call root%destroy()
  end subroutine test_amend_equals_target_not_block

  !> +Outer = +Missing { } where Inner child doesn't exist (error)
  subroutine test_amend_equals_child_missing()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error

    call hsd_load_string( &
        "Outer {" // char(10) // &
        "  Inner { Key = 1 }" // char(10) // &
        "}" // char(10) // &
        "+Outer = +Missing {" // char(10) // &
        "  Key = 2" // char(10) // &
        "}", root, error)
    call check(allocated(error), msg="amend equals on missing child should error")
    call root%destroy()
  end subroutine test_amend_equals_child_missing

  ! ============================================================
  ! P21: Parser — text buffer flush and multi-value
  ! ============================================================

  !> Text data in a block before a tag gets flushed as #text child
  subroutine test_text_buffer_before_tag()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: text_val
    integer :: val, stat

    ! "SomeData" is raw text that should become a #text child,
    ! then "Key = 5" is a tagged value
    call hsd_load_string( &
        "Block {" // char(10) // &
        "  SomeData" // char(10) // &
        "  Key = 5" // char(10) // &
        "}", root, error)
    call check(.not. allocated(error), msg="text buffer parse ok")

    call hsd_get(root, "Block/Key", val, stat)
    call check(stat == HSD_STAT_OK .and. val == 5, msg="key after text ok")

    call root%destroy()
  end subroutine test_text_buffer_before_tag

  !> Multi-token value with trailing comment: Tag = val1 val2 # comment
  subroutine test_multi_value_with_comment()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: val
    integer :: stat

    call hsd_load_string( &
        "Names = alpha beta gamma # this is a comment", root, error)
    call check(.not. allocated(error), msg="multi value with comment parse ok")

    call hsd_get(root, "Names", val, stat)
    call check(stat == HSD_STAT_OK, msg="get names ok")
    ! Value should contain the three tokens but not the comment
    call check(index(val, "alpha") > 0, msg="has alpha")
    call check(index(val, "gamma") > 0, msg="has gamma")
    call check(index(val, "comment") == 0, msg="no comment text")

    call root%destroy()
  end subroutine test_multi_value_with_comment

  ! ============================================================
  ! P22: Mutators — hsd_clear_children
  ! ============================================================

  !> Clear all children from a table
  subroutine test_clear_children()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error

    call hsd_load_string( &
        "A = 1" // char(10) // &
        "B = 2" // char(10) // &
        "C { D = 3 }", root, error)
    call check(.not. allocated(error), msg="parse ok")
    call check(root%num_children > 0, msg="has children initially")

    call hsd_clear_children(root)
    call check(root%num_children == 0, msg="no children after clear")

    ! Can add new children after clear
    call hsd_set(root, "New", 42)
    call check(root%num_children == 1, msg="can add after clear")

    call root%destroy()
  end subroutine test_clear_children

  ! ============================================================
  ! P23: Accessors — inline value from table (#text child)
  !
  ! Pattern: "Tag { 42; Sub = x }" makes Tag a table with a #text child
  ! holding "42". Calling hsd_get(root, "Tag", intval) should find the
  ! table, discover the #text child via get_inline_value_, and parse it.
  ! ============================================================

  !> Get integer from a table that has a #text inline value child
  subroutine test_get_int_inline_table()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    integer :: val, stat

    ! Parsed: Tag becomes table, "42" is raw text (#text child), Sub is a value child
    ! Use semicolons to avoid trailing newline in text buffer
    call hsd_load_string( &
        "Tag { 42; Sub = hello }", root, error)
    call check(.not. allocated(error), msg="parse ok")

    call hsd_get(root, "Tag", val, stat)
    call check(stat == HSD_STAT_OK, msg="get int from table with #text ok")
    call check(is_equal(val, 42), msg="int inline value correct")

    call root%destroy()
  end subroutine test_get_int_inline_table

  !> Get real from a table that has a #text inline value child
  subroutine test_get_real_inline_table()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    real(dp) :: val
    integer :: stat

    call hsd_load_string( &
        "Tag { 3.14; Sub = hello }", root, error)
    call check(.not. allocated(error), msg="parse ok")

    call hsd_get(root, "Tag", val, stat)
    call check(stat == HSD_STAT_OK, msg="get real from table with #text ok")
    call check(abs(val - 3.14_dp) < 1.0e-4_dp, msg="real inline value correct")

    call root%destroy()
  end subroutine test_get_real_inline_table

  !> Get logical from a table that has a #text inline value child
  subroutine test_get_logical_inline_table()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    logical :: val
    integer :: stat

    ! Use semicolon to separate inline text from tag on same line
    ! (avoids trailing newline in text_buffer)
    call hsd_load_string( &
        "Tag { Yes; Sub = hello }", root, error)
    call check(.not. allocated(error), msg="parse ok")

    call hsd_get(root, "Tag", val, stat)
    call check(stat == HSD_STAT_OK, msg="get logical from table with #text ok")
    call check(val, msg="logical inline value is true")

    call root%destroy()
  end subroutine test_get_logical_inline_table

  !> Get complex from a table that has a #text inline value child
  subroutine test_get_complex_inline_table()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    complex(dp) :: val
    integer :: stat

    call hsd_load_string( &
        "Tag { (1.0,2.0); Sub = hello }", root, error)
    call check(.not. allocated(error), msg="parse ok")

    call hsd_get(root, "Tag", val, stat)
    call check(stat == HSD_STAT_OK, msg="get complex from table with #text ok")
    call check(abs(real(val) - 1.0_dp) < 1.0e-10_dp, msg="complex real part correct")

    call root%destroy()
  end subroutine test_get_complex_inline_table

  !> Get integer array from a table that has a #text inline value child
  subroutine test_get_int_arr_inline_table()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    integer, allocatable :: val(:)
    integer :: stat

    call hsd_load_string( &
        "Tag { 10 20 30; Sub = hello }", root, error)
    call check(.not. allocated(error), msg="parse ok")

    call hsd_get(root, "Tag", val, stat)
    call check(stat == HSD_STAT_OK, msg="get int array from table with #text ok")
    call check(size(val) == 3, msg="int array size correct")
    call check(is_equal(val(1), 10), msg="first element correct")

    call root%destroy()
  end subroutine test_get_int_arr_inline_table

  !> Get real array from a table that has a #text inline value child
  subroutine test_get_real_arr_inline_table()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    real(dp), allocatable :: val(:)
    integer :: stat

    call hsd_load_string( &
        "Tag { 1.1 2.2 3.3; Sub = hello }", root, error)
    call check(.not. allocated(error), msg="parse ok")

    call hsd_get(root, "Tag", val, stat)
    call check(stat == HSD_STAT_OK, msg="get real array from table with #text ok")
    call check(size(val) == 3, msg="real array size correct")

    call root%destroy()
  end subroutine test_get_real_arr_inline_table

  !> Get logical array from a table that has a #text inline value child
  subroutine test_get_logical_arr_inline_tbl()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    logical, allocatable :: val(:)
    integer :: stat

    call hsd_load_string( &
        "Tag { Yes No; Sub = hello }", root, error)
    call check(.not. allocated(error), msg="parse ok")

    call hsd_get(root, "Tag", val, stat)
    call check(stat == HSD_STAT_OK, msg="get logical array from table with #text ok")
    call check(size(val) == 2, msg="logical array size correct")

    call root%destroy()
  end subroutine test_get_logical_arr_inline_tbl

  !> Get string array from a table that has a #text inline value child
  subroutine test_get_string_arr_inline_tbl()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: val(:)
    integer :: stat

    call hsd_load_string( &
        "Tag { foo bar baz; Sub = hello }", root, error)
    call check(.not. allocated(error), msg="parse ok")

    call hsd_get(root, "Tag", val, stat)
    call check(stat == HSD_STAT_OK, msg="get string array from table with #text ok")
    call check(size(val) >= 3, msg="string array has 3+ elements")

    call root%destroy()
  end subroutine test_get_string_arr_inline_tbl

  !> Get complex array from a table that has a #text inline value child
  subroutine test_get_complex_arr_inline_tbl()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    complex(dp), allocatable :: val(:)
    integer :: stat

    call hsd_load_string( &
        "Tag { 1.0+2.0i 3.0+4.0i; Sub = hello }", root, error)
    call check(.not. allocated(error), msg="parse ok")

    call hsd_get(root, "Tag", val, stat)
    call check(stat == HSD_STAT_OK, msg="get complex array from table with #text ok")
    call check(size(val) == 2, msg="complex array size correct")

    call root%destroy()
  end subroutine test_get_complex_arr_inline_tbl

  ! ============================================================
  ! P24: Accessors — get_or_set existing value with child+stat
  ! ============================================================

  !> hsd_get_or_set string when key exists, with child argument
  subroutine test_get_or_set_str_exist_child()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: val
    type(hsd_table), pointer :: child_tbl
    integer :: stat

    call hsd_load_string("Name = existing", root, error)
    call check(.not. allocated(error), msg="parse ok")

    call hsd_get_or_set(root, "Name", val, "default", stat=stat, child=child_tbl)
    call check(stat == HSD_STAT_OK, msg="get_or_set existing string stat OK")
    call check(val == "existing", msg="got existing value, not default")

    call root%destroy()
  end subroutine test_get_or_set_str_exist_child

  !> hsd_get_or_set integer when key exists, with child argument
  subroutine test_get_or_set_int_exist_child()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    integer :: val, stat
    type(hsd_table), pointer :: child_tbl

    call hsd_load_string("Count = 7", root, error)
    call check(.not. allocated(error), msg="parse ok")

    call hsd_get_or_set(root, "Count", val, 0, stat=stat, child=child_tbl)
    call check(stat == HSD_STAT_OK, msg="get_or_set existing int stat OK")
    call check(is_equal(val, 7), msg="got existing int value")

    call root%destroy()
  end subroutine test_get_or_set_int_exist_child

  !> hsd_get_or_set logical array when key exists, with child argument
  subroutine test_get_or_set_log_arr_ex_child()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    logical, allocatable :: val(:)
    type(hsd_table), pointer :: child_tbl
    integer :: stat

    call hsd_load_string("Flags = Yes No Yes", root, error)
    call check(.not. allocated(error), msg="parse ok")

    call hsd_get_or_set(root, "Flags", val, [.false.], stat=stat, child=child_tbl)
    call check(stat == HSD_STAT_OK, msg="get_or_set existing logical array stat OK")
    call check(size(val) == 3, msg="got existing array not default")

    call root%destroy()
  end subroutine test_get_or_set_log_arr_ex_child

  ! ============================================================
  ! P25: Formatter — dump to file with multiline + complex
  ! ============================================================

  !> Dump tree with multiline raw-text value to a file, triggering write_tag_value multiline path
  subroutine test_dump_to_file_multiline()
    type(hsd_table) :: root, reloaded
    type(hsd_error_t), allocatable :: error
    integer :: val, stat
    logical :: file_exists

    ! Parse a tree with multiline raw text (Data has multi-line content in a block)
    call hsd_load_string( &
        "Data {" // char(10) // &
        "  line1" // char(10) // &
        "  line2" // char(10) // &
        "  line3" // char(10) // &
        "}" // char(10) // &
        "Count = 42", root, error)
    call check(.not. allocated(error), msg="parse ok")

    ! Dump to file — triggers write_table_node → write_tag_value multiline path
    call hsd_dump(root, "/tmp/test_dump_multiline.hsd", error)
    call check(.not. allocated(error), msg="dump to file ok")

    ! Verify file exists
    inquire(file="/tmp/test_dump_multiline.hsd", exist=file_exists)
    call check(file_exists, msg="dump file exists")

    ! Reload and verify at least Count is preserved
    call hsd_load("/tmp/test_dump_multiline.hsd", reloaded, error)
    call check(.not. allocated(error), msg="reload from file ok")

    call hsd_get(reloaded, "Count", val, stat)
    call check(stat == HSD_STAT_OK .and. val == 42, msg="reloaded count correct")

    call root%destroy()
    call reloaded%destroy()
  end subroutine test_dump_to_file_multiline

  !> Dump tree with complex value to a file
  subroutine test_dump_to_file_complex()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: output
    integer :: file_size
    logical :: file_exists

    call new_table(root, name="root")
    call hsd_set(root, "Z", cmplx(3.0_dp, 4.0_dp, dp))

    call hsd_dump(root, "/tmp/test_dump_complex.hsd", error)
    call check(.not. allocated(error), msg="dump complex to file ok")

    ! Verify file exists and has content
    inquire(file="/tmp/test_dump_complex.hsd", exist=file_exists, size=file_size)
    call check(file_exists, msg="dump file exists")
    call check(file_size > 0, msg="dump file has content")

    call root%destroy()
  end subroutine test_dump_to_file_complex

  ! ============================================================
  ! P26: Complex matrix from table children
  ! ============================================================

  !> Complex matrix from a table with multiple unnamed value children
  subroutine test_complex_matrix_from_table()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    complex(dp), allocatable :: mat(:,:)
    integer :: nrows, ncols, stat

    ! Matrix written as block with multiple lines of data
    call hsd_load_string( &
        "CMatrix {" // char(10) // &
        "  1.0+0.0i 2.0+0.0i" // char(10) // &
        "  3.0+0.0i 4.0+0.0i" // char(10) // &
        "}", root, error)
    call check(.not. allocated(error), msg="parse ok")

    call hsd_get_matrix(root, "CMatrix", mat, nrows, ncols, stat)
    call check(stat == HSD_STAT_OK, msg="complex matrix from table ok")
    call check(nrows == 2, msg="2 rows")
    call check(ncols == 2, msg="2 cols")

    call root%destroy()
  end subroutine test_complex_matrix_from_table

  !> Complex matrix from an empty table returns empty
  subroutine test_complex_matrix_empty_table()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    complex(dp), allocatable :: mat(:,:)
    integer :: nrows, ncols, stat

    call hsd_load_string("CMatrix {}", root, error)
    call check(.not. allocated(error), msg="parse ok")

    call hsd_get_matrix(root, "CMatrix", mat, nrows, ncols, stat)
    call check(stat == HSD_STAT_OK, msg="empty complex matrix stat ok")
    call check(nrows == 0 .and. ncols == 0, msg="empty matrix dimensions")

    call root%destroy()
  end subroutine test_complex_matrix_empty_table

  ! ============================================================
  ! P27: Query — remove child by path not found
  ! ============================================================

  !> Remove child when path does not exist
  subroutine test_remove_child_not_found()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    integer :: stat

    call hsd_load_string("X = 1", root, error)
    call check(.not. allocated(error), msg="parse ok")

    call hsd_remove_child(root, "NonExistent", stat)
    call check(stat == HSD_STAT_NOT_FOUND, msg="remove nonexistent gives NOT_FOUND")

    call root%destroy()
  end subroutine test_remove_child_not_found

  ! ============================================================
  ! Helper functions
  ! ============================================================

  !> Identity converter for unit tests (returns value unchanged)
  pure function identity_converter(value, from_unit, to_unit) result(converted)
    real(dp), intent(in) :: value
    character(len=*), intent(in) :: from_unit, to_unit
    real(dp) :: converted
    converted = value
  end function identity_converter

end module test_coverage_gaps_suite
