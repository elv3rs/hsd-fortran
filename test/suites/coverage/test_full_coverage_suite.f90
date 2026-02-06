!> Comprehensive coverage tests targeting all remaining uncovered code paths
module test_full_coverage_suite
  use hsd, only: hsd_node, hsd_table, hsd_value, hsd_error_t, hsd_iterator, &
      & new_table, new_value, hsd_load, hsd_load_string, hsd_dump, hsd_dump_to_string, &
      & hsd_get, hsd_get_or, hsd_get_matrix, hsd_get_table, hsd_get_attrib, &
      & hsd_set, hsd_has_child, hsd_remove_child, hsd_merge, hsd_clone, &
      & hsd_require, hsd_validate_range, hsd_validate_one_of, &
      & hsd_accept, hsd_visitor_t, &
      & VALUE_TYPE_NONE, VALUE_TYPE_STRING, VALUE_TYPE_INTEGER, VALUE_TYPE_REAL, &
      & VALUE_TYPE_LOGICAL, VALUE_TYPE_ARRAY, VALUE_TYPE_COMPLEX, &
      & HSD_STAT_OK, HSD_STAT_NOT_FOUND, HSD_STAT_TYPE_ERROR, &
      & HSD_STAT_SYNTAX_ERROR, HSD_STAT_SCHEMA_ERROR, dp, sp
  use hsd_error, only: make_syntax_error, make_type_error, error_message
  use hsd_schema, only: hsd_schema_t, hsd_field_def_t, schema_init, schema_destroy, &
      & schema_add_field, schema_add_field_enum, schema_validate, &
      & FIELD_REQUIRED, FIELD_OPTIONAL, &
      & FIELD_TYPE_ANY, FIELD_TYPE_STRING, FIELD_TYPE_INTEGER, FIELD_TYPE_REAL, &
      & FIELD_TYPE_LOGICAL, FIELD_TYPE_TABLE, FIELD_TYPE_ARRAY, FIELD_TYPE_COMPLEX
  use count_visitor_mod, only: count_visitor
  use fortuno_serial, only: test => serial_case_item, &
      & check => serial_check, suite => serial_suite_item, test_list
  use build_env, only: build_dir
  implicit none (type, external)
  private

  public :: tests

contains

  function tests()
    type(test_list) :: tests

    tests = test_list([&
        suite("full_coverage", test_list([&
            ! === Error module ===
            test("error_messages", test_error_messages), &
            test("make_syntax_error_no_file", test_syntax_error_no_file), &
            test("make_type_error_all_args", test_type_error_all_args), &
            test("make_type_error_no_file", test_type_error_no_file), &
            ! === Schema - value branches ===
            test("schema_int_range_error", test_schema_int_range_err), &
            test("schema_real_convert_error", test_schema_real_convert_err), &
            test("schema_real_range_error", test_schema_real_range_err), &
            test("schema_logical_error", test_schema_logical_err), &
            test("schema_array_not_array", test_schema_array_not_array), &
            test("schema_complex_not_complex", test_schema_complex_not_complex), &
            test("schema_enum_reject", test_schema_enum_reject), &
            test("schema_capacity_growth", test_schema_capacity_growth), &
            ! === Schema - get_type_name branches ===
            test("schema_table_as_real", test_schema_table_as_real), &
            test("schema_table_as_logical", test_schema_table_as_logical), &
            test("schema_table_as_array", test_schema_table_as_array), &
            test("schema_table_as_complex", test_schema_table_as_complex), &
            test("schema_value_as_table", test_schema_value_as_table), &
            ! === Validation ===
            test("type_name_branches", test_type_name_branches), &
            test("validate_one_of_match", test_validate_one_of_match), &
            test("validate_one_of_table", test_validate_one_of_table), &
            test("validate_range_oob", test_validate_range_oob), &
            test("validate_range_non_value", test_validate_range_non_val), &
            test("require_type_mismatch", test_require_type_mismatch), &
            ! === Formatter - file dump ===
            test("dump_unnamed_table_value", test_dump_unnamed_tbl_val), &
            test("dump_unnamed_table_multi", test_dump_unnamed_tbl_multi), &
            test("dump_anon_value_single", test_dump_anon_val_single), &
            test("dump_anon_value_multiline", test_dump_anon_val_multi), &
            test("dump_value_with_attrib", test_dump_val_with_attrib), &
            test("dump_empty_string_value", test_dump_empty_string_val), &
            test("dump_default_type_value", test_dump_default_type_val), &
            ! === Query ===
            test("remove_nonexistent_parent", test_remove_nonexist_parent), &
            test("merge_new_table_child", test_merge_new_table_child), &
            test("merge_new_value_child", test_merge_new_value_child), &
            test("merge_replace_value", test_merge_replace_value), &
            test("merge_value_with_raw_text", test_merge_raw_text), &
            test("merge_value_with_arrays", test_merge_arrays), &
            test("clone_table_with_values", test_clone_table_values), &
            test("get_table_on_value", test_get_table_on_value), &
            ! === Parser - syntax ===
            test("parser_blank_lines", test_parser_blank_lines), &
            test("parser_ws_after_eq", test_parser_ws_after_eq), &
            test("parser_text_before_block", test_parser_text_before_block), &
            test("parser_tag_eq_block", test_parser_tag_eq_block), &
            test("parser_tag_eq_child_block", test_parser_tag_eq_child_block), &
            test("parser_semicolons", test_parser_semicolons), &
            ! === Parser - includes ===
            test("text_include", test_text_include), &
            test("text_include_append", test_text_include_append), &
            test("hsd_include", test_hsd_include), &
            test("include_cycle", test_include_cycle), &
            test("include_depth_limit", test_include_depth_limit), &
            test("include_push_error", test_include_push_error), &
            test("text_include_io_error", test_text_include_io_error), &
            test("parse_nonexistent_file", test_parse_nonexistent), &
            test("parse_error_in_block", test_parse_error_in_block), &
            test("parse_error_in_nested", test_parse_error_in_nested), &
            ! === Lexer ===
            test("lexer_single_quote_escape", test_lexer_squote_escape), &
            test("lexer_attrib_context", test_lexer_attrib_ctx), &
            ! === Types ===
            test("tokenize_string_fn", test_tokenize_string), &
            test("tokenize_quoted_string_fn", test_tokenize_quoted_string), &
            test("split_by_newlines_fn", test_split_by_newlines), &
            test("parse_complex_bad_imag", test_complex_bad_imag), &
            ! === Visitor ===
            test("visitor_unnamed_child", test_visitor_unnamed_child), &
            ! === Hash table ===
            test("hash_remove_empty", test_hash_remove_empty), &
            test("hash_overflow_chain", test_hash_overflow_chain), &
            ! === Matrix ===
            test("matrix_int_from_table", test_matrix_int_from_table), &
            test("matrix_real_from_table", test_matrix_real_from_table), &
            ! === Additional accessor coverage ===
            test("get_real_sp_with_default", test_get_real_sp_default), &
            test("path_into_value", test_path_into_value), &
            test("matrix_int_multi_values", test_matrix_int_multi_values), &
            test("matrix_real_multi_values", test_matrix_real_multi_values), &
            ! === Schema init without explicit call ===
            test("schema_auto_init", test_schema_auto_init), &
            ! === Additional formatter ===
            test("dump_text_before_block", test_dump_text_before_block) &
        ]))&
    ])
  end function tests

  ! ===========================================================================
  ! Error module
  ! ===========================================================================

  subroutine test_error_messages()
    call check(error_message(HSD_STAT_SCHEMA_ERROR) == "Schema validation error", &
        msg="Schema error msg")
    call check(error_message(999) == "Unknown error", msg="Unknown code")
    call check(error_message(-1) == "Unknown error", msg="Negative code")
  end subroutine test_error_messages

  subroutine test_syntax_error_no_file()
    type(hsd_error_t), allocatable :: err

    call make_syntax_error(err, "test syntax error")
    call check(allocated(err), msg="Allocated")
    call check(err%code == HSD_STAT_SYNTAX_ERROR, msg="Code")
    call check(err%filename == "<unknown>", msg="Default filename")
  end subroutine test_syntax_error_no_file

  subroutine test_type_error_all_args()
    type(hsd_error_t), allocatable :: err

    call make_type_error(err, "mismatch", filename="t.hsd", line=42, &
        expected="integer", actual="string", hint="convert")
    call check(allocated(err), msg="Allocated")
    call check(err%expected == "integer", msg="Expected")
    call check(err%line_start == 42, msg="Line")
  end subroutine test_type_error_all_args

  subroutine test_type_error_no_file()
    type(hsd_error_t), allocatable :: err

    call make_type_error(err, "mismatch")
    call check(err%filename == "<unknown>", msg="Default filename")
  end subroutine test_type_error_no_file

  ! ===========================================================================
  ! Schema - value-specific branches
  ! ===========================================================================

  subroutine test_schema_int_range_err()
    type(hsd_schema_t) :: schema
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: errors(:), parse_err

    call hsd_load_string("Value = 999", root, parse_err)
    call schema_init(schema)
    call schema_add_field(schema, "Value", FIELD_REQUIRED, &
        FIELD_TYPE_INTEGER, min_int=1, max_int=100)
    call schema_validate(schema, root, errors)
    call check(size(errors) == 1, msg="Out of range")
    call schema_destroy(schema)
    call root%destroy()
  end subroutine test_schema_int_range_err

  subroutine test_schema_real_convert_err()
    type(hsd_schema_t) :: schema
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: errors(:), parse_err

    call hsd_load_string("Value = notreal", root, parse_err)
    call schema_init(schema)
    call schema_add_field(schema, "Value", FIELD_REQUIRED, FIELD_TYPE_REAL)
    call schema_validate(schema, root, errors)
    call check(size(errors) == 1, msg="Cannot convert to real")
    call schema_destroy(schema)
    call root%destroy()
  end subroutine test_schema_real_convert_err

  subroutine test_schema_real_range_err()
    type(hsd_schema_t) :: schema
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: errors(:), parse_err

    call hsd_load_string("Value = 999.0", root, parse_err)
    call schema_init(schema)
    call schema_add_field(schema, "Value", FIELD_REQUIRED, &
        FIELD_TYPE_REAL, min_real=0.0_dp, max_real=100.0_dp)
    call schema_validate(schema, root, errors)
    call check(size(errors) == 1, msg="Real out of range")
    call schema_destroy(schema)
    call root%destroy()
  end subroutine test_schema_real_range_err

  subroutine test_schema_logical_err()
    type(hsd_schema_t) :: schema
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: errors(:), parse_err

    call hsd_load_string("Value = notabool", root, parse_err)
    call schema_init(schema)
    call schema_add_field(schema, "Value", FIELD_REQUIRED, FIELD_TYPE_LOGICAL)
    call schema_validate(schema, root, errors)
    call check(size(errors) == 1, msg="Invalid logical")
    call schema_destroy(schema)
    call root%destroy()
  end subroutine test_schema_logical_err

  subroutine test_schema_array_not_array()
    type(hsd_schema_t) :: schema
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: errors(:), parse_err

    call hsd_load_string("Value = 42", root, parse_err)
    call schema_init(schema)
    call schema_add_field(schema, "Value", FIELD_REQUIRED, FIELD_TYPE_ARRAY)
    call schema_validate(schema, root, errors)
    call check(size(errors) == 1, msg="Not an array")
    call schema_destroy(schema)
    call root%destroy()
  end subroutine test_schema_array_not_array

  subroutine test_schema_complex_not_complex()
    type(hsd_schema_t) :: schema
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: errors(:), parse_err

    call hsd_load_string("Value = hello", root, parse_err)
    call schema_init(schema)
    call schema_add_field(schema, "Value", FIELD_REQUIRED, FIELD_TYPE_COMPLEX)
    call schema_validate(schema, root, errors)
    call check(size(errors) == 1, msg="Not complex")
    call schema_destroy(schema)
    call root%destroy()
  end subroutine test_schema_complex_not_complex

  subroutine test_schema_enum_reject()
    type(hsd_schema_t) :: schema
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: errors(:), parse_err

    call hsd_load_string("Color = purple", root, parse_err)
    call schema_init(schema)
    call schema_add_field_enum(schema, "Color", FIELD_REQUIRED, &
        [character(len=10) :: "red", "green", "blue"])
    call schema_validate(schema, root, errors)
    call check(size(errors) == 1, msg="Invalid enum")
    call schema_destroy(schema)
    call root%destroy()
  end subroutine test_schema_enum_reject

  subroutine test_schema_capacity_growth()
    type(hsd_schema_t) :: schema
    integer :: ii
    character(len=32) :: name

    call schema_init(schema)
    do ii = 1, 20
      write(name, '(A,I0)') "field_", ii
      call schema_add_field(schema, trim(name), FIELD_OPTIONAL, FIELD_TYPE_STRING)
    end do
    call check(schema%num_fields == 20, msg="20 fields")
    call schema_destroy(schema)
  end subroutine test_schema_capacity_growth

  ! ===========================================================================
  ! Schema - get_type_name branches via "is a table, expected X" error
  ! ===========================================================================

  subroutine test_schema_table_as_real()
    type(hsd_schema_t) :: schema
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: errors(:), parse_err

    call hsd_load_string("Sect { x = 1 }", root, parse_err)
    call schema_init(schema)
    call schema_add_field(schema, "Sect", FIELD_REQUIRED, FIELD_TYPE_REAL)
    call schema_validate(schema, root, errors)
    call check(size(errors) == 1, msg="Table not real")
    call schema_destroy(schema)
    call root%destroy()
  end subroutine test_schema_table_as_real

  subroutine test_schema_table_as_logical()
    type(hsd_schema_t) :: schema
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: errors(:), parse_err

    call hsd_load_string("Sect { x = 1 }", root, parse_err)
    call schema_init(schema)
    call schema_add_field(schema, "Sect", FIELD_REQUIRED, FIELD_TYPE_LOGICAL)
    call schema_validate(schema, root, errors)
    call check(size(errors) == 1, msg="Table not logical")
    call schema_destroy(schema)
    call root%destroy()
  end subroutine test_schema_table_as_logical

  subroutine test_schema_table_as_array()
    type(hsd_schema_t) :: schema
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: errors(:), parse_err

    call hsd_load_string("Sect { x = 1 }", root, parse_err)
    call schema_init(schema)
    call schema_add_field(schema, "Sect", FIELD_REQUIRED, FIELD_TYPE_ARRAY)
    call schema_validate(schema, root, errors)
    call check(size(errors) == 1, msg="Table not array")
    call schema_destroy(schema)
    call root%destroy()
  end subroutine test_schema_table_as_array

  subroutine test_schema_table_as_complex()
    type(hsd_schema_t) :: schema
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: errors(:), parse_err

    call hsd_load_string("Sect { x = 1 }", root, parse_err)
    call schema_init(schema)
    call schema_add_field(schema, "Sect", FIELD_REQUIRED, FIELD_TYPE_COMPLEX)
    call schema_validate(schema, root, errors)
    call check(size(errors) == 1, msg="Table not complex")
    call schema_destroy(schema)
    call root%destroy()
  end subroutine test_schema_table_as_complex

  subroutine test_schema_value_as_table()
    type(hsd_schema_t) :: schema
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: errors(:), parse_err

    call hsd_load_string("Value = 42", root, parse_err)
    call schema_init(schema)
    call schema_add_field(schema, "Value", FIELD_REQUIRED, FIELD_TYPE_TABLE)
    call schema_validate(schema, root, errors)
    call check(size(errors) == 1, msg="Value not table")
    call schema_destroy(schema)
    call root%destroy()
  end subroutine test_schema_value_as_table

  ! ===========================================================================
  ! Validation
  ! ===========================================================================

  subroutine test_type_name_branches()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: err, parse_err

    call hsd_load_string("Key = hello", root, parse_err)

    call hsd_require(root, "Key", err, expected_type=VALUE_TYPE_REAL)
    call check(allocated(err), msg="Real mismatch")
    if (allocated(err)) deallocate(err)

    call hsd_require(root, "Key", err, expected_type=VALUE_TYPE_LOGICAL)
    call check(allocated(err), msg="Logical mismatch")
    if (allocated(err)) deallocate(err)

    call hsd_require(root, "Key", err, expected_type=VALUE_TYPE_ARRAY)
    call check(allocated(err), msg="Array mismatch")
    if (allocated(err)) deallocate(err)

    call hsd_require(root, "Key", err, expected_type=VALUE_TYPE_COMPLEX)
    call check(allocated(err), msg="Complex mismatch")
    if (allocated(err)) deallocate(err)

    call hsd_require(root, "Key", err, expected_type=999)
    call check(allocated(err), msg="Unknown type mismatch")

    call root%destroy()
  end subroutine test_type_name_branches

  subroutine test_validate_one_of_match()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: err, parse_err

    call hsd_load_string("Method = broyden", root, parse_err)
    call hsd_validate_one_of(root, "Method", &
        [character(len=10) :: "broyden", "anderson"], err)
    call check(.not. allocated(err), msg="Valid choice")
    call root%destroy()
  end subroutine test_validate_one_of_match

  subroutine test_validate_one_of_table()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: err, parse_err

    call hsd_load_string("Sect { x = 1 }", root, parse_err)
    call hsd_validate_one_of(root, "Sect", &
        [character(len=5) :: "a", "b"], err)
    call check(allocated(err), msg="Table rejected")
    call root%destroy()
  end subroutine test_validate_one_of_table

  subroutine test_validate_range_oob()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: err, parse_err

    call hsd_load_string("Value = 999.0", root, parse_err)
    call hsd_validate_range(root, "Value", 0.0_dp, 100.0_dp, err)
    call check(allocated(err), msg="Out of range")
    call root%destroy()
  end subroutine test_validate_range_oob

  subroutine test_validate_range_non_val()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: err, parse_err

    call hsd_load_string("Sect { x = 1 }", root, parse_err)
    call hsd_validate_range(root, "Sect", 0.0_dp, 100.0_dp, err)
    call check(allocated(err), msg="Non-value rejected")
    call root%destroy()
  end subroutine test_validate_range_non_val

  subroutine test_require_type_mismatch()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: err, parse_err

    call hsd_load_string("Key = hello", root, parse_err)
    call hsd_require(root, "Key", err, expected_type=VALUE_TYPE_INTEGER)
    call check(allocated(err), msg="Type mismatch")
    call root%destroy()
  end subroutine test_require_type_mismatch

  ! ===========================================================================
  ! Formatter - file dump with manually constructed trees
  ! ===========================================================================

  subroutine test_dump_unnamed_tbl_val()
    type(hsd_table) :: root, unnamed
    type(hsd_value) :: val
    type(hsd_error_t), allocatable :: err
    character(len=512) :: filepath

    call new_table(root, "")
    call new_table(unnamed, "")
    call new_value(val, "")
    call val%set_integer(42)
    call unnamed%add_child(val)
    call root%add_child(unnamed)

    filepath = trim(build_dir) // "/test_dump_unnamed_val.hsd"
    call hsd_dump(root, trim(filepath), err)
    call check(.not. allocated(err), msg="Dump ok")
    call root%destroy()
  end subroutine test_dump_unnamed_tbl_val

  subroutine test_dump_unnamed_tbl_multi()
    type(hsd_table) :: root, unnamed
    type(hsd_value) :: v1, v2
    type(hsd_error_t), allocatable :: err
    character(len=512) :: filepath

    call new_table(root, "")
    call new_table(unnamed, "")
    call new_value(v1, "X")
    call v1%set_integer(1)
    call unnamed%add_child(v1)
    call new_value(v2, "Y")
    call v2%set_integer(2)
    call unnamed%add_child(v2)
    call root%add_child(unnamed)

    filepath = trim(build_dir) // "/test_dump_unnamed_multi.hsd"
    call hsd_dump(root, trim(filepath), err)
    call check(.not. allocated(err), msg="Dump ok")
    call root%destroy()
  end subroutine test_dump_unnamed_tbl_multi

  subroutine test_dump_anon_val_single()
    type(hsd_table) :: root, parent
    type(hsd_value) :: val
    type(hsd_error_t), allocatable :: err
    character(len=512) :: filepath

    call new_table(root, "")
    call new_table(parent, "Parent")
    call new_value(val, "")
    call val%set_string("anonymous text")
    call parent%add_child(val)
    call root%add_child(parent)

    filepath = trim(build_dir) // "/test_dump_anon_single.hsd"
    call hsd_dump(root, trim(filepath), err)
    call check(.not. allocated(err), msg="Dump ok")
    call root%destroy()
  end subroutine test_dump_anon_val_single

  subroutine test_dump_anon_val_multi()
    type(hsd_table) :: root, parent
    type(hsd_value) :: val, dummy
    type(hsd_error_t), allocatable :: err
    character(len=512) :: filepath

    call new_table(root, "")
    call new_table(parent, "Parent")
    ! Add a named sibling to break single-child optimization
    call new_value(dummy, "Named")
    call dummy%set_integer(1)
    call parent%add_child(dummy)
    ! Add anonymous multiline value
    call new_value(val, "")
    val%value_type = VALUE_TYPE_STRING
    val%raw_text = "line1" // char(10) // "line2" // char(10) // "line3"
    call parent%add_child(val)
    call root%add_child(parent)

    filepath = trim(build_dir) // "/test_dump_anon_multi.hsd"
    call hsd_dump(root, trim(filepath), err)
    call check(.not. allocated(err), msg="Dump ok")
    call root%destroy()
  end subroutine test_dump_anon_val_multi

  subroutine test_dump_val_with_attrib()
    type(hsd_table) :: root, parent
    type(hsd_value) :: val
    type(hsd_error_t), allocatable :: err
    character(len=512) :: filepath

    call new_table(root, "")
    call new_table(parent, "Force")
    call new_value(val, "")
    val%attrib = "eV/Angstrom"
    call val%set_string("0.001")
    call parent%add_child(val)
    call root%add_child(parent)

    filepath = trim(build_dir) // "/test_dump_val_attrib.hsd"
    call hsd_dump(root, trim(filepath), err)
    call check(.not. allocated(err), msg="Dump ok")
    call root%destroy()
  end subroutine test_dump_val_with_attrib

  subroutine test_dump_empty_string_val()
    type(hsd_table) :: root
    type(hsd_value) :: val
    type(hsd_error_t), allocatable :: err
    character(len=512) :: filepath

    call new_table(root, "")
    call new_value(val, "Empty")
    val%value_type = VALUE_TYPE_STRING
    call root%add_child(val)

    filepath = trim(build_dir) // "/test_dump_empty_string.hsd"
    call hsd_dump(root, trim(filepath), err)
    call check(.not. allocated(err), msg="Dump ok")
    call root%destroy()
  end subroutine test_dump_empty_string_val

  subroutine test_dump_default_type_val()
    type(hsd_table) :: root
    type(hsd_value) :: val
    type(hsd_error_t), allocatable :: err
    character(len=512) :: filepath

    call new_table(root, "")
    call new_value(val, "Weird")
    val%value_type = VALUE_TYPE_NONE
    val%string_value = "something"
    call root%add_child(val)

    filepath = trim(build_dir) // "/test_dump_default_type.hsd"
    call hsd_dump(root, trim(filepath), err)
    call check(.not. allocated(err), msg="Dump ok")
    call root%destroy()
  end subroutine test_dump_default_type_val

  ! ===========================================================================
  ! Query
  ! ===========================================================================

  subroutine test_remove_nonexist_parent()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: err
    integer :: stat

    call hsd_load_string("A = 1", root, err)
    call hsd_remove_child(root, "NonExist/Child", stat)
    call check(stat == HSD_STAT_NOT_FOUND, msg="Parent not found")
    call root%destroy()
  end subroutine test_remove_nonexist_parent

  subroutine test_merge_new_table_child()
    type(hsd_table) :: base, overlay
    type(hsd_error_t), allocatable :: err
    integer :: stat

    call hsd_load_string("A = 1", base, err)
    call hsd_load_string("NewSect { X = 10 }", overlay, err)
    call hsd_merge(base, overlay, stat)
    call check(stat == HSD_STAT_OK, msg="Merge ok")
    call check(hsd_has_child(base, "NewSect"), msg="Table added")
    call base%destroy()
    call overlay%destroy()
  end subroutine test_merge_new_table_child

  subroutine test_merge_new_value_child()
    type(hsd_table) :: base, overlay
    type(hsd_error_t), allocatable :: err
    integer :: stat

    call hsd_load_string("A = 1", base, err)
    call hsd_load_string("NewKey = hello", overlay, err)
    call hsd_merge(base, overlay, stat)
    call check(stat == HSD_STAT_OK, msg="Merge ok")
    call check(hsd_has_child(base, "NewKey"), msg="Value added")
    call base%destroy()
    call overlay%destroy()
  end subroutine test_merge_new_value_child

  subroutine test_merge_replace_value()
    type(hsd_table) :: base, overlay
    type(hsd_error_t), allocatable :: err
    integer :: val, stat

    call hsd_load_string("Key = 1", base, err)
    call hsd_load_string("Key = 99", overlay, err)
    call hsd_merge(base, overlay, stat)
    call check(stat == HSD_STAT_OK, msg="Merge ok")
    call hsd_get(base, "Key", val, stat)
    call check(val == 99, msg="Value replaced")
    call base%destroy()
    call overlay%destroy()
  end subroutine test_merge_replace_value

  subroutine test_merge_raw_text()
    type(hsd_table) :: base, overlay
    type(hsd_value) :: v1, v2
    integer :: stat

    call new_table(base, "root")
    call new_value(v1, "Key")
    v1%value_type = VALUE_TYPE_STRING
    v1%raw_text = "original"
    call base%add_child(v1)

    call new_table(overlay, "root")
    call new_value(v2, "Key")
    v2%value_type = VALUE_TYPE_STRING
    v2%raw_text = "replaced"
    call overlay%add_child(v2)

    call hsd_merge(base, overlay, stat)
    call check(stat == HSD_STAT_OK, msg="Merge ok")
    call base%destroy()
    call overlay%destroy()
  end subroutine test_merge_raw_text

  subroutine test_merge_arrays()
    type(hsd_table) :: base, overlay
    type(hsd_value) :: v1, v2
    integer :: stat

    call new_table(base, "root")
    call new_value(v1, "Data")
    v1%value_type = VALUE_TYPE_ARRAY
    allocate(v1%int_array(3))
    v1%int_array = [1, 2, 3]
    allocate(v1%real_array(3))
    v1%real_array = [1.0_dp, 2.0_dp, 3.0_dp]
    call base%add_child(v1)

    call new_table(overlay, "root")
    call new_value(v2, "Data")
    v2%value_type = VALUE_TYPE_ARRAY
    allocate(v2%int_array(2))
    v2%int_array = [10, 20]
    allocate(v2%real_array(2))
    v2%real_array = [10.0_dp, 20.0_dp]
    call overlay%add_child(v2)

    call hsd_merge(base, overlay, stat)
    call check(stat == HSD_STAT_OK, msg="Array merge ok")
    call base%destroy()
    call overlay%destroy()
  end subroutine test_merge_arrays

  subroutine test_clone_table_values()
    type(hsd_table) :: source, dest
    type(hsd_value) :: v1
    type(hsd_table) :: sub
    integer :: stat

    call new_table(source, "root")
    call new_value(v1, "Val1")
    call v1%set_integer(42)
    call source%add_child(v1)
    call new_table(sub, "SubTable")
    call source%add_child(sub)

    call hsd_clone(source, dest, stat)
    call check(stat == HSD_STAT_OK, msg="Clone ok")
    call check(dest%has_child("Val1"), msg="Val1 cloned")
    call check(dest%has_child("SubTable"), msg="SubTable cloned")
    call source%destroy()
    call dest%destroy()
  end subroutine test_clone_table_values

  subroutine test_get_table_on_value()
    type(hsd_table) :: root
    type(hsd_table), pointer :: tbl
    type(hsd_error_t), allocatable :: err
    integer :: stat

    call hsd_load_string("Value = 42", root, err)
    call hsd_get_table(root, "Value", tbl, stat)
    call check(stat /= HSD_STAT_OK, msg="Value not table")
    call root%destroy()
  end subroutine test_get_table_on_value

  ! ===========================================================================
  ! Parser - syntax
  ! ===========================================================================

  subroutine test_parser_blank_lines()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: err
    integer :: val, stat

    call hsd_load_string("A = 1" // char(10) // char(10) // &
        char(10) // "B = 2", root, err)
    call check(.not. allocated(err), msg="Blank lines ok")
    call hsd_get(root, "A", val, stat)
    call check(val == 1, msg="A = 1")
    call hsd_get(root, "B", val, stat)
    call check(val == 2, msg="B = 2")
    call root%destroy()
  end subroutine test_parser_blank_lines

  subroutine test_parser_ws_after_eq()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: err
    integer :: val, stat

    call hsd_load_string("Tag   =   42", root, err)
    call check(.not. allocated(err), msg="WS after eq ok")
    call hsd_get(root, "Tag", val, stat)
    call check(val == 42, msg="Tag = 42")
    call root%destroy()
  end subroutine test_parser_ws_after_eq

  subroutine test_parser_text_before_block()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: err

    call hsd_load_string("some text" // char(10) // "Block { x = 1 }", root, err)
    call check(.not. allocated(err), msg="Text before block ok")
    call root%destroy()
  end subroutine test_parser_text_before_block

  subroutine test_parser_tag_eq_block()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: err
    integer :: val, stat

    call hsd_load_string("Outer = { Inner = 42 }", root, err)
    call check(.not. allocated(err), msg="Tag = { } ok")
    call hsd_get(root, "Outer/Inner", val, stat)
    call check(val == 42, msg="Inner = 42")
    call root%destroy()
  end subroutine test_parser_tag_eq_block

  subroutine test_parser_tag_eq_child_block()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: err

    call hsd_load_string("Ham = DFTB { SCC = Yes }", root, err)
    call check(.not. allocated(err), msg="Tag = Child { } ok")
    call check(hsd_has_child(root, "Ham/DFTB/SCC"), msg="Nested exists")
    call root%destroy()
  end subroutine test_parser_tag_eq_child_block

  subroutine test_parser_semicolons()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: err
    integer :: val, stat

    call hsd_load_string("A = 1; B = 2", root, err)
    call check(.not. allocated(err), msg="Semicolons ok")
    call hsd_get(root, "A", val, stat)
    call check(val == 1, msg="A = 1")
    call hsd_get(root, "B", val, stat)
    call check(val == 2, msg="B = 2")
    call root%destroy()
  end subroutine test_parser_semicolons

  ! ===========================================================================
  ! Parser - includes (must use hsd_load with real files)
  ! ===========================================================================

  subroutine test_text_include()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: err
    character(len=512) :: filepath, hsd_path

    filepath = trim(build_dir) // "/test_text_inc_content.txt"
    call write_test_file(filepath, "hello world")

    hsd_path = trim(build_dir) // "/test_text_inc.hsd"
    call write_test_file(hsd_path, 'Data {' // char(10) // &
        '  <<< "' // trim(filepath) // '"' // char(10) // '}')

    call hsd_load(trim(hsd_path), root, err)
    call check(.not. allocated(err), msg="Text include ok")
    call root%destroy()
  end subroutine test_text_include

  subroutine test_text_include_append()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: err
    character(len=512) :: f1, f2, hsd_path

    f1 = trim(build_dir) // "/test_tinc_a.txt"
    f2 = trim(build_dir) // "/test_tinc_b.txt"
    call write_test_file(f1, "first line")
    call write_test_file(f2, "second line")

    hsd_path = trim(build_dir) // "/test_tinc_append.hsd"
    call write_test_file(hsd_path, 'Data {' // char(10) // &
        '  <<< "' // trim(f1) // '"' // char(10) // &
        '  <<< "' // trim(f2) // '"' // char(10) // '}')

    call hsd_load(trim(hsd_path), root, err)
    call check(.not. allocated(err), msg="Double text include ok")
    call root%destroy()
  end subroutine test_text_include_append

  subroutine test_hsd_include()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: err
    character(len=512) :: inc_path, hsd_path

    inc_path = trim(build_dir) // "/test_hsd_inc_content.hsd"
    call write_test_file(inc_path, "Included = 42")

    hsd_path = trim(build_dir) // "/test_hsd_inc.hsd"
    call write_test_file(hsd_path, 'Outer {' // char(10) // &
        '  <<+ "' // trim(inc_path) // '"' // char(10) // '}')

    call hsd_load(trim(hsd_path), root, err)
    call check(.not. allocated(err), msg="HSD include ok")
    call root%destroy()
  end subroutine test_hsd_include

  subroutine test_include_cycle()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: err
    character(len=512) :: fa, fb

    fa = trim(build_dir) // "/test_cyc_a_cov.hsd"
    fb = trim(build_dir) // "/test_cyc_b_cov.hsd"
    call write_test_file(fa, '<<+ "' // trim(fb) // '"')
    call write_test_file(fb, '<<+ "' // trim(fa) // '"')

    call hsd_load(trim(fa), root, err)
    call check(allocated(err), msg="Cycle detected")
    call root%destroy()
  end subroutine test_include_cycle

  subroutine test_include_depth_limit()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: err
    character(len=512) :: filepath

    filepath = trim(build_dir) // "/test_self_inc_cov.hsd"
    call write_test_file(filepath, '<<+ "' // trim(filepath) // '"')

    call hsd_load(trim(filepath), root, err)
    call check(allocated(err), msg="Depth limit detected")
    call root%destroy()
  end subroutine test_include_depth_limit

  subroutine test_include_push_error()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: err
    character(len=512) :: fa, fb, fc

    fa = trim(build_dir) // "/test_push_a.hsd"
    fb = trim(build_dir) // "/test_push_b.hsd"
    fc = trim(build_dir) // "/test_push_c.hsd"
    call write_test_file(fa, '<<+ "' // trim(fb) // '"')
    call write_test_file(fb, '<<+ "' // trim(fc) // '"')
    call write_test_file(fc, '<<+ "' // trim(fa) // '"')

    call hsd_load(trim(fa), root, err)
    call check(allocated(err), msg="Push error detected")
    call root%destroy()
  end subroutine test_include_push_error

  subroutine test_text_include_io_error()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: err
    character(len=512) :: hsd_path

    hsd_path = trim(build_dir) // "/test_text_io_err.hsd"
    call write_test_file(hsd_path, 'Data {' // char(10) // &
        '  <<< "/nonexistent/file.txt"' // char(10) // '}')

    call hsd_load(trim(hsd_path), root, err)
    call check(allocated(err), msg="Text include IO error")
    call root%destroy()
  end subroutine test_text_include_io_error

  subroutine test_parse_nonexistent()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: err

    call hsd_load("/nonexistent/file.hsd", root, err)
    call check(allocated(err), msg="Nonexistent file error")
  end subroutine test_parse_nonexistent

  subroutine test_parse_error_in_block()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: err
    character(len=512) :: hsd_path

    hsd_path = trim(build_dir) // "/test_err_in_block.hsd"
    call write_test_file(hsd_path, 'Outer = {' // char(10) // &
        '  <<< "/nonexistent/file.txt"' // char(10) // '}')

    call hsd_load(trim(hsd_path), root, err)
    call check(allocated(err), msg="Error in block propagated")
    call root%destroy()
  end subroutine test_parse_error_in_block

  subroutine test_parse_error_in_nested()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: err
    character(len=512) :: hsd_path

    hsd_path = trim(build_dir) // "/test_err_in_nested.hsd"
    call write_test_file(hsd_path, 'Ham = DFTB {' // char(10) // &
        '  <<< "/nonexistent/file.txt"' // char(10) // '}')

    call hsd_load(trim(hsd_path), root, err)
    call check(allocated(err), msg="Error in nested propagated")
    call root%destroy()
  end subroutine test_parse_error_in_nested

  ! ===========================================================================
  ! Lexer
  ! ===========================================================================

  subroutine test_lexer_squote_escape()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: err
    character(len=:), allocatable :: val
    integer :: stat

    call hsd_load_string("Value = ""hello\'world""", root, err)
    call check(.not. allocated(err), msg="Parse ok")
    call hsd_get(root, "Value", val, stat)
    call check(stat == HSD_STAT_OK, msg="Value retrieved")
    call check(index(val, "'") > 0, msg="Contains apostrophe")
    call root%destroy()
  end subroutine test_lexer_squote_escape

  subroutine test_lexer_attrib_ctx()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: err
    character(len=:), allocatable :: attrib
    integer :: stat

    call hsd_load_string("Force [eV/Angstrom] = 0.001", root, err)
    call check(.not. allocated(err), msg="Attrib parsed")
    call hsd_get_attrib(root, "Force", attrib, stat)
    call check(attrib == "eV/Angstrom", msg="Attrib correct")
    call root%destroy()
  end subroutine test_lexer_attrib_ctx

  ! ===========================================================================
  ! Types
  ! ===========================================================================

  subroutine test_tokenize_string()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: err
    integer, allocatable :: vals(:)
    integer :: stat

    call hsd_load_string("Data = 1 2 3 4 5", root, err)
    call check(.not. allocated(err), msg="Parse ok")
    call hsd_get(root, "Data", vals, stat)
    call check(stat == HSD_STAT_OK, msg="Array got")
    call check(size(vals) == 5, msg="5 elements")
    call root%destroy()
  end subroutine test_tokenize_string

  subroutine test_tokenize_quoted_string()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: err

    call hsd_load_string('Options = "opt1" "opt2" "opt3"', root, err)
    call check(.not. allocated(err), msg="Parse ok")
    call root%destroy()
  end subroutine test_tokenize_quoted_string

  subroutine test_split_by_newlines()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: err
    integer, allocatable :: mat(:,:)
    integer :: nrows, ncols, stat

    call hsd_load_string("Matrix {" // char(10) // &
        "  1 2 3" // char(10) // &
        "  4 5 6" // char(10) // "}", root, err)
    call check(.not. allocated(err), msg="Parse ok")
    call hsd_get_matrix(root, "Matrix", mat, nrows, ncols, stat)
    call check(stat == HSD_STAT_OK, msg="Matrix ok")
    call check(nrows * ncols == 6, msg="6 elements")
    call root%destroy()
  end subroutine test_split_by_newlines

  subroutine test_complex_bad_imag()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: err
    complex(dp) :: val
    integer :: stat

    call hsd_load_string("Z = xi", root, err)
    call check(.not. allocated(err), msg="Parse ok")
    call hsd_get(root, "Z", val, stat)
    call check(stat /= HSD_STAT_OK, msg="Bad imaginary fails")
    call root%destroy()
  end subroutine test_complex_bad_imag

  ! ===========================================================================
  ! Visitor
  ! ===========================================================================

  subroutine test_visitor_unnamed_child()
    type(hsd_table) :: root, parent
    type(hsd_value) :: unnamed_val
    type(count_visitor) :: cv
    integer :: stat

    call new_table(root, "")
    call new_table(parent, "Parent")
    ! Create value WITHOUT name parameter so name is truly unallocated
    call new_value(unnamed_val)
    call unnamed_val%set_string("anonymous")
    call parent%add_child(unnamed_val)
    call root%add_child(parent)

    cv%table_count = 0
    cv%value_count = 0
    call hsd_accept(root, cv, stat)
    call check(cv%value_count > 0, msg="Visited values")
    call root%destroy()
  end subroutine test_visitor_unnamed_child

  ! ===========================================================================
  ! Hash table
  ! ===========================================================================

  subroutine test_hash_remove_empty()
    type(hsd_table) :: root
    integer :: stat

    call new_table(root, "empty")
    call root%remove_child_by_name("nonexistent", stat)
    call check(stat /= HSD_STAT_OK, msg="Remove from empty ok")
    call root%destroy()
  end subroutine test_hash_remove_empty

  subroutine test_hash_overflow_chain()
    type(hsd_table) :: root
    type(hsd_value) :: val
    integer :: ii, stat
    character(len=32) :: name
    class(hsd_node), pointer :: child

    call new_table(root, "root")

    do ii = 1, 200
      write(name, '(A,I0)') "item_", ii
      call new_value(val, trim(name))
      call val%set_integer(ii)
      call root%add_child(val)
    end do

    do ii = 1, 50
      write(name, '(A,I0)') "item_", ii
      call root%remove_child_by_name(trim(name), stat)
      call check(stat == HSD_STAT_OK, msg="Remove " // trim(name))
    end do

    do ii = 51, 200
      write(name, '(A,I0)') "item_", ii
      call root%get_child_by_name(trim(name), child)
      call check(associated(child), msg="Found " // trim(name))
    end do

    call root%destroy()
  end subroutine test_hash_overflow_chain

  ! ===========================================================================
  ! Matrix from table
  ! ===========================================================================

  subroutine test_matrix_int_from_table()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: err
    integer, allocatable :: mat(:,:)
    integer :: nrows, ncols, stat

    call hsd_load_string("M {" // char(10) // &
        "  1 2 3" // char(10) // &
        "  4 5 6" // char(10) // "}", root, err)
    call check(.not. allocated(err), msg="Parse ok")
    call hsd_get_matrix(root, "M", mat, nrows, ncols, stat)
    call check(stat == HSD_STAT_OK, msg="Int matrix ok")
    call check(nrows * ncols == 6, msg="6 elements")
    call root%destroy()
  end subroutine test_matrix_int_from_table

  subroutine test_matrix_real_from_table()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: err
    real(dp), allocatable :: mat(:,:)
    integer :: nrows, ncols, stat

    call hsd_load_string("M {" // char(10) // &
        "  1.0 2.0 3.0" // char(10) // &
        "  4.0 5.0 6.0" // char(10) // "}", root, err)
    call check(.not. allocated(err), msg="Parse ok")
    call hsd_get_matrix(root, "M", mat, nrows, ncols, stat)
    call check(stat == HSD_STAT_OK, msg="Real matrix ok")
    call check(nrows * ncols == 6, msg="6 elements")
    call root%destroy()
  end subroutine test_matrix_real_from_table

  ! ===========================================================================
  ! Additional accessor coverage
  ! ===========================================================================

  subroutine test_get_real_sp_default()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: err
    real(sp) :: val
    integer :: stat

    call hsd_load_string("Temp = 3.14", root, err)
    call hsd_get_or(root, "Temp", val, 0.0_sp, stat)
    call check(stat == HSD_STAT_OK, msg="Got real(sp) ok")
    call check(abs(val - 3.14_sp) < 0.01_sp, msg="Value correct")
    call root%destroy()
  end subroutine test_get_real_sp_default

  subroutine test_path_into_value()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: err
    integer :: val, stat

    call hsd_load_string("A = 42", root, err)
    ! Try to traverse A/B where A is a value, not a table
    call hsd_get(root, "A/B", val, stat)
    call check(stat /= HSD_STAT_OK, msg="Path into value fails")
    call root%destroy()
  end subroutine test_path_into_value

  subroutine test_matrix_int_multi_values()
    type(hsd_table) :: root, mtx_table
    type(hsd_value) :: v1, v2
    type(hsd_error_t), allocatable :: err
    integer, allocatable :: mat(:,:)
    integer :: nrows, ncols, stat

    ! Manually construct table with multiple unnamed value children
    call new_table(root, "")
    call new_table(mtx_table, "M")
    call new_value(v1, "")
    call v1%set_string("1 2 3")
    call mtx_table%add_child(v1)
    call new_value(v2, "")
    call v2%set_string("4 5 6")
    call mtx_table%add_child(v2)
    call root%add_child(mtx_table)

    call hsd_get_matrix(root, "M", mat, nrows, ncols, stat)
    call check(stat == HSD_STAT_OK, msg="Int matrix from multi ok")
    call root%destroy()
  end subroutine test_matrix_int_multi_values

  subroutine test_matrix_real_multi_values()
    type(hsd_table) :: root, mtx_table
    type(hsd_value) :: v1, v2
    type(hsd_error_t), allocatable :: err
    real(dp), allocatable :: mat(:,:)
    integer :: nrows, ncols, stat

    ! Manually construct table with multiple unnamed value children
    call new_table(root, "")
    call new_table(mtx_table, "M")
    call new_value(v1, "")
    call v1%set_string("1.0 2.0 3.0")
    call mtx_table%add_child(v1)
    call new_value(v2, "")
    call v2%set_string("4.0 5.0 6.0")
    call mtx_table%add_child(v2)
    call root%add_child(mtx_table)

    call hsd_get_matrix(root, "M", mat, nrows, ncols, stat)
    call check(stat == HSD_STAT_OK, msg="Real matrix from multi ok")
    call root%destroy()
  end subroutine test_matrix_real_multi_values

  subroutine test_schema_auto_init()
    type(hsd_schema_t) :: schema
    ! Don't call schema_init - let add_field trigger auto-init (line 200)
    call schema_add_field(schema, "test", FIELD_OPTIONAL, FIELD_TYPE_STRING)
    call check(schema%num_fields == 1, msg="Auto-init worked")
    call schema_destroy(schema)
  end subroutine test_schema_auto_init

  subroutine test_dump_text_before_block()
    type(hsd_table) :: root, child
    type(hsd_value) :: val
    type(hsd_error_t), allocatable :: err
    character(len=512) :: filepath

    ! Create tree where text content appears before a block child
    call new_table(root, "")
    call new_table(child, "Section")
    call new_value(val, "TextData")
    call val%set_string("some text")
    call child%add_child(val)
    call root%add_child(child)

    filepath = trim(build_dir) // "/test_text_before_block.hsd"
    call hsd_dump(root, trim(filepath), err)
    call check(.not. allocated(err), msg="Dump ok")
    call root%destroy()
  end subroutine test_dump_text_before_block

  ! ===========================================================================
  ! Helper
  ! ===========================================================================

  subroutine write_test_file(filepath, content)
    character(len=*), intent(in) :: filepath, content
    integer :: unit_num

    open(newunit=unit_num, file=trim(filepath), status='replace', action='write')
    write(unit_num, '(A)', advance='no') content
    close(unit_num)
  end subroutine write_test_file

end module test_full_coverage_suite
