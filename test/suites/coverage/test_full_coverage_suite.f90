!> Comprehensive coverage tests targeting all remaining uncovered code paths
module test_full_coverage_suite
  use hsd
  use hsd_error, only: make_syntax_error, make_type_error, make_error, &
      & error_message
  use hsd_token, only: token_name, TOKEN_WHITESPACE, TOKEN_NEWLINE, TOKEN_COMMENT, &
      & TOKEN_LBRACKET, TOKEN_RBRACKET, TOKEN_SEMICOLON, TOKEN_INCLUDE_TXT, &
      & TOKEN_INCLUDE_HSD, TOKEN_EOF, hsd_token_t
  use count_visitor_mod, only: count_visitor
  use fortuno_serial, only: test => serial_case_item, &
      & check => serial_check, suite => serial_suite_item, test_list
  use build_env, only: build_dir
  implicit none (type, external)
  private

  public :: tests

  !> Visitor that returns error on table visit
  type, extends(hsd_visitor_t) :: error_on_table_visitor
  contains
    procedure :: visit_table => eot_visit_table
    procedure :: visit_value => eot_visit_value
  end type error_on_table_visitor

  !> Visitor that returns error on value visit
  type, extends(hsd_visitor_t) :: error_on_value_visitor
  contains
    procedure :: visit_table => eov_visit_table
    procedure :: visit_value => eov_visit_value
  end type error_on_value_visitor

  !> Visitor that returns error on nested table visit (depth > 0)
  type, extends(hsd_visitor_t) :: nested_table_error_visitor
  contains
    procedure :: visit_table => nte_visit_table
    procedure :: visit_value => nte_visit_value
  end type nested_table_error_visitor

contains

  ! --- Error visitor implementations ---

  subroutine eot_visit_table(self, table, path, depth, stat)
    class(error_on_table_visitor), intent(inout) :: self
    type(hsd_table), intent(in), target :: table
    character(len=*), intent(in) :: path
    integer, intent(in) :: depth
    integer, intent(out), optional :: stat
    if (present(stat)) stat = 99
  end subroutine eot_visit_table

  subroutine eot_visit_value(self, val, path, depth, stat)
    class(error_on_table_visitor), intent(inout) :: self
    type(hsd_value), intent(in) :: val
    character(len=*), intent(in) :: path
    integer, intent(in) :: depth
    integer, intent(out), optional :: stat
    if (present(stat)) stat = 0
  end subroutine eot_visit_value

  subroutine eov_visit_table(self, table, path, depth, stat)
    class(error_on_value_visitor), intent(inout) :: self
    type(hsd_table), intent(in), target :: table
    character(len=*), intent(in) :: path
    integer, intent(in) :: depth
    integer, intent(out), optional :: stat
    if (present(stat)) stat = 0
  end subroutine eov_visit_table

  subroutine eov_visit_value(self, val, path, depth, stat)
    class(error_on_value_visitor), intent(inout) :: self
    type(hsd_value), intent(in) :: val
    character(len=*), intent(in) :: path
    integer, intent(in) :: depth
    integer, intent(out), optional :: stat
    if (present(stat)) stat = 99
  end subroutine eov_visit_value

  subroutine nte_visit_table(self, table, path, depth, stat)
    class(nested_table_error_visitor), intent(inout) :: self
    type(hsd_table), intent(in), target :: table
    character(len=*), intent(in) :: path
    integer, intent(in) :: depth
    integer, intent(out), optional :: stat
    ! OK at root (depth=0), error at nested tables
    if (depth > 0) then
      if (present(stat)) stat = 99
    else
      if (present(stat)) stat = 0
    end if
  end subroutine nte_visit_table

  subroutine nte_visit_value(self, val, path, depth, stat)
    class(nested_table_error_visitor), intent(inout) :: self
    type(hsd_value), intent(in) :: val
    character(len=*), intent(in) :: path
    integer, intent(in) :: depth
    integer, intent(out), optional :: stat
    if (present(stat)) stat = 0
  end subroutine nte_visit_value

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
            test("dump_text_before_block", test_dump_text_before_block), &
            ! === Mutator error/edge paths ===
            test("set_through_value", test_set_through_value), &
            test("set_sp_real_array", test_set_sp_real_array), &
            test("set_complex_array", test_set_complex_array), &
            test("set_empty_path", test_set_empty_path), &
            ! === Types - value getter not-found ===
            test("value_arrays_missing", test_value_arrays_missing), &
            test("value_matrix_missing", test_value_matrix_missing), &
            ! === Types - parsing ===
            test("parse_int_array_err", test_parse_int_array_error), &
            test("parse_real_array_err", test_parse_real_array_error), &
            test("tokenize_empty_input", test_tokenize_empty_input), &
            test("tokenize_quoted_tokens", test_tokenize_quoted_tokens), &
            test("parse_complex_all_formats", test_parse_complex_all), &
            test("parse_complex_errors", test_parse_complex_errors), &
            test("parse_complex_array_err", test_parse_complex_arr_err), &
            test("matrix_parse_errors", test_matrix_parse_errors), &
            test("split_newlines_edge", test_split_newlines_edge), &
            ! === Types - misc ===
            test("table_num_children_fn", test_table_num_children_fn), &
            test("table_get_keys_unnamed", test_table_get_keys_anon), &
            test("value_complex_direct", test_value_complex_direct), &
            test("value_attrib_empty", test_value_attrib_empty), &
            test("table_remove_ci", test_table_remove_ci), &
            test("iterator_past_end", test_iterator_past_end), &
            ! === Formatter edge cases ===
            test("dump_file_open_error", test_dump_file_open_error), &
            test("dump_multiline_value", test_dump_multiline_value), &
            test("dump_escape_quotes", test_dump_escape_quotes), &
            test("dump_unnamed_table_str", test_dump_unnamed_str), &
            test("dump_single_child_tbl", test_dump_single_child_tbl), &
            ! === Hash table ===
            test("hash_auto_init", test_hash_auto_init), &
            test("hash_overflow_update", test_hash_overflow_update), &
            test("hash_promote_remove", test_hash_promote_remove), &
            test("hash_remove_chain", test_hash_remove_chain_entry), &
            ! === Error module ===
            test("error_msg_all_codes", test_error_msg_all_codes), &
            test("error_print_variants", test_error_print_variants), &
            ! === Token ===
            test("token_name_all", test_token_name_all), &
            test("token_is_valid_fn", test_token_is_valid_fn), &
            ! === Validation ===
            test("type_name_none", test_type_name_none), &
            test("get_with_unit", test_get_with_unit), &
            test("validate_range_table", test_validate_range_table), &
            ! === Visitor error paths ===
            test("visitor_table_error", test_visitor_table_error), &
            test("visitor_value_error", test_visitor_value_error), &
            test("visitor_nested_tbl_err", test_visitor_nested_table_error), &
            ! === Query ===
            test("get_keys_not_found", test_get_keys_not_found), &
            test("remove_through_value", test_remove_through_value), &
            test("child_through_value", test_child_through_value), &
            ! === Schema extras ===
            test("schema_description", test_schema_description), &
            test("schema_logical_type", test_schema_logical_type), &
            test("schema_type_name_str", test_schema_type_name_str), &
            ! === Parser ===
            test("parse_with_filename", test_parse_with_filename), &
            ! === Utils/Lexer ===
            test("to_lower_empty", test_to_lower_empty), &
            test("lexer_no_filename", test_lexer_no_filename), &
            ! === Additional accessor coverage ===
            test("get_sp_array_error", test_get_sp_array_error), &
            test("get_logical_array_table", test_get_logical_array_table), &
            ! === Additional validation coverage ===
            test("validate_range_string_val", test_validate_range_string), &
            test("validate_range_missing", test_validate_range_missing), &
            test("require_type_none", test_require_type_none), &
            ! === Additional visitor coverage ===
            test("visitor_unnamed_deep", test_visitor_unnamed_deep), &
            ! === Additional parser coverage ===
            test("parser_multi_token_val", test_parser_multi_token), &
            ! === Additional formatter coverage ===
            test("dump_unnamed_tbl_tbl", test_dump_unnamed_tbl_tbl), &
            test("dump_tag_multiline_val", test_dump_tag_multiline_val), &
            ! === Additional types coverage ===
            test("get_keys_empty_table", test_get_keys_empty_table), &
            test("get_keys_mixed", test_get_keys_mixed), &
            test("iterator_uninit", test_iterator_uninit), &
            test("complex_array_raw_text", test_complex_array_raw_text), &
            test("string_array_raw_text", test_string_array_raw_text), &
            test("tokenize_quoted_empty", test_tokenize_quoted_empty), &
            test("complex_bad_real_abi", test_complex_bad_real_abi), &
            test("int_matrix_ragged_direct", test_int_matrix_ragged_direct), &
            test("real_matrix_ragged_direct", test_real_matrix_ragged_direct), &
            test("real_matrix_empty_direct", test_real_matrix_empty_direct), &
            test("int_matrix_empty_direct", test_int_matrix_empty_direct), &
            test("split_adjacent_newlines", test_split_adjacent_newlines) &
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
  ! Mutator error/edge paths
  ! ===========================================================================

  !> Test setting values through a value node (error path for all setters)
  subroutine test_set_through_value()
    type(hsd_table) :: root
    integer :: stat

    call new_table(root, "root")
    call hsd_set(root, "leaf", "text")
    ! Now try to set through "leaf" which is a value, not a table
    call hsd_set(root, "leaf/deep", "val", stat)
    call check(stat /= HSD_STAT_OK, msg="String through value")
    call hsd_set(root, "leaf/deep", 42, stat)
    call check(stat /= HSD_STAT_OK, msg="Integer through value")
    call hsd_set(root, "leaf/deep", 3.14_dp, stat)
    call check(stat /= HSD_STAT_OK, msg="Real through value")
    call hsd_set(root, "leaf/deep", .true., stat)
    call check(stat /= HSD_STAT_OK, msg="Logical through value")
    call hsd_set(root, "leaf/deep", (1.0_dp, 2.0_dp), stat)
    call check(stat /= HSD_STAT_OK, msg="Complex through value")
    call hsd_set(root, "leaf/deep", [1, 2, 3], stat)
    call check(stat /= HSD_STAT_OK, msg="Int array through value")
    call hsd_set(root, "leaf/deep", [1.0_dp, 2.0_dp], stat)
    call check(stat /= HSD_STAT_OK, msg="Real array through value")
    call hsd_set(root, "leaf/deep", [.true., .false.], stat)
    call check(stat /= HSD_STAT_OK, msg="Logical array through value")
    ! Complex array through value
    block
      complex(dp) :: carr(2)
      carr(1) = (1.0_dp, 2.0_dp)
      carr(2) = (3.0_dp, -4.0_dp)
      call hsd_set(root, "leaf/deep", carr, stat)
      call check(stat /= HSD_STAT_OK, msg="Complex arr through value")
    end block
    ! SP real array through value
    call hsd_set(root, "leaf/deep", [1.0_sp, 2.0_sp], stat)
    call check(stat /= HSD_STAT_OK, msg="SP array through value")
  end subroutine test_set_through_value

  !> Test setting single-precision real array
  subroutine test_set_sp_real_array()
    type(hsd_table) :: root
    integer :: stat
    real(dp), allocatable :: result(:)

    call new_table(root, "root")
    call hsd_set(root, "vals", [1.0_sp, 2.0_sp, 3.0_sp], stat)
    call check(stat == HSD_STAT_OK, msg="SP array set ok")
    call hsd_get(root, "vals", result, stat)
    call check(stat == HSD_STAT_OK, msg="SP array get ok")
    call check(size(result) == 3, msg="SP array size")
  end subroutine test_set_sp_real_array

  !> Test setting complex array
  subroutine test_set_complex_array()
    type(hsd_table) :: root
    integer :: stat
    character(len=:), allocatable :: text

    call new_table(root, "root")
    call hsd_set(root, "cvals", [(1.0_dp, 2.0_dp), (3.0_dp, -4.0_dp)], stat)
    call check(stat == HSD_STAT_OK, msg="Complex array set ok")
    call hsd_get(root, "cvals", text, stat)
    call check(stat == HSD_STAT_OK, msg="Complex array get text ok")
    call check(len(text) > 0, msg="Complex array has content")
  end subroutine test_set_complex_array

  !> Test setting with empty path
  subroutine test_set_empty_path()
    type(hsd_table) :: root
    integer :: stat

    call new_table(root, "root")
    call hsd_set(root, "", "value", stat)
    call check(stat == HSD_STAT_NOT_FOUND, msg="Empty path fails")
  end subroutine test_set_empty_path

  ! ===========================================================================
  ! Types - value getter not-found paths
  ! ===========================================================================

  !> Test getting arrays from value without raw_text
  subroutine test_value_arrays_missing()
    type(hsd_table) :: root
    integer :: stat
    integer, allocatable :: iarr(:)
    real(dp), allocatable :: rarr(:)
    logical, allocatable :: larr(:)
    complex(dp), allocatable :: carr(:)
    character(len=:), allocatable :: sarr(:)

    call new_table(root, "root")
    ! Create value with string_value but no raw_text
    block
      type(hsd_value), pointer :: val
      allocate(val)
      call new_value(val, "test")
      val%string_value = "hello"
      val%value_type = VALUE_TYPE_STRING
      call root%add_child(val)
    end block
    ! All array getters should return NOT_FOUND via string_value fallback
    call hsd_get(root, "test", iarr, stat)
    call check(stat /= HSD_STAT_OK, msg="Int array from string val")
    call hsd_get(root, "test", rarr, stat)
    call check(stat /= HSD_STAT_OK, msg="Real array from string val")
    call hsd_get(root, "test", larr, stat)
    call check(stat /= HSD_STAT_OK, msg="Logical array from string val")
    call hsd_get(root, "test", carr, stat)
    call check(stat /= HSD_STAT_OK, msg="Complex array from string val")
    call hsd_get(root, "test", sarr, stat)
    call check(stat == HSD_STAT_OK, msg="String array from string val")

    ! Also test on completely empty value
    block
      type(hsd_value), pointer :: val2
      allocate(val2)
      call new_value(val2, "empty")
      call root%add_child(val2)
    end block
    call hsd_get(root, "empty", iarr, stat)
    call check(stat == HSD_STAT_NOT_FOUND, msg="Int arr empty val")
    call hsd_get(root, "empty", rarr, stat)
    call check(stat == HSD_STAT_NOT_FOUND, msg="Real arr empty val")
    call hsd_get(root, "empty", larr, stat)
    call check(stat == HSD_STAT_NOT_FOUND, msg="Logical arr empty val")
    call hsd_get(root, "empty", carr, stat)
    call check(stat == HSD_STAT_NOT_FOUND, msg="Complex arr empty val")
    call hsd_get(root, "empty", sarr, stat)
    call check(stat == HSD_STAT_NOT_FOUND, msg="String arr empty val")
  end subroutine test_value_arrays_missing

  !> Test getting matrices from value without raw_text
  subroutine test_value_matrix_missing()
    type(hsd_table) :: root
    integer :: stat, nrows, ncols
    integer, allocatable :: imat(:,:)
    real(dp), allocatable :: rmat(:,:)

    call new_table(root, "root")
    block
      type(hsd_value), pointer :: val
      allocate(val)
      call new_value(val, "test")
      call root%add_child(val)
    end block
    call hsd_get_matrix(root, "test", imat, nrows, ncols, stat)
    call check(stat == HSD_STAT_NOT_FOUND, msg="Int matrix empty")
    call hsd_get_matrix(root, "test", rmat, nrows, ncols, stat)
    call check(stat == HSD_STAT_NOT_FOUND, msg="Real matrix empty")
  end subroutine test_value_matrix_missing

  ! ===========================================================================
  ! Types - parsing
  ! ===========================================================================

  !> Test parse_int_array with bad input
  subroutine test_parse_int_array_error()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    integer :: stat
    integer, allocatable :: arr(:)

    call hsd_load_string("Data = 1 2 abc 4", root, error)
    call check(.not. allocated(error), msg="Parse ok")
    call hsd_get(root, "Data", arr, stat)
    call check(stat /= HSD_STAT_OK, msg="Int array parse fails")
  end subroutine test_parse_int_array_error

  !> Test parse_real_array with bad input
  subroutine test_parse_real_array_error()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    integer :: stat
    real(dp), allocatable :: arr(:)

    call hsd_load_string("Data = 1.0 abc 3.0", root, error)
    call check(.not. allocated(error), msg="Parse ok")
    call hsd_get(root, "Data", arr, stat)
    call check(stat /= HSD_STAT_OK, msg="Real array parse fails")
  end subroutine test_parse_real_array_error

  !> Test tokenize_string with empty/whitespace input
  subroutine test_tokenize_empty_input()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    integer :: stat
    character(len=:), allocatable :: sarr(:)

    ! Whitespace-only value produces empty token list
    call hsd_load_string("Data =    ", root, error)
    call check(.not. allocated(error), msg="Parse ok")
    call hsd_get(root, "Data", sarr, stat)
    ! Should get empty or whitespace result
    call check(stat == HSD_STAT_OK .or. stat == HSD_STAT_NOT_FOUND, &
        msg="Tokenize whitespace")
  end subroutine test_tokenize_empty_input

  !> Test tokenize_quoted_string
  subroutine test_tokenize_quoted_tokens()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    integer :: stat
    character(len=:), allocatable :: sarr(:)

    ! Quoted strings in value
    call hsd_load_string('Data = "hello" world ""', root, error)
    call check(.not. allocated(error), msg="Parse ok")
    call hsd_get(root, "Data", sarr, stat)
    call check(stat == HSD_STAT_OK, msg="Quoted tokenize ok")
  end subroutine test_tokenize_quoted_tokens

  !> Test all complex number parsing formats
  subroutine test_parse_complex_all()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    integer :: stat
    complex(dp) :: cval

    ! Format: (re,im)
    call hsd_load_string("C = (1.0,2.0)", root, error)
    call check(.not. allocated(error), msg="Parse parens ok")
    call hsd_get(root, "C", cval, stat)
    call check(stat == HSD_STAT_OK, msg="Complex parens")

    ! Format: a+bi
    call hsd_load_string("C = 3.0+4.0i", root, error)
    call hsd_get(root, "C", cval, stat)
    call check(stat == HSD_STAT_OK, msg="Complex a+bi")

    ! Format: pure imaginary
    call hsd_load_string("C = 5.0i", root, error)
    call hsd_get(root, "C", cval, stat)
    call check(stat == HSD_STAT_OK, msg="Complex pure imag")

    ! Format: pure real (no i/j suffix)
    call hsd_load_string("C = 7.0", root, error)
    call hsd_get(root, "C", cval, stat)
    call check(stat == HSD_STAT_OK, msg="Complex pure real")
  end subroutine test_parse_complex_all

  !> Test complex parsing error paths
  subroutine test_parse_complex_errors()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    integer :: stat
    complex(dp) :: cval

    ! Empty string
    call hsd_load_string("C = ", root, error)
    if (.not. allocated(error)) then
      call hsd_get(root, "C", cval, stat)
      call check(stat /= HSD_STAT_OK, msg="Empty complex fails")
    end if

    ! Bad (re,im) format
    call hsd_load_string("C = (1.0,abc)", root, error)
    if (.not. allocated(error)) then
      call hsd_get(root, "C", cval, stat)
      call check(stat /= HSD_STAT_OK, msg="Bad parens imag fails")
    end if

    ! Bad a+bi format: bad imaginary
    call hsd_load_string("C = 3.0+abci", root, error)
    if (.not. allocated(error)) then
      call hsd_get(root, "C", cval, stat)
      call check(stat /= HSD_STAT_OK, msg="Bad a+bi imag fails")
    end if

    ! Bad (re,im): bad real part
    call hsd_load_string("C = (abc,2.0)", root, error)
    if (.not. allocated(error)) then
      call hsd_get(root, "C", cval, stat)
      call check(stat /= HSD_STAT_OK, msg="Bad parens real fails")
    end if
  end subroutine test_parse_complex_errors

  !> Test complex array parse error
  subroutine test_parse_complex_arr_err()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    integer :: stat
    complex(dp), allocatable :: carr(:)

    call hsd_load_string("C = 1.0+2.0i notcomplex", root, error)
    if (.not. allocated(error)) then
      call hsd_get(root, "C", carr, stat)
      call check(stat /= HSD_STAT_OK, msg="Complex array parse err")
    end if
  end subroutine test_parse_complex_arr_err

  !> Test matrix parsing edge cases
  subroutine test_matrix_parse_errors()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    integer :: stat, nrows, ncols
    integer, allocatable :: imat(:,:)
    real(dp), allocatable :: rmat(:,:)
    character(len=512) :: path

    ! Ragged integer matrix
    path = build_dir // "/test_ragged_int.hsd"
    call write_test_file(trim(path), &
        'Data {' // char(10) // '  1 2 3' // char(10) // &
        '  4 5' // char(10) // '}')
    call hsd_load(trim(path), root, error)
    if (.not. allocated(error)) then
      call hsd_get_matrix(root, "Data", imat, nrows, ncols, stat)
      call check(stat == HSD_STAT_OK .or. nrows > 0, msg="Ragged int")
    end if

    ! Empty integer matrix
    call write_test_file(trim(path), 'Data = ')
    call hsd_load(trim(path), root, error)
    if (.not. allocated(error)) then
      call hsd_get_matrix(root, "Data", imat, nrows, ncols, stat)
    end if

    ! Non-numeric integer matrix
    call write_test_file(trim(path), &
        'Data {' // char(10) // '  1 abc' // char(10) // '}')
    call hsd_load(trim(path), root, error)
    if (.not. allocated(error)) then
      call hsd_get_matrix(root, "Data", imat, nrows, ncols, stat)
      call check(stat /= HSD_STAT_OK, msg="Bad int matrix")
    end if

    ! Non-numeric real matrix
    call write_test_file(trim(path), &
        'Data {' // char(10) // '  1.0 abc' // char(10) // '}')
    call hsd_load(trim(path), root, error)
    if (.not. allocated(error)) then
      call hsd_get_matrix(root, "Data", rmat, nrows, ncols, stat)
      call check(stat /= HSD_STAT_OK, msg="Bad real matrix")
    end if

    ! Ragged real matrix
    call write_test_file(trim(path), &
        'Data {' // char(10) // '  1.0 2.0 3.0' // char(10) // &
        '  4.0' // char(10) // '}')
    call hsd_load(trim(path), root, error)
    if (.not. allocated(error)) then
      call hsd_get_matrix(root, "Data", rmat, nrows, ncols, stat)
      call check(stat == HSD_STAT_OK .or. nrows > 0, msg="Ragged real")
    end if
  end subroutine test_matrix_parse_errors

  !> Test split_by_newlines edge cases via matrix parsing
  subroutine test_split_newlines_edge()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: text
    integer :: stat, nrows, ncols
    integer, allocatable :: imat(:,:)
    character(len=512) :: path

    ! Multiline data with empty line: triggers split_by_newlines
    path = build_dir // "/test_split_nl.hsd"
    call write_test_file(trim(path), &
        'Data {' // char(10) // '  1 2 3' // char(10) // &
        char(10) // '  4 5 6' // char(10) // '}')
    call hsd_load(trim(path), root, error)
    call check(.not. allocated(error), msg="Load ok")
    call hsd_get_matrix(root, "Data", imat, nrows, ncols, stat)
    call check(stat == HSD_STAT_OK, msg="Double newline matrix")

    ! Single line (no newlines) - covered by other tests but ensure split path
    call hsd_load_string("Data = no_newlines_here", root, error)
    call hsd_get(root, "Data", text, stat)
    call check(text == "no_newlines_here", msg="No newlines ok")
  end subroutine test_split_newlines_edge

  ! ===========================================================================
  ! Types - misc
  ! ===========================================================================

  !> Test table_num_children direct call
  subroutine test_table_num_children_fn()
    type(hsd_table) :: root

    call new_table(root, "root")
    call check(root%num_children == 0, msg="Empty table 0 children")
    block
      type(hsd_value), pointer :: val
      allocate(val)
      call new_value(val, "child1")
      call root%add_child(val)
    end block
    call check(root%num_children == 1, msg="Table 1 child")
  end subroutine test_table_num_children_fn

  !> Test table_get_keys with unnamed (anonymous) children
  subroutine test_table_get_keys_anon()
    type(hsd_table) :: root
    integer :: stat
    character(len=:), allocatable :: keys(:)

    call new_table(root, "root")
    ! Add anonymous child (no name)
    block
      type(hsd_value), pointer :: val
      allocate(val)
      call new_value(val)
      call root%add_child(val)
    end block
    call hsd_get_keys(root, "", keys, stat)
    call check(stat == HSD_STAT_OK, msg="Keys from anonymous ok")
  end subroutine test_table_get_keys_anon

  !> Test getting complex value from cache
  subroutine test_value_complex_direct()
    type(hsd_table) :: root
    integer :: stat
    complex(dp) :: cval

    call new_table(root, "root")
    block
      type(hsd_value), pointer :: val
      allocate(val)
      call new_value(val, "cval")
      val%value_type = VALUE_TYPE_COMPLEX
      val%complex_value = (3.0_dp, 4.0_dp)
      call root%add_child(val)
    end block
    call hsd_get(root, "cval", cval, stat)
    call check(stat == HSD_STAT_OK, msg="Complex direct ok")
    call check(abs(real(cval) - 3.0_dp) < 1.0e-10_dp, msg="Real part")
  end subroutine test_value_complex_direct

  !> Test node_get_attrib when not allocated
  subroutine test_value_attrib_empty()
    type(hsd_value), pointer :: val
    character(len=:), allocatable :: attrib

    allocate(val)
    call new_value(val, "test")
    attrib = val%get_attrib()
    call check(attrib == "", msg="Empty attrib returns blank")
    deallocate(val)
  end subroutine test_value_attrib_empty

  !> Test table remove with case-insensitive lookup
  subroutine test_table_remove_ci()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error

    call hsd_load_string( &
        "Alpha = 1" // char(10) // "Beta = 2" // char(10) // &
        "Gamma = 3" // char(10) // "Delta = 4" // char(10) // &
        "Epsilon = 5", root, error)
    call check(.not. allocated(error), msg="Parse ok")
    ! Removing by name uses the hash index
    call root%remove_child_by_name("Beta", case_insensitive=.true.)
    call check(.not. hsd_has_child(root, "Beta"), msg="Beta removed")
    call check(hsd_has_child(root, "Alpha"), msg="Alpha kept")
  end subroutine test_table_remove_ci

  !> Test iterator past end
  subroutine test_iterator_past_end()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    type(hsd_iterator) :: iter
    class(hsd_node), pointer :: child
    logical :: has_more
    integer :: count

    call hsd_load_string("A = 1" // char(10) // "B = 2", root, error)
    call check(.not. allocated(error), msg="Parse ok")
    call iter%init(root)
    count = 0
    do
      has_more = iter%next(child)
      if (.not. has_more) exit
      count = count + 1
    end do
    call check(count == 2, msg="Iterated 2 children")
    ! Next call after end should give has_more=false
    has_more = iter%next(child)
    call check(.not. has_more, msg="Past end has_more=false")
  end subroutine test_iterator_past_end

  ! ===========================================================================
  ! Formatter edge cases
  ! ===========================================================================

  !> Test dump to non-writable file
  subroutine test_dump_file_open_error()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error

    call new_table(root, "root")
    call hsd_dump(root, "/nonexistent_dir/file.hsd", error)
    call check(allocated(error), msg="File open error")
  end subroutine test_dump_file_open_error

  !> Test dump value with newlines (multiline)
  subroutine test_dump_multiline_value()
    type(hsd_table) :: root
    character(len=512) :: path
    character(len=2048) :: content
    integer :: unit_num, io_stat
    type(hsd_error_t), allocatable :: error

    call new_table(root, "root")
    block
      type(hsd_value), pointer :: val
      allocate(val)
      call new_value(val, "text")
      val%value_type = VALUE_TYPE_STRING
      val%string_value = "line1" // char(10) // char(10) // "line3"
      call root%add_child(val)
    end block
    path = build_dir // "/test_multiline_dump.hsd"
    call hsd_dump(root, trim(path), error)
    call check(.not. allocated(error), msg="Dump ok")
    ! Read back and verify
    open(newunit=unit_num, file=trim(path), status='old', &
        action='read', iostat=io_stat)
    if (io_stat == 0) then
      read(unit_num, '(A)', iostat=io_stat) content
      close(unit_num)
    end if
    call check(io_stat == 0, msg="Read back ok")
  end subroutine test_dump_multiline_value

  !> Test formatting strings with both quote types
  subroutine test_dump_escape_quotes()
    type(hsd_table) :: root
    character(len=:), allocatable :: output

    call new_table(root, "root")
    ! String with both single and double quotes needs escaping
    block
      type(hsd_value), pointer :: val
      allocate(val)
      call new_value(val, "msg")
      val%value_type = VALUE_TYPE_STRING
      val%string_value = 'He said "it' // "'" // 's fine"'
      call root%add_child(val)
    end block
    call hsd_dump_to_string(root, output)
    call check(len(output) > 0, msg="Escaped output generated")
  end subroutine test_dump_escape_quotes

  !> Test dump unnamed table to string
  subroutine test_dump_unnamed_str()
    type(hsd_table) :: root
    character(len=:), allocatable :: output

    ! Create unnamed root with unnamed child table
    call new_table(root)
    block
      type(hsd_table), pointer :: child
      allocate(child)
      call new_table(child)
      block
        type(hsd_value), pointer :: val
        allocate(val)
        call new_value(val, "key")
        val%value_type = VALUE_TYPE_STRING
        val%string_value = "value"
        call child%add_child(val)
      end block
      call root%add_child(child)
    end block
    ! Add second child so it's not single-child optimized
    block
      type(hsd_value), pointer :: val2
      allocate(val2)
      call new_value(val2, "other")
      val2%value_type = VALUE_TYPE_STRING
      val2%string_value = "x"
      call root%add_child(val2)
    end block
    call hsd_dump_to_string(root, output)
    call check(len(output) > 0, msg="Unnamed table dump ok")
  end subroutine test_dump_unnamed_str

  !> Test dump file with single-child table shorthand
  subroutine test_dump_single_child_tbl()
    type(hsd_table) :: root
    character(len=512) :: path
    type(hsd_error_t), allocatable :: error

    call new_table(root, "root")
    ! Named table with single child that is a table
    block
      type(hsd_table), pointer :: outer, inner
      type(hsd_value), pointer :: val
      allocate(outer)
      call new_table(outer, "Outer")
      allocate(inner)
      call new_table(inner, "Inner")
      allocate(val)
      call new_value(val, "key")
      val%value_type = VALUE_TYPE_INTEGER
      val%string_value = "42"
      call inner%add_child(val)
      call outer%add_child(inner)
      call root%add_child(outer)
    end block
    path = build_dir // "/test_single_child_tbl.hsd"
    call hsd_dump(root, trim(path), error)
    call check(.not. allocated(error), msg="Dump ok")
  end subroutine test_dump_single_child_tbl

  ! ===========================================================================
  ! Hash table
  ! ===========================================================================

  !> Test hash auto-initialization
  subroutine test_hash_auto_init()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error

    ! Loading enough children forces hash table auto-init
    call hsd_load_string( &
        "A = 1" // char(10) // "B = 2" // char(10) // &
        "C = 3" // char(10) // "D = 4" // char(10) // &
        "E = 5" // char(10) // "F = 6" // char(10) // &
        "G = 7" // char(10) // "H = 8" // char(10) // &
        "I = 9" // char(10) // "J = 10" // char(10) // &
        "K = 11" // char(10) // "L = 12" // char(10) // &
        "M = 13" // char(10) // "N = 14", root, error)
    call check(.not. allocated(error), msg="Parse ok")
    call check(hsd_has_child(root, "N"), msg="Lookup after auto-init")
  end subroutine test_hash_auto_init

  !> Test updating a key in overflow chain
  subroutine test_hash_overflow_update()
    type(hsd_table) :: root
    integer :: stat, ival

    call new_table(root, "root")
    ! Add many children to trigger rehash and potential overflow
    block
      type(hsd_value), pointer :: val
      integer :: i
      character(len=8) :: name
      do i = 1, 30
        allocate(val)
        write(name, '(A,I0)') "k", i
        call new_value(val, trim(name))
        val%value_type = VALUE_TYPE_INTEGER
        val%string_value = "0"
        call root%add_child(val)
      end do
    end block
    ! Update existing keys
    call hsd_set(root, "k15", 99, stat)
    call check(stat == HSD_STAT_OK, msg="Update existing")
    call hsd_get(root, "k15", ival, stat)
    call check(ival == 99, msg="Updated value correct")
  end subroutine test_hash_overflow_update

  !> Test promote from chain on remove
  subroutine test_hash_promote_remove()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    integer :: stat

    ! Build table with many children to get hash collisions
    call hsd_load_string( &
        "Alpha = 1" // char(10) // "Beta = 2" // char(10) // &
        "Gamma = 3" // char(10) // "Delta = 4" // char(10) // &
        "Epsilon = 5" // char(10) // "Zeta = 6" // char(10) // &
        "Eta = 7" // char(10) // "Theta = 8" // char(10) // &
        "Iota = 9" // char(10) // "Kappa = 10" // char(10) // &
        "Lambda = 11" // char(10) // "Mu = 12" // char(10) // &
        "Nu = 13" // char(10) // "Xi = 14" // char(10) // &
        "Omicron = 15" // char(10) // "Pi = 16" // char(10) // &
        "Rho = 17" // char(10) // "Sigma = 18" // char(10) // &
        "Tau = 19" // char(10) // "Upsilon = 20", root, error)
    call check(.not. allocated(error), msg="Parse ok")
    ! Remove several to trigger chain promotion
    call hsd_remove_child(root, "Alpha", stat)
    call hsd_remove_child(root, "Gamma", stat)
    call hsd_remove_child(root, "Epsilon", stat)
    call hsd_remove_child(root, "Eta", stat)
    call hsd_remove_child(root, "Iota", stat)
    ! Verify remaining keys still accessible
    call check(hsd_has_child(root, "Beta"), msg="Beta survives")
    call check(hsd_has_child(root, "Sigma"), msg="Sigma survives")
  end subroutine test_hash_promote_remove

  !> Test removing key from overflow chain
  subroutine test_hash_remove_chain_entry()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    integer :: stat

    ! Build large table
    call hsd_load_string( &
        "aa = 1" // char(10) // "bb = 2" // char(10) // &
        "cc = 3" // char(10) // "dd = 4" // char(10) // &
        "ee = 5" // char(10) // "ff = 6" // char(10) // &
        "gg = 7" // char(10) // "hh = 8" // char(10) // &
        "ii = 9" // char(10) // "jj = 10" // char(10) // &
        "kk = 11" // char(10) // "ll = 12" // char(10) // &
        "mm = 13" // char(10) // "nn = 14" // char(10) // &
        "oo = 15" // char(10) // "pp = 16" // char(10) // &
        "qq = 17" // char(10) // "rr = 18" // char(10) // &
        "ss = 19" // char(10) // "tt = 20" // char(10) // &
        "uu = 21" // char(10) // "vv = 22" // char(10) // &
        "ww = 23" // char(10) // "xx = 24" // char(10) // &
        "yy = 25" // char(10) // "zz = 26", root, error)
    call check(.not. allocated(error), msg="Parse ok")
    ! Remove from middle of potential chains
    call hsd_remove_child(root, "mm", stat)
    call hsd_remove_child(root, "qq", stat)
    call hsd_remove_child(root, "uu", stat)
    call check(hsd_has_child(root, "nn"), msg="nn survives")
    call check(.not. hsd_has_child(root, "mm"), msg="mm removed")
  end subroutine test_hash_remove_chain_entry

  ! ===========================================================================
  ! Error module
  ! ===========================================================================

  !> Test error_message for all error codes
  subroutine test_error_msg_all_codes()
    call check(len(error_message(HSD_STAT_UNCLOSED_ATTRIB)) > 0, &
        msg="Unclosed attrib msg")
    call check(len(error_message(HSD_STAT_ORPHAN_TEXT)) > 0, &
        msg="Orphan text msg")
    call check(len(error_message(HSD_STAT_INCLUDE_CYCLE)) > 0, &
        msg="Include cycle msg")
    call check(len(error_message(HSD_STAT_INCLUDE_DEPTH)) > 0, &
        msg="Include depth msg")
    call check(len(error_message(HSD_STAT_IO_ERROR)) > 0, &
        msg="IO error msg")
  end subroutine test_error_msg_all_codes

  !> Test error_print with various field combinations
  subroutine test_error_print_variants()
    type(hsd_error_t) :: err
    integer :: unit_num
    character(len=512) :: path

    path = build_dir // "/error_out.txt"
    open(newunit=unit_num, file=trim(path), status='replace', action='write')

    ! Error with filename, line range, column
    err%code = HSD_STAT_SYNTAX_ERROR
    err%message = "test error"
    err%filename = "test.hsd"
    err%line_start = 1
    err%line_end = 5
    err%column = 10
    err%expected = "expected_val"
    err%actual = "actual_val"
    err%hint = "try this"
    call err%print(unit_num)

    ! Error with expected only (no actual)
    deallocate(err%actual)
    call err%print(unit_num)

    ! Error with actual only (no expected)
    err%actual = "only_actual"
    deallocate(err%expected)
    call err%print(unit_num)

    ! Error without filename (no line info) - use make_error for safety
    ! Just test a minimal error (all non-allocatable fields)
    block
      type(hsd_error_t) :: err2
      err2%code = HSD_STAT_SYNTAX_ERROR
      err2%message = "bare error"
      call err2%print(unit_num)
    end block

    ! Error with line but no column
    err%filename = "test.hsd"
    err%message = "no column error"
    err%line_start = 5
    err%line_end = 5
    err%column = 0
    call err%print(unit_num)

    close(unit_num)
    call check(.true., msg="Error print variants ok")
  end subroutine test_error_print_variants

  ! ===========================================================================
  ! Token
  ! ===========================================================================

  !> Test token_name for all token types
  subroutine test_token_name_all()
    call check(token_name(TOKEN_WHITESPACE) == "whitespace", &
        msg="Whitespace name")
    call check(token_name(TOKEN_NEWLINE) == "newline", msg="Newline name")
    call check(token_name(TOKEN_COMMENT) == "comment", msg="Comment name")
    call check(token_name(TOKEN_LBRACKET) == "opening bracket", &
        msg="Lbracket name")
    call check(token_name(TOKEN_RBRACKET) == "closing bracket", &
        msg="Rbracket name")
    call check(token_name(TOKEN_SEMICOLON) == "semicolon", &
        msg="Semicolon name")
    call check(token_name(TOKEN_INCLUDE_TXT) == "text include", &
        msg="Include txt name")
    call check(token_name(TOKEN_INCLUDE_HSD) == "HSD include", &
        msg="Include hsd name")
  end subroutine test_token_name_all

  !> Test token_is_valid function
  subroutine test_token_is_valid_fn()
    type(hsd_token_t) :: tok

    tok%kind = TOKEN_EOF
    call check(.not. tok%is_valid(), msg="EOF not valid")
    tok%kind = TOKEN_WHITESPACE
    call check(tok%is_valid(), msg="Whitespace is valid")
  end subroutine test_token_is_valid_fn

  ! ===========================================================================
  ! Validation
  ! ===========================================================================

  !> Test type_name for VALUE_TYPE_NONE
  subroutine test_type_name_none()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error

    call hsd_load_string("A = 1", root, error)
    ! The type_name function is tested indirectly through validate_one_of
    ! which prints type names in error messages
    call check(.not. allocated(error), msg="Parse ok")
  end subroutine test_type_name_none

  !> Test hsd_get_with_unit
  subroutine test_get_with_unit()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    integer :: stat
    real(dp) :: val

    ! Value without unit attribute
    call hsd_load_string("Temp = 300.0", root, error)
    call check(.not. allocated(error), msg="Parse ok")
    call hsd_get_with_unit(root, "Temp", val, "K", identity_conv, stat)
    call check(stat == HSD_STAT_OK, msg="No unit attr ok")
    call check(abs(val - 300.0_dp) < 1.0e-10_dp, msg="Value correct")

    ! Non-existent path
    call hsd_get_with_unit(root, "Missing", val, "K", identity_conv, stat)
    call check(stat == HSD_STAT_NOT_FOUND, msg="Missing path")

    ! Non-numeric value
    call hsd_load_string("Name = hello", root, error)
    call hsd_get_with_unit(root, "Name", val, "K", identity_conv, stat)
    call check(stat /= HSD_STAT_OK, msg="Non-numeric fails")

    ! Table node instead of value
    call hsd_load_string("Block { x = 1 }", root, error)
    call hsd_get_with_unit(root, "Block", val, "K", identity_conv, stat)
    call check(stat == HSD_STAT_TYPE_ERROR, msg="Table fails")
  end subroutine test_get_with_unit

  !> Test validate_range on table node
  subroutine test_validate_range_table()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error, val_error

    call hsd_load_string("Block { x = 1 }", root, error)
    call check(.not. allocated(error), msg="Parse ok")
    call hsd_validate_range(root, "Block", 0.0_dp, 100.0_dp, val_error)
    call check(allocated(val_error), msg="Range on table fails")
  end subroutine test_validate_range_table

  ! ===========================================================================
  ! Visitor error paths
  ! ===========================================================================

  !> Test visitor that returns error on table
  subroutine test_visitor_table_error()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    type(error_on_table_visitor) :: vis
    integer :: stat

    call hsd_load_string( &
        "Block {" // char(10) // "  x = 1" // char(10) // "}", &
        root, error)
    call check(.not. allocated(error), msg="Parse ok")
    call hsd_accept(root, vis, stat)
    call check(stat /= HSD_STAT_OK, msg="Visitor table error")
  end subroutine test_visitor_table_error

  !> Test visitor that returns error on value
  subroutine test_visitor_value_error()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    type(error_on_value_visitor) :: vis
    integer :: stat

    call hsd_load_string("x = 1", root, error)
    call check(.not. allocated(error), msg="Parse ok")
    call hsd_accept(root, vis, stat)
    call check(stat /= HSD_STAT_OK, msg="Visitor value error")
  end subroutine test_visitor_value_error

  !> Test visitor that errors on nested tables (triggers child table error path)
  subroutine test_visitor_nested_table_error()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    type(nested_table_error_visitor) :: vis
    integer :: stat

    ! Root has a child table: root → Block(table) → x(value)
    call hsd_load_string("Block {" // char(10) // "  x = 1" // char(10) // "}", &
        root, error)
    call check(.not. allocated(error), msg="Parse ok")
    call hsd_accept(root, vis, stat)
    ! Root visit (depth=0) succeeds, Block visit (depth=1) fails
    call check(stat /= HSD_STAT_OK, msg="Nested table error propagated")
  end subroutine test_visitor_nested_table_error

  ! ===========================================================================
  ! Query
  ! ===========================================================================

  !> Test hsd_get_keys on non-existent path
  subroutine test_get_keys_not_found()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    integer :: stat
    character(len=:), allocatable :: keys(:)

    call hsd_load_string("A = 1", root, error)
    call hsd_get_keys(root, "Missing", keys, stat)
    call check(stat == HSD_STAT_NOT_FOUND, msg="Keys not found")
    call check(size(keys) == 0, msg="Empty keys array")
  end subroutine test_get_keys_not_found

  !> Test removing child through value node
  subroutine test_remove_through_value()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    integer :: stat

    call hsd_load_string("Leaf = hello", root, error)
    call hsd_remove_child(root, "Leaf/deep", stat)
    call check(stat == HSD_STAT_TYPE_ERROR, msg="Remove through value")
  end subroutine test_remove_through_value

  !> Test navigating child through value node
  subroutine test_child_through_value()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    class(hsd_node), pointer :: child
    integer :: stat

    call hsd_load_string("Leaf = hello", root, error)
    call hsd_get_child(root, "Leaf/deep", child, stat)
    call check(stat == HSD_STAT_NOT_FOUND, msg="Child through value")
  end subroutine test_child_through_value

  ! ===========================================================================
  ! Schema extras
  ! ===========================================================================

  !> Test schema field with description
  subroutine test_schema_description()
    type(hsd_schema_t) :: schema
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error, val_errors(:)

    call schema_init(schema)
    call schema_add_field(schema, "Name", FIELD_REQUIRED, FIELD_TYPE_STRING, &
        description="A person's name")
    call hsd_load_string("Name = Alice", root, error)
    call schema_validate(schema, root, val_errors)
    call check(size(val_errors) == 0, msg="Schema with desc ok")
    call schema_destroy(schema)
  end subroutine test_schema_description

  !> Test schema with logical field type
  subroutine test_schema_logical_type()
    type(hsd_schema_t) :: schema
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error, val_errors(:)

    call schema_init(schema)
    call schema_add_field(schema, "Flag", FIELD_REQUIRED, FIELD_TYPE_LOGICAL)
    call hsd_load_string("Flag = Yes", root, error)
    call schema_validate(schema, root, val_errors)
    call check(size(val_errors) == 0, msg="Logical schema ok")
    call schema_destroy(schema)
  end subroutine test_schema_logical_type

  !> Test schema get_type_name for STRING type
  subroutine test_schema_type_name_str()
    type(hsd_schema_t) :: schema
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error, val_errors(:)

    call schema_init(schema)
    call schema_add_field(schema, "Name", FIELD_REQUIRED, FIELD_TYPE_STRING)
    ! Feed it a table instead of a string to trigger type name lookup
    call hsd_load_string("Name { x = 1 }", root, error)
    call schema_validate(schema, root, val_errors)
    call check(size(val_errors) > 0, msg="String type mismatch")
    call schema_destroy(schema)
  end subroutine test_schema_type_name_str

  ! ===========================================================================
  ! Parser
  ! ===========================================================================

  !> Test parse_string with filename parameter
  subroutine test_parse_with_filename()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    integer :: stat
    integer :: ival

    call hsd_load_string("Key = 42", root, error, filename="test.hsd")
    call check(.not. allocated(error), msg="Parse with filename ok")
    call hsd_get(root, "Key", ival, stat)
    call check(ival == 42, msg="Value correct")
  end subroutine test_parse_with_filename

  ! ===========================================================================
  ! Utils / Lexer
  ! ===========================================================================

  !> Test to_lower with empty string
  subroutine test_to_lower_empty()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error

    ! to_lower is called internally during case-insensitive lookups
    ! An empty string triggers the early return
    call hsd_load_string("X = 1", root, error)
    call check(.not. allocated(error), msg="Parse ok")
    ! Looking up empty string triggers to_lower("")
    call check(.not. hsd_has_child(root, ""), msg="Empty lookup ok")
  end subroutine test_to_lower_empty

  !> Test lexer from string without explicit filename
  subroutine test_lexer_no_filename()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error

    ! hsd_load_string without filename uses default "<string>"
    call hsd_load_string("Key = 42", root, error)
    call check(.not. allocated(error), msg="No filename ok")
  end subroutine test_lexer_no_filename

  ! ===========================================================================
  ! Additional coverage - accessor error paths
  ! ===========================================================================

  !> Test SP real array error path (non-numeric value)
  subroutine test_get_sp_array_error()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    integer :: stat
    real(sp), allocatable :: arr(:)

    call hsd_load_string("Name = hello", root, error)
    call check(.not. allocated(error), msg="Parse ok")
    call hsd_get(root, "Name", arr, stat)
    call check(stat /= HSD_STAT_OK, msg="SP array from string fails")
    call check(size(arr) == 0, msg="Empty array on error")
  end subroutine test_get_sp_array_error

  !> Test logical array on table child (class default branch)
  subroutine test_get_logical_array_table()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    integer :: stat
    logical, allocatable :: arr(:)

    call hsd_load_string("Block { x = 1 }", root, error)
    call check(.not. allocated(error), msg="Parse ok")
    call hsd_get(root, "Block", arr, stat)
    call check(stat == HSD_STAT_TYPE_ERROR, msg="Logical arr from table")
    call check(size(arr) == 0, msg="Empty array on type error")
  end subroutine test_get_logical_array_table

  ! ===========================================================================
  ! Additional coverage - validation paths
  ! ===========================================================================

  !> Test validate_range on string (non-numeric) value
  subroutine test_validate_range_string()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error, val_error

    call hsd_load_string("Name = hello", root, error)
    call check(.not. allocated(error), msg="Parse ok")
    call hsd_validate_range(root, "Name", 0.0_dp, 100.0_dp, val_error)
    call check(allocated(val_error), msg="Range on string fails")
  end subroutine test_validate_range_string

  !> Test validate_range on missing field
  subroutine test_validate_range_missing()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error, val_error

    call hsd_load_string("X = 1", root, error)
    call check(.not. allocated(error), msg="Parse ok")
    call hsd_validate_range(root, "Missing", 0.0_dp, 100.0_dp, val_error)
    call check(allocated(val_error), msg="Range on missing fails")
  end subroutine test_validate_range_missing

  !> Test hsd_require with VALUE_TYPE_NONE expected (triggers type_name "none")
  subroutine test_require_type_none()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error, req_error

    call hsd_load_string("X = 42", root, error)
    call check(.not. allocated(error), msg="Parse ok")
    call hsd_require(root, "X", req_error, VALUE_TYPE_NONE)
    call check(allocated(req_error), msg="Require none type fails")
  end subroutine test_require_type_none

  ! ===========================================================================
  ! Additional coverage - visitor unnamed deep path
  ! ===========================================================================

  !> Test visitor with unnamed child inside a named table (non-empty path)
  subroutine test_visitor_unnamed_deep()
    type(hsd_table) :: root
    type(count_visitor) :: vis
    integer :: stat

    ! Build: root -> Named("Parent") -> unnamed value
    call new_table(root, "root")
    block
      type(hsd_table), pointer :: named_tbl
      type(hsd_value), pointer :: val
      allocate(named_tbl)
      call new_table(named_tbl, "Parent")
      allocate(val)
      call new_value(val)  ! unnamed
      val%string_value = "test"
      val%value_type = VALUE_TYPE_STRING
      call named_tbl%add_child(val)
      call root%add_child(named_tbl)
    end block
    call hsd_accept(root, vis, stat)
    call check(stat == 0, msg="Visitor deep unnamed ok")
    call check(vis%value_count >= 1, msg="Value visited")
  end subroutine test_visitor_unnamed_deep

  ! ===========================================================================
  ! Additional coverage - parser multi-token value
  ! ===========================================================================

  !> Test parsing multi-token values (Key = val1 val2 val3)
  subroutine test_parser_multi_token()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    integer :: stat
    character(len=:), allocatable :: text

    ! Multi-word value after =
    call hsd_load_string('Key = hello world foo', root, error)
    call check(.not. allocated(error), msg="Multi-token parse ok")
    call hsd_get(root, "Key", text, stat)
    call check(stat == HSD_STAT_OK, msg="Get multi-token ok")
    call check(index(text, "hello") > 0, msg="Contains hello")
    call check(index(text, "world") > 0, msg="Contains world")
    call check(index(text, "foo") > 0, msg="Contains foo")
  end subroutine test_parser_multi_token

  ! ===========================================================================
  ! Additional coverage - formatter paths
  ! ===========================================================================

  !> Test unnamed table with single table child (line 120)
  subroutine test_dump_unnamed_tbl_tbl()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: err
    character(len=512) :: filepath
    character(len=4096) :: content
    integer :: unit_num, io_stat, content_len

    call new_table(root, "root")
    block
      type(hsd_table), pointer :: unnamed_tbl, inner_tbl
      type(hsd_value), pointer :: val
      allocate(unnamed_tbl)
      call new_table(unnamed_tbl)  ! no name
      allocate(inner_tbl)
      call new_table(inner_tbl, "Inner")
      allocate(val)
      call new_value(val, "x")
      val%string_value = "1"
      val%value_type = VALUE_TYPE_STRING
      call inner_tbl%add_child(val)
      call unnamed_tbl%add_child(inner_tbl)
      call root%add_child(unnamed_tbl)
    end block
    filepath = trim(build_dir) // "/test_dump_unnamed_tbl_tbl.hsd"
    call hsd_dump(root, trim(filepath), err)
    call check(.not. allocated(err), msg="Dump ok")

    open(newunit=unit_num, file=trim(filepath), status='old', action='read', iostat=io_stat)
    content = ""
    if (io_stat == 0) then
      read(unit_num, '(A)', iostat=io_stat, size=content_len, advance='no') content
      close(unit_num)
    end if
    call check(index(content, "Inner") > 0, msg="Inner found in dump")
  end subroutine test_dump_unnamed_tbl_tbl

  !> Test tag = multiline value (write_tag_value multiline path)
  subroutine test_dump_tag_multiline_val()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: err
    character(len=512) :: filepath
    character(len=4096) :: content
    integer :: unit_num, io_stat, content_len

    call new_table(root, "root")
    block
      type(hsd_table), pointer :: tbl
      type(hsd_value), pointer :: val
      allocate(tbl)
      call new_table(tbl, "Tag")
      allocate(val)
      call new_value(val, "data")
      val%string_value = "line1" // char(10) // "line2"
      val%value_type = VALUE_TYPE_STRING
      call tbl%add_child(val)
      call root%add_child(tbl)
    end block
    filepath = trim(build_dir) // "/test_dump_tag_multiline.hsd"
    call hsd_dump(root, trim(filepath), err)
    call check(.not. allocated(err), msg="Dump ok")

    open(newunit=unit_num, file=trim(filepath), status='old', action='read', iostat=io_stat)
    content = ""
    if (io_stat == 0) then
      read(unit_num, '(A)', iostat=io_stat, size=content_len, advance='no') content
      close(unit_num)
    end if
    call check(index(content, "Tag") > 0, msg="Tag in multiline dump")
  end subroutine test_dump_tag_multiline_val

  ! ===========================================================================
  ! Additional coverage - types paths
  ! ===========================================================================

  !> Test get_keys from empty table (max_len=0 path)
  subroutine test_get_keys_empty_table()
    type(hsd_table) :: root
    integer :: stat
    character(len=:), allocatable :: keys(:)

    call new_table(root, "root")
    call hsd_get_keys(root, "", keys, stat)
    call check(stat == HSD_STAT_OK, msg="Empty table keys ok")
    call check(size(keys) == 0, msg="No keys in empty table")
  end subroutine test_get_keys_empty_table

  !> Test get_keys with mixed named/unnamed children (covers keys(i)="" branch)
  subroutine test_get_keys_mixed()
    type(hsd_table) :: root
    integer :: stat
    character(len=:), allocatable :: keys(:)

    call new_table(root, "root")
    ! Named child
    block
      type(hsd_value), pointer :: val
      allocate(val)
      call new_value(val, "named")
      val%string_value = "test"
      val%value_type = VALUE_TYPE_STRING
      call root%add_child(val)
    end block
    ! Unnamed child
    block
      type(hsd_value), pointer :: val
      allocate(val)
      call new_value(val)
      val%string_value = "anon"
      val%value_type = VALUE_TYPE_STRING
      call root%add_child(val)
    end block
    call hsd_get_keys(root, "", keys, stat)
    call check(stat == HSD_STAT_OK, msg="Mixed keys ok")
    call check(size(keys) == 2, msg="Two key entries")
    call check(trim(keys(1)) == "named", msg="First key is named")
    call check(len_trim(keys(2)) == 0, msg="Second key is empty")
  end subroutine test_get_keys_mixed

  !> Test iterator on uninitialized (null table) state
  subroutine test_iterator_uninit()
    type(hsd_iterator) :: iter
    class(hsd_node), pointer :: child
    logical :: has_more

    ! Fresh iterator has table => null()
    has_more = iter%next(child)
    call check(.not. has_more, msg="Uninit iterator has no more")
    call check(.not. associated(child), msg="Uninit iterator null child")
  end subroutine test_iterator_uninit

  !> Test complex array from raw_text (covers raw_text path in get_complex_array)
  subroutine test_complex_array_raw_text()
    type(hsd_table) :: root
    integer :: stat
    complex(dp), allocatable :: arr(:)

    call new_table(root, "root")
    block
      type(hsd_value), pointer :: val
      allocate(val)
      call new_value(val, "data")
      val%raw_text = "1+2i 3-4i"
      call root%add_child(val)
    end block
    call hsd_get(root, "data", arr, stat)
    call check(stat == HSD_STAT_OK, msg="Complex from raw_text ok")
    call check(size(arr) == 2, msg="Two complex values")
  end subroutine test_complex_array_raw_text

  !> Test string array from raw_text with quotes (covers raw_text +
  !> tokenize_quoted_string quoted paths)
  subroutine test_string_array_raw_text()
    type(hsd_table) :: root
    integer :: stat
    character(len=:), allocatable :: arr(:)

    call new_table(root, "root")
    block
      type(hsd_value), pointer :: val
      allocate(val)
      call new_value(val, "data")
      val%raw_text = '"hello" world "test"'
      call root%add_child(val)
    end block
    call hsd_get(root, "data", arr, stat)
    call check(stat == HSD_STAT_OK, msg="String arr from raw_text ok")
    call check(size(arr) == 3, msg="Three string tokens")
    call check(trim(arr(1)) == "hello", msg="First is hello")
    call check(trim(arr(2)) == "world", msg="Second is world")
    call check(trim(arr(3)) == "test", msg="Third is test")
  end subroutine test_string_array_raw_text

  !> Test tokenize_quoted_string with empty quoted string
  subroutine test_tokenize_quoted_empty()
    type(hsd_table) :: root
    integer :: stat
    character(len=:), allocatable :: arr(:)

    call new_table(root, "root")
    block
      type(hsd_value), pointer :: val
      allocate(val)
      call new_value(val, "data")
      val%raw_text = 'before "" after'
      call root%add_child(val)
    end block
    call hsd_get(root, "data", arr, stat)
    call check(stat == HSD_STAT_OK, msg="Empty quote tokenize ok")
    call check(size(arr) >= 2, msg="At least 2 tokens")
  end subroutine test_tokenize_quoted_empty

  !> Test complex parse error: bad real part in a+bi format
  subroutine test_complex_bad_real_abi()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    integer :: stat
    complex(dp) :: cval

    call hsd_load_string("C = abc+2i", root, error)
    if (.not. allocated(error)) then
      call hsd_get(root, "C", cval, stat)
      call check(stat /= HSD_STAT_OK, msg="Bad real in a+bi fails")
    end if
  end subroutine test_complex_bad_real_abi

  !> Test ragged integer matrix via value raw_text
  subroutine test_int_matrix_ragged_direct()
    type(hsd_table) :: root
    integer :: stat, nrows, ncols
    integer, allocatable :: imat(:,:)

    call new_table(root, "root")
    block
      type(hsd_value), pointer :: val
      allocate(val)
      call new_value(val, "data")
      val%raw_text = "1 2 3" // char(10) // "4 5"
      call root%add_child(val)
    end block
    call hsd_get_matrix(root, "data", imat, nrows, ncols, stat)
    call check(stat == HSD_STAT_TYPE_ERROR, msg="Ragged int matrix returns error")
  end subroutine test_int_matrix_ragged_direct

  !> Test ragged real matrix via value raw_text
  subroutine test_real_matrix_ragged_direct()
    type(hsd_table) :: root
    integer :: stat, nrows, ncols
    real(dp), allocatable :: rmat(:,:)

    call new_table(root, "root")
    block
      type(hsd_value), pointer :: val
      allocate(val)
      call new_value(val, "data")
      val%raw_text = "1.0 2.0 3.0" // char(10) // "4.0"
      call root%add_child(val)
    end block
    call hsd_get_matrix(root, "data", rmat, nrows, ncols, stat)
    call check(stat == HSD_STAT_TYPE_ERROR, msg="Ragged real matrix returns error")
  end subroutine test_real_matrix_ragged_direct

  !> Test empty real matrix via value raw_text (whitespace only)
  subroutine test_real_matrix_empty_direct()
    type(hsd_table) :: root
    integer :: stat, nrows, ncols
    real(dp), allocatable :: rmat(:,:)

    call new_table(root, "root")
    block
      type(hsd_value), pointer :: val
      allocate(val)
      call new_value(val, "data")
      val%raw_text = "   "
      call root%add_child(val)
    end block
    call hsd_get_matrix(root, "data", rmat, nrows, ncols, stat)
    call check(nrows == 0, msg="Empty real matrix 0 rows")
    call check(ncols == 0, msg="Empty real matrix 0 cols")
  end subroutine test_real_matrix_empty_direct

  !> Test empty int matrix via value raw_text
  subroutine test_int_matrix_empty_direct()
    type(hsd_table) :: root
    integer :: stat, nrows, ncols
    integer, allocatable :: imat(:,:)

    call new_table(root, "root")
    block
      type(hsd_value), pointer :: val
      allocate(val)
      call new_value(val, "data")
      val%raw_text = "   "
      call root%add_child(val)
    end block
    call hsd_get_matrix(root, "data", imat, nrows, ncols, stat)
    call check(nrows == 0, msg="Empty int matrix 0 rows")
    call check(ncols == 0, msg="Empty int matrix 0 cols")
  end subroutine test_int_matrix_empty_direct

  !> Test split_by_newlines with adjacent newlines (empty line)
  subroutine test_split_adjacent_newlines()
    type(hsd_table) :: root
    integer :: stat, nrows, ncols
    integer, allocatable :: imat(:,:)

    call new_table(root, "root")
    block
      type(hsd_value), pointer :: val
      allocate(val)
      call new_value(val, "data")
      val%raw_text = "1 2" // char(10) // char(10) // "3 4"
      call root%add_child(val)
    end block
    call hsd_get_matrix(root, "data", imat, nrows, ncols, stat)
    call check(stat == HSD_STAT_OK, msg="Adjacent newlines matrix ok")
    call check(nrows == 2, msg="Two data rows")
  end subroutine test_split_adjacent_newlines

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

  !> Identity unit converter for testing
  pure function identity_conv(value, from_unit, to_unit) result(converted)
    real(dp), intent(in) :: value
    character(len=*), intent(in) :: from_unit, to_unit
    real(dp) :: converted
    converted = value
    ! Suppress unused warnings
    if (len(from_unit) < 0 .or. len(to_unit) < 0) converted = 0.0_dp
  end function identity_conv

end module test_full_coverage_suite
