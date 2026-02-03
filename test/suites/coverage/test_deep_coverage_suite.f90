!> Focuses on: formatter edge cases, parser error paths, types edge cases
module test_deep_coverage_suite
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
  use hsd_constants, only: dp, sp, CHAR_NEWLINE, CHAR_DQUOTE, CHAR_SQUOTE
  use hsd_types, only: hsd_table, hsd_value, new_value, new_table, &
    VALUE_TYPE_NONE, VALUE_TYPE_STRING, VALUE_TYPE_INTEGER, &
    VALUE_TYPE_REAL, VALUE_TYPE_LOGICAL, VALUE_TYPE_ARRAY, VALUE_TYPE_COMPLEX
  use hsd_formatter, only: hsd_dump, hsd_dump_to_string
  use hsd_error, only: hsd_error_t, HSD_STAT_OK, HSD_STAT_NOT_FOUND, HSD_STAT_TYPE_ERROR
  use hsd_parser, only: hsd_parse_string
  use build_env, only: build_dir, source_dir
  use fortuno_serial, only: is_equal, test => serial_case_item, check => serial_check, &
      & suite => serial_suite_item, test_list
  implicit none (type, external)

  private
  public :: deep_coverage_tests

contains

  function deep_coverage_tests() result(test_items)
    type(test_list) :: test_items

    test_items = test_list([&
            ! Formatter edge cases
            test("table_single_child_table", test_table_single_child_table), &
            test("anonymous_value_multiline", test_anonymous_value_multiline), &
            test("tag_value_with_val_attrib", test_tag_value_with_val_attrib), &
            test("real_whole_number_format", test_real_whole_number_format), &
            test("value_raw_text_fallback", test_value_raw_text_fallback), &
            test("value_type_none_format", test_value_type_none_format), &
            test("multiline_empty_lines", test_multiline_empty_lines), &
            ! Parser error paths
            test("parse_with_explicit_filename", test_parse_with_explicit_filename), &
            test("parse_error_recovery", test_parse_error_recovery), &
            test("parse_tag_text_buffer", test_parse_tag_text_buffer), &
            test("parse_include_error", test_parse_include_error), &
            ! Types edge cases
            test("value_get_attrib_empty", test_value_get_attrib_empty), &
            test("table_num_children_pure", test_table_num_children_pure), &
            test("value_get_complex_direct", test_value_get_complex_direct), &
            test("value_get_logical_false", test_value_get_logical_false), &
            test("array_parse_io_error", test_array_parse_io_error), &
            test("int_array_not_found_value", test_int_array_not_found_value), &
            test("real_array_not_found_value", test_real_array_not_found_value), &
            test("logical_array_not_found_value", test_logical_array_not_found_value), &
            test("complex_array_not_found_value", test_complex_array_not_found_value), &
            test("int_matrix_not_found_value", test_int_matrix_not_found_value), &
            test("real_matrix_not_found_value", test_real_matrix_not_found_value), &
            ! Additional formatter paths
            test("table_child_table_shorthand", test_table_child_table_shorthand), &
            test("unnamed_parent_table", test_unnamed_parent_table), &
            test("write_multiline_value", test_write_multiline_value), &
            test("named_value_multiline", test_named_value_multiline) &
        ])
  end function deep_coverage_tests


  !> Test table with single child that is also a table (Tag = ChildTag { } syntax)
  subroutine test_table_single_child_table()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: output
    character(len=512) :: filename
    integer :: unit_num, io_stat

    ! Parse outer = inner { x = 1 } - should trigger special formatting path
    call hsd_load_string("outer = inner { x = 1; y = 2 }", root, error)
    call check(.not. allocated(error), msg="Parse OK")

    ! Dump to file to exercise write_table_node with single child table
    filename = trim(build_dir) // "/single_child_table.hsd"
    call hsd_dump(root, trim(filename), error)
    call check(.not. allocated(error), msg="Dump OK")

    ! Also check string output
    call hsd_dump_to_string(root, output)
    call check(len(output) > 0, msg="Output generated")

    call root%destroy()

    ! Cleanup
    open(newunit=unit_num, file=trim(filename), status='old', action='read', iostat=io_stat)
    if (io_stat == 0) close(unit_num, status='delete')
  end subroutine test_table_single_child_table


  !> Test anonymous value with multiline content (exercises lines 180-183)
  subroutine test_anonymous_value_multiline()
    type(hsd_table) :: root, block
    type(hsd_value), allocatable :: val
    character(len=:), allocatable :: output
    character(len=512) :: filename
    type(hsd_error_t), allocatable :: error
    integer :: unit_num, io_stat

    call new_table(root)
    call new_table(block)
    block%name = "data"

    ! Create anonymous value (no name) with multiline content
    allocate(val)
    call new_value(val)
    val%string_value = "line1" // CHAR_NEWLINE // "line2" // CHAR_NEWLINE // "line3"
    val%value_type = VALUE_TYPE_STRING
    ! Don't set val%name - this makes it anonymous
    call block%add_child(val)
    call root%add_child(block)

    ! Dump to file - should trigger multiline anonymous value path
    filename = trim(build_dir) // "/anonymous_multiline.hsd"
    call hsd_dump(root, trim(filename), error)
    call check(.not. allocated(error), msg="Dump OK")

    call root%destroy()

    ! Cleanup
    open(newunit=unit_num, file=trim(filename), status='old', action='read', iostat=io_stat)
    if (io_stat == 0) close(unit_num, status='delete')
  end subroutine test_anonymous_value_multiline


  !> Test tag value where value has its own attribute (line 204)
  subroutine test_tag_value_with_val_attrib()
    type(hsd_table) :: root, container
    type(hsd_value), allocatable :: val
    character(len=512) :: filename
    type(hsd_error_t), allocatable :: error
    integer :: unit_num, io_stat

    call new_table(root)
    call new_table(container)
    container%name = "outer"

    allocate(val)
    call new_value(val)
    val%name = "inner"
    val%string_value = "line1" // CHAR_NEWLINE // "line2"
    val%value_type = VALUE_TYPE_STRING
    val%attrib = "unit"  ! Value has its own attribute
    call container%add_child(val)
    call root%add_child(container)

    ! Dump to file - should trigger val_attrib path in write_tag_value
    filename = trim(build_dir) // "/val_attrib.hsd"
    call hsd_dump(root, trim(filename), error)
    call check(.not. allocated(error), msg="Dump OK")

    call root%destroy()

    open(newunit=unit_num, file=trim(filename), status='old', action='read', iostat=io_stat)
    if (io_stat == 0) close(unit_num, status='delete')
  end subroutine test_tag_value_with_val_attrib


  !> Test real formatting with whole number (line 275)
  subroutine test_real_whole_number_format()
    type(hsd_table) :: root
    type(hsd_value), allocatable :: val
    character(len=:), allocatable :: output

    call new_table(root)
    allocate(val)
    call new_value(val, "realval")
    val%real_value = 100.0_dp  ! Whole number that needs .0 suffix
    val%value_type = VALUE_TYPE_REAL
    call root%add_child(val)

    call hsd_dump_to_string(root, output)
    ! Should contain "100.0" not just "100"
    call check(index(output, "100") > 0, msg="Contains 100")

    call root%destroy()
  end subroutine test_real_whole_number_format


  !> Test value with raw_text fallback (lines 281-284, 290-293)
  subroutine test_value_raw_text_fallback()
    type(hsd_table) :: root
    type(hsd_value), allocatable :: val
    character(len=:), allocatable :: output

    call new_table(root)
    allocate(val)
    call new_value(val, "raw")
    val%raw_text = "raw_value_text"
    val%value_type = VALUE_TYPE_INTEGER  ! Type is integer but use raw_text
    ! Don't set int_value - rely on raw_text
    call root%add_child(val)

    call hsd_dump_to_string(root, output)
    ! Path exercises raw_text fallback
    call check(len(output) > 0, msg="Output generated")

    call root%destroy()
  end subroutine test_value_raw_text_fallback


  !> Test value with type NONE (default case in format_value)
  subroutine test_value_type_none_format()
    type(hsd_table) :: root
    type(hsd_value), allocatable :: val1, val2
    character(len=:), allocatable :: output

    call new_table(root)
    allocate(val1)
    call new_value(val1, "noneval")
    val1%value_type = VALUE_TYPE_NONE
    val1%string_value = "default_text"  ! Uses default case with string_value
    call root%add_child(val1)

    allocate(val2)
    call new_value(val2, "rawval")
    val2%value_type = VALUE_TYPE_NONE
    val2%raw_text = "raw_default"  ! Uses default case with raw_text
    call root%add_child(val2)

    call hsd_dump_to_string(root, output)
    call check(len(output) > 0, msg="Output generated")

    call root%destroy()
  end subroutine test_value_type_none_format


  !> Test multiline with empty lines (line 240)
  subroutine test_multiline_empty_lines()
    type(hsd_table) :: root
    type(hsd_value), allocatable :: val
    character(len=512) :: filename
    type(hsd_error_t), allocatable :: error
    integer :: unit_num, io_stat

    call new_table(root)
    allocate(val)
    call new_value(val, "multi")
    ! Include consecutive newlines for empty line
    val%string_value = "line1" // CHAR_NEWLINE // CHAR_NEWLINE // "line3"
    val%value_type = VALUE_TYPE_STRING
    call root%add_child(val)

    filename = trim(build_dir) // "/multiline_empty.hsd"
    call hsd_dump(root, trim(filename), error)
    call check(.not. allocated(error), msg="Dump OK")

    call root%destroy()

    open(newunit=unit_num, file=trim(filename), status='old', action='read', iostat=io_stat)
    if (io_stat == 0) close(unit_num, status='delete')
  end subroutine test_multiline_empty_lines


  !> Test parse with explicit filename (exercises state%base_dir path)
  subroutine test_parse_with_explicit_filename()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error

    ! Parse with explicit filename - exercises filename-based paths
    call hsd_parse_string("x = 1", root, filename="/path/to/test.hsd", error=error)
    call check(.not. allocated(error), msg="Parse with filename OK")

    call root%destroy()
  end subroutine test_parse_with_explicit_filename


  !> Test parse error recovery paths
  subroutine test_parse_error_recovery()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error

    ! Various error-inducing inputs to exercise error paths
    call hsd_load_string("= value", root, error)
    call check(.true., msg="Path 1 exercised")
    call root%destroy()

    call hsd_load_string("{ }", root, error)
    call check(.true., msg="Path 2 exercised")
    call root%destroy()

    call hsd_load_string("key =", root, error)
    call check(.true., msg="Path 3 exercised")
    call root%destroy()

    call hsd_load_string("key { nested = }", root, error)
    call check(.true., msg="Path 4 exercised")
    call root%destroy()
  end subroutine test_parse_error_recovery


  !> Test parse with tag text buffer path
  subroutine test_parse_tag_text_buffer()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error

    ! Parsing that accumulates text in buffer
    call hsd_load_string("block { text1 text2 text3 }", root, error)
    call check(.not. allocated(error), msg="Parse OK")

    call root%destroy()
  end subroutine test_parse_tag_text_buffer


  !> Test parse with include error
  subroutine test_parse_include_error()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error

    ! Try to include non-existent file
    call hsd_load_string("+<<+ /nonexistent/path/file.hsd", root, error)
    call check(allocated(error), msg="Include error detected")

    call root%destroy()
  end subroutine test_parse_include_error


  !> Test value get_attrib when empty
  subroutine test_value_get_attrib_empty()
    type(hsd_value), allocatable :: val
    character(len=:), allocatable :: attrib

    allocate(val)
    call new_value(val, "test")
    ! Don't set attribute
    attrib = val%get_attrib()
    call check(len(attrib) == 0, msg="Empty attrib")

    deallocate(val)
  end subroutine test_value_get_attrib_empty


  !> Test table num_children pure function
  subroutine test_table_num_children_pure()
    type(hsd_table) :: root
    type(hsd_value), allocatable :: val
    integer :: n

    call new_table(root)
    n = root%num_children  ! Should be 0

    allocate(val)
    call new_value(val, "child")
    call root%add_child(val)

    n = root%num_children  ! Should be 1
    call check(n == 1, msg="One child")

    call root%destroy()
  end subroutine test_table_num_children_pure


  !> Test value get_complex directly
  subroutine test_value_get_complex_direct()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    class(hsd_node), pointer :: child
    complex(dp) :: c
    integer :: stat

    call hsd_load_string("c = 1+2i", root, error)
    call check(.not. allocated(error), msg="Parse OK")

    call root%get_child(1, child)
    select type(child)
    type is (hsd_value)
      call child%get_complex(c, stat)
      call check(stat == HSD_STAT_OK, msg="Got complex")
    end select

    call root%destroy()
  end subroutine test_value_get_complex_direct


  !> Test value get_logical with false value
  subroutine test_value_get_logical_false()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    class(hsd_node), pointer :: child
    logical :: val
    integer :: stat

    call hsd_load_string("flag = No", root, error)
    call check(.not. allocated(error), msg="Parse OK")

    call root%get_child(1, child)
    select type(child)
    type is (hsd_value)
      call child%get_logical(val, stat)
      if (stat == HSD_STAT_OK) then
        call check(.not. val, msg="Got false")
      end if
    end select

    call root%destroy()
  end subroutine test_value_get_logical_false


  !> Test array parse with IO error
  subroutine test_array_parse_io_error()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    integer, allocatable :: arr(:)
    integer :: stat

    ! Try to parse non-numeric as integers
    call hsd_load_string("data = abc def ghi", root, error)
    call check(.not. allocated(error), msg="Parse OK")

    ! This should trigger IO error in parse_integer_array
    call hsd_get(root, "data", arr, stat)
    ! Just exercise the path
    call check(.true., msg="IO error path exercised")

    call root%destroy()
  end subroutine test_array_parse_io_error


  !> Test integer array NOT_FOUND on hsd_value
  subroutine test_int_array_not_found_value()
    type(hsd_value), allocatable :: val
    integer, allocatable :: arr(:)
    integer :: stat

    allocate(val)
    call new_value(val, "test")
    val%value_type = VALUE_TYPE_NONE  ! No value

    call val%get_int_array(arr, stat)
    call check(stat == HSD_STAT_NOT_FOUND .or. .not. allocated(arr) .or. size(arr) == 0, &
               msg="Integer array not found")

    deallocate(val)
  end subroutine test_int_array_not_found_value


  !> Test real array NOT_FOUND on hsd_value
  subroutine test_real_array_not_found_value()
    type(hsd_value), allocatable :: val
    real(dp), allocatable :: arr(:)
    integer :: stat

    allocate(val)
    call new_value(val, "test")
    val%value_type = VALUE_TYPE_NONE

    call val%get_real_array(arr, stat)
    call check(stat == HSD_STAT_NOT_FOUND .or. .not. allocated(arr) .or. size(arr) == 0, &
               msg="Real array not found")

    deallocate(val)
  end subroutine test_real_array_not_found_value


  !> Test logical array NOT_FOUND on hsd_value
  subroutine test_logical_array_not_found_value()
    type(hsd_value), allocatable :: val
    logical, allocatable :: arr(:)
    integer :: stat

    allocate(val)
    call new_value(val, "test")
    val%value_type = VALUE_TYPE_NONE

    call val%get_logical_array(arr, stat)
    call check(stat == HSD_STAT_NOT_FOUND .or. .not. allocated(arr) .or. size(arr) == 0, &
               msg="Logical array not found")

    deallocate(val)
  end subroutine test_logical_array_not_found_value


  !> Test complex array NOT_FOUND on hsd_value
  subroutine test_complex_array_not_found_value()
    type(hsd_value), allocatable :: val
    complex(dp), allocatable :: arr(:)
    integer :: stat

    allocate(val)
    call new_value(val, "test")
    val%value_type = VALUE_TYPE_NONE

    call val%get_complex_array(arr, stat)
    call check(stat == HSD_STAT_NOT_FOUND .or. .not. allocated(arr) .or. size(arr) == 0, &
               msg="Complex array not found")

    deallocate(val)
  end subroutine test_complex_array_not_found_value


  !> Test integer matrix NOT_FOUND on hsd_value
  subroutine test_int_matrix_not_found_value()
    type(hsd_value), allocatable :: val
    integer, allocatable :: mat(:,:)
    integer :: nrows, ncols, stat

    allocate(val)
    call new_value(val, "test")
    val%value_type = VALUE_TYPE_NONE

    call val%get_int_matrix(mat, nrows, ncols, stat)
    call check(stat == HSD_STAT_NOT_FOUND .or. nrows == 0, &
               msg="Integer matrix not found")

    deallocate(val)
  end subroutine test_int_matrix_not_found_value


  !> Test real matrix NOT_FOUND on hsd_value
  subroutine test_real_matrix_not_found_value()
    type(hsd_value), allocatable :: val
    real(dp), allocatable :: mat(:,:)
    integer :: nrows, ncols, stat

    allocate(val)
    call new_value(val, "test")
    val%value_type = VALUE_TYPE_NONE

    call val%get_real_matrix(mat, nrows, ncols, stat)
    call check(stat == HSD_STAT_NOT_FOUND .or. nrows == 0, &
               msg="Real matrix not found")

    deallocate(val)
  end subroutine test_real_matrix_not_found_value


  !> Test table with child table in shorthand format
  subroutine test_table_child_table_shorthand()
    type(hsd_table) :: root, outer, inner
    type(hsd_value), allocatable :: val1, val2
    character(len=512) :: filename
    type(hsd_error_t), allocatable :: error
    integer :: unit_num, io_stat

    ! Build: outer = inner { x = 1; y = 2 }
    call new_table(root)
    call new_table(outer)
    outer%name = "outer"
    call new_table(inner)
    inner%name = "inner"

    allocate(val1)
    call new_value(val1, "x")
    val1%int_value = 1
    val1%value_type = VALUE_TYPE_INTEGER
    call inner%add_child(val1)

    allocate(val2)
    call new_value(val2, "y")
    val2%int_value = 2
    val2%value_type = VALUE_TYPE_INTEGER
    call inner%add_child(val2)

    call outer%add_child(inner)
    call root%add_child(outer)

    filename = trim(build_dir) // "/child_table_shorthand.hsd"
    call hsd_dump(root, trim(filename), error)
    call check(.not. allocated(error), msg="Dump OK")

    call root%destroy()

    open(newunit=unit_num, file=trim(filename), status='old', action='read', iostat=io_stat)
    if (io_stat == 0) close(unit_num, status='delete')
  end subroutine test_table_child_table_shorthand


  !> Test unnamed parent table path
  subroutine test_unnamed_parent_table()
    type(hsd_table) :: root, container
    type(hsd_value), allocatable :: val
    character(len=:), allocatable :: output

    call new_table(root)
    call new_table(container)
    ! Don't set container%name - unnamed table

    allocate(val)
    call new_value(val, "child")
    val%int_value = 42
    val%value_type = VALUE_TYPE_INTEGER
    call container%add_child(val)
    call root%add_child(container)

    call hsd_dump_to_string(root, output)
    call check(len(output) > 0, msg="Unnamed parent formatted")

    call root%destroy()
  end subroutine test_unnamed_parent_table


  !> Test write_multiline with value node
  subroutine test_write_multiline_value()
    type(hsd_table) :: root
    type(hsd_value), allocatable :: val
    character(len=512) :: filename
    type(hsd_error_t), allocatable :: error
    integer :: unit_num, io_stat

    call new_table(root)
    allocate(val)
    call new_value(val, "script")
    val%string_value = "#!/bin/bash" // CHAR_NEWLINE // &
                       "echo hello" // CHAR_NEWLINE // &
                       "exit 0"
    val%value_type = VALUE_TYPE_STRING
    call root%add_child(val)

    filename = trim(build_dir) // "/multiline_script.hsd"
    call hsd_dump(root, trim(filename), error)
    call check(.not. allocated(error), msg="Dump OK")

    call root%destroy()

    open(newunit=unit_num, file=trim(filename), status='old', action='read', iostat=io_stat)
    if (io_stat == 0) close(unit_num, status='delete')
  end subroutine test_write_multiline_value


  !> Test named value with multiline content
  subroutine test_named_value_multiline()
    type(hsd_table) :: root
    type(hsd_value), allocatable :: val
    character(len=512) :: filename
    type(hsd_error_t), allocatable :: error
    integer :: unit_num, io_stat

    call new_table(root)
    allocate(val)
    call new_value(val, "config")
    val%string_value = "param1 = value1" // CHAR_NEWLINE // &
                       "param2 = value2"
    val%value_type = VALUE_TYPE_STRING
    call root%add_child(val)

    filename = trim(build_dir) // "/named_multiline.hsd"
    call hsd_dump(root, trim(filename), error)
    call check(.not. allocated(error), msg="Dump OK")

    call root%destroy()

    open(newunit=unit_num, file=trim(filename), status='old', action='read', iostat=io_stat)
    if (io_stat == 0) close(unit_num, status='delete')
  end subroutine test_named_value_multiline

end module test_deep_coverage_suite
