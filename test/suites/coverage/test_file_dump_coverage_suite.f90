!> File-based formatter coverage tests targeting dump-to-file paths
module test_file_dump_coverage_suite
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
  use hsd_constants, only: dp, CHAR_NEWLINE
  use hsd_types, only: hsd_table, hsd_value, new_value, new_table, &
    VALUE_TYPE_STRING, VALUE_TYPE_INTEGER, VALUE_TYPE_REAL
  use hsd_formatter, only: hsd_dump
  use hsd_error, only: hsd_error_t
  use build_env, only: build_dir
  use fortuno_serial, only: test => serial_case_item, check => serial_check, &
      & suite => serial_suite_item, test_list
  implicit none (type, external)

  private
  public :: file_dump_coverage_tests

contains

  function file_dump_coverage_tests() result(test_items)
    type(test_list) :: test_items

    test_items = test_list([&
            test("file_dump_named_single_table", test_file_dump_named_single_table), &
            test("file_dump_unnamed_single_table", test_file_dump_unnamed_single_table), &
            test("file_dump_single_value_child", test_file_dump_single_value_child), &
            test("file_dump_anonymous_multiline", test_file_dump_anonymous_multiline), &
            test("file_dump_anonymous_singleline", test_file_dump_anonymous_singleline), &
            test("file_dump_val_attrib_multiline", test_file_dump_val_attrib_multiline), &
            test("file_dump_real_needs_decimal", test_file_dump_real_needs_decimal), &
            test("file_dump_value_raw_text", test_file_dump_value_raw_text), &
            test("file_dump_value_empty_fallback", test_file_dump_value_empty_fallback), &
            test("file_dump_multiline_empty_line", test_file_dump_multiline_empty_line) &
        ])
  end function file_dump_coverage_tests


  !> Test named table with single table child (lines 111-115)
  subroutine test_file_dump_named_single_table()
    type(hsd_table) :: root, outer, inner
    type(hsd_value), allocatable :: val
    type(hsd_error_t), allocatable :: error
    character(len=512) :: filename
    integer :: unit_num, io_stat

    call new_table(root)
    call new_table(outer)
    outer%name = "config"
    call new_table(inner)
    inner%name = "settings"

    allocate(val)
    call new_value(val, "option")
    val%int_value = 42
    val%value_type = VALUE_TYPE_INTEGER
    call inner%add_child(val)
    call outer%add_child(inner)
    call root%add_child(outer)

    filename = trim(build_dir) // "/dump_named_single_table.hsd"
    call hsd_dump(root, trim(filename), error)
    call check(.not. allocated(error), msg="Dump named single table to file")
    call root%destroy()

    open(newunit=unit_num, file=trim(filename), status='old', action='read', iostat=io_stat)
    if (io_stat == 0) close(unit_num, status='delete')
  end subroutine test_file_dump_named_single_table


  !> Test unnamed table with single table child (line 118)
  subroutine test_file_dump_unnamed_single_table()
    type(hsd_table) :: root, outer, inner
    type(hsd_value), allocatable :: val
    type(hsd_error_t), allocatable :: error
    character(len=512) :: filename
    integer :: unit_num, io_stat

    call new_table(root)
    call new_table(outer)
    ! No name for outer
    call new_table(inner)
    inner%name = "inner_block"

    allocate(val)
    call new_value(val, "x")
    val%int_value = 1
    val%value_type = VALUE_TYPE_INTEGER
    call inner%add_child(val)
    call outer%add_child(inner)
    call root%add_child(outer)

    filename = trim(build_dir) // "/dump_unnamed_single_table.hsd"
    call hsd_dump(root, trim(filename), error)
    call check(.not. allocated(error), msg="Dump unnamed single table to file")
    call root%destroy()

    open(newunit=unit_num, file=trim(filename), status='old', action='read', iostat=io_stat)
    if (io_stat == 0) close(unit_num, status='delete')
  end subroutine test_file_dump_unnamed_single_table


  !> Test table with single value child (line 128)
  subroutine test_file_dump_single_value_child()
    type(hsd_table) :: root, block
    type(hsd_value), allocatable :: val
    type(hsd_error_t), allocatable :: error
    character(len=512) :: filename
    integer :: unit_num, io_stat

    call new_table(root)
    call new_table(block)
    block%name = "container"

    allocate(val)
    call new_value(val, "only_child")
    val%string_value = "value"
    val%value_type = VALUE_TYPE_STRING
    call block%add_child(val)
    call root%add_child(block)

    filename = trim(build_dir) // "/dump_single_value_child.hsd"
    call hsd_dump(root, trim(filename), error)
    call check(.not. allocated(error), msg="Dump single value child to file")
    call root%destroy()

    open(newunit=unit_num, file=trim(filename), status='old', action='read', iostat=io_stat)
    if (io_stat == 0) close(unit_num, status='delete')
  end subroutine test_file_dump_single_value_child


  !> Test anonymous value with multiline (lines 180-181)
  subroutine test_file_dump_anonymous_multiline()
    type(hsd_table) :: root, block
    type(hsd_value), allocatable :: val
    type(hsd_error_t), allocatable :: error
    character(len=512) :: filename
    integer :: unit_num, io_stat

    call new_table(root)
    call new_table(block)
    block%name = "data"

    allocate(val)
    call new_value(val)
    val%string_value = "line1" // CHAR_NEWLINE // "line2"
    val%value_type = VALUE_TYPE_STRING
    ! No name - anonymous
    call block%add_child(val)
    call root%add_child(block)

    filename = trim(build_dir) // "/dump_anon_multiline.hsd"
    call hsd_dump(root, trim(filename), error)
    call check(.not. allocated(error), msg="Dump anonymous multiline to file")
    call root%destroy()

    open(newunit=unit_num, file=trim(filename), status='old', action='read', iostat=io_stat)
    if (io_stat == 0) close(unit_num, status='delete')
  end subroutine test_file_dump_anonymous_multiline


  !> Test anonymous value single line (line 183)
  subroutine test_file_dump_anonymous_singleline()
    type(hsd_table) :: root, block
    type(hsd_value), allocatable :: val
    type(hsd_error_t), allocatable :: error
    character(len=512) :: filename
    integer :: unit_num, io_stat

    call new_table(root)
    call new_table(block)
    block%name = "item"

    allocate(val)
    call new_value(val)
    val%string_value = "single_line_value"
    val%value_type = VALUE_TYPE_STRING
    call block%add_child(val)
    call root%add_child(block)

    filename = trim(build_dir) // "/dump_anon_singleline.hsd"
    call hsd_dump(root, trim(filename), error)
    call check(.not. allocated(error), msg="Dump anonymous single line to file")
    call root%destroy()

    open(newunit=unit_num, file=trim(filename), status='old', action='read', iostat=io_stat)
    if (io_stat == 0) close(unit_num, status='delete')
  end subroutine test_file_dump_anonymous_singleline


  !> Test value with attribute and multiline (lines 204, 211-213)
  subroutine test_file_dump_val_attrib_multiline()
    type(hsd_table) :: root
    type(hsd_value), allocatable :: val
    type(hsd_error_t), allocatable :: error
    character(len=512) :: filename
    integer :: unit_num, io_stat

    call new_table(root)
    allocate(val)
    call new_value(val, "param")
    val%string_value = "value1" // CHAR_NEWLINE // "value2" // CHAR_NEWLINE // "value3"
    val%value_type = VALUE_TYPE_STRING
    val%attrib = "unit=eV"
    call root%add_child(val)

    filename = trim(build_dir) // "/dump_val_attrib_multiline.hsd"
    call hsd_dump(root, trim(filename), error)
    call check(.not. allocated(error), msg="Dump value with attrib multiline to file")
    call root%destroy()

    open(newunit=unit_num, file=trim(filename), status='old', action='read', iostat=io_stat)
    if (io_stat == 0) close(unit_num, status='delete')
  end subroutine test_file_dump_val_attrib_multiline


  !> Test real number formatting with .0 suffix (line 275)
  subroutine test_file_dump_real_needs_decimal()
    type(hsd_table) :: root
    type(hsd_value), allocatable :: val
    type(hsd_error_t), allocatable :: error
    character(len=512) :: filename
    integer :: unit_num, io_stat

    call new_table(root)
    allocate(val)
    call new_value(val, "number")
    val%real_value = 42.0_dp
    val%value_type = VALUE_TYPE_REAL
    call root%add_child(val)

    filename = trim(build_dir) // "/dump_real_decimal.hsd"
    call hsd_dump(root, trim(filename), error)
    call check(.not. allocated(error), msg="Dump real with decimal to file")
    call root%destroy()

    open(newunit=unit_num, file=trim(filename), status='old', action='read', iostat=io_stat)
    if (io_stat == 0) close(unit_num, status='delete')
  end subroutine test_file_dump_real_needs_decimal


  !> Test value with raw_text fallback (lines 281-282, 289)
  subroutine test_file_dump_value_raw_text()
    type(hsd_table) :: root
    type(hsd_value), allocatable :: val1, val2
    type(hsd_error_t), allocatable :: error
    character(len=512) :: filename
    integer :: unit_num, io_stat

    call new_table(root)

    ! Integer value with raw_text
    allocate(val1)
    call new_value(val1, "int_raw")
    val1%value_type = VALUE_TYPE_INTEGER
    val1%raw_text = "raw_int_text"
    call root%add_child(val1)

    ! String value without string_value, using raw_text
    allocate(val2)
    call new_value(val2, "str_raw")
    val2%value_type = VALUE_TYPE_STRING
    val2%raw_text = "raw_string"
    call root%add_child(val2)

    filename = trim(build_dir) // "/dump_raw_text.hsd"
    call hsd_dump(root, trim(filename), error)
    call check(.not. allocated(error), msg="Dump values with raw_text to file")
    call root%destroy()

    open(newunit=unit_num, file=trim(filename), status='old', action='read', iostat=io_stat)
    if (io_stat == 0) close(unit_num, status='delete')
  end subroutine test_file_dump_value_raw_text


  !> Test value with empty string fallback (line 284)
  subroutine test_file_dump_value_empty_fallback()
    type(hsd_table) :: root
    type(hsd_value), allocatable :: val
    type(hsd_error_t), allocatable :: error
    character(len=512) :: filename
    integer :: unit_num, io_stat

    call new_table(root)
    allocate(val)
    call new_value(val, "empty")
    val%value_type = VALUE_TYPE_INTEGER
    ! No int_value, no string_value, no raw_text
    call root%add_child(val)

    filename = trim(build_dir) // "/dump_empty_fallback.hsd"
    call hsd_dump(root, trim(filename), error)
    call check(.not. allocated(error), msg="Dump value with empty fallback to file")
    call root%destroy()

    open(newunit=unit_num, file=trim(filename), status='old', action='read', iostat=io_stat)
    if (io_stat == 0) close(unit_num, status='delete')
  end subroutine test_file_dump_value_empty_fallback


  !> Test multiline with empty lines (line 240)
  subroutine test_file_dump_multiline_empty_line()
    type(hsd_table) :: root
    type(hsd_value), allocatable :: val
    type(hsd_error_t), allocatable :: error
    character(len=512) :: filename
    integer :: unit_num, io_stat

    call new_table(root)
    allocate(val)
    call new_value(val, "text")
    val%string_value = "line1" // CHAR_NEWLINE // CHAR_NEWLINE // "line3"
    val%value_type = VALUE_TYPE_STRING
    call root%add_child(val)

    filename = trim(build_dir) // "/dump_multiline_empty.hsd"
    call hsd_dump(root, trim(filename), error)
    call check(.not. allocated(error), msg="Dump multiline with empty line to file")
    call root%destroy()

    open(newunit=unit_num, file=trim(filename), status='old', action='read', iostat=io_stat)
    if (io_stat == 0) close(unit_num, status='delete')
  end subroutine test_file_dump_multiline_empty_line

end module test_file_dump_coverage_suite
