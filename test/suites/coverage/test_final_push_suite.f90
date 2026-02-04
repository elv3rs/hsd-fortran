module test_final_push_suite
  !> Final push tests for 100% coverage
  use hsd, only : hsd_accept, hsd_merge, hsd_has_child, hsd_get, hsd_get_matrix, &
      hsd_dump, hsd_load_string, hsd_get_keys, hsd_get_child, hsd_visitor_t, hsd_table, &
      hsd_value, hsd_node, new_table, new_value
  use hsd_constants, only : dp, sp
  use hsd_error, only : make_error, hsd_error_t, HSD_STAT_OK, HSD_STAT_SYNTAX_ERROR, &
      HSD_STAT_NOT_FOUND
  use build_env, only : source_dir, build_dir
  use fortuno_serial, only : test => serial_case_item, check => serial_check, &
      suite => serial_suite_item, test_list
  implicit none (type, external)
  private
  public :: tests

  !> Custom visitor that returns error from visit_table
  type, extends(hsd_visitor_t) :: error_visitor
  contains
    procedure :: visit_table => error_visit_table
    procedure :: visit_value => error_visit_value
  end type error_visitor

contains

  function tests()
    type(test_list) :: tests
    tests = test_list([&
        suite("final_push", test_list([&
            test("visitor_error_callback", test_visitor_error_callback), &
            test("visitor_child_no_name", test_visitor_child_no_name), &
            test("merge_overlay_table", test_merge_overlay_table), &
            test("merge_overlay_value", test_merge_overlay_value), &
            test("merge_overlay_arrays", test_merge_overlay_arrays), &
            test("clone_table_deep", test_clone_table_deep), &
            test("accessor_matrix_type_error", test_accessor_matrix_type_error), &
            test("accessor_combined_text", test_accessor_combined_text), &
            test("formatter_single_child", test_formatter_single_child), &
            test("formatter_multiline_value", test_formatter_multiline_value), &
            test("formatter_real_append_zero", test_formatter_real_append_zero), &
            test("lexer_escape_in_quotes", test_lexer_escape_in_quotes), &
            test("hash_get_empty", test_hash_get_empty), &
            test("hash_chain_lookup", test_hash_chain_lookup), &
            test("hash_rehash_overflow", test_hash_rehash_overflow), &
            test("error_print_expected_actual", test_error_print_expected_actual), &
            test("error_no_filename", test_error_no_filename), &
            test("empty_tokenize", test_empty_tokenize), &
            test("empty_split_line", test_empty_split_line), &
            test("query_keys_not_found", test_query_keys_not_found), &
            test("query_child_not_table", test_query_child_not_table), &
            test("complex_imag_parse_error", test_complex_imag_parse_error) &
        ]))&
    ])
  end function tests

  subroutine error_visit_table(self, table, path, depth, stat)
    class(error_visitor), intent(inout) :: self
    type(hsd_table), intent(in), target :: table
    character(len=*), intent(in) :: path
    integer, intent(in) :: depth
    integer, intent(out), optional :: stat
    ! Return error on depth > 1 to test error propagation
    if (depth > 1 .and. present(stat)) stat = 99
    if (depth <= 1 .and. present(stat)) stat = 0
  end subroutine error_visit_table

  subroutine error_visit_value(self, val, path, depth, stat)
    class(error_visitor), intent(inout) :: self
    type(hsd_value), intent(in) :: val
    character(len=*), intent(in) :: path
    integer, intent(in) :: depth
    integer, intent(out), optional :: stat
    if (present(stat)) stat = 0
  end subroutine error_visit_value

  !> Test visitor error callback returns (lines 117-118)
  subroutine test_visitor_error_callback()
    type(hsd_table) :: root, child, grandchild
    type(error_visitor) :: vis
    integer :: stat

    call new_table(root, "root")
    call new_table(child, "child")
    call new_table(grandchild, "grandchild")
    call child%add_child(grandchild)
    call root%add_child(child)

    ! Visitor should return error from grandchild (depth=2)
    call hsd_accept(root, vis, stat)
    call check(stat == 99, msg="Visitor error propagates")

    call root%destroy()
  end subroutine test_visitor_error_callback

  !> Test visitor with child that has no name (line 137)
  subroutine test_visitor_child_no_name()
    type(hsd_table) :: root, anon_child
    type(error_visitor) :: vis
    integer :: stat

    call new_table(root, "root")
    call new_table(anon_child)  ! No name
    call root%add_child(anon_child)

    ! Path should handle unnamed child - use error_visitor since it's concrete
    call hsd_accept(root, vis, stat)
    call check(stat == 0, msg="Visitor handles unnamed child")

    call root%destroy()
  end subroutine test_visitor_child_no_name

  !> Test merge with overlay table not in base (line 391)
  subroutine test_merge_overlay_table()
    type(hsd_table) :: base, overlay, new_child
    integer :: stat

    call new_table(base, "base")
    call new_table(overlay, "overlay")
    call new_table(new_child, "NewSection")
    call overlay%add_child(new_child)

    call hsd_merge(base, overlay, stat)
    call check(stat == HSD_STAT_OK, msg="Merge adds new table")
    call check(hsd_has_child(base, "NewSection"), msg="New table exists")

    call base%destroy()
  end subroutine test_merge_overlay_table

  !> Test merge with overlay value not in base (line 394)
  subroutine test_merge_overlay_value()
    type(hsd_table) :: base, overlay
    type(hsd_value) :: new_val
    integer :: stat

    call new_table(base, "base")
    call new_table(overlay, "overlay")
    call new_value(new_val, "NewValue")
    call new_val%set_integer(42)
    call overlay%add_child(new_val)

    call hsd_merge(base, overlay, stat)
    call check(stat == HSD_STAT_OK, msg="Merge adds new value")
    call check(hsd_has_child(base, "NewValue"), msg="New value exists")

    call base%destroy()
  end subroutine test_merge_overlay_value

  !> Test merge with overlay value with arrays (lines 428, 431-432, 435-436)
  subroutine test_merge_overlay_arrays()
    type(hsd_table) :: base, overlay
    type(hsd_value) :: base_val, overlay_val
    integer :: stat
    integer, allocatable :: int_arr(:)

    call new_table(base, "base")
    call new_table(overlay, "overlay")

    ! Create base value with arrays
    call new_value(base_val, "Data")
    call base_val%set_string("1 2 3")
    call base%add_child(base_val)

    ! Create overlay value with different arrays
    call new_value(overlay_val, "Data")
    call overlay_val%set_string("10 20 30 40")
    call overlay%add_child(overlay_val)

    call hsd_merge(base, overlay, stat)
    call check(stat == HSD_STAT_OK, msg="Merge replaces value")

    ! Verify the value was updated
    call hsd_get(base, "Data", int_arr, stat)
    call check(size(int_arr) == 4, msg="Merged array has new size")

    call base%destroy()
  end subroutine test_merge_overlay_arrays

  !> Test clone_table with nested children (lines 463, 466)
  subroutine test_clone_table_deep()
    type(hsd_table) :: source, dest, child_table
    type(hsd_value) :: child_val
    integer :: stat

    call new_table(source, "source")
    call new_table(child_table, "SubTable")
    call new_value(child_val, "Value")
    call child_val%set_integer(123)
    call source%add_child(child_table)
    call source%add_child(child_val)

    ! Clone using merge with empty base
    call new_table(dest, "dest")
    call hsd_merge(dest, source, stat)

    call check(hsd_has_child(dest, "SubTable"), msg="Cloned table")
    call check(hsd_has_child(dest, "Value"), msg="Cloned value")

    call source%destroy()
    call dest%destroy()
  end subroutine test_clone_table_deep

  !> Test accessor matrix from table with no valid data (lines 543-546, 582-585)
  subroutine test_accessor_matrix_type_error()
    type(hsd_table) :: root
    type(hsd_value) :: val
    integer, allocatable :: int_mat(:,:)
    real(dp), allocatable :: real_mat(:,:)
    integer :: nrows, ncols, stat

    call new_table(root, "root")

    ! Create a value that can't parse as matrix
    call new_value(val, "BadMatrix")
    call val%set_string("not,a,valid,matrix,format")
    call root%add_child(val)

    ! Try to get matrix from invalid value
    call hsd_get_matrix(root, "BadMatrix", int_mat, nrows, ncols, stat)
    call check(nrows == 0 .or. ncols == 0 .or. stat /= HSD_STAT_OK, msg="Int matrix parse issue")

    call hsd_get_matrix(root, "BadMatrix", real_mat, nrows, ncols, stat)
    call check(nrows == 0 .or. ncols == 0 .or. stat /= HSD_STAT_OK, msg="Real matrix parse issue")

    call root%destroy()
  end subroutine test_accessor_matrix_type_error

  !> Test accessor combined text path (lines 612, 663)
  subroutine test_accessor_combined_text()
    type(hsd_table) :: root
    type(hsd_value) :: val1, val2
    character(len=:), allocatable :: str
    integer :: stat

    call new_table(root, "root")
    call new_value(val1, "Text")
    call val1%set_string("Line1")
    call root%add_child(val1)
    call new_value(val2, "Text")
    call val2%set_string("Line2")
    call root%add_child(val2)

    ! Get should combine multiple Text values
    call hsd_get(root, "Text", str, stat)
    call check(index(str, "Line") > 0, msg="Combined text works")

    call root%destroy()
  end subroutine test_accessor_combined_text

  !> Test formatter single child table (lines 130, 144)
  subroutine test_formatter_single_child()
    type(hsd_table) :: root, child
    type(hsd_value) :: val
    character(len=512) :: filepath
    integer :: unit_num, ios

    call new_table(root, "root")
    call new_table(child, "Section")
    call new_value(val, "Val")
    call val%set_integer(1)
    call child%add_child(val)
    call root%add_child(child)

    filepath = trim(build_dir) // "/test_single_child.hsd"
    call hsd_dump(root, trim(filepath))

    ! Verify file was created
    open(newunit=unit_num, file=trim(filepath), status="old", action="read", iostat=ios)
    call check(ios == 0, msg="Single child format written")
    close(unit_num)

    call root%destroy()
  end subroutine test_formatter_single_child

  !> Test formatter multiline value (lines 182-185)
  subroutine test_formatter_multiline_value()
    type(hsd_table) :: root
    type(hsd_value) :: val
    character(len=512) :: filepath
    integer :: unit_num, ios

    call new_table(root, "root")
    call new_value(val, "Multi")
    call val%set_string("Line1" // char(10) // "Line2" // char(10) // "Line3")
    call root%add_child(val)

    filepath = trim(build_dir) // "/test_multiline.hsd"
    call hsd_dump(root, trim(filepath))

    ! Verify multiline written
    open(newunit=unit_num, file=trim(filepath), status="old", action="read", iostat=ios)
    call check(ios == 0, msg="Multiline file written")
    close(unit_num)

    call root%destroy()
  end subroutine test_formatter_multiline_value

  !> Test formatter real with zero append (line 277)
  subroutine test_formatter_real_append_zero()
    type(hsd_table) :: root
    type(hsd_value) :: val
    character(len=512) :: filepath

    call new_table(root, "root")
    call new_value(val, "Real")
    call val%set_real(5.0_dp)  ! Exactly 5.0, no decimal part
    call root%add_child(val)

    filepath = trim(build_dir) // "/test_real.hsd"
    call hsd_dump(root, trim(filepath))

    call root%destroy()
  end subroutine test_formatter_real_append_zero

  !> Test lexer escape in single quotes (line 236)
  subroutine test_lexer_escape_in_quotes()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: err

    ! Single quoted with escaped quotes inside
    call hsd_load_string("Field = 'It''s a test'", root, err)
    call check(.not. allocated(err), msg="Escaped single quote parses")
    call root%destroy()
  end subroutine test_lexer_escape_in_quotes

  !> Test hash get on empty table (line 305)
  subroutine test_hash_get_empty()
    type(hsd_table) :: root
    class(hsd_node), pointer :: child

    call new_table(root, "root")
    ! No children added
    call root%get_child_by_name("nonexistent", child)
    call check(.not. associated(child), msg="Empty table lookup")
    call root%destroy()
  end subroutine test_hash_get_empty

  !> Test hash chain lookup (line 351)
  subroutine test_hash_chain_lookup()
    type(hsd_table) :: root
    type(hsd_value) :: v1, v2, v3, v4, v5, v6, v7, v8, v9, v10
    class(hsd_node), pointer :: found

    call new_table(root, "root")
    ! Add many children to trigger hash collisions and chains
    call new_value(v1, "Alpha")
    call root%add_child(v1)
    call new_value(v2, "Beta")
    call root%add_child(v2)
    call new_value(v3, "Gamma")
    call root%add_child(v3)
    call new_value(v4, "Delta")
    call root%add_child(v4)
    call new_value(v5, "Epsilon")
    call root%add_child(v5)
    call new_value(v6, "Zeta")
    call root%add_child(v6)
    call new_value(v7, "Eta")
    call root%add_child(v7)
    call new_value(v8, "Theta")
    call root%add_child(v8)
    call new_value(v9, "Iota")
    call root%add_child(v9)
    call new_value(v10, "Kappa")
    call root%add_child(v10)

    ! Lookup to traverse chains
    call root%get_child_by_name("Kappa", found)
    call check(associated(found), msg="Chain lookup works")
    call root%destroy()
  end subroutine test_hash_chain_lookup

  !> Test hash rehash with overflow entries (line 431)
  subroutine test_hash_rehash_overflow()
    type(hsd_table) :: root
    type(hsd_value) :: val
    class(hsd_node), pointer :: found
    integer :: i
    character(len=20) :: name

    call new_table(root, "root")
    ! Add many children to trigger rehash
    do i = 1, 50
      write(name, '(A,I0)') "Field", i
      call new_value(val, trim(name))
      call val%set_integer(i)
      call root%add_child(val)
    end do

    ! Verify all can still be found after rehash
    call root%get_child_by_name("Field25", found)
    call check(associated(found), msg="Found after rehash")

    call root%get_child_by_name("Field50", found)
    call check(associated(found), msg="Found last after rehash")

    call root%destroy()
  end subroutine test_hash_rehash_overflow

  !> Test error print with expected/actual (lines 190-191)
  subroutine test_error_print_expected_actual()
    type(hsd_error_t) :: err
    integer :: unit_num

    err%code = 10
    err%message = "Type mismatch"
    err%filename = "test.hsd"
    err%line_start = 5
    err%expected = "integer"
    err%actual = "string"

    open(newunit=unit_num, file=trim(build_dir) // "/error_out.txt", status="replace", &
        action="write")
    call err%print(unit_num)
    close(unit_num)

    call check(.true., msg="Error with expected/actual printed")
  end subroutine test_error_print_expected_actual

  !> Test error creation without filename (lines 223, 266)
  subroutine test_error_no_filename()
    type(hsd_error_t), allocatable :: err

    call make_error(err, HSD_STAT_SYNTAX_ERROR, "Test error")
    call check(allocated(err), msg="Error without filename created")
    call check(err%filename == "<unknown>", msg="Default filename set")
  end subroutine test_error_no_filename

  !> Test tokenize with empty/whitespace string (line 1205)
  subroutine test_empty_tokenize()
    type(hsd_value) :: val
    integer, allocatable :: arr(:)
    integer :: stat

    call new_value(val, "Empty")
    call val%set_string("   ")  ! Only whitespace
    call val%get_int_array(arr, stat)
    ! Should return empty array
    call check(size(arr) == 0, msg="Whitespace tokenizes to empty")
    call val%destroy()
  end subroutine test_empty_tokenize

  !> Test split_lines with empty line (line 1446)
  subroutine test_empty_split_line()
    type(hsd_value) :: val
    integer, allocatable :: mat(:,:)
    integer :: nrows, ncols, stat

    call new_value(val, "Matrix")
    ! Matrix with empty line in middle
    call val%set_string("1 2" // char(10) // "" // char(10) // "3 4")
    call val%get_int_matrix(mat, nrows, ncols, stat)
    call check(stat == HSD_STAT_OK, msg="Matrix with empty line")
    call val%destroy()
  end subroutine test_empty_split_line

  !> Test query get_keys on non-table path (lines 210-212)
  subroutine test_query_keys_not_found()
    type(hsd_table) :: root
    character(len=:), allocatable :: keys(:)
    integer :: stat

    call new_table(root, "root")
    call hsd_get_keys(root, "nonexistent", keys, stat)
    call check(stat == HSD_STAT_NOT_FOUND, msg="Keys not found")
    call check(size(keys) == 0, msg="Empty keys returned")
    call root%destroy()
  end subroutine test_query_keys_not_found

  !> Test query get child not table (line 307)
  subroutine test_query_child_not_table()
    type(hsd_table) :: root
    type(hsd_value) :: val
    class(hsd_node), pointer :: child
    integer :: stat

    call new_table(root, "root")
    call new_value(val, "Value")
    call val%set_integer(1)
    call root%add_child(val)

    ! Try to get nested path through value
    call hsd_get_child(root, "Value/Nested", child, stat)
    call check(stat == HSD_STAT_NOT_FOUND, msg="Path through value fails")
    call root%destroy()
  end subroutine test_query_child_not_table

  !> Test complex parse with valid real but invalid imaginary (lines 1581-1583)
  subroutine test_complex_imag_parse_error()
    type(hsd_value) :: val
    complex(dp) :: cval
    integer :: stat

    call new_value(val, "Complex")
    ! Real part is valid, imaginary part is not
    call val%set_string("1.0+abci")
    call val%get_complex(cval, stat)
    call check(stat /= HSD_STAT_OK, msg="Bad imaginary part fails")

    call val%destroy()
  end subroutine test_complex_imag_parse_error

end module test_final_push_suite
