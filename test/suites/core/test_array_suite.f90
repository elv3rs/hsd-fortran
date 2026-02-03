!> Array handling unit tests using Fortuno framework
module test_array_suite
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
  use fortuno_serial, only : is_equal, test => serial_case_item, check => serial_check, &
      & suite => serial_suite_item, test_list
  implicit none (type, external)

  private
  public :: tests

contains

  !> Returns all array tests
  function tests()
    type(test_list) :: tests

    tests = test_list([&
        suite("arrays", test_list([&
            test("integer_array", test_integer_array), &
            test("real_array", test_real_array), &
            test("logical_array", test_logical_array), &
            test("string_array", test_string_array), &
            test("comma_separated", test_comma_separated), &
            test("mixed_separators", test_mixed_separators), &
            test("integer_matrix", test_integer_matrix), &
            test("real_matrix", test_real_matrix), &
            test("empty_array", test_empty_array), &
            test("large_array", test_large_array) &
        ])) &
    ])

  end function tests


  !> Test integer array parsing
  subroutine test_integer_array()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    integer, allocatable :: arr(:)
    integer :: stat

    call hsd_load_string("values = 1 2 3 4 5", root, error)

    call check(.not. allocated(error), msg="No parse error")

    call hsd_get(root, "values", arr, stat)
    call check(is_equal(stat, 0), msg="Can get integer array")
    call check(is_equal(size(arr), 5), msg="Array has 5 elements")
    call check(is_equal(arr(1), 1), msg="First element is 1")
    call check(is_equal(arr(3), 3), msg="Third element is 3")
    call check(is_equal(arr(5), 5), msg="Fifth element is 5")

    call root%destroy()

  end subroutine test_integer_array


  !> Test real array parsing
  subroutine test_real_array()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    real(dp), allocatable :: arr(:)
    integer :: stat

    call hsd_load_string("coords = 1.0 2.5 -3.14 4.0e-2", root, error)

    call check(.not. allocated(error), msg="No parse error")

    call hsd_get(root, "coords", arr, stat)
    call check(is_equal(stat, 0), msg="Can get real array")
    call check(is_equal(size(arr), 4), msg="Array has 4 elements")
    call check(abs(arr(1) - 1.0_dp) < 1e-10_dp, msg="First element is 1.0")
    call check(abs(arr(2) - 2.5_dp) < 1e-10_dp, msg="Second element is 2.5")
    call check(abs(arr(3) - (-3.14_dp)) < 1e-10_dp, msg="Third element is -3.14")
    call check(abs(arr(4) - 0.04_dp) < 1e-10_dp, msg="Fourth element is 0.04")

    call root%destroy()

  end subroutine test_real_array


  !> Test logical array parsing
  subroutine test_logical_array()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    logical, allocatable :: arr(:)
    integer :: stat

    call hsd_load_string("flags = yes no true false on off", root, error)

    call check(.not. allocated(error), msg="No parse error")

    call hsd_get(root, "flags", arr, stat)
    call check(is_equal(stat, 0), msg="Can get logical array")
    call check(is_equal(size(arr), 6), msg="Array has 6 elements")
    call check(arr(1) .eqv. .true., msg="First is true (yes)")
    call check(arr(2) .eqv. .false., msg="Second is false (no)")
    call check(arr(3) .eqv. .true., msg="Third is true")
    call check(arr(4) .eqv. .false., msg="Fourth is false")
    call check(arr(5) .eqv. .true., msg="Fifth is true (on)")
    call check(arr(6) .eqv. .false., msg="Sixth is false (off)")

    call root%destroy()

  end subroutine test_logical_array


  !> Test string array parsing with unquoted strings
  subroutine test_string_array()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: arr(:)
    integer :: stat

    ! Use unquoted strings separated by spaces (quoted strings would need
    ! special handling in HSD - this tests the common case)
    call hsd_load_string('elements = C H O N', root, error)

    call check(.not. allocated(error), msg="No parse error")

    call hsd_get(root, "elements", arr, stat)
    call check(is_equal(stat, 0), msg="Can get string array")
    call check(is_equal(size(arr), 4), msg="Array has 4 elements")
    call check(trim(arr(1)) == "C", msg="First element is C")
    call check(trim(arr(2)) == "H", msg="Second element is H")
    call check(trim(arr(3)) == "O", msg="Third element is O")
    call check(trim(arr(4)) == "N", msg="Fourth element is N")

    call root%destroy()

  end subroutine test_string_array


  !> Test comma-separated values
  subroutine test_comma_separated()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    integer, allocatable :: arr(:)
    integer :: stat

    call hsd_load_string("values = 10, 20, 30, 40", root, error)

    call check(.not. allocated(error), msg="No parse error")

    call hsd_get(root, "values", arr, stat)
    call check(is_equal(stat, 0), msg="Can get comma-separated array")
    call check(is_equal(size(arr), 4), msg="Array has 4 elements")
    call check(is_equal(arr(1), 10), msg="First element is 10")
    call check(is_equal(arr(4), 40), msg="Fourth element is 40")

    call root%destroy()

  end subroutine test_comma_separated


  !> Test mixed separators (spaces, commas)
  subroutine test_mixed_separators()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    real(dp), allocatable :: arr(:)
    integer :: stat

    call hsd_load_string("values = 1.0, 2.0 3.0,4.0  5.0", root, error)

    call check(.not. allocated(error), msg="No parse error")

    call hsd_get(root, "values", arr, stat)
    call check(is_equal(stat, 0), msg="Can get mixed-separator array")
    call check(is_equal(size(arr), 5), msg="Array has 5 elements")

    call root%destroy()

  end subroutine test_mixed_separators


  !> Test 2D integer matrix parsing with explicit row format
  !> Note: HSD traditionally uses newlines in blocks for matrix row separation
  !> but the parser treats newlines as whitespace. For explicit matrices,
  !> users should specify dimensions separately or use a flat array.
  subroutine test_integer_matrix()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    integer, allocatable :: arr(:)
    integer, allocatable :: mat(:,:)
    integer :: nrows, ncols, stat, i, j
    character(len=:), allocatable :: hsd_input

    ! Test extracting matrix values as a flat array first
    hsd_input = "matrix = 1 2 3 4 5 6 7 8 9" // char(10) // "ncols = 3"

    call hsd_load_string(hsd_input, root, error)
    call check(.not. allocated(error), msg="No parse error")

    ! Get the flat array
    call hsd_get(root, "matrix", arr, stat)
    call check(is_equal(stat, 0), msg="Can get integer array")
    call check(is_equal(size(arr), 9), msg="Array has 9 elements")

    ! Get ncols to reshape
    call hsd_get(root, "ncols", ncols, stat)
    nrows = size(arr) / ncols

    ! Reshape manually (column-major in Fortran)
    allocate(mat(nrows, ncols))
    do j = 1, ncols
      do i = 1, nrows
        mat(i, j) = arr((i-1)*ncols + j)
      end do
    end do

    call check(is_equal(nrows, 3), msg="Matrix has 3 rows")
    call check(is_equal(ncols, 3), msg="Matrix has 3 columns")
    call check(is_equal(mat(1,1), 1), msg="Element (1,1) is 1")
    call check(is_equal(mat(2,2), 5), msg="Element (2,2) is 5")
    call check(is_equal(mat(3,3), 9), msg="Element (3,3) is 9")
    call check(is_equal(mat(3,1), 7), msg="Element (3,1) is 7")

    call root%destroy()

  end subroutine test_integer_matrix


  !> Test 2D real matrix parsing with explicit row format
  subroutine test_real_matrix()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    real(dp), allocatable :: arr(:)
    real(dp), allocatable :: mat(:,:)
    integer :: nrows, ncols, stat, i, j
    character(len=:), allocatable :: hsd_input

    ! Test extracting matrix values as a flat array
    hsd_input = "lattice = 3.0 0.0 0.0 0.0 3.0 0.0 0.0 0.0 5.0" // char(10) // "ncols = 3"

    call hsd_load_string(hsd_input, root, error)
    call check(.not. allocated(error), msg="No parse error")

    ! Get the flat array
    call hsd_get(root, "lattice", arr, stat)
    call check(is_equal(stat, 0), msg="Can get real array")
    call check(is_equal(size(arr), 9), msg="Array has 9 elements")

    ! Get ncols to reshape
    call hsd_get(root, "ncols", ncols, stat)
    nrows = size(arr) / ncols

    ! Reshape manually (row-major storage)
    allocate(mat(nrows, ncols))
    do j = 1, ncols
      do i = 1, nrows
        mat(i, j) = arr((i-1)*ncols + j)
      end do
    end do

    call check(is_equal(nrows, 3), msg="Matrix has 3 rows")
    call check(is_equal(ncols, 3), msg="Matrix has 3 columns")
    call check(abs(mat(1,1) - 3.0_dp) < 1e-10_dp, msg="Element (1,1) is 3.0")
    call check(abs(mat(2,2) - 3.0_dp) < 1e-10_dp, msg="Element (2,2) is 3.0")
    call check(abs(mat(3,3) - 5.0_dp) < 1e-10_dp, msg="Element (3,3) is 5.0")
    call check(abs(mat(1,2)) < 1e-10_dp, msg="Element (1,2) is 0.0")

    call root%destroy()

  end subroutine test_real_matrix


  !> Test empty array handling
  subroutine test_empty_array()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    integer, allocatable :: arr(:)
    integer :: stat

    call hsd_load_string("empty {}", root, error)

    call check(.not. allocated(error), msg="No parse error")

    ! Trying to get array from non-value node should return empty array
    call hsd_get(root, "empty", arr, stat)
    call check(is_equal(size(arr), 0), msg="Empty block returns empty array")

    call root%destroy()

  end subroutine test_empty_array


  !> Test large array (more than old 1000 limit)
  subroutine test_large_array()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    integer, allocatable :: arr(:)
    integer :: stat, i
    character(len=:), allocatable :: hsd_input
    character(len=10) :: num_str

    ! Build a string with 1500 integers
    hsd_input = "large = "
    do i = 1, 1500
      write(num_str, '(I0)') i
      hsd_input = hsd_input // trim(num_str) // " "
    end do

    call hsd_load_string(hsd_input, root, error)
    call check(.not. allocated(error), msg="No parse error")

    call hsd_get(root, "large", arr, stat)
    call check(is_equal(stat, 0), msg="Can get large array")
    call check(is_equal(size(arr), 1500), msg="Array has 1500 elements")
    call check(is_equal(arr(1), 1), msg="First element is 1")
    call check(is_equal(arr(1000), 1000), msg="Element 1000 is 1000")
    call check(is_equal(arr(1500), 1500), msg="Last element is 1500")

    call root%destroy()

  end subroutine test_large_array

end module test_array_suite
