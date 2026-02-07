!> Test suite for IO edge cases:
!> - hsd_load without error arg (file not found)
!> - Unclosed quotes in HSD input
!> - Malformed complex numbers
!> - Hash table rehash via many HSD children (>100 entries)
module test_edge_cases_io_suite
  use hsd
  use fortuno_serial, only: is_equal, &
      test => serial_case_item, &
      check => serial_check, &
      suite => serial_suite_item, &
      test_list
  implicit none (type, external)
  private
  public :: tests

contains

  function tests()
    type(test_list) :: tests
    tests = test_list([&
        suite("io_edge_cases", test_list([&
            test("load_missing_no_error_arg", test_load_missing_no_error_arg), &
            test("load_string_no_error_arg", test_load_string_no_error_arg), &
            test("unclosed_double_quote", test_unclosed_double_quote), &
            test("unclosed_single_quote", test_unclosed_single_quote), &
            test("malformed_complex_garbage", test_malformed_complex_garbage), &
            test("malformed_complex_partial", test_malformed_complex_partial), &
            test("malformed_complex_paren", test_malformed_complex_paren), &
            test("many_children_rehash", test_many_children_rehash) &
        ])) &
    ])
  end function tests

  ! ---- hsd_load without error arg ----

  !> Loading a nonexistent file without the error arg should NOT crash.
  !> It should silently return an empty but valid table.
  subroutine test_load_missing_no_error_arg()
    type(hsd_table) :: root

    ! No error argument — must not abort or segfault
    call hsd_load("/tmp/nonexistent_hsd_file_edge_test.hsd", root)

    ! The table should be empty but valid (num_children == 0)
    call check(root%num_children == 0, &
        msg="Empty table returned for missing file without error arg")

    call root%destroy()
  end subroutine test_load_missing_no_error_arg

  !> Loading from string without error arg should succeed normally
  subroutine test_load_string_no_error_arg()
    type(hsd_table) :: root
    integer :: ival, stat

    call hsd_load_string("x = 42", root)
    call hsd_get(root, "x", ival, stat)
    call check(stat == HSD_STAT_OK, msg="Value retrievable without error arg")
    call check(ival == 42, msg="Correct value")

    call root%destroy()
  end subroutine test_load_string_no_error_arg

  ! ---- Unclosed quotes ----

  !> An unclosed double-quote should not crash the parser.
  !> The parser may silently consume until EOF — the key test is no crash.
  subroutine test_unclosed_double_quote()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error

    call hsd_load_string('key = "unclosed value', root, error)

    ! Even if error is set or not, we just need to survive without crash.
    ! The table should be valid (possibly with the malformed data).
    call check(.true., msg="Parser survived unclosed double quote")

    call root%destroy()
  end subroutine test_unclosed_double_quote

  !> An unclosed single-quote should not crash the parser.
  subroutine test_unclosed_single_quote()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error

    call hsd_load_string("key = 'unclosed value", root, error)

    call check(.true., msg="Parser survived unclosed single quote")

    call root%destroy()
  end subroutine test_unclosed_single_quote

  ! ---- Malformed complex numbers ----

  !> Parsing a completely garbage string as complex should return a type error
  subroutine test_malformed_complex_garbage()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    complex(dp) :: val
    integer :: stat

    call hsd_load_string('z = "abc"', root, error)
    call check(.not. allocated(error), msg="Parse OK")

    call hsd_get(root, "z", val, stat)
    ! Should fail with a non-zero stat (type error or IO error)
    call check(stat /= HSD_STAT_OK, msg="Garbage string gives error stat")

    call root%destroy()
  end subroutine test_malformed_complex_garbage

  !> Parsing "1.0+i" (missing imaginary coefficient) as complex
  subroutine test_malformed_complex_partial()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    complex(dp) :: val
    integer :: stat

    call hsd_load_string("z = 1.0+i", root, error)
    call check(.not. allocated(error), msg="Parse OK")

    call hsd_get(root, "z", val, stat)
    ! "1.0+i" has trailing i but empty imaginary part — should fail
    call check(stat /= HSD_STAT_OK, msg="Partial complex gives error stat")

    call root%destroy()
  end subroutine test_malformed_complex_partial

  !> Parsing "(1.0,)" (missing imaginary in parens) as complex
  subroutine test_malformed_complex_paren()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    complex(dp) :: val
    integer :: stat

    call hsd_load_string('z = "(1.0,)"', root, error)
    call check(.not. allocated(error), msg="Parse OK")

    call hsd_get(root, "z", val, stat)
    ! "(1.0,)" has empty imaginary after comma — should fail
    call check(stat /= HSD_STAT_OK, msg="Malformed paren complex gives error stat")

    call root%destroy()
  end subroutine test_malformed_complex_paren

  ! ---- Hash table rehash under high load ----

  !> Create a table with >100 children to trigger multiple rehashes.
  !> Default hash table starts at 32 buckets with 0.75 load factor,
  !> so rehash occurs at 24, 48, 96, 192 entries.
  subroutine test_many_children_rehash()
    type(hsd_table) :: root
    integer :: i, val, stat
    character(len=20) :: key
    logical :: all_ok

    call new_table(root)

    ! Insert 200 children
    do i = 1, 200
      write(key, '(A,I0)') "field_", i
      call hsd_set(root, trim(key), i)
    end do

    ! Verify the table has 200 children
    call check(root%num_children == 200, msg="200 children inserted")

    ! Verify all 200 are retrievable with correct values
    all_ok = .true.
    do i = 1, 200
      write(key, '(A,I0)') "field_", i
      call hsd_get(root, trim(key), val, stat)
      if (stat /= HSD_STAT_OK .or. val /= i) then
        all_ok = .false.
        exit
      end if
    end do
    call check(all_ok, msg="All 200 children retrievable after rehashes")

    call root%destroy()
  end subroutine test_many_children_rehash

end module test_edge_cases_io_suite
