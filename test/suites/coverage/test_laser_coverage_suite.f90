!> Laser-focused tests targeting specific uncovered lines
module test_laser_coverage_suite
  use hsd, only : hsd_table, hsd_error_t, hsd_load_string, hsd_get, &
      HSD_STAT_OK
  use hsd_hash_table, only: hsd_name_index_t
  use hsd_constants, only : dp
  use hsd_schema, only : hsd_schema_t, schema_init, schema_destroy, &
       schema_add_field, schema_validate, FIELD_REQUIRED, FIELD_OPTIONAL, &
       FIELD_TYPE_REAL, FIELD_TYPE_INTEGER, FIELD_TYPE_TABLE
  use fortuno_serial, only : test => serial_case_item, check => serial_check, &
      suite => serial_suite_item, test_list
  implicit none (type, external)
  private
  public :: tests

contains

  function tests()
    type(test_list) :: tests
    tests = test_list([&
        suite("laser", test_list([&
            test("hash_update_in_overflow", test_hash_update_overflow), &
            test("hash_update_in_chain", test_hash_update_chain), &
            test("hash_lookup_with_found", test_hash_lookup_found), &
            test("hash_remove_chain_entry", test_hash_remove_from_chain), &
            test("schema_table_validation", test_schema_table_type), &
            test("parser_empty_lines", test_parser_empty), &
            test("types_array_operations", test_types_matrix), &
            test("hash_massive_chain_update", test_hash_massive_update), &
            test("hash_lookup_ci_with_found", test_hash_ci_found) &
        ]))&
    ])
  end function tests

  !> Target lines 178-179 in hash_table: update existing entry in overflow
  subroutine test_hash_update_overflow()
    type(hsd_name_index_t) :: ht
    integer :: val
    character(len=10) :: key
    integer :: i

    call ht%init(capacity=2)  ! Small capacity to force overflow

    ! Fill buckets and overflow
    do i = 1, 20
      write(key, '(A,I0)') "k", i
      call ht%insert(trim(key), i)
    end do

    ! Update an entry that's in overflow (not in bucket)
    ! This should hit lines 178-179
    call ht%insert("k10", 1000)  ! Update existing
    val = ht%lookup("k10")
    call check(val == 1000, msg="Updated in overflow")

    call ht%destroy()
  end subroutine test_hash_update_overflow

  !> Target lines 185: update logic when traversing chain
  subroutine test_hash_update_chain()
    type(hsd_name_index_t) :: ht
    integer :: val
    character(len=20) :: key
    integer :: i

    call ht%init(capacity=1)  ! Minimal capacity

    ! Create long chain
    do i = 1, 50
      write(key, '(A,I0)') "chain", i
      call ht%insert(trim(key), i)
    end do

    ! Update entry deep in chain
    call ht%insert("chain25", 2500)
    val = ht%lookup("chain25")
    call check(val == 2500, msg="Updated in chain")

    call ht%destroy()
  end subroutine test_hash_update_chain

  !> Target line 254, 256: lookup with found parameter
  subroutine test_hash_lookup_found()
    type(hsd_name_index_t) :: ht
    integer :: val
    logical :: found

    call ht%init()
    call ht%insert("exists", 42)

    ! Lookup with found parameter (line 254, 256)
    val = ht%lookup("exists", found)
    call check(found .and. val == 42, msg="Found existing")

    val = ht%lookup("missing", found)
    call check(.not. found .and. val == 0, msg="Not found")

    call ht%destroy()
  end subroutine test_hash_lookup_found

  !> Target lines 337-344, 356-359: remove from chain
  subroutine test_hash_remove_from_chain()
    type(hsd_name_index_t) :: ht
    integer :: val
    character(len=10) :: key
    integer :: i

    call ht%init(capacity=2)

    ! Create entries that hash to same bucket
    do i = 1, 30
      write(key, '(A,I0)') "x", i
      call ht%insert(trim(key), i * 10)
    end do

    ! Remove entry from bucket (should promote chain)
    ! This targets lines 337-344
    call ht%remove("x1")

    ! Remove entry from overflow chain
    ! This targets lines 356-359
    call ht%remove("x15")

    ! Verify remaining entries
    val = ht%lookup("x2")
    call check(val /= 0, msg="Other entries remain")

    call ht%destroy()
  end subroutine test_hash_remove_from_chain

  !> Target schema validation for TABLE type
  subroutine test_schema_table_type()
    type(hsd_schema_t) :: schema
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: errors(:)
    type(hsd_error_t), allocatable :: parse_error

    call schema_init(schema)
    call schema_add_field(schema, "Config", FIELD_REQUIRED, FIELD_TYPE_TABLE)

    ! Valid: table
    call hsd_load_string('Config {}', root, parse_error)
    call schema_validate(schema, root, errors)
    call check(size(errors) == 0, msg="Table type valid")
    call root%destroy()

    ! Invalid: not a table
    call hsd_load_string('Config = 42', root, parse_error)
    call schema_validate(schema, root, errors)
    call check(size(errors) > 0, msg="Non-table fails")
    call root%destroy()

    call schema_destroy(schema)
  end subroutine test_schema_table_type

  !> Target parser handling of empty/whitespace lines
  subroutine test_parser_empty()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    integer :: val, stat

    ! Multiple blank lines
    call hsd_load_string(char(10) // char(10) // 'X = 1' // char(10) // char(10), root, error)
    call hsd_get(root, "X", val, stat=stat)
    call check(val == 1, msg="Blank lines OK")

    call root%destroy()
  end subroutine test_parser_empty

  !> Target types matrix/array edge cases
  subroutine test_types_matrix()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    real(dp), allocatable :: arr(:)
    integer :: stat

    ! Just test basic array parsing
    call hsd_load_string('Arr = 1.0 2.0 3.0', root, error)

    call hsd_get(root, "Arr", arr, stat=stat)
    call check(stat == HSD_STAT_OK .and. size(arr) == 3, msg="Array parsed")

    call root%destroy()
  end subroutine test_types_matrix

  !> Create extremely long chain and update middle entry
  subroutine test_hash_massive_update()
    type(hsd_name_index_t) :: ht
    integer :: val
    character(len=20) :: key
    integer :: i

    ! Single bucket forces everything into overflow chain
    call ht%init(capacity=1)

    ! Create 100+ entry chain
    do i = 1, 150
      write(key, '(A,I0)') "long", i
      call ht%insert(trim(key), i)
    end do

    ! Update entry in middle of chain (lines 178-179, 185)
    do i = 50, 100, 10
      write(key, '(A,I0)') "long", i
      call ht%insert(trim(key), i * 100)  ! Update
    end do

    ! Verify updates
    val = ht%lookup("long60")
    call check(val == 6000, msg="Chain update worked")

    call ht%destroy()
  end subroutine test_hash_massive_update

  !> Test lookup_case_insensitive with found parameter (lines 306)
  subroutine test_hash_ci_found()
    type(hsd_name_index_t) :: ht
    integer :: val
    logical :: found

    call ht%init()
    call ht%insert("TestKey", 99)

    val = ht%lookup_case_insensitive("TESTKEY", found)
    call check(found .and. val == 99, msg="CI lookup found")

    val = ht%lookup_case_insensitive("missing", found)
    call check(.not. found .and. val == 0, msg="CI not found")

    call ht%destroy()
  end subroutine test_hash_ci_found

end module test_laser_coverage_suite
