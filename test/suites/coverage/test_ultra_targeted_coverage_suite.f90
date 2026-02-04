!> Ultra-targeted coverage tests for reaching 95%
module test_ultra_targeted_coverage_suite
  use hsd, only : hsd_table, hsd_error_t, new_table, hsd_set, hsd_get, &
      HSD_STAT_OK, hsd_load_string, hsd_load, hsd_iterator, hsd_node
  use hsd_hash_table, only: hsd_name_index_t
  use hsd_constants, only : dp, sp
  use hsd_schema, only : hsd_schema_t, schema_init, schema_destroy, &
       schema_add_field, schema_validate, FIELD_REQUIRED, FIELD_OPTIONAL, &
       FIELD_TYPE_REAL, FIELD_TYPE_INTEGER, FIELD_TYPE_LOGICAL
  use build_env, only : source_dir
  use fortuno_serial, only : test => serial_case_item, check => serial_check, &
      suite => serial_suite_item, test_list
  implicit none (type, external)
  private
  public :: tests

contains

  function tests()
    type(test_list) :: tests
    tests = test_list([&
        suite("ultra_targeted", test_list([&
            test("hash_rehash_operation", test_hash_rehash), &
            test("hash_clear_operation", test_hash_clear), &
            test("hash_remove_middle_chain", test_hash_remove_chain), &
            test("mutator_nested_creation", test_mutator_nested), &
            test("mutator_complex_arrays", test_mutator_complex_arrays), &
            test("schema_real_range_both_bounds", test_schema_real_range), &
            test("schema_int_range_both_bounds", test_schema_int_range), &
            test("parser_various_errors", test_parser_errors), &
            test("visitor_depth_tracking", test_visitor_depth), &
            test("types_array_operations", test_types_arrays), &
            test("hash_case_insensitive_not_found", test_hash_ci_notfound), &
            test("mutator_sp_arrays_extended", test_mutator_sp_extended), &
            test("schema_description_field", test_schema_desc), &
            test("hash_multiple_removes", test_hash_multi_remove), &
            test("types_get_child_variations", test_types_get_child) &
        ]))&
    ])
  end function tests

  subroutine test_hash_rehash()
    type(hsd_name_index_t) :: ht
    integer :: i, idx
    character(len=10) :: key

    call ht%init(capacity=4)

    ! Force rehash by exceeding load factor
    do i = 1, 10
      write(key, '(A,I0)') "r", i
      call ht%insert(trim(key), i * 100)
    end do

    call ht%rehash()  ! Explicit rehash call

    do i = 1, 10
      write(key, '(A,I0)') "r", i
      idx = ht%lookup(trim(key))
      call check(idx == i * 100, msg="After rehash")
    end do

    call ht%destroy()
  end subroutine test_hash_rehash

  subroutine test_hash_clear()
    type(hsd_name_index_t) :: ht
    integer :: idx

    call ht%init()
    call ht%insert("a", 1)
    call ht%insert("b", 2)

    call ht%clear()

    idx = ht%lookup("a")
    call check(idx == 0, msg="Cleared table")

    call ht%destroy()
  end subroutine test_hash_clear

  subroutine test_hash_remove_chain()
    type(hsd_name_index_t) :: ht
    integer :: i, idx
    character(len=5) :: key

    call ht%init(capacity=2)

    ! Create chain
    do i = 1, 20
      write(key, '(I0)') i
      call ht%insert(trim(key), i)
    end do

    ! Remove from various positions in chain
    call ht%remove("5")
    call ht%remove("10")
    call ht%remove("15")

    idx = ht%lookup("5")
    call check(idx == 0, msg="Removed from chain")

    idx = ht%lookup("11")
    call check(idx == 11, msg="Adjacent still there")

    call ht%destroy()
  end subroutine test_hash_remove_chain

  subroutine test_mutator_nested()
    type(hsd_table) :: root
    integer :: val, stat

    call new_table(root)

    ! Create nested structure via set
    call hsd_set(root, "A/B/C/D", 42, stat=stat)
    call check(stat == HSD_STAT_OK, msg="Deep nesting")

    call hsd_get(root, "A/B/C/D", val, stat=stat)
    call check(val == 42, msg="Retrieved nested")

    call root%destroy()
  end subroutine test_mutator_nested

  subroutine test_mutator_complex_arrays()
    type(hsd_table) :: root
    complex(dp), allocatable :: carr(:)
    integer :: stat

    call new_table(root)

    call hsd_set(root, "CArray", [(1.0_dp, 1.0_dp), (2.0_dp, 2.0_dp), (3.0_dp, 3.0_dp)], stat=stat)
    call check(stat == HSD_STAT_OK, msg="Set complex array")

    call hsd_get(root, "CArray", carr, stat=stat)
    call check(stat == HSD_STAT_OK .and. size(carr) == 3, msg="Get complex array")

    call root%destroy()
  end subroutine test_mutator_complex_arrays

  subroutine test_schema_real_range()
    type(hsd_schema_t) :: schema
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: errors(:)
    type(hsd_error_t), allocatable :: parse_error

    call schema_init(schema)
    call schema_add_field(schema, "Value", FIELD_REQUIRED, FIELD_TYPE_REAL, &
                          min_real=0.0_dp, max_real=100.0_dp)

    ! Within range
    call hsd_load_string('Value = 50.5', root, parse_error)
    call schema_validate(schema, root, errors)
    call check(size(errors) == 0, msg="Real in range")
    call root%destroy()

    ! Out of range - below
    call hsd_load_string('Value = -1.0', root, parse_error)
    call schema_validate(schema, root, errors)
    call check(size(errors) > 0, msg="Real below range fails")
    call root%destroy()

    ! Out of range - above
    call hsd_load_string('Value = 101.0', root, parse_error)
    call schema_validate(schema, root, errors)
    call check(size(errors) > 0, msg="Real above range fails")
    call root%destroy()

    call schema_destroy(schema)
  end subroutine test_schema_real_range

  subroutine test_schema_int_range()
    type(hsd_schema_t) :: schema
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: errors(:)
    type(hsd_error_t), allocatable :: parse_error

    call schema_init(schema)
    call schema_add_field(schema, "Count", FIELD_REQUIRED, FIELD_TYPE_INTEGER, &
                          min_int=-10, max_int=10)

    call hsd_load_string('Count = 0', root, parse_error)
    call schema_validate(schema, root, errors)
    call check(size(errors) == 0, msg="Int in range")
    call root%destroy()

    call hsd_load_string('Count = -20', root, parse_error)
    call schema_validate(schema, root, errors)
    call check(size(errors) > 0, msg="Int below fails")
    call root%destroy()

    call hsd_load_string('Count = 20', root, parse_error)
    call schema_validate(schema, root, errors)
    call check(size(errors) > 0, msg="Int above fails")
    call root%destroy()

    call schema_destroy(schema)
  end subroutine test_schema_int_range

  subroutine test_parser_errors()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=512) :: filepath
    integer :: unit, stat

    ! Test file I/O error path
    filepath = "/nonexistent/path/file.hsd"
    call hsd_load(filepath, root, error)
    call check(allocated(error), msg="File not found error")
  end subroutine test_parser_errors

  subroutine test_visitor_depth()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: parse_error
    type(hsd_iterator) :: iter
    class(hsd_node), pointer :: child
    integer :: count
    logical :: has_more

    call hsd_load_string('A = 1' // char(10) // 'B = 2' // char(10) // 'C = 3', root, parse_error)

    ! Traverse and count children
    call iter%init(root)
    count = 0
    do while (iter%has_next())
      has_more = iter%next(child)
      count = count + 1
    end do
    call check(count == 3, msg="Iterator traversal")

    call root%destroy()
  end subroutine test_visitor_depth

  subroutine test_types_arrays()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: parse_error
    integer, allocatable :: iarr(:)
    real(dp), allocatable :: rarr(:)
    logical, allocatable :: larr(:)
    integer :: stat

    call hsd_load_string('IntArr = 1 2 3' // char(10) // &
                         'RealArr = 1.0 2.0 3.0' // char(10) // &
                         'LogArr = Yes No Yes', root, parse_error)

    call hsd_get(root, "IntArr", iarr, stat=stat)
    call check(size(iarr) == 3, msg="Int array")

    call hsd_get(root, "RealArr", rarr, stat=stat)
    call check(size(rarr) == 3, msg="Real array")

    call hsd_get(root, "LogArr", larr, stat=stat)
    call check(size(larr) == 3, msg="Logical array")

    call root%destroy()
  end subroutine test_types_arrays

  subroutine test_hash_ci_notfound()
    type(hsd_name_index_t) :: ht
    integer :: idx

    call ht%init()
    call ht%insert("Found", 1)

    idx = ht%lookup_case_insensitive("notfound")
    call check(idx == 0, msg="CI not found")

    call ht%destroy()
  end subroutine test_hash_ci_notfound

  subroutine test_mutator_sp_extended()
    type(hsd_table) :: root
    real(sp) :: val_sp
    real(sp), allocatable :: arr_sp(:)
    integer :: stat

    call new_table(root)

    call hsd_set(root, "SP1", 1.5_sp, stat=stat)
    call hsd_get(root, "SP1", val_sp, stat=stat)
    call check(stat == HSD_STAT_OK, msg="SP scalar")

    call hsd_set(root, "SPA", [1.0_sp, 2.0_sp], stat=stat)
    call hsd_get(root, "SPA", arr_sp, stat=stat)
    call check(size(arr_sp) == 2, msg="SP array")

    call root%destroy()
  end subroutine test_mutator_sp_extended

  subroutine test_schema_desc()
    type(hsd_schema_t) :: schema

    call schema_init(schema, name="TestSchema")
    call schema_add_field(schema, "Field", FIELD_OPTIONAL, FIELD_TYPE_INTEGER, &
                          description="Test field description")

    call check(schema%num_fields == 1, msg="Field with description")
    call schema_destroy(schema)
  end subroutine test_schema_desc

  subroutine test_hash_multi_remove()
    type(hsd_name_index_t) :: ht
    integer :: i, idx
    character(len=10) :: key

    call ht%init()

    do i = 1, 10
      write(key, '(A,I0)') "x", i
      call ht%insert(trim(key), i)
    end do

    ! Remove and check count decreases
    call ht%remove("x5")
    call check(ht%num_entries == 9, msg="Entry count after remove")

    call ht%destroy()
  end subroutine test_hash_multi_remove

  subroutine test_types_get_child()
    type(hsd_table) :: root
    class(hsd_node), pointer :: child_ptr
    type(hsd_error_t), allocatable :: parse_error

    call hsd_load_string('Child = 42', root, parse_error)

    call root%get_child_by_name("Child", child_ptr)
    call check(associated(child_ptr), msg="Get child ptr")

    call root%get_child_by_name("NonExistent", child_ptr)
    call check(.not. associated(child_ptr), msg="Child not found")

    call root%destroy()
  end subroutine test_types_get_child

end module test_ultra_targeted_coverage_suite
