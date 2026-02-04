!> Comprehensive test suite for hsd_hash_table module
module test_hash_table_suite
  use hsd_hash_table, only : hsd_name_index_t
  use fortuno_serial, only : is_equal, test => serial_case_item, &
      & check => serial_check, suite => serial_suite_item, test_list
  implicit none (type, external)
  private
  public :: tests

contains

  function tests()
    type(test_list) :: tests
    tests = test_list([&
        suite("hash_table", test_list([&
            test("basic_insert_lookup", test_basic_insert_lookup), &
            test("case_insensitive_lookup", test_case_insensitive_lookup), &
            test("collision_handling", test_collision_handling), &
            test("rehashing_on_load", test_rehashing_on_load), &
            test("remove_operations", test_remove_operations), &
            test("update_existing_key", test_update_existing_key), &
            test("chain_traversal", test_chain_traversal), &
            test("overflow_handling", test_overflow_handling), &
            test("init_with_custom_capacity", test_init_with_custom_capacity), &
            test("clear_and_reuse", test_clear_and_reuse), &
            test("empty_table_lookup", test_empty_table_lookup), &
            test("single_char_keys", test_single_char_keys), &
            test("long_keys", test_long_keys), &
            test("special_character_keys", test_special_character_keys), &
            test("numeric_string_keys", test_numeric_string_keys), &
            test("mixed_case_variations", test_mixed_case_variations), &
            test("remove_from_chain", test_remove_from_chain), &
            test("remove_bucket_entry", test_remove_bucket_entry), &
            test("remove_overflow_entry", test_remove_overflow_entry), &
            test("large_table_stress", test_large_table_stress) &
        ])) &
    ])
  end function tests

  !> Test basic insert and lookup
  subroutine test_basic_insert_lookup()
    type(hsd_name_index_t) :: ht
    integer :: idx

    call ht%init()
    call ht%insert("key1", 10)
    call ht%insert("key2", 20)
    call ht%insert("key3", 30)

    idx = ht%lookup("key1")
    call check(idx == 10, msg="Lookup key1")

    idx = ht%lookup("key2")
    call check(idx == 20, msg="Lookup key2")

    idx = ht%lookup("key3")
    call check(idx == 30, msg="Lookup key3")

    idx = ht%lookup("nonexistent")
    call check(idx == 0, msg="Lookup nonexistent key")

    call ht%destroy()
  end subroutine test_basic_insert_lookup

  !> Test case-insensitive lookup
  subroutine test_case_insensitive_lookup()
    type(hsd_name_index_t) :: ht
    integer :: idx

    call ht%init()
    call ht%insert("TestKey", 42)

    idx = ht%lookup_case_insensitive("testkey")
    call check(idx == 42, msg="CI lookup lowercase")

    idx = ht%lookup_case_insensitive("TESTKEY")
    call check(idx == 42, msg="CI lookup uppercase")

    idx = ht%lookup_case_insensitive("TeStKeY")
    call check(idx == 42, msg="CI lookup mixed case")

    idx = ht%lookup_case_insensitive("wrongkey")
    call check(idx == 0, msg="CI lookup nonexistent")

    call ht%destroy()
  end subroutine test_case_insensitive_lookup

  !> Test collision handling with keys that hash to same bucket
  subroutine test_collision_handling()
    type(hsd_name_index_t) :: ht
    integer :: idx, i

    call ht%init(capacity=4)  ! Small capacity to force collisions

    ! Insert multiple keys that will likely collide
    do i = 1, 20
      call ht%insert("key" // char(i + 64), i * 10)
    end do

    ! Verify all can be retrieved
    do i = 1, 20
      idx = ht%lookup("key" // char(i + 64))
      call check(idx == i * 10, msg="Lookup after collision")
    end do

    call ht%destroy()
  end subroutine test_collision_handling

  !> Test automatic rehashing when load factor exceeds threshold
  subroutine test_rehashing_on_load()
    type(hsd_name_index_t) :: ht
    integer :: idx, i

    call ht%init(capacity=8)

    ! Insert enough entries to trigger rehash (> 0.75 load factor)
    do i = 1, 10
      call ht%insert("entry" // char(48 + mod(i, 10)), i)
    end do

    ! Verify all entries are still accessible after potential rehash
    do i = 1, 10
      idx = ht%lookup("entry" // char(48 + mod(i, 10)))
      call check(idx > 0, msg="Entry found after rehash")
    end do

    call ht%destroy()
  end subroutine test_rehashing_on_load

  !> Test remove operations
  subroutine test_remove_operations()
    type(hsd_name_index_t) :: ht
    integer :: idx

    call ht%init()
    call ht%insert("remove_me", 100)
    call ht%insert("keep_me", 200)

    idx = ht%lookup("remove_me")
    call check(idx == 100, msg="Before remove")

    call ht%remove("remove_me")
    idx = ht%lookup("remove_me")
    call check(idx == 0, msg="After remove")

    idx = ht%lookup("keep_me")
    call check(idx == 200, msg="Other entry unaffected")

    call ht%destroy()
  end subroutine test_remove_operations

  !> Test updating existing key
  subroutine test_update_existing_key()
    type(hsd_name_index_t) :: ht
    integer :: idx

    call ht%init()
    call ht%insert("update", 50)
    idx = ht%lookup("update")
    call check(idx == 50, msg="Initial value")

    call ht%insert("update", 75)
    idx = ht%lookup("update")
    call check(idx == 75, msg="Updated value")

    call ht%destroy()
  end subroutine test_update_existing_key

  !> Test chain traversal in overflow area
  subroutine test_chain_traversal()
    type(hsd_name_index_t) :: ht
    integer :: i, idx
    character(len=20) :: key

    call ht%init(capacity=2)  ! Very small to force overflow usage

    ! Insert many keys to create chains
    do i = 1, 50
      write(key, '(A,I0)') "chain_", i
      call ht%insert(trim(key), i * 3)
    end do

    ! Verify all can be found
    do i = 1, 50
      write(key, '(A,I0)') "chain_", i
      idx = ht%lookup(trim(key))
      call check(idx == i * 3, msg="Chain lookup " // trim(key))
    end do

    call ht%destroy()
  end subroutine test_chain_traversal

  !> Test overflow handling
  subroutine test_overflow_handling()
    type(hsd_name_index_t) :: ht
    integer :: i, idx
    character(len=10) :: key

    call ht%init(capacity=4)

    ! Force overflow by adding many entries
    do i = 1, 30
      write(key, '(A,I0)') "ovf", i
      call ht%insert(trim(key), i + 1000)
    end do

    ! Spot check some values
    idx = ht%lookup("ovf15")
    call check(idx == 1015, msg="Overflow lookup 15")

    idx = ht%lookup("ovf25")
    call check(idx == 1025, msg="Overflow lookup 25")

    call ht%destroy()
  end subroutine test_overflow_handling

  !> Test init with custom capacity
  subroutine test_init_with_custom_capacity()
    type(hsd_name_index_t) :: ht
    integer :: idx

    call ht%init(capacity=64)
    call ht%insert("test", 999)
    idx = ht%lookup("test")
    call check(idx == 999, msg="Custom capacity init")

    call ht%destroy()
  end subroutine test_init_with_custom_capacity

  !> Test clear and reuse
  subroutine test_clear_and_reuse()
    type(hsd_name_index_t) :: ht
    integer :: idx

    call ht%init()
    call ht%insert("first", 1)
    call ht%clear()

    idx = ht%lookup("first")
    call check(idx == 0, msg="After clear")

    call ht%insert("second", 2)
    idx = ht%lookup("second")
    call check(idx == 2, msg="Reuse after clear")

    call ht%destroy()
  end subroutine test_clear_and_reuse

  !> Test lookup on empty table
  subroutine test_empty_table_lookup()
    type(hsd_name_index_t) :: ht
    integer :: idx

    call ht%init()
    idx = ht%lookup("anything")
    call check(idx == 0, msg="Empty table lookup")

    call ht%destroy()
  end subroutine test_empty_table_lookup

  !> Test single character keys
  subroutine test_single_char_keys()
    type(hsd_name_index_t) :: ht
    integer :: idx

    call ht%init()
    call ht%insert("A", 1)
    call ht%insert("B", 2)
    call ht%insert("z", 26)

    idx = ht%lookup("A")
    call check(idx == 1, msg="Single char A")

    idx = ht%lookup("B")
    call check(idx == 2, msg="Single char B")

    idx = ht%lookup("z")
    call check(idx == 26, msg="Single char z")

    call ht%destroy()
  end subroutine test_single_char_keys

  !> Test long keys
  subroutine test_long_keys()
    type(hsd_name_index_t) :: ht
    integer :: idx

    call ht%init()
    call ht%insert("ThisIsAVeryLongKeyNameThatShouldStillWork", 1234)
    call ht%insert("AnotherExtremelyLongKeyForTesting", 5678)

    idx = ht%lookup("ThisIsAVeryLongKeyNameThatShouldStillWork")
    call check(idx == 1234, msg="Long key 1")

    idx = ht%lookup("AnotherExtremelyLongKeyForTesting")
    call check(idx == 5678, msg="Long key 2")

    call ht%destroy()
  end subroutine test_long_keys

  !> Test keys with special characters
  subroutine test_special_character_keys()
    type(hsd_name_index_t) :: ht
    integer :: idx

    call ht%init()
    call ht%insert("key_with_underscore", 11)
    call ht%insert("key-with-dash", 22)
    call ht%insert("key.with.dot", 33)

    idx = ht%lookup("key_with_underscore")
    call check(idx == 11, msg="Underscore key")

    idx = ht%lookup("key-with-dash")
    call check(idx == 22, msg="Dash key")

    idx = ht%lookup("key.with.dot")
    call check(idx == 33, msg="Dot key")

    call ht%destroy()
  end subroutine test_special_character_keys

  !> Test numeric string keys
  subroutine test_numeric_string_keys()
    type(hsd_name_index_t) :: ht
    integer :: idx

    call ht%init()
    call ht%insert("123", 100)
    call ht%insert("456", 200)
    call ht%insert("0", 300)

    idx = ht%lookup("123")
    call check(idx == 100, msg="Numeric string 123")

    idx = ht%lookup("456")
    call check(idx == 200, msg="Numeric string 456")

    idx = ht%lookup("0")
    call check(idx == 300, msg="Numeric string 0")

    call ht%destroy()
  end subroutine test_numeric_string_keys

  !> Test mixed case variations
  subroutine test_mixed_case_variations()
    type(hsd_name_index_t) :: ht
    integer :: idx

    call ht%init()
    call ht%insert("MixedCase", 111)
    call ht%insert("ALLCAPS", 222)
    call ht%insert("alllower", 333)

    ! Exact case lookup
    idx = ht%lookup("MixedCase")
    call check(idx == 111, msg="Exact case MixedCase")

    idx = ht%lookup("ALLCAPS")
    call check(idx == 222, msg="Exact case ALLCAPS")

    ! Wrong case should not match in case-sensitive lookup
    idx = ht%lookup("mixedcase")
    call check(idx == 0, msg="Wrong case should fail")

    ! But should match in case-insensitive
    idx = ht%lookup_case_insensitive("mixedcase")
    call check(idx == 111, msg="CI mixedcase")

    call ht%destroy()
  end subroutine test_mixed_case_variations

  !> Test removing from middle of chain
  subroutine test_remove_from_chain()
    type(hsd_name_index_t) :: ht
    integer :: i, idx
    character(len=10) :: key

    call ht%init(capacity=2)  ! Force chaining

    ! Add several keys
    do i = 1, 10
      write(key, '(A,I0)') "k", i
      call ht%insert(trim(key), i * 100)
    end do

    ! Remove one from the middle
    call ht%remove("k5")
    idx = ht%lookup("k5")
    call check(idx == 0, msg="Removed k5")

    ! Check others still exist
    idx = ht%lookup("k4")
    call check(idx > 0, msg="k4 still exists")

    idx = ht%lookup("k6")
    call check(idx > 0, msg="k6 still exists")

    call ht%destroy()
  end subroutine test_remove_from_chain

  !> Test removing bucket entry
  subroutine test_remove_bucket_entry()
    type(hsd_name_index_t) :: ht
    integer :: idx

    call ht%init()
    call ht%insert("bucket_key", 777)

    idx = ht%lookup("bucket_key")
    call check(idx == 777, msg="Before bucket remove")

    call ht%remove("bucket_key")
    idx = ht%lookup("bucket_key")
    call check(idx == 0, msg="After bucket remove")

    call ht%destroy()
  end subroutine test_remove_bucket_entry

  !> Test removing overflow entry
  subroutine test_remove_overflow_entry()
    type(hsd_name_index_t) :: ht
    integer :: i, idx
    character(len=10) :: key

    call ht%init(capacity=2)

    ! Create overflow entries
    do i = 1, 15
      write(key, '(A,I0)') "ov", i
      call ht%insert(trim(key), i * 50)
    end do

    ! Remove an overflow entry
    call ht%remove("ov10")
    idx = ht%lookup("ov10")
    call check(idx == 0, msg="Removed overflow entry")

    ! Check others
    idx = ht%lookup("ov11")
    call check(idx > 0, msg="Adjacent entry exists")

    call ht%destroy()
  end subroutine test_remove_overflow_entry

  !> Stress test with large number of entries
  subroutine test_large_table_stress()
    type(hsd_name_index_t) :: ht
    integer :: i, idx, failures
    character(len=20) :: key

    call ht%init(capacity=16)
    failures = 0

    ! Insert 1000 entries
    do i = 1, 1000
      write(key, '(A,I0)') "stress_", i
      call ht%insert(trim(key), i)
    end do

    ! Verify random samples
    do i = 1, 1000, 100
      write(key, '(A,I0)') "stress_", i
      idx = ht%lookup(trim(key))
      if (idx /= i) failures = failures + 1
    end do

    call check(failures == 0, msg="Large table stress test")

    call ht%destroy()
  end subroutine test_large_table_stress

end module test_hash_table_suite
