!> Tests for hsd_set_processed: marking nodes as processed
module test_set_processed_suite
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
        suite("set_processed", test_list([&
            test("non_recursive", test_non_recursive), &
            test("recursive_flat", test_recursive_flat), &
            test("recursive_nested", test_recursive_nested), &
            test("default_not_recursive", test_default_not_recursive) &
        ])) &
    ])
  end function tests


  !> Non-recursive: only the target table is marked
  subroutine test_non_recursive()
    type(hsd_table) :: root, child_tbl
    class(hsd_node), pointer :: cptr

    call new_table(root, name="root")
    call hsd_set(root, "Key", 42)
    call new_table(child_tbl, name="Sub")
    call hsd_set(child_tbl, "Inner", 1)
    call root%add_child(child_tbl)

    ! Precondition: nothing is processed
    call check(.not. root%processed, msg="root starts unprocessed")

    call hsd_set_processed(root, recursive=.false.)

    call check(root%processed, msg="root should be processed")

    ! Children should NOT be processed
    call hsd_get_child(root, "Key", cptr)
    call check(.not. cptr%processed, msg="Key should stay unprocessed")

    call hsd_get_child(root, "Sub", cptr)
    call check(.not. cptr%processed, msg="Sub table should stay unprocessed")

    call root%destroy()
  end subroutine test_non_recursive


  !> Recursive on a flat table: all values get marked
  subroutine test_recursive_flat()
    type(hsd_table) :: root
    class(hsd_node), pointer :: cptr

    call new_table(root, name="root")
    call hsd_set(root, "A", 1)
    call hsd_set(root, "B", 2)
    call hsd_set(root, "C", 3)

    call hsd_set_processed(root, recursive=.true.)

    call check(root%processed, msg="root processed")

    call hsd_get_child(root, "A", cptr)
    call check(cptr%processed, msg="A processed")
    call hsd_get_child(root, "B", cptr)
    call check(cptr%processed, msg="B processed")
    call hsd_get_child(root, "C", cptr)
    call check(cptr%processed, msg="C processed")

    call root%destroy()
  end subroutine test_recursive_flat


  !> Recursive on a nested tree: everything is marked
  subroutine test_recursive_nested()
    type(hsd_table) :: root, lvl1, lvl2
    class(hsd_node), pointer :: cptr

    call new_table(root, name="root")
    call hsd_set(root, "TopVal", "hello")

    call new_table(lvl1, name="Level1")
    call hsd_set(lvl1, "Mid", 10)

    call new_table(lvl2, name="Level2")
    call hsd_set(lvl2, "Deep", 99)
    call lvl1%add_child(lvl2)
    call root%add_child(lvl1)

    call hsd_set_processed(root, recursive=.true.)

    ! Everything should be processed
    call check(root%processed, msg="root processed")

    call hsd_get_child(root, "TopVal", cptr)
    call check(cptr%processed, msg="TopVal processed")

    call hsd_get_child(root, "Level1", cptr)
    call check(cptr%processed, msg="Level1 processed")

    call hsd_get_child(root, "Level1/Mid", cptr)
    call check(cptr%processed, msg="Level1/Mid processed")

    call hsd_get_child(root, "Level1/Level2", cptr)
    call check(cptr%processed, msg="Level1/Level2 processed")

    call hsd_get_child(root, "Level1/Level2/Deep", cptr)
    call check(cptr%processed, msg="Level1/Level2/Deep processed")

    call root%destroy()
  end subroutine test_recursive_nested


  !> Omitting the recursive argument defaults to non-recursive
  subroutine test_default_not_recursive()
    type(hsd_table) :: root
    class(hsd_node), pointer :: cptr

    call new_table(root, name="root")
    call hsd_set(root, "Val", 5)

    call hsd_set_processed(root)

    call check(root%processed, msg="root processed with default")

    call hsd_get_child(root, "Val", cptr)
    call check(.not. cptr%processed, msg="Val should stay unprocessed with default")

    call root%destroy()
  end subroutine test_default_not_recursive

end module test_set_processed_suite
