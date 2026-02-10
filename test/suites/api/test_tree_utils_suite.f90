!> Tests for upstream tree utilities:
!> - hsd_has_value_children
!> - hsd_get_inline_text
!> - hsd_get_name
module test_tree_utils_suite
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
        suite("tree_utils", test_list([&
            test("has_value_children_yes", test_has_value_children_yes), &
            test("has_value_children_no", test_has_value_children_no), &
            test("has_value_children_empty", test_has_value_children_empty), &
            test("get_inline_text_single", test_get_inline_text_single), &
            test("get_inline_text_multi", test_get_inline_text_multi), &
            test("get_inline_text_none", test_get_inline_text_none), &
            test("get_name_table", test_get_name_table), &
            test("get_name_value", test_get_name_value), &
            test("get_name_unnamed_default", test_get_name_unnamed_default), &
            test("get_name_unnamed_custom", test_get_name_unnamed_custom) &
        ])) &
    ])
  end function tests


  ! ---- hsd_has_value_children tests ----

  !> Table with value children should return .true.
  subroutine test_has_value_children_yes()
    type(hsd_table) :: root

    call new_table(root, name="root")
    call hsd_set(root, "Key", 42)

    call check(hsd_has_value_children(root), msg="should have value children")

    call root%destroy()
  end subroutine test_has_value_children_yes


  !> Table with only table children should return .false.
  subroutine test_has_value_children_no()
    type(hsd_table) :: root, sub

    call new_table(root, name="root")
    call new_table(sub, name="Sub")
    call hsd_set(sub, "Inner", 1)
    call root%add_child(sub)

    call check(.not. hsd_has_value_children(root), &
        & msg="should not have value children (only tables)")

    call root%destroy()
  end subroutine test_has_value_children_no


  !> Empty table should return .false.
  subroutine test_has_value_children_empty()
    type(hsd_table) :: root

    call new_table(root, name="root")

    call check(.not. hsd_has_value_children(root), &
        & msg="empty table should not have value children")

    call root%destroy()
  end subroutine test_has_value_children_empty


  ! ---- hsd_get_inline_text tests ----

  !> Table with a single unnamed text child
  subroutine test_get_inline_text_single()
    type(hsd_table) :: root
    type(hsd_value) :: txt
    character(len=:), allocatable :: text
    integer :: stat

    call new_table(root, name="root")
    call new_value(txt, name="#text")
    call txt%set_string("hello world")
    call root%add_child(txt)

    call hsd_get_inline_text(root, text, stat)

    call check(stat == HSD_STAT_OK, msg="stat should be OK")
    call check(text == "hello world", msg="text should match")

    call root%destroy()
  end subroutine test_get_inline_text_single


  !> Table with multiple unnamed text children should concatenate with spaces
  subroutine test_get_inline_text_multi()
    type(hsd_table) :: root
    type(hsd_value) :: t1, t2, t3
    character(len=:), allocatable :: text
    integer :: stat

    call new_table(root, name="root")
    call new_value(t1, name="#text")
    call t1%set_string("alpha")
    call root%add_child(t1)
    call new_value(t2)  ! unnamed
    call t2%set_string("beta")
    call root%add_child(t2)
    call new_value(t3, name="#text")
    call t3%set_string("gamma")
    call root%add_child(t3)

    call hsd_get_inline_text(root, text, stat)

    call check(stat == HSD_STAT_OK, msg="stat should be OK")
    call check(text == "alpha beta gamma", msg="text should be concatenated")

    call root%destroy()
  end subroutine test_get_inline_text_multi


  !> Table with no text children returns NOT_FOUND
  subroutine test_get_inline_text_none()
    type(hsd_table) :: root, sub
    character(len=:), allocatable :: text
    integer :: stat

    call new_table(root, name="root")
    call new_table(sub, name="Sub")
    call root%add_child(sub)

    call hsd_get_inline_text(root, text, stat)

    call check(stat == HSD_STAT_NOT_FOUND, msg="stat should be NOT_FOUND")
    call check(len(text) == 0, msg="text should be empty")

    call root%destroy()
  end subroutine test_get_inline_text_none


  ! ---- hsd_get_name tests ----

  !> Named table should return lowercased name
  subroutine test_get_name_table()
    type(hsd_table) :: tbl
    character(len=:), allocatable :: name

    call new_table(tbl, name="MyTable")

    call hsd_get_name(tbl, name)

    call check(name == "mytable", msg="should return lowercased table name")

    call tbl%destroy()
  end subroutine test_get_name_table


  !> Named value should return lowercased name
  subroutine test_get_name_value()
    type(hsd_value) :: val
    character(len=:), allocatable :: name

    call new_value(val, name="SomeKey")
    call val%set_integer(99)

    call hsd_get_name(val, name)

    call check(name == "somekey", msg="should return lowercased value name")
  end subroutine test_get_name_value


  !> Unnamed node with no explicit default should return ""
  subroutine test_get_name_unnamed_default()
    type(hsd_value) :: val
    character(len=:), allocatable :: name

    call new_value(val)
    call val%set_string("data")

    call hsd_get_name(val, name)

    call check(name == "", msg="unnamed node default should be empty string")
  end subroutine test_get_name_unnamed_default


  !> Unnamed node with explicit default should return that default
  subroutine test_get_name_unnamed_custom()
    type(hsd_value) :: val
    character(len=:), allocatable :: name

    call new_value(val)
    call val%set_string("data")

    call hsd_get_name(val, name, default="#text")

    call check(name == "#text", msg="unnamed node with custom default")
  end subroutine test_get_name_unnamed_custom

end module test_tree_utils_suite
