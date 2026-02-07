!> Tests for newly added API features:
!> - Path normalization (F8)
!> - hsd_get_children (F6)
!> - hsd_get_array_with_unit / hsd_get_matrix_with_unit (F1)
module test_new_apis_suite
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
        suite("new_apis", test_list([&
            test("path_trailing_slash", test_path_trailing_slash), &
            test("path_leading_slash", test_path_leading_slash), &
            test("path_double_slash", test_path_double_slash), &
            test("path_empty", test_path_empty), &
            test("path_slash_only", test_path_slash_only), &
            test("get_children_basic", test_get_children_basic), &
            test("get_children_none", test_get_children_none), &
            test("get_children_nested", test_get_children_nested), &
            test("array_with_unit", test_array_with_unit), &
            test("array_with_unit_no_attrib", test_array_with_unit_no_attrib), &
            test("matrix_with_unit", test_matrix_with_unit) &
        ])) &
    ])
  end function tests

  ! ---- Path normalization tests ----

  subroutine test_path_trailing_slash()
    type(hsd_table) :: root
    class(hsd_node), pointer :: child
    integer :: stat

    call new_table(root)
    call hsd_set(root, "Geometry", "dummy")

    ! Trailing slash should still find the child
    call hsd_get_child(root, "Geometry/", child, stat)
    call check(stat == HSD_STAT_OK, msg="Trailing slash should work")
    call check(associated(child), msg="Child should be found")

    call root%destroy()
  end subroutine test_path_trailing_slash

  subroutine test_path_leading_slash()
    type(hsd_table) :: root
    class(hsd_node), pointer :: child
    integer :: stat

    call new_table(root)
    call hsd_set(root, "Geometry", "dummy")

    ! Leading slash should be stripped
    call hsd_get_child(root, "/Geometry", child, stat)
    call check(stat == HSD_STAT_OK, msg="Leading slash should work")
    call check(associated(child), msg="Child should be found")

    call root%destroy()
  end subroutine test_path_leading_slash

  subroutine test_path_double_slash()
    type(hsd_table) :: root, sub
    class(hsd_node), pointer :: child
    integer :: stat

    call new_table(root)
    call new_table(sub, name="Inner")
    call hsd_set(sub, "Key", 42)
    call root%add_child(sub)

    ! Double slash should be collapsed
    call hsd_get_child(root, "Inner//Key", child, stat)
    call check(stat == HSD_STAT_OK, msg="Double slash should be collapsed")
    call check(associated(child), msg="Child should be found through double slash")

    call root%destroy()
  end subroutine test_path_double_slash

  subroutine test_path_empty()
    type(hsd_table) :: root
    class(hsd_node), pointer :: child
    integer :: stat

    call new_table(root)
    call hsd_set(root, "Key", 1)

    ! Empty path should return NOT_FOUND
    call hsd_get_child(root, "", child, stat)
    call check(stat == HSD_STAT_NOT_FOUND, msg="Empty path should be NOT_FOUND")

    call root%destroy()
  end subroutine test_path_empty

  subroutine test_path_slash_only()
    type(hsd_table) :: root
    class(hsd_node), pointer :: child
    integer :: stat

    call new_table(root)
    call hsd_set(root, "Key", 1)

    ! Slash-only path normalizes to empty → NOT_FOUND
    call hsd_get_child(root, "///", child, stat)
    call check(stat == HSD_STAT_NOT_FOUND, msg="Slash-only path should be NOT_FOUND")

    call root%destroy()
  end subroutine test_path_slash_only

  ! ---- hsd_get_children tests ----

  subroutine test_get_children_basic()
    type(hsd_table) :: root, child1, child2, child3
    type(hsd_child_ptr), allocatable :: children(:)
    integer :: stat

    call new_table(root)
    call new_table(child1, name="Atom")
    call hsd_set(child1, "Element", "C")
    call root%add_child(child1)

    call new_table(child2, name="Atom")
    call hsd_set(child2, "Element", "H")
    call root%add_child(child2)

    call new_table(child3, name="Other")
    call root%add_child(child3)

    call hsd_get_children(root, "Atom", children, stat)
    call check(stat == HSD_STAT_OK, msg="Should find children")
    call check(size(children) == 2, msg="Should find exactly 2 'Atom' children")

    call root%destroy()
  end subroutine test_get_children_basic

  subroutine test_get_children_none()
    type(hsd_table) :: root
    type(hsd_child_ptr), allocatable :: children(:)
    integer :: stat

    call new_table(root)
    call hsd_set(root, "Key", 1)

    call hsd_get_children(root, "NonExistent", children, stat)
    call check(stat == HSD_STAT_OK, msg="Non-matching should return OK")
    call check(size(children) == 0, msg="Should find 0 children")

    call root%destroy()
  end subroutine test_get_children_none

  subroutine test_get_children_nested()
    type(hsd_table) :: root, parent, c1, c2
    type(hsd_child_ptr), allocatable :: children(:)
    integer :: stat

    call new_table(root)
    call new_table(parent, name="Geometry")

    call new_table(c1, name="Atom")
    call hsd_set(c1, "Element", "O")
    call parent%add_child(c1)

    call new_table(c2, name="Atom")
    call hsd_set(c2, "Element", "H")
    call parent%add_child(c2)

    call root%add_child(parent)

    ! Path-based lookup for nested children
    call hsd_get_children(root, "Geometry/Atom", children, stat)
    call check(stat == HSD_STAT_OK, msg="Should navigate nested path")
    call check(size(children) == 2, msg="Should find 2 nested 'Atom' children")

    call root%destroy()
  end subroutine test_get_children_nested

  ! ---- Unit conversion tests ----

  pure function kelvin_to_celsius(value, from_unit, to_unit) result(converted)
    real(dp), intent(in) :: value
    character(len=*), intent(in) :: from_unit, to_unit
    real(dp) :: converted
    ! Simple converter: Kelvin to Celsius
    if (from_unit == "Kelvin" .and. to_unit == "Celsius") then
      converted = value - 273.15_dp
    else
      converted = value  ! no conversion
    end if
  end function kelvin_to_celsius

  subroutine test_array_with_unit()
    type(hsd_table) :: root
    type(hsd_value) :: val
    real(dp), allocatable :: arr(:)
    integer :: stat

    call new_table(root)
    ! Create a value node with attrib (unit)
    call new_value(val, name="Temps")
    val%attrib = "Kelvin"
    val%value_type = VALUE_TYPE_ARRAY
    val%real_array = [300.0_dp, 373.15_dp, 273.15_dp]
    call root%add_child(val)

    call hsd_get_array_with_unit(root, "Temps", arr, "Celsius", kelvin_to_celsius, stat)

    call check(stat == HSD_STAT_OK, msg="Should get array with unit")
    call check(size(arr) == 3, msg="Should have 3 elements")
    ! 300 K = 26.85 C, 373.15 K = 100 C, 273.15 K = 0 C
    call check(abs(arr(1) - 26.85_dp) < 1.0e-10_dp, msg="First element should be 26.85")
    call check(abs(arr(2) - 100.0_dp) < 1.0e-10_dp, msg="Second element should be 100.0")
    call check(abs(arr(3) - 0.0_dp) < 1.0e-10_dp, msg="Third element should be 0.0")

    call root%destroy()
  end subroutine test_array_with_unit

  subroutine test_array_with_unit_no_attrib()
    type(hsd_table) :: root
    type(hsd_value) :: val
    real(dp), allocatable :: arr(:)
    integer :: stat

    call new_table(root)
    ! Create a value node WITHOUT attrib — should assume target unit (no conversion)
    call new_value(val, name="Temps")
    val%value_type = VALUE_TYPE_ARRAY
    val%real_array = [100.0_dp, 200.0_dp]
    call root%add_child(val)

    call hsd_get_array_with_unit(root, "Temps", arr, "Celsius", kelvin_to_celsius, stat)

    call check(stat == HSD_STAT_OK, msg="Should get array without unit attrib")
    call check(size(arr) == 2, msg="Should have 2 elements")
    ! No unit → target_unit assumed → no conversion
    call check(abs(arr(1) - 100.0_dp) < 1.0e-10_dp, msg="First element should be unchanged")
    call check(abs(arr(2) - 200.0_dp) < 1.0e-10_dp, msg="Second element should be unchanged")

    call root%destroy()
  end subroutine test_array_with_unit_no_attrib

  subroutine test_matrix_with_unit()
    type(hsd_table) :: root
    type(hsd_value) :: val
    real(dp), allocatable :: mat(:,:)
    integer :: nrows, ncols, stat

    call new_table(root)
    call new_value(val, name="Data")
    val%attrib = "Kelvin"
    val%value_type = VALUE_TYPE_ARRAY
    val%real_matrix = reshape([300.0_dp, 373.15_dp, &
        273.15_dp, 400.0_dp, 500.0_dp, 600.0_dp], [2, 3])
    val%nrows = 2
    val%ncols = 3
    call root%add_child(val)

    call hsd_get_matrix_with_unit(root, "Data", mat, nrows, ncols, &
        "Celsius", kelvin_to_celsius, stat)

    call check(stat == HSD_STAT_OK, msg="Should get matrix with unit")
    call check(nrows == 2 .and. ncols == 3, msg="Should be 2x3 matrix")
    call check(abs(mat(1,1) - 26.85_dp) < 1.0e-10_dp, msg="(1,1) should be 26.85")
    call check(abs(mat(2,1) - 100.0_dp) < 1.0e-10_dp, msg="(2,1) should be 100.0")

    call root%destroy()
  end subroutine test_matrix_with_unit

end module test_new_apis_suite
