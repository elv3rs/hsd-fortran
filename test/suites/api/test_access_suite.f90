!> Tests for hsd_access_t — the primary user-facing API
module test_access_suite
  use hsd
  use hsd_api, only: hsd_get, hsd_set
  use fortuno_serial, only: is_equal, test => serial_case_item, &
      & check => serial_check, suite => serial_suite_item, test_list
  implicit none (type, external)
  private
  public :: tests

contains

  function tests()
    type(test_list) :: tests
    tests = test_list([&
        suite("access", test_list([&
            test("init_defaults", test_init_defaults), &
            test("init_custom", test_init_custom), &
            test("get_integer", test_get_integer), &
            test("get_real", test_get_real), &
            test("get_string", test_get_string), &
            test("get_logical", test_get_logical), &
            test("get_complex", test_get_complex), &
            test("get_integer_array", test_get_integer_array), &
            test("get_real_array", test_get_real_array), &
            test("get_logical_array", test_get_logical_array), &
            test("get_string_array", test_get_string_array), &
            test("get_complex_array", test_get_complex_array), &
            test("get_matrix_real", test_get_matrix_real), &
            test("get_integer_default", test_get_integer_default), &
            test("get_string_default", test_get_string_default), &
            test("get_real_default", test_get_real_default), &
            test("get_logical_default", test_get_logical_default), &
            test("get_default_writes_to_tree", &
                & test_get_default_writes_to_tree), &
            test("get_default_return_only", &
                & test_get_default_return_only), &
            test("missing_required_pushes_error", &
                & test_missing_required_pushes_error), &
            test("type_mismatch_pushes_error", &
                & test_type_mismatch_pushes_error), &
            test("multiple_errors_accumulate", &
                & test_multiple_errors_accumulate), &
            test("clear_errors", test_clear_errors), &
            test("get_errors_returns_copy", &
                & test_get_errors_returns_copy), &
            test("print_errors", test_print_errors), &
            test("set_integer", test_set_integer), &
            test("set_string", test_set_string), &
            test("set_real", test_set_real), &
            test("set_logical", test_set_logical), &
            test("set_integer_array", test_set_integer_array), &
            test("set_real_array", test_set_real_array), &
            test("set_matrix", test_set_matrix), &
            test("mark_processed_on", test_mark_processed_on), &
            test("mark_processed_off", test_mark_processed_off), &
            test("get_choice", test_get_choice), &
            test("get_choice_missing", test_get_choice_missing), &
            test("array_default", test_array_default), &
            test("inline_text_value", test_inline_text_value), &
            test("nested_path", test_nested_path) &
        ]))&
    ])
  end function tests

  ! ===== Init tests =====

  subroutine test_init_defaults()
    type(hsd_node_t), target :: root
    type(hsd_access_t) :: acc

    call hsd_load_string("Foo = 1", root)
    call acc%init(root)

    call check(associated(acc%root), msg="root is associated")
    call check(acc%mark_processed, msg="mark_processed defaults to true")
    call check(acc%on_missing == HSD_ON_MISSING_SET, &
        & msg="on_missing defaults to SET")
    call check(.not. acc%has_errors(), msg="no errors initially")
    call root%destroy()
  end subroutine test_init_defaults

  subroutine test_init_custom()
    type(hsd_node_t), target :: root
    type(hsd_access_t) :: acc

    call hsd_load_string("Foo = 1", root)
    call acc%init(root, mark_processed=.false., &
        & on_missing=HSD_ON_MISSING_RETURN)

    call check(.not. acc%mark_processed, &
        & msg="mark_processed set to false")
    call check(acc%on_missing == HSD_ON_MISSING_RETURN, &
        & msg="on_missing set to RETURN")
    call root%destroy()
  end subroutine test_init_custom

  ! ===== Scalar getter tests =====

  subroutine test_get_integer()
    type(hsd_node_t), target :: root
    type(hsd_access_t) :: acc
    integer :: val

    call hsd_load_string("Count = 42", root)
    call acc%init(root)
    call acc%get("Count", val)

    call check(.not. acc%has_errors(), msg="no errors")
    call check(val == 42, msg="got integer value")
    call root%destroy()
  end subroutine test_get_integer

  subroutine test_get_real()
    type(hsd_node_t), target :: root
    type(hsd_access_t) :: acc
    real(dp) :: val

    call hsd_load_string("Tolerance = 1.5e-3", root)
    call acc%init(root)
    call acc%get("Tolerance", val)

    call check(.not. acc%has_errors(), msg="no errors")
    call check(abs(val - 1.5e-3_dp) < 1.0e-10_dp, msg="got real value")
    call root%destroy()
  end subroutine test_get_real

  subroutine test_get_string()
    type(hsd_node_t), target :: root
    type(hsd_access_t) :: acc
    character(len=:), allocatable :: val

    call hsd_load_string('Name = "Hello World"', root)
    call acc%init(root)
    call acc%get("Name", val)

    call check(.not. acc%has_errors(), msg="no errors")
    call check(val == "Hello World", msg="got string value")
    call root%destroy()
  end subroutine test_get_string

  subroutine test_get_logical()
    type(hsd_node_t), target :: root
    type(hsd_access_t) :: acc
    logical :: val

    call hsd_load_string("Enabled = Yes", root)
    call acc%init(root)
    call acc%get("Enabled", val)

    call check(.not. acc%has_errors(), msg="no errors")
    call check(val, msg="got logical true")
    call root%destroy()
  end subroutine test_get_logical

  subroutine test_get_complex()
    type(hsd_node_t), target :: root
    type(hsd_access_t) :: acc
    complex(dp) :: val

    call hsd_load_string("Z = 3.0+4.0i", root)
    call acc%init(root)
    call acc%get("Z", val)

    call check(.not. acc%has_errors(), msg="no errors")
    call check(abs(real(val) - 3.0_dp) < 1.0e-10_dp, &
        & msg="real part")
    call check(abs(aimag(val) - 4.0_dp) < 1.0e-10_dp, &
        & msg="imaginary part")
    call root%destroy()
  end subroutine test_get_complex

  ! ===== Array getter tests =====

  subroutine test_get_integer_array()
    type(hsd_node_t), target :: root
    type(hsd_access_t) :: acc
    integer, allocatable :: val(:)

    call hsd_load_string("Nums = 1 2 3 4", root)
    call acc%init(root)
    call acc%get("Nums", val)

    call check(.not. acc%has_errors(), msg="no errors")
    call check(size(val) == 4, msg="array size")
    call check(val(1) == 1, msg="first element")
    call check(val(4) == 4, msg="last element")
    call root%destroy()
  end subroutine test_get_integer_array

  subroutine test_get_real_array()
    type(hsd_node_t), target :: root
    type(hsd_access_t) :: acc
    real(dp), allocatable :: val(:)

    call hsd_load_string("Coords = 1.0 2.5 3.7", root)
    call acc%init(root)
    call acc%get("Coords", val)

    call check(.not. acc%has_errors(), msg="no errors")
    call check(size(val) == 3, msg="array size")
    call check(abs(val(2) - 2.5_dp) < 1.0e-10_dp, msg="second element")
    call root%destroy()
  end subroutine test_get_real_array

  subroutine test_get_logical_array()
    type(hsd_node_t), target :: root
    type(hsd_access_t) :: acc
    logical, allocatable :: val(:)

    call hsd_load_string("Flags = Yes No Yes", root)
    call acc%init(root)
    call acc%get("Flags", val)

    call check(.not. acc%has_errors(), msg="no errors")
    call check(size(val) == 3, msg="array size")
    call check(val(1), msg="first true")
    call check(.not. val(2), msg="second false")
    call root%destroy()
  end subroutine test_get_logical_array

  subroutine test_get_string_array()
    type(hsd_node_t), target :: root
    type(hsd_access_t) :: acc
    character(len=:), allocatable :: val(:)

    call hsd_load_string("Tags = alpha beta gamma", root)
    call acc%init(root)
    call acc%get("Tags", val)

    call check(.not. acc%has_errors(), msg="no errors")
    call check(size(val) == 3, msg="array size")
    call check(val(1) == "alpha", msg="first element")
    call root%destroy()
  end subroutine test_get_string_array

  subroutine test_get_complex_array()
    type(hsd_node_t), target :: root
    type(hsd_access_t) :: acc
    complex(dp), allocatable :: val(:)

    call hsd_load_string("Zs = 1+2i 3+4i", root)
    call acc%init(root)
    call acc%get("Zs", val)

    call check(.not. acc%has_errors(), msg="no errors")
    call check(size(val) == 2, msg="array size")
    call root%destroy()
  end subroutine test_get_complex_array

  ! ===== Matrix getter tests =====

  subroutine test_get_matrix_real()
    type(hsd_node_t), target :: root
    type(hsd_access_t) :: acc
    real(dp), allocatable :: mat(:,:)
    integer :: nr, nc
    character(len=:), allocatable :: input

    input = 'Matrix {' // char(10) // &
        & '  1.0 2.0' // char(10) // &
        & '  3.0 4.0' // char(10) // &
        & '}'
    call hsd_load_string(input, root)
    call acc%init(root)
    call acc%get_matrix("Matrix", mat, nr, nc)

    call check(.not. acc%has_errors(), msg="no errors")
    call check(nr == 2, msg="nrows")
    call check(nc == 2, msg="ncols")
    call check(abs(mat(1,1) - 1.0_dp) < 1.0e-10_dp, msg="(1,1)")
    call check(abs(mat(2,2) - 4.0_dp) < 1.0e-10_dp, msg="(2,2)")
    call root%destroy()
  end subroutine test_get_matrix_real

  ! ===== Default handling =====

  subroutine test_get_integer_default()
    type(hsd_node_t), target :: root
    type(hsd_access_t) :: acc
    integer :: val

    call hsd_load_string("Other = 10", root)
    call acc%init(root)
    call acc%get("Missing", val, default=99)

    call check(.not. acc%has_errors(), msg="no error with default")
    call check(val == 99, msg="got default value")
    call root%destroy()
  end subroutine test_get_integer_default

  subroutine test_get_string_default()
    type(hsd_node_t), target :: root
    type(hsd_access_t) :: acc
    character(len=:), allocatable :: val

    call hsd_load_string("Other = 10", root)
    call acc%init(root)
    call acc%get("Missing", val, default="fallback")

    call check(.not. acc%has_errors(), msg="no error with default")
    call check(val == "fallback", msg="got default string")
    call root%destroy()
  end subroutine test_get_string_default

  subroutine test_get_real_default()
    type(hsd_node_t), target :: root
    type(hsd_access_t) :: acc
    real(dp) :: val

    call hsd_load_string("Other = 10", root)
    call acc%init(root)
    call acc%get("Missing", val, default=3.14_dp)

    call check(.not. acc%has_errors(), msg="no error with default")
    call check(abs(val - 3.14_dp) < 1.0e-10_dp, msg="got default real")
    call root%destroy()
  end subroutine test_get_real_default

  subroutine test_get_logical_default()
    type(hsd_node_t), target :: root
    type(hsd_access_t) :: acc
    logical :: val

    call hsd_load_string("Other = 10", root)
    call acc%init(root)
    call acc%get("Missing", val, default=.true.)

    call check(.not. acc%has_errors(), msg="no error with default")
    call check(val, msg="got default logical")
    call root%destroy()
  end subroutine test_get_logical_default

  subroutine test_get_default_writes_to_tree()
    type(hsd_node_t), target :: root
    type(hsd_access_t) :: acc
    integer :: val, readback

    call hsd_load_string("Other = 10", root)
    call acc%init(root, on_missing=HSD_ON_MISSING_SET)
    call acc%get("NewKey", val, default=42)

    ! Verify default was written to tree
    call hsd_get(root, "NewKey", readback)
    call check(readback == 42, msg="default written to tree")
    call root%destroy()
  end subroutine test_get_default_writes_to_tree

  subroutine test_get_default_return_only()
    type(hsd_node_t), target :: root
    type(hsd_access_t) :: acc
    integer :: val

    call hsd_load_string("Other = 10", root)
    call acc%init(root, on_missing=HSD_ON_MISSING_RETURN)
    call acc%get("NewKey", val, default=42)

    ! Verify default was NOT written to tree
    call check(.not. hsd_has_child(root, "NewKey"), &
        & msg="default not written to tree")
    call check(val == 42, msg="default still returned")
    call root%destroy()
  end subroutine test_get_default_return_only

  ! ===== Error accumulation =====

  subroutine test_missing_required_pushes_error()
    type(hsd_node_t), target :: root
    type(hsd_access_t) :: acc
    integer :: val

    call hsd_load_string("Other = 10", root)
    call acc%init(root)
    call acc%get("Missing", val)

    call check(acc%has_errors(), msg="error pushed for missing field")
    call check(acc%error_count() == 1, msg="exactly 1 error")
    call root%destroy()
  end subroutine test_missing_required_pushes_error

  subroutine test_type_mismatch_pushes_error()
    type(hsd_node_t), target :: root
    type(hsd_access_t) :: acc
    integer :: val

    call hsd_load_string("Name = hello", root)
    call acc%init(root)
    call acc%get("Name", val)

    call check(acc%has_errors(), msg="error pushed for type mismatch")
    call root%destroy()
  end subroutine test_type_mismatch_pushes_error

  subroutine test_multiple_errors_accumulate()
    type(hsd_node_t), target :: root
    type(hsd_access_t) :: acc
    integer :: ival
    real(dp) :: rval
    character(len=:), allocatable :: sval

    call hsd_load_string("Other = 10", root)
    call acc%init(root)
    call acc%get("Missing1", ival)
    call acc%get("Missing2", rval)
    call acc%get("Missing3", sval)

    call check(acc%error_count() == 3, msg="3 errors accumulated")
    call root%destroy()
  end subroutine test_multiple_errors_accumulate

  subroutine test_clear_errors()
    type(hsd_node_t), target :: root
    type(hsd_access_t) :: acc
    integer :: val

    call hsd_load_string("Other = 10", root)
    call acc%init(root)
    call acc%get("Missing", val)
    call check(acc%has_errors(), msg="has errors before clear")

    call acc%clear_errors()
    call check(.not. acc%has_errors(), msg="no errors after clear")
    call check(acc%error_count() == 0, msg="count is 0")
    call root%destroy()
  end subroutine test_clear_errors

  subroutine test_get_errors_returns_copy()
    type(hsd_node_t), target :: root
    type(hsd_access_t) :: acc
    type(hsd_error_entry_t), allocatable :: errs(:)
    integer :: val

    call hsd_load_string("Other = 10", root)
    call acc%init(root)
    call acc%get("Missing", val)
    call acc%get_errors(errs)

    call check(size(errs) == 1, msg="got 1 error entry")
    call check(errs(1)%path == "Missing", msg="error path correct")
    call check(errs(1)%stat == HSD_STAT_NOT_FOUND, &
        & msg="error stat correct")
    call root%destroy()
  end subroutine test_get_errors_returns_copy

  subroutine test_print_errors()
    type(hsd_node_t), target :: root
    type(hsd_access_t) :: acc
    integer :: val, iu
    character(len=256) :: line

    call hsd_load_string("Other = 10", root)
    call acc%init(root)
    call acc%get("TestPath", val)

    ! Print to a temporary file and verify content
    open(newunit=iu, status="scratch", action="readwrite")
    call acc%print_errors(unit=iu)
    rewind(iu)
    read(iu, '(A)') line
    close(iu)

    call check(index(line, "TestPath") > 0, &
        & msg="error output contains path")
    call root%destroy()
  end subroutine test_print_errors

  ! ===== Setter tests =====

  subroutine test_set_integer()
    type(hsd_node_t), target :: root
    type(hsd_access_t) :: acc
    integer :: val

    call hsd_load_string("X = 0", root)
    call acc%init(root)
    call acc%set("X", 99)
    call acc%get("X", val)

    call check(.not. acc%has_errors(), msg="no errors")
    call check(val == 99, msg="set integer value")
    call root%destroy()
  end subroutine test_set_integer

  subroutine test_set_string()
    type(hsd_node_t), target :: root
    type(hsd_access_t) :: acc
    character(len=:), allocatable :: val

    call hsd_load_string("X = old", root)
    call acc%init(root)
    call acc%set("X", "new")
    call acc%get("X", val)

    call check(.not. acc%has_errors(), msg="no errors")
    call check(val == "new", msg="set string value")
    call root%destroy()
  end subroutine test_set_string

  subroutine test_set_real()
    type(hsd_node_t), target :: root
    type(hsd_access_t) :: acc
    real(dp) :: val

    call hsd_load_string("X = 0.0", root)
    call acc%init(root)
    call acc%set("X", 2.718_dp)
    call acc%get("X", val)

    call check(.not. acc%has_errors(), msg="no errors")
    call check(abs(val - 2.718_dp) < 1.0e-10_dp, msg="set real value")
    call root%destroy()
  end subroutine test_set_real

  subroutine test_set_logical()
    type(hsd_node_t), target :: root
    type(hsd_access_t) :: acc
    logical :: val

    call hsd_load_string("X = No", root)
    call acc%init(root)
    call acc%set("X", .true.)
    call acc%get("X", val)

    call check(.not. acc%has_errors(), msg="no errors")
    call check(val, msg="set logical value")
    call root%destroy()
  end subroutine test_set_logical

  subroutine test_set_integer_array()
    type(hsd_node_t), target :: root
    type(hsd_access_t) :: acc
    integer, allocatable :: val(:)

    call hsd_load_string("Nums = 0", root)
    call acc%init(root)
    call acc%set("Nums", [10, 20, 30])
    call acc%get("Nums", val)

    call check(.not. acc%has_errors(), msg="no errors")
    call check(size(val) == 3, msg="array size")
    call check(val(2) == 20, msg="second element")
    call root%destroy()
  end subroutine test_set_integer_array

  subroutine test_set_real_array()
    type(hsd_node_t), target :: root
    type(hsd_access_t) :: acc
    real(dp), allocatable :: val(:)

    call hsd_load_string("Vals = 0", root)
    call acc%init(root)
    call acc%set("Vals", [1.1_dp, 2.2_dp])
    call acc%get("Vals", val)

    call check(.not. acc%has_errors(), msg="no errors")
    call check(size(val) == 2, msg="array size")
    call root%destroy()
  end subroutine test_set_real_array

  subroutine test_set_matrix()
    type(hsd_node_t), target :: root
    type(hsd_access_t) :: acc
    real(dp), allocatable :: mat(:,:)
    integer :: nr, nc

    call hsd_load_string("M = 0", root)
    call acc%init(root)
    call acc%set("M", reshape([1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp], &
        & [2, 2]))
    call acc%get_matrix("M", mat, nr, nc)

    call check(.not. acc%has_errors(), msg="no errors")
    call check(nr == 2, msg="nrows")
    call check(nc == 2, msg="ncols")
    call root%destroy()
  end subroutine test_set_matrix

  ! ===== Processed flag tests =====

  subroutine test_mark_processed_on()
    type(hsd_node_t), target :: root
    type(hsd_access_t) :: acc
    type(hsd_node_t), pointer :: child
    integer :: val, stat

    call hsd_load_string("X = 42", root)
    call acc%init(root, mark_processed=.true.)
    call acc%get("X", val)

    call hsd_get_child(root, "X", child, stat)
    call check(child%processed, msg="child marked processed")
    call root%destroy()
  end subroutine test_mark_processed_on

  subroutine test_mark_processed_off()
    type(hsd_node_t), target :: root
    type(hsd_access_t) :: acc
    type(hsd_node_t), pointer :: child
    integer :: val, stat

    call hsd_load_string("X = 42", root)
    call acc%init(root, mark_processed=.false.)
    call acc%get("X", val)

    call hsd_get_child(root, "X", child, stat)
    call check(.not. child%processed, msg="child NOT marked processed")
    call root%destroy()
  end subroutine test_mark_processed_off

  ! ===== Choice tests =====

  subroutine test_get_choice()
    type(hsd_node_t), target :: root
    type(hsd_access_t) :: acc
    character(len=:), allocatable :: choice_name
    type(hsd_node_t), pointer :: choice_table
    character(len=:), allocatable :: input

    input = 'Method {' // char(10) // &
        & '  DFTB {' // char(10) // &
        & '    SCC = Yes' // char(10) // &
        & '  }' // char(10) // &
        & '}'
    call hsd_load_string(input, root)
    call acc%init(root)
    call acc%get_choice("Method", choice_name, choice_table)

    call check(.not. acc%has_errors(), msg="no errors")
    call check(choice_name == "dftb", msg="choice name is dftb")
    call check(associated(choice_table), msg="choice table returned")
    call root%destroy()
  end subroutine test_get_choice

  subroutine test_get_choice_missing()
    type(hsd_node_t), target :: root
    type(hsd_access_t) :: acc
    character(len=:), allocatable :: choice_name
    type(hsd_node_t), pointer :: choice_table

    call hsd_load_string("Other = 1", root)
    call acc%init(root)
    call acc%get_choice("Missing", choice_name, choice_table)

    call check(acc%has_errors(), msg="error for missing choice")
    call check(.not. associated(choice_table), &
        & msg="null table for missing")
    call root%destroy()
  end subroutine test_get_choice_missing

  ! ===== Array default tests =====

  subroutine test_array_default()
    type(hsd_node_t), target :: root
    type(hsd_access_t) :: acc
    integer, allocatable :: val(:)

    call hsd_load_string("Other = 10", root)
    call acc%init(root)
    call acc%get("Missing", val, default=[1, 2, 3])

    call check(.not. acc%has_errors(), msg="no error with array default")
    call check(size(val) == 3, msg="got default array size")
    call check(val(2) == 2, msg="got default array value")
    call root%destroy()
  end subroutine test_array_default

  ! ===== Inline text value test =====

  subroutine test_inline_text_value()
    type(hsd_node_t), target :: root
    type(hsd_access_t) :: acc
    integer :: val
    character(len=:), allocatable :: input

    input = 'Block {' // char(10) // &
        & '  Value = 42' // char(10) // &
        & '}'
    call hsd_load_string(input, root)
    call acc%init(root)
    call acc%get("Block/Value", val)

    call check(.not. acc%has_errors(), msg="no errors")
    call check(val == 42, msg="got nested value")
    call root%destroy()
  end subroutine test_inline_text_value

  ! ===== Nested path test =====

  subroutine test_nested_path()
    type(hsd_node_t), target :: root
    type(hsd_access_t) :: acc
    integer :: val
    character(len=:), allocatable :: input

    input = 'A {' // char(10) // &
        & '  B {' // char(10) // &
        & '    C = 99' // char(10) // &
        & '  }' // char(10) // &
        & '}'
    call hsd_load_string(input, root)
    call acc%init(root)
    call acc%get("A/B/C", val)

    call check(.not. acc%has_errors(), msg="no errors")
    call check(val == 99, msg="got deeply nested value")
    call root%destroy()
  end subroutine test_nested_path

end module test_access_suite
