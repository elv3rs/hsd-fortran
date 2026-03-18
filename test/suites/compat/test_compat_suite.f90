!> Tests for the DFTB+ compatibility layer
module test_compat_suite
  use hsd
  use hsd_compat
  use fortuno_serial, only: test => serial_case_item, &
      & check => serial_check, suite => serial_suite_item, test_list
  implicit none (type, external)
  private
  public :: tests

contains

  function tests()
    type(test_list) :: tests
    tests = test_list([&
        suite("compat", test_list([&
            test("string_type", test_string_type), &
            test("getChildValue_real", test_get_child_value_real), &
            test("getChildValue_int", test_get_child_value_int), &
            test("getChildValue_logical", &
                & test_get_child_value_logical), &
            test("getChildValue_string", &
                & test_get_child_value_string), &
            test("getChildValue_default", &
                & test_get_child_value_default), &
            test("getChildValue_modifier", &
                & test_get_child_value_modifier), &
            test("getChild_found", test_get_child_found), &
            test("getChild_not_requested", &
                & test_get_child_not_requested), &
            test("getChild_emptyIfMissing", &
                & test_get_child_empty_if_missing), &
            test("setChildValue_real", test_set_child_value_real), &
            test("setChildValue_replace", &
                & test_set_child_value_replace), &
            test("setProcessed_compat", &
                & test_set_processed_compat), &
            test("warnUnprocessed_compat", &
                & test_warn_unprocessed_compat), &
            test("getNodeHSDName_compat", &
                & test_get_node_hsd_name), &
            test("getFirstTextChild_compat", &
                & test_get_first_text_child), &
            test("createElement_compat", &
                & test_create_element), &
            test("setChild_compat", test_set_child), &
            test("getChildValue_array", &
                & test_get_child_value_array), &
            test("setChildValue_string", &
                & test_set_child_value_string) &
        ]))&
    ])
  end function tests

  ! --- String type tests ---

  subroutine test_string_type()
    type(string) :: ss
    character(len=:), allocatable :: cc

    ss = "hello"
    call check(char(ss) == "hello", msg="char() extracts string")

    cc = ss
    call check(cc == "hello", msg="assignment to char works")
  end subroutine test_string_type

  ! --- getChildValue tests ---

  subroutine test_get_child_value_real()
    type(hsd_node) :: root
    type(hsd_error_t), allocatable :: error
    real(dp) :: val

    call hsd_load_string('Energy = 3.14', root, error)
    call check(.not. allocated(error), msg="parse ok")

    call getChildValue(root, "Energy", val)
    call check(abs(val - 3.14_dp) < 1.0e-10_dp, &
        & msg="real value extracted")
  end subroutine test_get_child_value_real

  subroutine test_get_child_value_int()
    type(hsd_node) :: root
    type(hsd_error_t), allocatable :: error
    integer :: val

    call hsd_load_string('Steps = 42', root, error)
    call check(.not. allocated(error), msg="parse ok")

    call getChildValue(root, "Steps", val)
    call check(val == 42, msg="integer value extracted")
  end subroutine test_get_child_value_int

  subroutine test_get_child_value_logical()
    type(hsd_node) :: root
    type(hsd_error_t), allocatable :: error
    logical :: val

    call hsd_load_string('Verbose = Yes', root, error)
    call check(.not. allocated(error), msg="parse ok")

    call getChildValue(root, "Verbose", val)
    call check(val, msg="logical value extracted")
  end subroutine test_get_child_value_logical

  subroutine test_get_child_value_string()
    type(hsd_node) :: root
    type(hsd_error_t), allocatable :: error
    type(string) :: val

    call hsd_load_string('Method = "DFTB"', root, error)
    call check(.not. allocated(error), msg="parse ok")

    call getChildValue(root, "Method", val)
    call check(char(val) == "DFTB", msg="string value extracted")
  end subroutine test_get_child_value_string

  subroutine test_get_child_value_default()
    type(hsd_node) :: root
    type(hsd_error_t), allocatable :: error
    real(dp) :: val

    call hsd_load_string('Dummy = 1.0', root, error)
    call check(.not. allocated(error), msg="parse ok")

    call getChildValue(root, "Missing", val, 99.0_dp)
    call check(abs(val - 99.0_dp) < 1.0e-10_dp, &
        & msg="default value used")
  end subroutine test_get_child_value_default

  subroutine test_get_child_value_modifier()
    type(hsd_node) :: root
    type(hsd_error_t), allocatable :: error
    real(dp) :: val
    type(string) :: modifier
    type(hsd_node), pointer :: child

    call hsd_load_string('Pressure [Pa] = 101325.0', root, error)
    call check(.not. allocated(error), msg="parse ok")

    call getChildValue(root, "Pressure", val, &
        & modifier=modifier, child=child)
    call check(abs(val - 101325.0_dp) < 1.0e-6_dp, &
        & msg="value with modifier")
    call check(char(modifier) == "Pa", &
        & msg="modifier extracted")
    call check(associated(child), msg="child pointer set")
  end subroutine test_get_child_value_modifier

  ! --- getChild tests ---

  subroutine test_get_child_found()
    type(hsd_node) :: root
    type(hsd_error_t), allocatable :: error
    type(hsd_node), pointer :: child

    call hsd_load_string('Driver { MaxSteps = 100 }', root, error)
    call check(.not. allocated(error), msg="parse ok")

    call getChild(root, "Driver", child)
    call check(associated(child), msg="child found")
    call check(child%processed, msg="child marked processed")
  end subroutine test_get_child_found

  subroutine test_get_child_not_requested()
    type(hsd_node) :: root
    type(hsd_error_t), allocatable :: error
    type(hsd_node), pointer :: child

    call hsd_load_string('Dummy = 1', root, error)
    call check(.not. allocated(error), msg="parse ok")

    call getChild(root, "Missing", child, requested=.false.)
    call check(.not. associated(child), &
        & msg="null when not requested and missing")
  end subroutine test_get_child_not_requested

  subroutine test_get_child_empty_if_missing()
    type(hsd_node) :: root
    type(hsd_error_t), allocatable :: error
    type(hsd_node), pointer :: child

    call hsd_load_string('Dummy = 1', root, error)
    call check(.not. allocated(error), msg="parse ok")

    call getChild(root, "NewBlock", child, emptyIfMissing=.true.)
    call check(associated(child), &
        & msg="empty child created when missing")
    call check(child%processed, &
        & msg="created child marked processed")
  end subroutine test_get_child_empty_if_missing

  ! --- setChildValue tests ---

  subroutine test_set_child_value_real()
    type(hsd_node) :: root
    type(hsd_error_t), allocatable :: error
    real(dp) :: val

    call hsd_load_string('Dummy = 1', root, error)
    call check(.not. allocated(error), msg="parse ok")

    call setChildValue(root, "Energy", 42.0_dp)

    call getChildValue(root, "Energy", val)
    call check(abs(val - 42.0_dp) < 1.0e-10_dp, &
        & msg="set then get real")
  end subroutine test_set_child_value_real

  subroutine test_set_child_value_replace()
    type(hsd_node) :: root
    type(hsd_error_t), allocatable :: error
    real(dp) :: val

    call hsd_load_string('Energy = 1.0', root, error)
    call check(.not. allocated(error), msg="parse ok")

    call setChildValue(root, "Energy", 99.0_dp, replace=.true.)

    call getChildValue(root, "Energy", val)
    call check(abs(val - 99.0_dp) < 1.0e-10_dp, &
        & msg="replaced value")
  end subroutine test_set_child_value_replace

  subroutine test_set_child_value_string()
    type(hsd_node) :: root
    type(hsd_error_t), allocatable :: error
    type(string) :: val

    call hsd_load_string('Dummy = 1', root, error)
    call check(.not. allocated(error), msg="parse ok")

    call setChildValue(root, "Method", "DFTB")

    call getChildValue(root, "Method", val)
    call check(char(val) == "DFTB", msg="set then get string")
  end subroutine test_set_child_value_string

  ! --- Processing tests ---

  subroutine test_set_processed_compat()
    type(hsd_node) :: root
    type(hsd_error_t), allocatable :: error
    type(hsd_node), pointer :: child

    call hsd_load_string('Block { A = 1' // char(10) // 'B = 2 }', &
        & root, error)
    call check(.not. allocated(error), msg="parse ok")

    call getChild(root, "Block", child)
    call setProcessed(child, recursive=.true.)
    call check(child%processed, msg="block marked processed")
  end subroutine test_set_processed_compat

  subroutine test_warn_unprocessed_compat()
    type(hsd_node) :: root
    type(hsd_error_t), allocatable :: error

    call hsd_load_string('A = 1', root, error)
    call check(.not. allocated(error), msg="parse ok")

    ! Should not crash with ignore flag
    call warnUnprocessedNodes(root, tIgnoreUnprocessed=.true.)
    call check(.true., msg="warnUnprocessed with ignore ok")
  end subroutine test_warn_unprocessed_compat

  ! --- Node info tests ---

  subroutine test_get_node_hsd_name()
    type(hsd_node) :: root
    type(hsd_error_t), allocatable :: error
    type(hsd_node), pointer :: child
    character(len=:), allocatable :: name

    call hsd_load_string('Driver { }', root, error)
    call check(.not. allocated(error), msg="parse ok")

    call getChild(root, "Driver", child)
    call getNodeHSDName(child, name)
    call check(name == "driver", msg="node name retrieved")
  end subroutine test_get_node_hsd_name

  subroutine test_get_first_text_child()
    type(hsd_node) :: root
    type(hsd_error_t), allocatable :: error
    type(hsd_node), pointer :: child
    character(len=:), allocatable :: text

    call hsd_load_string('Value = 42', root, error)
    call check(.not. allocated(error), msg="parse ok")

    call getChild(root, "Value", child)
    call getFirstTextChild(child, text)
    call check(text == "42", msg="text child extracted")
  end subroutine test_get_first_text_child

  ! --- DOM compat tests ---

  subroutine test_create_element()
    type(hsd_node) :: doc, elem
    real(dp) :: val

    call createDocumentNode(doc)
    elem = createElement("TestKey")
    call appendChild(doc, elem)

    call check(hsd_has_child(doc, "TestKey"), &
        & msg="element appended")
    call destroyNode(doc)
  end subroutine test_create_element

  subroutine test_set_child()
    type(hsd_node) :: root
    type(hsd_error_t), allocatable :: error
    type(hsd_node), pointer :: child

    call hsd_load_string('Dummy = 1', root, error)
    call check(.not. allocated(error), msg="parse ok")

    call setChild(root, "NewBlock", child)
    call check(associated(child), msg="child created")
    call check(hsd_has_child(root, "NewBlock"), &
        & msg="child exists in tree")
  end subroutine test_set_child

  ! --- Array tests ---

  subroutine test_get_child_value_array()
    type(hsd_node) :: root
    type(hsd_error_t), allocatable :: error
    real(dp), allocatable :: val(:)

    call hsd_load_string('Coords = 1.0 2.0 3.0', root, error)
    call check(.not. allocated(error), msg="parse ok")

    call getChildValue(root, "Coords", val)
    call check(size(val) == 3, msg="array size correct")
    call check(abs(val(1) - 1.0_dp) < 1.0e-10_dp, &
        & msg="array value 1")
    call check(abs(val(3) - 3.0_dp) < 1.0e-10_dp, &
        & msg="array value 3")
  end subroutine test_get_child_value_array

end module test_compat_suite
