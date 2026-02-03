!> Parser unit tests using Fortuno framework
module test_parser_suite
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

  !> Returns all parser tests
  function tests()
    type(test_list) :: tests

    tests = test_list([&
        suite("parser", test_list([&
            test("simple_parse", test_simple_parse), &
            test("attributes", test_attributes), &
            test("nested_structure", test_nested_structure), &
            test("integer_values", test_integer_values), &
            test("real_values", test_real_values), &
            test("boolean_yes_no", test_boolean_yes_no), &
            test("string_values", test_string_values), &
            test("path_access", test_path_access), &
            test("empty_block", test_empty_block), &
            test("equal_syntax", test_equal_syntax), &
            test("multiple_children", test_multiple_children) &
        ])) &
    ])

  end function tests


  !> Test simple key-value parsing
  subroutine test_simple_parse()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    integer :: int_val
    integer :: stat

    call hsd_load_string("max_steps = 100", root, error)

    call check(.not. allocated(error), msg="No parse error")
    call check(root%num_children > 0, msg="Root has children")

    call hsd_get(root, "max_steps", int_val, stat)
    call check(is_equal(stat, 0), msg="Can get max_steps value")
    call check(is_equal(int_val, 100), msg="max_steps value is 100")

    call root%destroy()

  end subroutine test_simple_parse


  !> Test attribute parsing
  subroutine test_attributes()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    class(hsd_node), pointer :: child
    real(dp) :: real_val
    integer :: stat

    call hsd_load_string("temperature [Kelvin] = 300.0", root, error)

    call check(.not. allocated(error), msg="No parse error")

    call hsd_get_child(root, "temperature", child, stat)
    call check(is_equal(stat, 0), msg="Can get temperature node")
    call check(associated(child), msg="Child is associated")

    if (associated(child)) then
      call check(child%has_attrib(), msg="Node has attribute")
      call check(child%get_attrib() == "Kelvin", msg="Attribute is 'Kelvin'")
    end if

    call hsd_get(root, "temperature", real_val, stat)
    call check(is_equal(stat, 0), msg="Can get temperature value")
    call check(abs(real_val - 300.0_dp) < 0.001_dp, msg="Temperature value is 300.0")

    call root%destroy()

  end subroutine test_attributes


  !> Test nested structure parsing
  subroutine test_nested_structure()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    logical :: bool_val
    integer :: stat
    character(len=:), allocatable :: hsd_input

    hsd_input = &
      "hamiltonian {" // char(10) // &
      "  dftb {" // char(10) // &
      "    scc = yes" // char(10) // &
      "  }" // char(10) // &
      "}"

    call hsd_load_string(hsd_input, root, error)

    call check(.not. allocated(error), msg="No parse error")

    call hsd_get(root, "hamiltonian/dftb/scc", bool_val, stat)
    call check(is_equal(stat, 0), msg="Can get nested scc value")
    call check(bool_val .eqv. .true., msg="scc value is true")

    call root%destroy()

  end subroutine test_nested_structure


  !> Test integer value parsing
  subroutine test_integer_values()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    integer :: val, stat
    character(len=:), allocatable :: hsd_input

    hsd_input = &
      "positive = 42" // char(10) // &
      "negative = -17" // char(10) // &
      "zero = 0"

    call hsd_load_string(hsd_input, root, error)
    call check(.not. allocated(error), msg="No parse error")

    call hsd_get(root, "positive", val, stat)
    call check(is_equal(val, 42), msg="Positive integer parsed correctly")

    call hsd_get(root, "negative", val, stat)
    call check(is_equal(val, -17), msg="Negative integer parsed correctly")

    call hsd_get(root, "zero", val, stat)
    call check(is_equal(val, 0), msg="Zero parsed correctly")

    call root%destroy()

  end subroutine test_integer_values


  !> Test real value parsing
  subroutine test_real_values()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    real(dp) :: val
    integer :: stat
    character(len=:), allocatable :: hsd_input

    hsd_input = &
      "decimal = 3.14159" // char(10) // &
      "scientific = 1.23e-4" // char(10) // &
      "negative = -2.5"

    call hsd_load_string(hsd_input, root, error)
    call check(.not. allocated(error), msg="No parse error")

    call hsd_get(root, "decimal", val, stat)
    call check(abs(val - 3.14159_dp) < 1.0e-5_dp, msg="Decimal real parsed correctly")

    call hsd_get(root, "scientific", val, stat)
    call check(abs(val - 1.23e-4_dp) < 1.0e-8_dp, msg="Scientific notation parsed correctly")

    call hsd_get(root, "negative", val, stat)
    call check(abs(val - (-2.5_dp)) < 1.0e-5_dp, msg="Negative real parsed correctly")

    call root%destroy()

  end subroutine test_real_values


  !> Test boolean yes/no parsing
  subroutine test_boolean_yes_no()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    logical :: val
    integer :: stat
    character(len=:), allocatable :: hsd_input

    hsd_input = &
      "flag_yes = yes" // char(10) // &
      "flag_Yes = Yes" // char(10) // &
      "flag_YES = YES" // char(10) // &
      "flag_no = no" // char(10) // &
      "flag_No = No" // char(10) // &
      "flag_NO = NO"

    call hsd_load_string(hsd_input, root, error)
    call check(.not. allocated(error), msg="No parse error")

    call hsd_get(root, "flag_yes", val, stat)
    call check(val .eqv. .true., msg="'yes' is true")

    call hsd_get(root, "flag_Yes", val, stat)
    call check(val .eqv. .true., msg="'Yes' is true")

    call hsd_get(root, "flag_YES", val, stat)
    call check(val .eqv. .true., msg="'YES' is true")

    call hsd_get(root, "flag_no", val, stat)
    call check(val .eqv. .false., msg="'no' is false")

    call hsd_get(root, "flag_No", val, stat)
    call check(val .eqv. .false., msg="'No' is false")

    call hsd_get(root, "flag_NO", val, stat)
    call check(val .eqv. .false., msg="'NO' is false")

    call root%destroy()

  end subroutine test_boolean_yes_no


  !> Test string value parsing
  subroutine test_string_values()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: val
    integer :: stat
    character(len=:), allocatable :: hsd_input

    hsd_input = &
      'simple = hello' // char(10) // &
      'quoted = "hello world"' // char(10) // &
      "single = 'with spaces'"

    call hsd_load_string(hsd_input, root, error)
    call check(.not. allocated(error), msg="No parse error")

    call hsd_get(root, "simple", val, stat)
    call check(val == "hello", msg="Simple string parsed correctly")

    call hsd_get(root, "quoted", val, stat)
    call check(val == "hello world", msg="Quoted string parsed correctly")

    call hsd_get(root, "single", val, stat)
    call check(val == "with spaces", msg="Single-quoted string parsed correctly")

    call root%destroy()

  end subroutine test_string_values


  !> Test path-based access
  subroutine test_path_access()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    integer :: val
    integer :: stat
    character(len=:), allocatable :: hsd_input

    hsd_input = &
      "a {" // char(10) // &
      "  b {" // char(10) // &
      "    c {" // char(10) // &
      "      value = 42" // char(10) // &
      "    }" // char(10) // &
      "  }" // char(10) // &
      "}"

    call hsd_load_string(hsd_input, root, error)
    call check(.not. allocated(error), msg="No parse error")

    call hsd_get(root, "a/b/c/value", val, stat)
    call check(is_equal(stat, 0), msg="Deep path access works")
    call check(is_equal(val, 42), msg="Deep path value is correct")

    call root%destroy()

  end subroutine test_path_access


  !> Test empty block parsing
  subroutine test_empty_block()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    class(hsd_node), pointer :: child
    integer :: stat

    call hsd_load_string("empty_block {}", root, error)

    call check(.not. allocated(error), msg="No parse error")

    call hsd_get_child(root, "empty_block", child, stat)
    call check(is_equal(stat, 0), msg="Can get empty block")
    call check(associated(child), msg="Empty block exists")

    call root%destroy()

  end subroutine test_empty_block


  !> Test equal-sign syntax for child assignment
  subroutine test_equal_syntax()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    integer :: val
    integer :: stat
    character(len=:), allocatable :: hsd_input

    ! This syntax assigns a child node using =
    hsd_input = &
      "hamiltonian = dftb {" // char(10) // &
      "  scc = yes" // char(10) // &
      "}"

    call hsd_load_string(hsd_input, root, error)
    call check(.not. allocated(error), msg="No parse error")

    call root%destroy()

  end subroutine test_equal_syntax


  !> Test multiple children with same level
  subroutine test_multiple_children()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    integer :: val1, val2, val3, stat
    character(len=:), allocatable :: hsd_input

    hsd_input = &
      "first = 1" // char(10) // &
      "second = 2" // char(10) // &
      "third = 3"

    call hsd_load_string(hsd_input, root, error)
    call check(.not. allocated(error), msg="No parse error")

    call hsd_get(root, "first", val1, stat)
    call hsd_get(root, "second", val2, stat)
    call hsd_get(root, "third", val3, stat)

    call check(is_equal(val1, 1), msg="First child value correct")
    call check(is_equal(val2, 2), msg="Second child value correct")
    call check(is_equal(val3, 3), msg="Third child value correct")

    call root%destroy()

  end subroutine test_multiple_children

end module test_parser_suite
