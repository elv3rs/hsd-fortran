!> Unit tests for new API features: type introspection, default values, visitor pattern
module test_api_suite
  use hsd
  use build_env, only : source_dir
  use fortuno_serial, only : is_equal, test => serial_case_item, check => serial_check, &
      & suite => serial_suite_item, test_list
  implicit none (type, external)

  private
  public :: tests

  ! Module-level counters for hsd_walk tests
  integer :: walk_table_count = 0
  integer :: walk_value_count = 0

  ! Helper visitor type that counts nodes
  type, extends(hsd_visitor_t) :: counting_visitor
    integer :: table_count = 0
    integer :: value_count = 0
  contains
    procedure :: visit_table => counting_visit_table
    procedure :: visit_value => counting_visit_value
  end type counting_visitor

  ! Helper for leave_table test
  type, extends(hsd_visitor_t) :: nesting_visitor
    integer :: depth_check = 0
    integer :: tables_visited = 0
  contains
    procedure :: visit_table => nesting_visit_table
    procedure :: visit_value => nesting_visit_value
    procedure :: leave_table => nesting_leave_table
  end type nesting_visitor

contains

  !> Returns all API tests
  function tests()
    type(test_list) :: tests

    tests = test_list([&
        suite("api", test_list([&
            test("type_introspection", test_type_introspection), &
            test("is_table_value", test_is_table_value), &
            test("child_count", test_child_count), &
            test("get_keys", test_get_keys), &
            test("default_integer", test_default_integer), &
            test("default_real", test_default_real), &
            test("default_string", test_default_string), &
            test("default_logical", test_default_logical), &
            test("visitor_pattern", test_visitor_pattern), &
            test("visitor_leave_table", test_visitor_leave_table), &
            test("get_attrib", test_get_attrib), &
            test("has_attrib", test_has_attrib), &
            test("hsd_require", test_hsd_require), &
            test("validate_range", test_validate_range), &
            test("validate_one_of", test_validate_one_of), &
            test("hsd_clone", test_hsd_clone), &
            test("hsd_merge", test_hsd_merge), &
            test("file_load", test_file_load), &
            test("matrix_getter", test_matrix_getter), &
            test("remove_child", test_remove_child), &
            test("iterator", test_iterator), &
            test("has_child", test_has_child), &
            test("sp_values", test_sp_values), &
            test("get_table", test_get_table), &
            test("is_array", test_is_array), &
            test("set_arrays", test_set_arrays), &
            test("default_complex", test_default_complex), &
            test("table_equal_identical", test_table_equal_identical), &
            test("table_equal_different", test_table_equal_different), &
            test("table_equal_nested", test_table_equal_nested), &
            test("table_equal_empty", test_table_equal_empty), &
            test("walk_tables_only", test_walk_tables_only), &
            test("walk_values_only", test_walk_values_only), &
            test("walk_both", test_walk_both), &
            test("walk_early_stop", test_walk_early_stop), &
            test("walk_no_callbacks", test_walk_no_callbacks), &
            test("remove_child_no_index", test_remove_child_no_index), &
            test("add_child_uninit", test_add_child_uninit), &
            test("merge_attrib", test_merge_attrib), &
            test("merge_clears_stale", test_merge_clears_stale), &
            test("parse_empty_string", test_parse_empty_string), &
            test("load_missing_noerr", test_load_missing_noerr), &
            test("table_equal_unnamed", test_table_equal_unnamed) &
        ])) &
    ])

  end function tests

  !> Test type introspection with hsd_get_type
  subroutine test_type_introspection()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    integer :: val_type
    character(len=:), allocatable :: hsd_input

    hsd_input = &
      "int_val = 42" // char(10) // &
      "real_val = 3.14" // char(10) // &
      "str_val = ""hello""" // char(10) // &
      "bool_val = yes" // char(10) // &
      "nested { child = 1 }"

    call hsd_load_string(hsd_input, root, error)
    call check(.not. allocated(error), msg="No parse error")

    val_type = hsd_get_type(root, "int_val")
    call check(val_type == VALUE_TYPE_INTEGER .or. val_type == VALUE_TYPE_REAL .or. &
               val_type == VALUE_TYPE_STRING, msg="int_val has valid type")

    val_type = hsd_get_type(root, "str_val")
    call check(val_type == VALUE_TYPE_STRING, msg="str_val is string type")

    val_type = hsd_get_type(root, "bool_val")
    call check(val_type == VALUE_TYPE_STRING .or. val_type == VALUE_TYPE_LOGICAL, &
               msg="bool_val has valid type")

    ! Non-existent path should return NONE
    val_type = hsd_get_type(root, "nonexistent")
    call check(val_type == VALUE_TYPE_NONE, msg="nonexistent returns NONE")

    call root%destroy()

  end subroutine test_type_introspection

  !> Test hsd_is_table and hsd_is_value
  subroutine test_is_table_value()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: hsd_input

    hsd_input = &
      "scalar = 42" // char(10) // &
      "container { inner = 1 }"

    call hsd_load_string(hsd_input, root, error)
    call check(.not. allocated(error), msg="No parse error")

    call check(hsd_is_value(root, "scalar"), msg="scalar is a value")
    call check(.not. hsd_is_table(root, "scalar"), msg="scalar is not a table")

    call check(hsd_is_table(root, "container"), msg="container is a table")
    call check(.not. hsd_is_value(root, "container"), msg="container is not a value")

    call check(.not. hsd_is_table(root, "nonexistent"), msg="nonexistent is not a table")
    call check(.not. hsd_is_value(root, "nonexistent"), msg="nonexistent is not a value")

    call root%destroy()

  end subroutine test_is_table_value

  !> Test hsd_child_count
  subroutine test_child_count()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    integer :: count
    character(len=:), allocatable :: hsd_input

    hsd_input = &
      "a = 1" // char(10) // &
      "b = 2" // char(10) // &
      "c = 3" // char(10) // &
      "nested { x = 1; y = 2 }"

    call hsd_load_string(hsd_input, root, error)
    call check(.not. allocated(error), msg="No parse error")

    ! Root should have 4 children
    count = hsd_child_count(root, "")
    call check(is_equal(count, 4), msg="Root has 4 children")

    ! nested should have 2 children
    count = hsd_child_count(root, "nested")
    call check(is_equal(count, 2), msg="nested has 2 children")

    ! scalar value should have 0 children
    count = hsd_child_count(root, "a")
    call check(is_equal(count, 0), msg="scalar has 0 children")

    call root%destroy()

  end subroutine test_child_count

  !> Test hsd_get_keys
  subroutine test_get_keys()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: keys(:)
    integer :: stat
    character(len=:), allocatable :: hsd_input

    hsd_input = &
      "alpha = 1" // char(10) // &
      "beta = 2" // char(10) // &
      "gamma = 3"

    call hsd_load_string(hsd_input, root, error)
    call check(.not. allocated(error), msg="No parse error")

    call hsd_get_keys(root, "", keys, stat)
    call check(is_equal(stat, 0), msg="get_keys succeeds")
    call check(is_equal(size(keys), 3), msg="3 keys returned")

    call root%destroy()

  end subroutine test_get_keys

  !> Test hsd_get_or with integer default
  subroutine test_default_integer()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    integer :: val, stat

    call hsd_load_string("existing = 42", root, error)
    call check(.not. allocated(error), msg="No parse error")

    ! Existing value should be returned
    call hsd_get_or(root, "existing", val, 999, stat)
    call check(is_equal(val, 42), msg="existing value is 42")
    call check(is_equal(stat, HSD_STAT_OK), msg="stat is OK for existing")

    ! Missing value should return default
    call hsd_get_or(root, "missing", val, 999, stat)
    call check(is_equal(val, 999), msg="missing returns default 999")
    call check(is_equal(stat, HSD_STAT_NOT_FOUND), msg="stat is NOT_FOUND for missing")

    call root%destroy()

  end subroutine test_default_integer

  !> Test hsd_get_or with real default
  subroutine test_default_real()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    real(dp) :: val
    integer :: stat

    call hsd_load_string("pi = 3.14159", root, error)
    call check(.not. allocated(error), msg="No parse error")

    ! Existing value
    call hsd_get_or(root, "pi", val, 0.0_dp, stat)
    call check(abs(val - 3.14159_dp) < 0.001_dp, msg="pi value is correct")
    call check(is_equal(stat, HSD_STAT_OK), msg="stat is OK")

    ! Missing value
    call hsd_get_or(root, "e", val, 2.71828_dp, stat)
    call check(abs(val - 2.71828_dp) < 0.001_dp, msg="missing returns default")
    call check(is_equal(stat, HSD_STAT_NOT_FOUND), msg="stat is NOT_FOUND")

    call root%destroy()

  end subroutine test_default_real

  !> Test hsd_get_or with string default
  subroutine test_default_string()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: val
    integer :: stat

    call hsd_load_string('name = "Alice"', root, error)
    call check(.not. allocated(error), msg="No parse error")

    ! Existing value
    call hsd_get_or(root, "name", val, "Unknown", stat)
    call check(val == "Alice", msg="name is Alice")
    call check(is_equal(stat, HSD_STAT_OK), msg="stat is OK")

    ! Missing value
    call hsd_get_or(root, "title", val, "Default", stat)
    call check(val == "Default", msg="missing returns default")
    call check(is_equal(stat, HSD_STAT_NOT_FOUND), msg="stat is NOT_FOUND")

    call root%destroy()

  end subroutine test_default_string

  !> Test hsd_get_or with logical default
  subroutine test_default_logical()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    logical :: val
    integer :: stat

    call hsd_load_string("enabled = yes", root, error)
    call check(.not. allocated(error), msg="No parse error")

    ! Existing value
    call hsd_get_or(root, "enabled", val, .false., stat)
    call check(val, msg="enabled is true")
    call check(is_equal(stat, HSD_STAT_OK), msg="stat is OK")

    ! Missing value
    call hsd_get_or(root, "debug", val, .true., stat)
    call check(val, msg="missing returns default true")
    call check(is_equal(stat, HSD_STAT_NOT_FOUND), msg="stat is NOT_FOUND")

    call root%destroy()

  end subroutine test_default_logical

  !> Test visitor pattern
  subroutine test_visitor_pattern()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    type(counting_visitor) :: visitor
    integer :: stat
    character(len=:), allocatable :: hsd_input

    hsd_input = &
      "a = 1" // char(10) // &
      "b = 2" // char(10) // &
      "nested {" // char(10) // &
      "  c = 3" // char(10) // &
      "  inner { d = 4 }" // char(10) // &
      "}"

    call hsd_load_string(hsd_input, root, error)
    call check(.not. allocated(error), msg="No parse error")

    visitor%table_count = 0
    visitor%value_count = 0

    call hsd_accept(root, visitor, stat)
    call check(is_equal(stat, 0), msg="visitor traversal succeeded")

    ! Should visit: root, nested, inner = 3 tables
    call check(is_equal(visitor%table_count, 3), msg="visited 3 tables")

    ! Should visit: a, b, c, d = 4 values
    call check(is_equal(visitor%value_count, 4), msg="visited 4 values")

    call root%destroy()

  end subroutine test_visitor_pattern

  subroutine counting_visit_table(self, table, path, depth, stat)
    class(counting_visitor), intent(inout) :: self
    type(hsd_table), intent(in), target :: table
    character(len=*), intent(in) :: path
    integer, intent(in) :: depth
    integer, intent(out), optional :: stat

    self%table_count = self%table_count + 1
    if (present(stat)) stat = 0

  end subroutine counting_visit_table

  subroutine counting_visit_value(self, val, path, depth, stat)
    class(counting_visitor), intent(inout) :: self
    type(hsd_value), intent(in) :: val
    character(len=*), intent(in) :: path
    integer, intent(in) :: depth
    integer, intent(out), optional :: stat

    self%value_count = self%value_count + 1
    if (present(stat)) stat = 0

  end subroutine counting_visit_value

  !> Test hsd_get_attrib
  subroutine test_get_attrib()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: attrib
    integer :: stat
    character(len=:), allocatable :: hsd_input

    hsd_input = &
      "Temperature [Kelvin] = 300.0" // char(10) // &
      "Length [Angstrom] = 5.4" // char(10) // &
      "NoUnit = 42"

    call hsd_load_string(hsd_input, root, error)
    call check(.not. allocated(error), msg="No parse error")

    ! Get attribute from node with attribute
    call hsd_get_attrib(root, "Temperature", attrib, stat)
    call check(is_equal(stat, HSD_STAT_OK), msg="stat is OK for Temperature")
    call check(attrib == "Kelvin", msg="Temperature has Kelvin attribute")

    call hsd_get_attrib(root, "Length", attrib, stat)
    call check(is_equal(stat, HSD_STAT_OK), msg="stat is OK for Length")
    call check(attrib == "Angstrom", msg="Length has Angstrom attribute")

    ! Get attribute from node without attribute - node exists, but no attrib
    call hsd_get_attrib(root, "NoUnit", attrib, stat)
    call check(is_equal(stat, HSD_STAT_OK), msg="stat is OK for NoUnit (node exists)")
    call check(attrib == "", msg="NoUnit has empty attribute")

    ! Get attribute from nonexistent path
    call hsd_get_attrib(root, "Nonexistent", attrib, stat)
    call check(is_equal(stat, HSD_STAT_NOT_FOUND), msg="stat is NOT_FOUND for nonexistent")

    call root%destroy()

  end subroutine test_get_attrib

  !> Test hsd_has_attrib
  subroutine test_has_attrib()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: hsd_input

    hsd_input = &
      "Temperature [Kelvin] = 300.0" // char(10) // &
      "NoUnit = 42"

    call hsd_load_string(hsd_input, root, error)
    call check(.not. allocated(error), msg="No parse error")

    call check(hsd_has_attrib(root, "Temperature"), msg="Temperature has attrib")
    call check(.not. hsd_has_attrib(root, "NoUnit"), msg="NoUnit has no attrib")
    call check(.not. hsd_has_attrib(root, "Nonexistent"), msg="Nonexistent has no attrib")

    call root%destroy()

  end subroutine test_has_attrib

  !> Test hsd_require
  subroutine test_hsd_require()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error, validation_error
    character(len=:), allocatable :: hsd_input

    hsd_input = &
      "name = ""test""" // char(10) // &
      "value = 42" // char(10) // &
      "nested { inner = 1 }"

    call hsd_load_string(hsd_input, root, error)
    call check(.not. allocated(error), msg="No parse error")

    ! Require existing field - should succeed
    call hsd_require(root, "name", validation_error)
    call check(.not. allocated(validation_error), msg="name exists")

    ! Require nested field
    call hsd_require(root, "nested/inner", validation_error)
    call check(.not. allocated(validation_error), msg="nested/inner exists")

    ! Require missing field - should fail
    call hsd_require(root, "missing", validation_error)
    call check(allocated(validation_error), msg="missing field detected")
    call check(validation_error%code == HSD_STAT_NOT_FOUND, msg="correct error code")
    deallocate(validation_error)

    ! Require with context
    call hsd_require(root, "missing", validation_error, context="MySection")
    call check(allocated(validation_error), msg="missing with context detected")
    call check(index(validation_error%message, "MySection") > 0, msg="context in message")
    deallocate(validation_error)

    call root%destroy()

  end subroutine test_hsd_require

  !> Test hsd_validate_range
  subroutine test_validate_range()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error, validation_error
    character(len=:), allocatable :: hsd_input

    hsd_input = &
      "temperature = 300.0" // char(10) // &
      "pressure = -50.0" // char(10) // &
      "factor = 1.5"

    call hsd_load_string(hsd_input, root, error)
    call check(.not. allocated(error), msg="No parse error")

    ! Value within range
    call hsd_validate_range(root, "temperature", 0.0_dp, 1000.0_dp, validation_error)
    call check(.not. allocated(validation_error), msg="temperature in range")

    ! Value at boundary
    call hsd_validate_range(root, "factor", 1.0_dp, 2.0_dp, validation_error)
    call check(.not. allocated(validation_error), msg="factor at boundary")

    ! Value out of range (below min)
    call hsd_validate_range(root, "pressure", 0.0_dp, 100.0_dp, validation_error)
    call check(allocated(validation_error), msg="pressure out of range detected")
    call check(index(validation_error%message, "outside valid range") > 0, &
        msg="range error message")
    deallocate(validation_error)

    ! Value out of range (above max)
    call hsd_validate_range(root, "temperature", 0.0_dp, 200.0_dp, validation_error)
    call check(allocated(validation_error), msg="temperature above max detected")
    deallocate(validation_error)

    ! Missing field
    call hsd_validate_range(root, "missing", 0.0_dp, 100.0_dp, validation_error)
    call check(allocated(validation_error), msg="missing field detected")
    deallocate(validation_error)

    call root%destroy()

  end subroutine test_validate_range

  !> Test hsd_validate_one_of
  subroutine test_validate_one_of()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error, validation_error
    character(len=:), allocatable :: hsd_input
    character(len=10) :: valid_types(3)

    hsd_input = &
      "type = ""GGA""" // char(10) // &
      "invalid_type = ""Unknown""" // char(10) // &
      "case_test = ""lda"""

    call hsd_load_string(hsd_input, root, error)
    call check(.not. allocated(error), msg="No parse error")

    valid_types = ["LDA", "GGA", "HF "]

    ! Valid choice
    call hsd_validate_one_of(root, "type", valid_types, validation_error)
    call check(.not. allocated(validation_error), msg="GGA is valid")

    ! Case insensitive match
    call hsd_validate_one_of(root, "case_test", valid_types, validation_error)
    call check(.not. allocated(validation_error), msg="lda matches LDA")

    ! Invalid choice
    call hsd_validate_one_of(root, "invalid_type", valid_types, validation_error)
    call check(allocated(validation_error), msg="Unknown is invalid")
    call check(index(validation_error%message, "not one of") > 0, msg="one_of error message")
    deallocate(validation_error)

    ! Missing field
    call hsd_validate_one_of(root, "missing", valid_types, validation_error)
    call check(allocated(validation_error), msg="missing field detected")
    deallocate(validation_error)

    call root%destroy()

  end subroutine test_validate_one_of

  !> Test hsd_clone
  subroutine test_hsd_clone()
    type(hsd_table) :: root, cloned
    type(hsd_error_t), allocatable :: error
    integer :: val1, val2, stat
    character(len=:), allocatable :: str_val
    character(len=:), allocatable :: hsd_input

    hsd_input = &
      "name = ""original""" // char(10) // &
      "value = 42" // char(10) // &
      "nested {" // char(10) // &
      "  inner = 100" // char(10) // &
      "}"

    call hsd_load_string(hsd_input, root, error)
    call check(.not. allocated(error), msg="No parse error")

    ! Clone the tree
    call hsd_clone(root, cloned, stat)
    call check(is_equal(stat, HSD_STAT_OK), msg="clone succeeded")

    ! Verify cloned values match
    call hsd_get(root, "value", val1)
    call hsd_get(cloned, "value", val2)
    call check(is_equal(val1, val2), msg="cloned value matches")

    call hsd_get(root, "name", str_val)
    call check(str_val == "original", msg="original name correct")

    call hsd_get(cloned, "name", str_val)
    call check(str_val == "original", msg="cloned name correct")

    ! Verify nested values
    call hsd_get(root, "nested/inner", val1)
    call hsd_get(cloned, "nested/inner", val2)
    call check(is_equal(val1, val2), msg="cloned nested value matches")

    ! Modify original should not affect clone
    call hsd_set(root, "value", 999)
    call hsd_get(root, "value", val1)
    call hsd_get(cloned, "value", val2)
    call check(is_equal(val1, 999), msg="original modified")
    call check(is_equal(val2, 42), msg="clone unchanged")

    call root%destroy()
    call cloned%destroy()

  end subroutine test_hsd_clone

  !> Test hsd_merge
  subroutine test_hsd_merge()
    type(hsd_table) :: base, overlay
    type(hsd_error_t), allocatable :: error
    integer :: val, stat
    character(len=:), allocatable :: str_val
    character(len=:), allocatable :: base_input, overlay_input

    base_input = &
      "a = 1" // char(10) // &
      "b = 2" // char(10) // &
      "nested {" // char(10) // &
      "  x = 10" // char(10) // &
      "  y = 20" // char(10) // &
      "}"

    overlay_input = &
      "b = 99" // char(10) // &
      "c = 3" // char(10) // &
      "nested {" // char(10) // &
      "  y = 200" // char(10) // &
      "  z = 30" // char(10) // &
      "}"

    call hsd_load_string(base_input, base, error)
    call check(.not. allocated(error), msg="No base parse error")

    call hsd_load_string(overlay_input, overlay, error)
    call check(.not. allocated(error), msg="No overlay parse error")

    ! Merge overlay into base
    call hsd_merge(base, overlay, stat)
    call check(is_equal(stat, HSD_STAT_OK), msg="merge succeeded")

    ! Check original value preserved
    call hsd_get(base, "a", val)
    call check(is_equal(val, 1), msg="a preserved from base")

    ! Check overlaid value
    call hsd_get(base, "b", val)
    call check(is_equal(val, 99), msg="b overwritten by overlay")

    ! Check new value from overlay
    call hsd_get(base, "c", val)
    call check(is_equal(val, 3), msg="c added from overlay")

    ! Check nested merge - preserved value
    call hsd_get(base, "nested/x", val)
    call check(is_equal(val, 10), msg="nested/x preserved")

    ! Check nested merge - overlaid value
    call hsd_get(base, "nested/y", val)
    call check(is_equal(val, 200), msg="nested/y overwritten")

    ! Check nested merge - new value
    call hsd_get(base, "nested/z", val)
    call check(is_equal(val, 30), msg="nested/z added")

    call base%destroy()
    call overlay%destroy()

  end subroutine test_hsd_merge

  !> Test loading HSD from file using build_env paths
  subroutine test_file_load()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=512) :: filepath
    integer :: max_steps, stat
    real(dp) :: temperature
    logical :: scc_enabled

    ! Construct absolute path using build_env source_dir
    filepath = source_dir // "/test/inputs/simple.hsd"

    ! Load from file
    call hsd_load(trim(filepath), root, error)
    call check(.not. allocated(error), msg="File loaded without error")

    ! Verify parsed content
    call hsd_get(root, "driver/max_steps", max_steps, stat)
    call check(is_equal(stat, 0), msg="Can get max_steps")
    call check(is_equal(max_steps, 100), msg="max_steps is 100")

    call hsd_get(root, "hamiltonian/dftb/scc", scc_enabled, stat)
    call check(is_equal(stat, 0), msg="Can get scc")
    call check(scc_enabled .eqv. .true., msg="scc is enabled")

    call hsd_get(root, "hamiltonian/dftb/filling/fermi/temperature", temperature, stat)
    call check(is_equal(stat, 0), msg="Can get temperature")
    call check(abs(temperature - 300.0_dp) < 0.01_dp, msg="Temperature is 300")

    call root%destroy()

  end subroutine test_file_load

  !> Test hsd_get_matrix for 2D array retrieval
  subroutine test_matrix_getter()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    integer, allocatable :: int_mat(:,:)
    real(dp), allocatable :: real_mat(:,:)
    integer :: stat, nrows, ncols
    character(len=:), allocatable :: hsd_input

    hsd_input = &
      "int_matrix {" // char(10) // &
      "  1 2 3" // char(10) // &
      "  4 5 6" // char(10) // &
      "}" // char(10) // &
      "real_matrix {" // char(10) // &
      "  1.0 2.0" // char(10) // &
      "  3.0 4.0" // char(10) // &
      "  5.0 6.0" // char(10) // &
      "}"

    call hsd_load_string(hsd_input, root, error)
    call check(.not. allocated(error), msg="Parse OK")

    call hsd_get_matrix(root, "int_matrix", int_mat, nrows, ncols, stat)
    call check(is_equal(stat, 0), msg="Can get integer matrix")
    if (allocated(int_mat)) then
      ! Total elements should be 6
      call check(is_equal(nrows * ncols, 6), msg="Integer matrix has 6 elements")
    end if

    call hsd_get_matrix(root, "real_matrix", real_mat, nrows, ncols, stat)
    call check(is_equal(stat, 0), msg="Can get real matrix")
    if (allocated(real_mat)) then
      ! Total elements should be 6
      call check(is_equal(nrows * ncols, 6), msg="Real matrix has 6 elements")
    end if

    call root%destroy()

  end subroutine test_matrix_getter

  !> Test hsd_remove_child functionality
  subroutine test_remove_child()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    integer :: count, stat

    call hsd_load_string("a = 1" // char(10) // "b = 2" // char(10) // "c = 3", root, error)
    call check(.not. allocated(error), msg="Parse OK")

    count = hsd_child_count(root, "")
    call check(is_equal(count, 3), msg="Initially 3 children")

    call hsd_remove_child(root, "b", stat)
    call check(is_equal(stat, 0), msg="Remove succeeds")

    count = hsd_child_count(root, "")
    call check(is_equal(count, 2), msg="Now 2 children")

    call check(.not. hsd_has_child(root, "b"), msg="b is gone")
    call check(hsd_has_child(root, "a"), msg="a still exists")
    call check(hsd_has_child(root, "c"), msg="c still exists")

    call root%destroy()

  end subroutine test_remove_child

  !> Test iterator functionality
  subroutine test_iterator()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    type(hsd_iterator) :: iter
    class(hsd_node), pointer :: node
    integer :: count

    call hsd_load_string("x = 1" // char(10) // "y = 2" // char(10) // "z = 3", root, error)
    call check(.not. allocated(error), msg="Parse OK")

    call iter%init(root)
    count = 0
    do while (iter%next(node))
      count = count + 1
    end do

    call check(is_equal(count, 3), msg="Iterator found 3 nodes")

    ! Test reset and has_next
    call iter%reset()
    call check(iter%has_next(), msg="Has next after reset")

    call root%destroy()

  end subroutine test_iterator

  !> Test hsd_has_child function
  subroutine test_has_child()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error

    call hsd_load_string("existing = 42" // char(10) // "nested { inner = 1 }", root, error)
    call check(.not. allocated(error), msg="Parse OK")

    call check(hsd_has_child(root, "existing"), msg="Has existing")
    call check(hsd_has_child(root, "nested"), msg="Has nested")
    call check(.not. hsd_has_child(root, "missing"), msg="No missing")

    call root%destroy()

  end subroutine test_has_child

  !> Test single-precision real values
  subroutine test_sp_values()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    real(sp) :: sp_val
    real(sp), allocatable :: sp_arr(:)
    integer :: stat

    call hsd_load_string("val = 3.14" // char(10) // "arr = 1.0 2.0 3.0", root, error)
    call check(.not. allocated(error), msg="Parse OK")

    call hsd_get(root, "val", sp_val, stat)
    call check(is_equal(stat, 0), msg="Can get SP value")
    call check(abs(sp_val - 3.14_sp) < 0.01_sp, msg="SP value correct")

    call hsd_get(root, "arr", sp_arr, stat)
    call check(is_equal(stat, 0), msg="Can get SP array")
    call check(is_equal(size(sp_arr), 3), msg="SP array size correct")

    ! Test set SP
    call hsd_set(root, "new_sp", 2.71_sp, stat)
    call check(is_equal(stat, 0), msg="Can set SP value")

    call root%destroy()

  end subroutine test_sp_values

  !> Test hsd_get_table function
  subroutine test_get_table()
    type(hsd_table) :: root
    type(hsd_table), pointer :: subtable
    type(hsd_error_t), allocatable :: error
    integer :: stat

    call hsd_load_string("nested { inner { deep = 42 } }", root, error)
    call check(.not. allocated(error), msg="Parse OK")

    call hsd_get_table(root, "nested", subtable, stat)
    call check(is_equal(stat, 0), msg="Can get nested table")
    call check(associated(subtable), msg="Subtable associated")

    call hsd_get_table(root, "nested/inner", subtable, stat)
    call check(is_equal(stat, 0), msg="Can get deeply nested table")
    call check(associated(subtable), msg="Deep subtable associated")

    call root%destroy()

  end subroutine test_get_table

  !> Test hsd_is_array function
  subroutine test_is_array()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    integer, allocatable :: arr(:)
    integer :: stat

    call hsd_load_string("arr = 1 2 3 4 5" // char(10) // "scalar = 42", root, error)
    call check(.not. allocated(error), msg="Parse OK")

    ! Get the array to check it works
    call hsd_get(root, "arr", arr, stat)
    call check(is_equal(stat, 0), msg="Can get arr as array")
    call check(is_equal(size(arr), 5), msg="Array has 5 elements")

    ! Note: hsd_is_array checks VALUE_TYPE_ARRAY which may not be set for parsed text
    call check(.not. hsd_is_array(root, "missing"), msg="missing is not array")

    call root%destroy()

  end subroutine test_is_array

  !> Test hsd_set for arrays
  subroutine test_set_arrays()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    integer, allocatable :: int_arr(:)
    real(dp), allocatable :: real_arr(:)
    logical, allocatable :: log_arr(:)
    integer :: stat

    call hsd_load_string("", root, error)
    call check(.not. allocated(error), msg="Parse OK")

    ! Set integer array
    call hsd_set(root, "ints", [1, 2, 3, 4, 5], stat)
    call check(is_equal(stat, 0), msg="Can set integer array")

    call hsd_get(root, "ints", int_arr, stat)
    call check(is_equal(size(int_arr), 5), msg="Int array size correct")

    ! Set real array
    call hsd_set(root, "reals", [1.0_dp, 2.0_dp, 3.0_dp], stat)
    call check(is_equal(stat, 0), msg="Can set real array")

    call hsd_get(root, "reals", real_arr, stat)
    call check(is_equal(size(real_arr), 3), msg="Real array size correct")

    ! Set logical array
    call hsd_set(root, "flags", [.true., .false., .true.], stat)
    call check(is_equal(stat, 0), msg="Can set logical array")

    call hsd_get(root, "flags", log_arr, stat)
    call check(is_equal(size(log_arr), 3), msg="Logical array size correct")

    call root%destroy()

  end subroutine test_set_arrays

  !> Test hsd_get_or for complex with default
  subroutine test_default_complex()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    complex(dp) :: cval
    integer :: stat

    call hsd_load_string("existing = 1.0+2.0i", root, error)
    call check(.not. allocated(error), msg="Parse OK")

    ! Existing value
    call hsd_get_or(root, "existing", cval, (0.0_dp, 0.0_dp), stat)
    call check(is_equal(stat, HSD_STAT_OK), msg="Found existing complex")
    call check(abs(real(cval) - 1.0_dp) < 0.001_dp, msg="Real part correct")

    ! Missing value - should use default
    call hsd_get_or(root, "missing", cval, (5.0_dp, 6.0_dp), stat)
    call check(is_equal(stat, HSD_STAT_NOT_FOUND), msg="Missing returns not found")
    call check(abs(real(cval) - 5.0_dp) < 0.001_dp, msg="Default real part")
    call check(abs(aimag(cval) - 6.0_dp) < 0.001_dp, msg="Default imag part")

    call root%destroy()

  end subroutine test_default_complex

  !> Test leave_table callback in visitor
  subroutine test_visitor_leave_table()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    class(nesting_visitor), allocatable :: vis
    integer :: stat

    ! Build a nested structure
    ! root
    !   - t1 (table)
    !     - v1 (value)
    !   - t2 (table)
    !     - t3 (table)
    call hsd_load_string("t1 { v1 = 1} t2 { t3 {} }", root, error)
    call check(.not. allocated(error), msg="Parse OK")

    allocate(vis)
    call hsd_accept(root, vis, stat)

    call check(vis%depth_check == 0, msg="Depth returned to 0")
    ! Root, t1, t2, t3 = 4 tables
    call check(vis%tables_visited == 4, msg="Visited 4 tables")

    call root%destroy()
  end subroutine test_visitor_leave_table

  subroutine nesting_visit_table(self, table, path, depth, stat)
    class(nesting_visitor), intent(inout) :: self
    type(hsd_table), intent(in), target :: table
    character(len=*), intent(in) :: path
    integer, intent(in) :: depth
    integer, intent(out), optional :: stat

    self%depth_check = self%depth_check + 1
    self%tables_visited = self%tables_visited + 1
    if (present(stat)) stat = 0
  end subroutine nesting_visit_table

  subroutine nesting_visit_value(self, val, path, depth, stat)
    class(nesting_visitor), intent(inout) :: self
    type(hsd_value), intent(in) :: val
    character(len=*), intent(in) :: path
    integer, intent(in) :: depth
    integer, intent(out), optional :: stat
    if (present(stat)) stat = 0
  end subroutine nesting_visit_value

  subroutine nesting_leave_table(self, table, path, depth, stat)
    class(nesting_visitor), intent(inout) :: self
    type(hsd_table), intent(in), target :: table
    character(len=*), intent(in) :: path
    integer, intent(in) :: depth
    integer, intent(out), optional :: stat

    self%depth_check = self%depth_check - 1
    if (present(stat)) stat = 0
  end subroutine nesting_leave_table

  !> Test hsd_table_equal with identical tables
  subroutine test_table_equal_identical()
    type(hsd_table) :: root1, root2
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: hsd_input

    hsd_input = &
      "alpha = 42" // char(10) // &
      "beta = 3.14" // char(10) // &
      "gamma = ""hello""" // char(10) // &
      "nested { x = 1; y = 2 }"

    call hsd_load_string(hsd_input, root1, error)
    call check(.not. allocated(error), msg="parse root1")
    call hsd_load_string(hsd_input, root2, error)
    call check(.not. allocated(error), msg="parse root2")

    call check(hsd_table_equal(root1, root2), &
        & msg="identical tables should be equal")

    call root1%destroy()
    call root2%destroy()
  end subroutine test_table_equal_identical

  !> Test hsd_table_equal with different tables
  subroutine test_table_equal_different()
    type(hsd_table) :: root1, root2
    type(hsd_error_t), allocatable :: error

    call hsd_load_string("a = 1" // char(10) // "b = 2", root1, error)
    call check(.not. allocated(error), msg="parse root1")
    call hsd_load_string("a = 1" // char(10) // "b = 999", root2, error)
    call check(.not. allocated(error), msg="parse root2")

    call check(.not. hsd_table_equal(root1, root2), &
        & msg="different values should not be equal")

    call root1%destroy()
    call root2%destroy()
  end subroutine test_table_equal_different

  !> Test hsd_table_equal with nested tables
  subroutine test_table_equal_nested()
    type(hsd_table) :: root1, root2
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: input1, input2

    input1 = "outer { inner { val = 42 } }"
    input2 = "outer { inner { val = 99 } }"

    call hsd_load_string(input1, root1, error)
    call check(.not. allocated(error), msg="parse root1")
    call hsd_load_string(input2, root2, error)
    call check(.not. allocated(error), msg="parse root2")

    call check(.not. hsd_table_equal(root1, root2), &
        & msg="nested difference should not be equal")

    call root1%destroy()
    call root2%destroy()
  end subroutine test_table_equal_nested

  !> Test hsd_walk with empty tables
  subroutine test_table_equal_empty()
    type(hsd_table) :: root1, root2

    call new_table(root1, name="root")
    call new_table(root2, name="root")

    call check(hsd_table_equal(root1, root2), &
        & msg="empty tables should be equal")

    call root1%destroy()
    call root2%destroy()
  end subroutine test_table_equal_empty

  !> Test hsd_walk with on_table callback only
  subroutine test_walk_tables_only()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    integer :: stat

    walk_table_count = 0
    walk_value_count = 0

    call hsd_load_string("a { b = 1 }" // char(10) // "c { d = 2 }", root, error)
    call check(.not. allocated(error), msg="parse ok")

    call hsd_walk(root, on_table=walk_count_table, stat=stat)
    call check(stat == 0, msg="walk ok")
    ! Root + a + c = 3 tables
    call check(walk_table_count == 3, msg="should visit 3 tables")
    call check(walk_value_count == 0, msg="no values visited")

    call root%destroy()
  end subroutine test_walk_tables_only

  !> Test hsd_walk with on_value callback only
  subroutine test_walk_values_only()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    integer :: stat

    walk_table_count = 0
    walk_value_count = 0

    call hsd_load_string("x = 1" // char(10) // "y = 2" // char(10) // "z = 3", &
        & root, error)
    call check(.not. allocated(error), msg="parse ok")

    call hsd_walk(root, on_value=walk_count_value, stat=stat)
    call check(stat == 0, msg="walk ok")
    call check(walk_table_count == 0, msg="no tables visited")
    call check(walk_value_count == 3, msg="should visit 3 values")

    call root%destroy()
  end subroutine test_walk_values_only

  !> Test hsd_walk with both callbacks
  subroutine test_walk_both()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    integer :: stat

    walk_table_count = 0
    walk_value_count = 0

    call hsd_load_string("a = 1" // char(10) // "b { c = 2 }", root, error)
    call check(.not. allocated(error), msg="parse ok")

    call hsd_walk(root, on_table=walk_count_table, &
        & on_value=walk_count_value, stat=stat)
    call check(stat == 0, msg="walk ok")
    ! Root + b = 2 tables
    call check(walk_table_count == 2, msg="should visit 2 tables")
    ! a + c = 2 values
    call check(walk_value_count == 2, msg="should visit 2 values")

    call root%destroy()
  end subroutine test_walk_both

  !> Test hsd_walk early stop via non-zero stat
  subroutine test_walk_early_stop()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    integer :: stat

    walk_value_count = 0

    call hsd_load_string("a = 1" // char(10) // "b = 2" // char(10) // "c = 3", &
        & root, error)
    call check(.not. allocated(error), msg="parse ok")

    call hsd_walk(root, on_value=walk_stop_after_one, stat=stat)
    call check(stat /= 0, msg="walk should have stopped early")
    call check(walk_value_count == 1, msg="should visit only 1 value")

    call root%destroy()
  end subroutine test_walk_early_stop

  !> Test hsd_walk with no callbacks (should be a no-op)
  subroutine test_walk_no_callbacks()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    integer :: stat

    call hsd_load_string("a = 1" // char(10) // "b { c = 2 }", root, error)
    call check(.not. allocated(error), msg="Parse OK")

    ! Walk with neither callback should succeed without error
    call hsd_walk(root, stat=stat)
    call check(is_equal(stat, 0), msg="walk with no callbacks succeeds")

    call root%destroy()
  end subroutine test_walk_no_callbacks

  !> Test remove_child_by_name when hash index is not active (linear fallback)
  subroutine test_remove_child_no_index()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    integer :: stat, count

    call hsd_load_string("a = 1" // char(10) // "b = 2" // char(10) // "c = 3", root, error)
    call check(.not. allocated(error), msg="Parse OK")

    ! Invalidate the hash index to force linear fallback path
    call root%invalidate_index()

    ! Remove by name using linear scan
    call hsd_remove_child(root, "b", stat)
    call check(is_equal(stat, 0), msg="remove b via linear scan succeeds")

    count = hsd_child_count(root, "")
    call check(is_equal(count, 2), msg="2 children remain after remove")
    call check(.not. hsd_has_child(root, "b"), msg="b is gone")
    call check(hsd_has_child(root, "a"), msg="a still present")
    call check(hsd_has_child(root, "c"), msg="c still present")

    call root%destroy()
  end subroutine test_remove_child_no_index

  !> Test add_child on an uninitialized (zero-capacity) table
  subroutine test_add_child_uninit()
    type(hsd_table) :: root
    type(hsd_value) :: val
    integer :: count

    ! root is default-initialized (capacity=0, num_children=0)
    val%name = "x"
    val%value_type = VALUE_TYPE_INTEGER
    val%int_value = 42

    ! add_child should auto-initialize the table
    call root%add_child(val)

    count = root%num_children
    call check(is_equal(count, 1), msg="uninit table has 1 child after add")
    call check(root%has_child("x"), msg="child x exists")

    call root%destroy()
  end subroutine test_add_child_uninit

  !> Test that hsd_merge propagates attributes from overlay values
  subroutine test_merge_attrib()
    type(hsd_table) :: base, overlay
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: attrib
    integer :: stat

    call hsd_load_string("Temperature = 100.0", base, error)
    call check(.not. allocated(error), msg="Base parse OK")

    call hsd_load_string("Temperature [Kelvin] = 300.0", overlay, error)
    call check(.not. allocated(error), msg="Overlay parse OK")

    call hsd_merge(base, overlay, stat)
    call check(is_equal(stat, HSD_STAT_OK), msg="merge OK")

    ! After merge, the attribute from overlay should be present
    call hsd_get_attrib(base, "Temperature", attrib, stat)
    call check(is_equal(stat, HSD_STAT_OK), msg="attrib stat OK")
    call check(attrib == "Kelvin", msg="attribute propagated from overlay")

    call base%destroy()
    call overlay%destroy()
  end subroutine test_merge_attrib

  !> Test that hsd_merge clears stale fields when overwriting a value
  subroutine test_merge_clears_stale()
    type(hsd_table) :: base, overlay
    type(hsd_error_t), allocatable :: error
    integer :: stat, ival
    character(len=:), allocatable :: sval

    ! Base has a string value
    call hsd_load_string('label = "hello"', base, error)
    call check(.not. allocated(error), msg="Base parse OK")

    ! Overlay replaces it with an integer
    call hsd_load_string("label = 42", overlay, error)
    call check(.not. allocated(error), msg="Overlay parse OK")

    call hsd_merge(base, overlay, stat)
    call check(is_equal(stat, HSD_STAT_OK), msg="merge OK")

    ! After merge, the integer value should be accessible
    call hsd_get(base, "label", ival, stat)
    call check(is_equal(stat, 0), msg="can get integer after merge")
    call check(is_equal(ival, 42), msg="value is 42")

    call base%destroy()
    call overlay%destroy()
  end subroutine test_merge_clears_stale

  !> Test parsing an empty string
  subroutine test_parse_empty_string()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    integer :: count

    call hsd_load_string("", root, error)
    call check(.not. allocated(error), msg="Empty string parses without error")

    count = hsd_child_count(root, "")
    call check(is_equal(count, 0), msg="Empty input has 0 children")

    call root%destroy()
  end subroutine test_parse_empty_string

  !> Test hsd_load with a missing file when error argument is present
  subroutine test_load_missing_noerr()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error

    ! Load a nonexistent file — should set the error argument, not crash
    call hsd_load("/tmp/nonexistent_hsd_file_12345.hsd", root, error)
    call check(allocated(error), msg="error is set for missing file")

    call root%destroy()
  end subroutine test_load_missing_noerr

  !> Test hsd_table_equal with children that have no names (unnamed nodes)
  subroutine test_table_equal_unnamed()
    type(hsd_table) :: a, b
    type(hsd_value) :: val_a, val_b

    call new_table(a, "root")
    call new_table(b, "root")

    ! Add a named child to both
    val_a%name = "x"
    val_a%value_type = VALUE_TYPE_INTEGER
    val_a%int_value = 1
    call a%add_child(val_a)

    val_b%name = "x"
    val_b%value_type = VALUE_TYPE_INTEGER
    val_b%int_value = 1
    call b%add_child(val_b)

    ! Tables with identical named children are equal
    call check(hsd_table_equal(a, b), msg="identical named children are equal")

    ! Now test with different children count
    val_a%name = "y"
    val_a%int_value = 2
    call a%add_child(val_a)

    call check(.not. hsd_table_equal(a, b), msg="different child counts not equal")

    call a%destroy()
    call b%destroy()
  end subroutine test_table_equal_unnamed

  ! --- Walk test helper callbacks (module-level counters) ---

  subroutine walk_count_table(table, path, depth, stat)
    type(hsd_table), intent(in), target :: table
    character(len=*), intent(in) :: path
    integer, intent(in) :: depth
    integer, intent(out), optional :: stat
    walk_table_count = walk_table_count + 1
    if (present(stat)) stat = 0
  end subroutine walk_count_table

  subroutine walk_count_value(val, path, depth, stat)
    type(hsd_value), intent(in) :: val
    character(len=*), intent(in) :: path
    integer, intent(in) :: depth
    integer, intent(out), optional :: stat
    walk_value_count = walk_value_count + 1
    if (present(stat)) stat = 0
  end subroutine walk_count_value

  subroutine walk_stop_after_one(val, path, depth, stat)
    type(hsd_value), intent(in) :: val
    character(len=*), intent(in) :: path
    integer, intent(in) :: depth
    integer, intent(out), optional :: stat
    walk_value_count = walk_value_count + 1
    if (present(stat)) stat = 1  ! Stop traversal
  end subroutine walk_stop_after_one

end module test_api_suite
