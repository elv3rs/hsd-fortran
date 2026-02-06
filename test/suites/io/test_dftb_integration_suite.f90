!> DFTB+ integration tests
!>
!> Tests that hsd-fortran can correctly parse and handle realistic DFTB+ input
!> files, covering all HSD features used by DFTB+:
!>   - Named method blocks (Hamiltonian = DFTB { ... })
!>   - Attributes/modifiers for units ([Kelvin], [Angstrom])
!>   - Empty blocks (Driver = {})
!>   - Inline raw data (GenFormat coordinates)
!>   - Repeated child sections (multiple Region blocks)
!>   - Deep path navigation
!>   - Round-trip parse/dump/re-parse fidelity
module test_dftb_integration_suite
  use hsd
  use fortuno_serial, only : is_equal, test => serial_case_item, &
      & check => serial_check, suite => serial_suite_item, test_list
  use build_env, only : source_dir, build_dir
  implicit none (type, external)

  private
  public :: tests

contains

  !> Returns all DFTB+ integration tests
  function tests()
    type(test_list) :: tests

    tests = test_list([&
        suite("dftb_integration", test_list([&
            test("parse_full_input", test_parse_full_input), &
            test("top_level_sections", test_top_level_sections), &
            test("named_method_block", test_named_method_block), &
            test("empty_driver_block", test_empty_driver_block), &
            test("unit_attributes", test_unit_attributes), &
            test("deep_path_navigation", test_deep_path_navigation), &
            test("genformat_raw_data", test_genformat_raw_data), &
            test("boolean_values", test_boolean_values), &
            test("integer_values", test_integer_values), &
            test("real_values", test_real_values), &
            test("string_values", test_string_values), &
            test("repeated_sections", test_repeated_sections), &
            test("nested_method_blocks", test_nested_method_blocks), &
            test("default_values", test_default_values), &
            test("round_trip", test_round_trip), &
            test("programmatic_dftb_input", test_programmatic_dftb_input), &
            test("parse_inline_dftb", test_parse_inline_dftb) &
        ])) &
    ])

  end function tests

  ! ---------------------------------------------------------------------------
  ! Helper
  ! ---------------------------------------------------------------------------

  !> Load the shared DFTB+ test input file
  subroutine load_dftb_input(root, error)
    type(hsd_table), intent(out) :: root
    type(hsd_error_t), allocatable, intent(out) :: error

    character(len=512) :: filepath

    filepath = source_dir // "/test/inputs/dftb_input.hsd"
    call hsd_load(trim(filepath), root, error)
  end subroutine load_dftb_input

  ! ---------------------------------------------------------------------------
  ! Tests
  ! ---------------------------------------------------------------------------

  !> Verify the file parses without errors
  subroutine test_parse_full_input()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error

    call load_dftb_input(root, error)
    call check(.not. allocated(error), msg="DFTB+ input parses without error")
    call check(root%num_children > 0, msg="Root has children")
    call root%destroy()
  end subroutine test_parse_full_input

  !> Verify all top-level sections exist
  subroutine test_top_level_sections()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error

    call load_dftb_input(root, error)
    call check(.not. allocated(error), msg="Parse OK")

    call check(hsd_has_child(root, "Geometry"), msg="Has Geometry")
    call check(hsd_has_child(root, "Driver"), msg="Has Driver")
    call check(hsd_has_child(root, "Hamiltonian"), msg="Has Hamiltonian")
    call check(hsd_has_child(root, "Options"), msg="Has Options")
    call check(hsd_has_child(root, "Analysis"), msg="Has Analysis")
    call check(hsd_has_child(root, "ParserOptions"), msg="Has ParserOptions")

    call root%destroy()
  end subroutine test_top_level_sections

  !> Test named method block pattern: Hamiltonian = DFTB { ... }
  subroutine test_named_method_block()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error

    call load_dftb_input(root, error)
    call check(.not. allocated(error), msg="Parse OK")

    ! Hamiltonian should be a table
    call check(hsd_is_table(root, "Hamiltonian"), msg="Hamiltonian is table")

    ! Hamiltonian/DFTB should be a nested table (the named method)
    call check(hsd_is_table(root, "Hamiltonian/DFTB"), msg="Hamiltonian/DFTB is table")

    ! SlaterKosterFiles uses Type2FileNames method block
    call check(hsd_is_table(root, "Hamiltonian/DFTB/SlaterKosterFiles"), &
        msg="SlaterKosterFiles is table")
    call check(hsd_is_table(root, "Hamiltonian/DFTB/SlaterKosterFiles/Type2FileNames"), &
        msg="Type2FileNames is table")

    ! Filling uses Fermi method block
    call check(hsd_is_table(root, "Hamiltonian/DFTB/Filling"), &
        msg="Filling is table")
    call check(hsd_is_table(root, "Hamiltonian/DFTB/Filling/Fermi"), &
        msg="Fermi is table")

    ! Mixer uses Simple method block
    call check(hsd_is_table(root, "Hamiltonian/DFTB/Mixer"), &
        msg="Mixer is table")
    call check(hsd_is_table(root, "Hamiltonian/DFTB/Mixer/Simple"), &
        msg="Simple is table")

    call root%destroy()
  end subroutine test_named_method_block

  !> Test empty block: Driver = {}
  subroutine test_empty_driver_block()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error

    call load_dftb_input(root, error)
    call check(.not. allocated(error), msg="Parse OK")

    call check(hsd_is_table(root, "Driver"), msg="Driver is a table")
    call check(is_equal(hsd_child_count(root, "Driver"), 0), &
        msg="Empty driver has 0 children")

    call root%destroy()
  end subroutine test_empty_driver_block

  !> Test attribute/modifier extraction: Temperature [Kelvin] = 300.0
  subroutine test_unit_attributes()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: attrib
    real(dp) :: temperature
    integer :: stat

    call load_dftb_input(root, error)
    call check(.not. allocated(error), msg="Parse OK")

    ! Check attribute exists
    call check(hsd_has_attrib(root, "Hamiltonian/DFTB/Filling/Fermi/Temperature"), &
        msg="Temperature has attribute")

    ! Get attribute value
    call hsd_get_attrib(root, "Hamiltonian/DFTB/Filling/Fermi/Temperature", attrib, stat)
    call check(is_equal(stat, 0), msg="Get attrib OK")
    call check(attrib == "Kelvin", msg="Attribute is 'Kelvin'")

    ! Get the numeric value
    call hsd_get(root, "Hamiltonian/DFTB/Filling/Fermi/Temperature", temperature, stat)
    call check(is_equal(stat, 0), msg="Get temperature OK")
    call check(abs(temperature - 300.0_dp) < 1.0e-10_dp, msg="Temperature is 300.0")

    call root%destroy()
  end subroutine test_unit_attributes

  !> Test deep path navigation across multiple levels
  subroutine test_deep_path_navigation()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: str_val
    real(dp) :: real_val
    integer :: stat

    call load_dftb_input(root, error)
    call check(.not. allocated(error), msg="Parse OK")

    ! 4-level deep: Hamiltonian/DFTB/SlaterKosterFiles/Type2FileNames/Prefix
    call hsd_get(root, "Hamiltonian/DFTB/SlaterKosterFiles/Type2FileNames/Prefix", &
        str_val, stat)
    call check(is_equal(stat, 0), msg="Deep path access OK")
    call check(str_val == "slakos/origin/mio-1-1/", msg="SK prefix correct")

    ! 4-level deep: Hamiltonian/DFTB/Filling/Fermi/Temperature
    call hsd_get(root, "Hamiltonian/DFTB/Filling/Fermi/Temperature", real_val, stat)
    call check(is_equal(stat, 0), msg="Deep real access OK")
    call check(abs(real_val - 300.0_dp) < 1.0e-10_dp, msg="Deep real value correct")

    ! 4-level deep: Hamiltonian/DFTB/Mixer/Simple/MixingParameter
    call hsd_get(root, "Hamiltonian/DFTB/Mixer/Simple/MixingParameter", real_val, stat)
    call check(is_equal(stat, 0), msg="Mixer path access OK")
    call check(abs(real_val - 0.05_dp) < 1.0e-10_dp, msg="MixingParameter correct")

    call root%destroy()
  end subroutine test_deep_path_navigation

  !> Test that GenFormat raw data is accessible
  subroutine test_genformat_raw_data()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error

    call load_dftb_input(root, error)
    call check(.not. allocated(error), msg="Parse OK")

    ! Geometry should contain GenFormat as a child table
    call check(hsd_is_table(root, "Geometry"), msg="Geometry is table")
    call check(hsd_is_table(root, "Geometry/GenFormat"), msg="GenFormat is table")

    ! GenFormat should have child content (the raw coordinate data)
    call check(hsd_child_count(root, "Geometry/GenFormat") > 0, &
        msg="GenFormat has content")

    call root%destroy()
  end subroutine test_genformat_raw_data

  !> Test DFTB+-style boolean values (Yes/No)
  subroutine test_boolean_values()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    logical :: bool_val
    integer :: stat

    call load_dftb_input(root, error)
    call check(.not. allocated(error), msg="Parse OK")

    ! SCC = Yes
    call hsd_get(root, "Hamiltonian/DFTB/SCC", bool_val, stat)
    call check(is_equal(stat, 0), msg="Get SCC OK")
    call check(bool_val, msg="SCC is true")

    ! WriteDetailedXML = No
    call hsd_get(root, "Options/WriteDetailedXML", bool_val, stat)
    call check(is_equal(stat, 0), msg="Get WriteDetailedXML OK")
    call check(.not. bool_val, msg="WriteDetailedXML is false")

    ! CalculateForces = Yes
    call hsd_get(root, "Options/CalculateForces", bool_val, stat)
    call check(is_equal(stat, 0), msg="Get CalculateForces OK")
    call check(bool_val, msg="CalculateForces is true")

    ! ParserOptions/WriteHSDInput = Yes
    call hsd_get(root, "ParserOptions/WriteHSDInput", bool_val, stat)
    call check(is_equal(stat, 0), msg="Get WriteHSDInput OK")
    call check(bool_val, msg="WriteHSDInput is true")

    ! ParserOptions/StopAfterParsing = No
    call hsd_get(root, "ParserOptions/StopAfterParsing", bool_val, stat)
    call check(is_equal(stat, 0), msg="Get StopAfterParsing OK")
    call check(.not. bool_val, msg="StopAfterParsing is false")

    call root%destroy()
  end subroutine test_boolean_values

  !> Test DFTB+-style integer values
  subroutine test_integer_values()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    integer :: int_val, stat

    call load_dftb_input(root, error)
    call check(.not. allocated(error), msg="Parse OK")

    ! MaxSCCIterations = 100
    call hsd_get(root, "Hamiltonian/DFTB/MaxSCCIterations", int_val, stat)
    call check(is_equal(stat, 0), msg="Get MaxSCCIterations OK")
    call check(is_equal(int_val, 100), msg="MaxSCCIterations is 100")

    ! RandomSeed = 42
    call hsd_get(root, "Options/RandomSeed", int_val, stat)
    call check(is_equal(stat, 0), msg="Get RandomSeed OK")
    call check(is_equal(int_val, 42), msg="RandomSeed is 42")

    ! ParserVersion = 14
    call hsd_get(root, "ParserOptions/ParserVersion", int_val, stat)
    call check(is_equal(stat, 0), msg="Get ParserVersion OK")
    call check(is_equal(int_val, 14), msg="ParserVersion is 14")

    call root%destroy()
  end subroutine test_integer_values

  !> Test DFTB+-style real values (including scientific notation)
  subroutine test_real_values()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    real(dp) :: real_val
    integer :: stat

    call load_dftb_input(root, error)
    call check(.not. allocated(error), msg="Parse OK")

    ! SCCTolerance = 1.0e-5
    call hsd_get(root, "Hamiltonian/DFTB/SCCTolerance", real_val, stat)
    call check(is_equal(stat, 0), msg="Get SCCTolerance OK")
    call check(abs(real_val - 1.0e-5_dp) < 1.0e-15_dp, msg="SCCTolerance correct")

    ! Charge = 0.0
    call hsd_get(root, "Hamiltonian/DFTB/Charge", real_val, stat)
    call check(is_equal(stat, 0), msg="Get Charge OK")
    call check(abs(real_val) < 1.0e-15_dp, msg="Charge is 0.0")

    ! MixingParameter = 0.05
    call hsd_get(root, "Hamiltonian/DFTB/Mixer/Simple/MixingParameter", real_val, stat)
    call check(is_equal(stat, 0), msg="Get MixingParameter OK")
    call check(abs(real_val - 0.05_dp) < 1.0e-15_dp, msg="MixingParameter correct")

    call root%destroy()
  end subroutine test_real_values

  !> Test string value extraction
  subroutine test_string_values()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: str_val
    integer :: stat

    call load_dftb_input(root, error)
    call check(.not. allocated(error), msg="Parse OK")

    ! MaxAngularMomentum/C = "p"
    call hsd_get(root, "Hamiltonian/DFTB/MaxAngularMomentum/C", str_val, stat)
    call check(is_equal(stat, 0), msg="Get C angular momentum OK")
    call check(str_val == "p", msg="C angular momentum is 'p'")

    ! MaxAngularMomentum/H = "s"
    call hsd_get(root, "Hamiltonian/DFTB/MaxAngularMomentum/H", str_val, stat)
    call check(is_equal(stat, 0), msg="Get H angular momentum OK")
    call check(str_val == "s", msg="H angular momentum is 's'")

    ! Separator = "-"
    call hsd_get(root, &
        "Hamiltonian/DFTB/SlaterKosterFiles/Type2FileNames/Separator", str_val, stat)
    call check(is_equal(stat, 0), msg="Get Separator OK")
    call check(str_val == "-", msg="Separator is '-'")

    ! Suffix = ".skf"
    call hsd_get(root, &
        "Hamiltonian/DFTB/SlaterKosterFiles/Type2FileNames/Suffix", str_val, stat)
    call check(is_equal(stat, 0), msg="Get Suffix OK")
    call check(str_val == ".skf", msg="Suffix is '.skf'")

    call root%destroy()
  end subroutine test_string_values

  !> Test repeated sections (multiple Region blocks in ProjectStates)
  subroutine test_repeated_sections()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    type(hsd_iterator) :: iter
    class(hsd_node), pointer :: child
    type(hsd_table), pointer :: project_states
    integer :: stat, region_count

    call load_dftb_input(root, error)
    call check(.not. allocated(error), msg="Parse OK")

    ! ProjectStates should have multiple Region children
    call check(hsd_is_table(root, "Analysis/ProjectStates"), &
        msg="ProjectStates is table")

    call hsd_get_table(root, "Analysis/ProjectStates", project_states, stat)
    call check(is_equal(stat, 0), msg="Get ProjectStates table OK")

    ! Count Region children by iterating
    region_count = 0
    call iter%init(project_states)
    do while (iter%next(child))
      if (child%name == "Region") region_count = region_count + 1
    end do
    call check(is_equal(region_count, 2), msg="Two Region blocks found")

    call root%destroy()
  end subroutine test_repeated_sections

  !> Test deeply nested method blocks
  !> Note: when multiple children share the same name (like Region),
  !> path-based access returns the LAST one. Use iteration for all duplicates.
  subroutine test_nested_method_blocks()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: str_val
    integer :: stat

    call load_dftb_input(root, error)
    call check(.not. allocated(error), msg="Parse OK")

    ! Analysis/ProjectStates/Region should have sub-values
    ! With duplicate "Region" names, path access returns the last one
    call check(hsd_is_table(root, "Analysis/ProjectStates/Region"), &
        msg="Region is table")

    call hsd_get(root, "Analysis/ProjectStates/Region/Label", str_val, stat)
    call check(is_equal(stat, 0), msg="Get Region Label OK")
    ! Last Region has Label = "hydrogen" (hash table last-writer-wins)
    call check(str_val == "hydrogen", msg="Last Region label is 'hydrogen'")

    call root%destroy()
  end subroutine test_nested_method_blocks

  !> Test hsd_get_or for missing values (default fallback)
  subroutine test_default_values()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    integer :: int_val, stat
    real(dp) :: real_val
    logical :: bool_val
    character(len=:), allocatable :: str_val

    call load_dftb_input(root, error)
    call check(.not. allocated(error), msg="Parse OK")

    ! Nonexistent integer with default
    call hsd_get_or(root, "Hamiltonian/DFTB/NonexistentField", int_val, 999, stat)
    call check(is_equal(int_val, 999), msg="Default int value used")

    ! Nonexistent real with default
    call hsd_get_or(root, "Options/Timeout", real_val, 60.0_dp, stat)
    call check(abs(real_val - 60.0_dp) < 1.0e-10_dp, msg="Default real value used")

    ! Nonexistent boolean with default
    call hsd_get_or(root, "Options/VerboseOutput", bool_val, .false., stat)
    call check(.not. bool_val, msg="Default bool value used")

    ! Nonexistent string with default
    call hsd_get_or(root, "Options/LogFile", str_val, "output.log", stat)
    call check(str_val == "output.log", msg="Default string value used")

    call root%destroy()
  end subroutine test_default_values

  !> Test round-trip: parse → dump → re-parse → compare
  subroutine test_round_trip()
    type(hsd_table) :: root, reloaded
    type(hsd_error_t), allocatable :: error, error2
    character(len=512) :: dump_path
    integer :: stat
    logical :: scc_val
    real(dp) :: temp_val
    integer :: parser_version
    character(len=:), allocatable :: prefix

    call load_dftb_input(root, error)
    call check(.not. allocated(error), msg="Initial parse OK")

    ! Dump to file
    dump_path = build_dir // "/test_dftb_roundtrip.hsd"
    call hsd_dump(root, trim(dump_path), error)
    call check(.not. allocated(error), msg="Dump OK")

    ! Re-parse
    call hsd_load(trim(dump_path), reloaded, error2)
    call check(.not. allocated(error2), msg="Re-parse OK")

    ! Verify key values survived round-trip
    call hsd_get(reloaded, "Hamiltonian/DFTB/SCC", scc_val, stat)
    call check(is_equal(stat, 0), msg="Round-trip: SCC accessible")
    call check(scc_val, msg="Round-trip: SCC is true")

    call hsd_get(reloaded, "Hamiltonian/DFTB/Filling/Fermi/Temperature", temp_val, stat)
    call check(is_equal(stat, 0), msg="Round-trip: Temperature accessible")
    call check(abs(temp_val - 300.0_dp) < 1.0e-10_dp, msg="Round-trip: Temperature correct")

    call hsd_get(reloaded, "ParserOptions/ParserVersion", parser_version, stat)
    call check(is_equal(stat, 0), msg="Round-trip: ParserVersion accessible")
    call check(is_equal(parser_version, 14), msg="Round-trip: ParserVersion correct")

    call hsd_get(reloaded, &
        "Hamiltonian/DFTB/SlaterKosterFiles/Type2FileNames/Prefix", prefix, stat)
    call check(is_equal(stat, 0), msg="Round-trip: deep path accessible")
    call check(prefix == "slakos/origin/mio-1-1/", msg="Round-trip: deep path correct")

    call root%destroy()
    call reloaded%destroy()
  end subroutine test_round_trip

  !> Test building a DFTB+-like input programmatically
  subroutine test_programmatic_dftb_input()
    type(hsd_table) :: root, ham, dftb, filling, fermi
    type(hsd_table) :: reloaded
    type(hsd_error_t), allocatable :: error
    character(len=512) :: dump_path
    real(dp) :: temp_val
    logical :: scc_val
    integer :: stat

    ! Build DFTB+ input tree
    call new_table(root)
    call new_table(ham, "Hamiltonian")
    call new_table(dftb, "DFTB")
    call hsd_set(dftb, "SCC", .true.)
    call hsd_set(dftb, "SCCTolerance", 1.0e-5_dp)
    call new_table(filling, "Filling")
    call new_table(fermi, "Fermi")
    call hsd_set(fermi, "Temperature", 300.0_dp)
    call filling%add_child(fermi)
    call dftb%add_child(filling)
    call ham%add_child(dftb)
    call root%add_child(ham)

    ! Dump and re-parse
    dump_path = build_dir // "/test_dftb_programmatic.hsd"
    call hsd_dump(root, trim(dump_path), error)
    call check(.not. allocated(error), msg="Programmatic dump OK")

    call hsd_load(trim(dump_path), reloaded, error)
    call check(.not. allocated(error), msg="Programmatic re-parse OK")

    ! Verify
    call hsd_get(reloaded, "Hamiltonian/DFTB/SCC", scc_val, stat)
    call check(is_equal(stat, 0), msg="Programmatic: SCC accessible")
    call check(scc_val, msg="Programmatic: SCC is true")

    call hsd_get(reloaded, "Hamiltonian/DFTB/Filling/Fermi/Temperature", temp_val, stat)
    call check(is_equal(stat, 0), msg="Programmatic: Temperature accessible")
    call check(abs(temp_val - 300.0_dp) < 1.0e-10_dp, msg="Programmatic: Temperature correct")

    call root%destroy()
    call reloaded%destroy()
  end subroutine test_programmatic_dftb_input

  !> Test parsing inline DFTB+ patterns with hsd_load_string
  subroutine test_parse_inline_dftb()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    logical :: scc_val
    real(dp) :: real_val
    integer :: stat, int_val
    character(len=:), allocatable :: str_val

    ! Realistic DFTB+ snippet
    call hsd_load_string( &
        'Hamiltonian = DFTB {' // char(10) // &
        '  SCC = Yes' // char(10) // &
        '  SCCTolerance = 1.0e-10' // char(10) // &
        '  MaxAngularMomentum {' // char(10) // &
        '    C = "p"' // char(10) // &
        '    H = "s"' // char(10) // &
        '  }' // char(10) // &
        '  Filling = Fermi {' // char(10) // &
        '    Temperature [Kelvin] = 300.0' // char(10) // &
        '  }' // char(10) // &
        '}' // char(10) // &
        'Driver = {}' // char(10) // &
        'Options {' // char(10) // &
        '  WriteAutotestTag = Yes' // char(10) // &
        '}' // char(10) // &
        'ParserOptions {' // char(10) // &
        '  ParserVersion = 14' // char(10) // &
        '}', &
        root, error)
    call check(.not. allocated(error), msg="Inline DFTB+ parse OK")

    ! Verify named method block
    call check(hsd_is_table(root, "Hamiltonian"), msg="Inline: Hamiltonian is table")
    call check(hsd_is_table(root, "Hamiltonian/DFTB"), msg="Inline: DFTB is table")

    ! Verify values
    call hsd_get(root, "Hamiltonian/DFTB/SCC", scc_val, stat)
    call check(is_equal(stat, 0), msg="Inline: SCC OK")
    call check(scc_val, msg="Inline: SCC is true")

    call hsd_get(root, "Hamiltonian/DFTB/SCCTolerance", real_val, stat)
    call check(is_equal(stat, 0), msg="Inline: SCCTolerance OK")
    call check(abs(real_val - 1.0e-10_dp) < 1.0e-20_dp, msg="Inline: SCCTolerance correct")

    ! Verify attribute
    call check(hsd_has_attrib(root, "Hamiltonian/DFTB/Filling/Fermi/Temperature"), &
        msg="Inline: Temperature has attrib")

    ! Verify empty block
    call check(hsd_is_table(root, "Driver"), msg="Inline: Driver is table")
    call check(is_equal(hsd_child_count(root, "Driver"), 0), &
        msg="Inline: Driver is empty")

    ! Verify nested value
    call hsd_get(root, "Hamiltonian/DFTB/MaxAngularMomentum/C", str_val, stat)
    call check(is_equal(stat, 0), msg="Inline: C angular momentum OK")
    call check(str_val == "p", msg="Inline: C is 'p'")

    ! ParserVersion
    call hsd_get(root, "ParserOptions/ParserVersion", int_val, stat)
    call check(is_equal(stat, 0), msg="Inline: ParserVersion OK")
    call check(is_equal(int_val, 14), msg="Inline: ParserVersion is 14")

    call root%destroy()
  end subroutine test_parse_inline_dftb

end module test_dftb_integration_suite
