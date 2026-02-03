! HSD Library - Feature Showcase Example
!
! This example demonstrates:
! - Loading HSD from file
! - Path-based navigation
! - Type introspection
! - Default values with hsd_get_or
! - Attribute extraction (units)
! - Array and matrix handling
! - Tree operations (merge, clone)
! - Validation helpers
! - Writing modified data
program simple_read
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
  implicit none (type, external)

  type(hsd_table) :: root, modified_root, merged_root
  type(hsd_error_t), allocatable :: error
  integer :: stat, max_steps, random_seed, nrows, ncols
  real(dp) :: temperature, tolerance, max_force
  real(dp), allocatable :: kpoints(:,:), coords(:,:)
  integer, allocatable :: atoms(:)
  logical :: scc_enabled, detailed_xml
  character(len=:), allocatable :: attrib, output_prefix, method
  character(len=:), allocatable :: keys(:), convergence_opts(:)

  print '(A)', repeat("=", 70)
  print '(A)', "HSD Library - Feature Showcase"
  print '(A)', repeat("=", 70)
  print *

  ! ===========================================================================
  ! 1. Load HSD from file
  ! ===========================================================================
  print '(A)', "1. Loading from file 'sample_input.hsd'..."
  call hsd_load("sample_input.hsd", root, error)

  if (allocated(error)) then
    print '(A)', "   ERROR: Failed to parse file"
    call error%print()
    stop 1
  end if
  print '(A)', "   ✓ Parsed successfully!"
  print *

  ! ===========================================================================
  ! 2. Type introspection and navigation
  ! ===========================================================================
  print '(A)', "2. Exploring structure..."
  print '(A,I0,A)', "   Root has ", hsd_child_count(root, ""), " top-level sections"

  call hsd_get_keys(root, "", keys)
  print '(A)', "   Sections: " // keys(1)
  do stat = 2, size(keys)
    print '(A)', "             " // keys(stat)
  end do
  print *

  ! Check types
  if (hsd_is_table(root, "Hamiltonian")) then
    print '(A)', "   ✓ 'Hamiltonian' is a table (nested structure)"
  end if
  if (hsd_is_value(root, "Driver/MaxSteps")) then
    print '(A)', "   ✓ 'Driver/MaxSteps' is a value (leaf node)"
  end if
  print *

  ! ===========================================================================
  ! 3. Extracting values with hsd_get
  ! ===========================================================================
  print '(A)', "3. Reading configuration values..."

  call hsd_get(root, "Driver/MaxSteps", max_steps, stat)
  print '(A,I0)', "   Driver/MaxSteps = ", max_steps

  call hsd_get(root, "Hamiltonian/DFTB/SCC", scc_enabled, stat)
  print '(A,L1)', "   Hamiltonian/DFTB/SCC = ", scc_enabled

  call hsd_get(root, "Hamiltonian/DFTB/SCCTolerance", tolerance, stat)
  print '(A,ES10.3)', "   Hamiltonian/DFTB/SCCTolerance = ", tolerance
  print *

  ! ===========================================================================
  ! 4. Using hsd_get_or for defaults
  ! ===========================================================================
  print '(A)', "4. Reading values with defaults (hsd_get_or)..."

  call hsd_get_or(root, "Options/RandomSeed", random_seed, 12345, stat)
  print '(A,I0)', "   Options/RandomSeed = ", random_seed

  call hsd_get_or(root, "Options/MissingOption", random_seed, 99999, stat)
  print '(A,I0,A)', "   Options/MissingOption = ", random_seed, " (default)"

  call hsd_get_or(root, "Driver/OutputPrefix", output_prefix, "output", stat)
  print '(A,A,A)', '   Driver/OutputPrefix = "', output_prefix, '"'
  print *

  ! ===========================================================================
  ! 5. Attribute extraction (units)
  ! ===========================================================================
  print '(A)', "5. Extracting attributes (units)..."

  call hsd_get_attrib(root, "Hamiltonian/DFTB/Filling/Fermi/Temperature", attrib, stat)
  call hsd_get(root, "Hamiltonian/DFTB/Filling/Fermi/Temperature", temperature, stat)
  print '(A,F6.1,A,A,A)', "   Temperature = ", temperature, " [", attrib, "]"

  if (hsd_has_attrib(root, "Driver/MaxForceComponent")) then
    call hsd_get_attrib(root, "Driver/MaxForceComponent", attrib, stat)
    call hsd_get(root, "Driver/MaxForceComponent", max_force, stat)
    print '(A,ES9.2,A,A,A)', "   MaxForceComponent = ", max_force, " [", attrib, "]"
  end if
  print *

  ! ===========================================================================
  ! 6. Array handling
  ! ===========================================================================
  print '(A)', "6. Working with arrays..."

  ! String array
  call hsd_get(root, "Options/ConvergenceOptions", convergence_opts, stat)
  if (stat == HSD_STAT_OK) then
    print '(A,I0,A)', "   ConvergenceOptions has ", size(convergence_opts), " elements:"
    do stat = 1, size(convergence_opts)
      print '(A,I0,A,A,A)', "     [", stat, "] = '", convergence_opts(stat), "'"
    end do
  end if
  print *

  ! ===========================================================================
  ! 7. Matrix handling
  ! ===========================================================================
  print '(A)', "7. Reading matrix data..."

  call hsd_get_matrix(root, "Hamiltonian/DFTB/KPointsAndWeights", kpoints, nrows, ncols, stat)
  if (stat == HSD_STAT_OK) then
    print '(A,I0,A,I0,A)', "   KPointsAndWeights: ", nrows, " x ", ncols, " matrix"
    print '(A)', "   First row: "
    print '(A,4F8.3)', "   ", kpoints(1,:)
  end if

  call hsd_get_matrix(root, &
    "Hamiltonian/DFTB/ElectricField/PointCharges/CoordsAndCharges", &
    coords, nrows, ncols, stat)
  if (stat == HSD_STAT_OK) then
    print '(A,I0,A,I0,A)', "   CoordsAndCharges: ", nrows, " x ", ncols, " matrix"
    print '(A)', "   Showing point charges:"
    do stat = 1, nrows
      print '(A,3F7.2,F7.2)', "     ", coords(stat, 1:3), coords(stat, 4)
    end do
  end if
  print *

  ! ===========================================================================
  ! 8. Validation helpers
  ! ===========================================================================
  print '(A)', "8. Validating configuration..."

  ! Check required fields
  call hsd_require(root, "Hamiltonian/DFTB/SCC", error, context="Validation")
  if (.not. allocated(error)) then
    print '(A)', "   ✓ Required field 'Hamiltonian/DFTB/SCC' exists"
  end if

  ! Validate range
  call hsd_validate_range(root, "Hamiltonian/DFTB/SCCTolerance", &
    1.0e-10_dp, 1.0e-2_dp, error, context="Tolerance check")
  if (.not. allocated(error)) then
    print '(A)', "   ✓ SCCTolerance is within valid range"
  else
    print '(A)', "   ✗ SCCTolerance validation failed"
    call error%print()
    deallocate(error)
  end if

  ! Validate enum
  call hsd_get_or(root, "Hamiltonian/DFTB/MaxAngularMomentum/C", method, "unknown")
  call hsd_validate_one_of(root, "Hamiltonian/DFTB/MaxAngularMomentum/C", &
    [character(len=3) :: "s", "p", "d", "f"], error)
  if (.not. allocated(error)) then
    print '(A,A,A)', "   ✓ MaxAngularMomentum/C = '", method, "' is valid"
  end if
  print *

  ! ===========================================================================
  ! 9. Tree operations - Modify and write
  ! ===========================================================================
  print '(A)', "9. Modifying configuration..."

  ! Clone the tree
  call hsd_clone(root, modified_root)

  ! Set new values
  call hsd_set(modified_root, "Driver/MaxSteps", 500)
  call hsd_set(modified_root, "NewSection/EnableFeature", .true.)
  call hsd_set(modified_root, "NewSection/Coefficients", [1.0_dp, 2.5_dp, 3.7_dp])

  print '(A)', "   ✓ Cloned and modified tree"
  print '(A)', "   Added NewSection with EnableFeature and Coefficients array"
  print *

  ! ===========================================================================
  ! 10. Merge configurations
  ! ===========================================================================
  print '(A)', "10. Merging configurations..."

  ! Create an overlay config
  block
    type(hsd_table) :: overlay
    character(len=:), allocatable :: overlay_text

    overlay_text = "Driver { MaxSteps = 1000 }" // char(10) // &
                   "Analysis { ComputeBandStructure = Yes }"

    call hsd_load_string(overlay_text, overlay, error)
    if (.not. allocated(error)) then
      call hsd_clone(root, merged_root)
      call hsd_merge(merged_root, overlay)
      print '(A)', "   ✓ Merged overlay configuration"

      call hsd_get(merged_root, "Driver/MaxSteps", max_steps)
      print '(A,I0)', "   Merged Driver/MaxSteps = ", max_steps

      if (hsd_has_child(merged_root, "Analysis/ComputeBandStructure")) then
        print '(A)', "   ✓ New Analysis/ComputeBandStructure field added"
      end if

      call overlay%destroy()
      call merged_root%destroy()
    end if
  end block
  print *

  ! ===========================================================================
  ! 11. Write modified configuration
  ! ===========================================================================
  print '(A)', "11. Writing modified configuration to 'modified_output.hsd'..."

  call hsd_dump(modified_root, "modified_output.hsd", error)
  if (.not. allocated(error)) then
    print '(A)', "   ✓ Written successfully"
  else
    print '(A)', "   ✗ Write failed"
    call error%print()
  end if
  print *

  ! ===========================================================================
  ! Cleanup
  ! ===========================================================================
  call root%destroy()
  call modified_root%destroy()

  print '(A)', repeat("=", 70)
  print '(A)', "Example completed successfully!"
  print '(A)', repeat("=", 70)

end program simple_read
