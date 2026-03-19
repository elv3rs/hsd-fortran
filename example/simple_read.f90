! HSD Library - Feature Showcase Example
!
! This example demonstrates:
! - Loading HSD from file
! - The hsd_access_t object for reading/writing values
! - Error accumulation (batch error checking)
! - Path-based navigation
! - Type introspection
! - Default values
! - Attribute extraction (units)
! - Array and matrix handling
! - Tree operations (merge, clone)
! - Validation helpers
! - Writing modified data
program simple_read
  use hsd, only: &
      & hsd_node_t, hsd_error_t, hsd_access_t, dp, &
      & HSD_STAT_OK, HSD_STAT_NOT_FOUND, &
      & hsd_load_file, hsd_load_string, hsd_dump, &
      & hsd_get_attrib, hsd_get_keys, &
      & hsd_has_child, hsd_has_attrib, hsd_is_table, hsd_is_value, &
      & hsd_child_count, &
      & hsd_require, hsd_validate_range, hsd_validate_one_of, &
      & hsd_merge, hsd_clone
  implicit none (type, external)

  type(hsd_node_t), target :: root
  type(hsd_node_t), target :: modified_root, merged_root
  type(hsd_access_t) :: access, mod_access
  type(hsd_error_t), allocatable :: error
  integer :: stat, max_steps, random_seed, nrows, ncols
  real(dp) :: temperature, tolerance, max_force
  real(dp), allocatable :: kpoints(:,:), coords(:,:)
  logical :: scc_enabled
  character(len=:), allocatable :: attrib, output_prefix, method
  character(len=:), allocatable :: keys(:), convergence_opts(:)

  print '(A)', repeat("=", 70)
  print '(A)', "HSD Library - Feature Showcase"
  print '(A)', repeat("=", 70)
  print *

  ! ===========================================================================
  ! 1. Load HSD from file and initialize access object
  ! ===========================================================================
  print '(A)', "1. Loading from file 'sample_input.hsd'..."
  call hsd_load_file("sample_input.hsd", root, error)

  if (allocated(error)) then
    print '(A)', "   ERROR: Failed to parse file"
    call error%print()
    stop 1
  end if
  call access%init(root)
  print '(A)', "   ✓ Parsed successfully!"
  print *

  ! ===========================================================================
  ! 2. Type introspection and navigation (free functions)
  ! ===========================================================================
  print '(A)', "2. Exploring structure..."
  print '(A,I0,A)', "   Root has ", &
      & hsd_child_count(root, ""), " top-level sections"

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
  ! 3. Reading values with access%get (no per-call error checking!)
  ! ===========================================================================
  print '(A)', "3. Reading configuration values..."

  call access%get("Driver/MaxSteps", max_steps)
  print '(A,I0)', "   Driver/MaxSteps = ", max_steps

  call access%get("Hamiltonian/DFTB/SCC", scc_enabled)
  print '(A,L1)', "   Hamiltonian/DFTB/SCC = ", scc_enabled

  call access%get("Hamiltonian/DFTB/SCCTolerance", tolerance)
  print '(A,ES10.3)', "   Hamiltonian/DFTB/SCCTolerance = ", tolerance
  print *

  ! ===========================================================================
  ! 4. Using access%get with defaults (writes defaults back to tree)
  ! ===========================================================================
  print '(A)', "4. Reading values with defaults..."

  call access%get("Options/RandomSeed", random_seed, default=12345)
  print '(A,I0)', "   Options/RandomSeed = ", random_seed

  call access%get("Options/MissingOption", random_seed, default=99999)
  print '(A,I0,A)', "   Options/MissingOption = ", random_seed, &
      & " (default)"

  call access%get("Driver/OutputPrefix", output_prefix, &
      & default="output")
  print '(A,A,A)', '   Driver/OutputPrefix = "', output_prefix, '"'
  print *

  ! ===========================================================================
  ! 5. Attribute extraction (units) — free functions
  ! ===========================================================================
  print '(A)', "5. Extracting attributes (units)..."

  call hsd_get_attrib(root, &
      & "Hamiltonian/DFTB/Filling/Fermi/Temperature", attrib, stat)
  call access%get( &
      & "Hamiltonian/DFTB/Filling/Fermi/Temperature", temperature)
  print '(A,F6.1,A,A,A)', "   Temperature = ", temperature, &
      & " [", attrib, "]"

  if (hsd_has_attrib(root, "Driver/MaxForceComponent")) then
    call hsd_get_attrib(root, "Driver/MaxForceComponent", attrib, stat)
    call access%get("Driver/MaxForceComponent", max_force)
    print '(A,ES9.2,A,A,A)', "   MaxForceComponent = ", max_force, &
        & " [", attrib, "]"
  end if
  print *

  ! ===========================================================================
  ! 6. Array handling
  ! ===========================================================================
  print '(A)', "6. Working with arrays..."

  call access%get("Options/ConvergenceOptions", convergence_opts)
  if (.not. access%has_errors() .and. allocated(convergence_opts)) then
    print '(A,I0,A)', "   ConvergenceOptions has ", &
        & size(convergence_opts), " elements:"
    do stat = 1, size(convergence_opts)
      print '(A,I0,A,A,A)', "     [", stat, "] = '", &
          & convergence_opts(stat), "'"
    end do
  end if
  print *

  ! ===========================================================================
  ! 7. Matrix handling
  ! ===========================================================================
  print '(A)', "7. Reading matrix data..."

  call access%get_matrix("Hamiltonian/DFTB/KPointsAndWeights", &
      & kpoints, nrows, ncols)
  if (.not. access%has_errors()) then
    print '(A,I0,A,I0,A)', "   KPointsAndWeights: ", &
        & nrows, " x ", ncols, " matrix"
    print '(A)', "   First row: "
    print '(A,4F8.3)', "   ", kpoints(1,:)
  end if

  call access%get_matrix( &
      & "Hamiltonian/DFTB/ElectricField/PointCharges/CoordsAndCharges", &
      & coords, nrows, ncols)
  if (.not. access%has_errors()) then
    print '(A,I0,A,I0,A)', "   CoordsAndCharges: ", &
        & nrows, " x ", ncols, " matrix"
    print '(A)', "   Showing point charges:"
    do stat = 1, nrows
      print '(A,3F7.2,F7.2)', "     ", &
          & coords(stat, 1:3), coords(stat, 4)
    end do
  end if
  print *

  ! Check for any accumulated errors
  if (access%has_errors()) then
    print '(A)', "WARNING: Some reads had errors:"
    call access%print_errors()
    call access%clear_errors()
  end if

  ! ===========================================================================
  ! 8. Validation helpers
  ! ===========================================================================
  print '(A)', "8. Validating configuration..."

  call hsd_require(root, "Hamiltonian/DFTB/SCC", error, &
      & context="Validation")
  if (.not. allocated(error)) then
    print '(A)', "   ✓ Required field 'Hamiltonian/DFTB/SCC' exists"
  end if

  call hsd_validate_range(root, "Hamiltonian/DFTB/SCCTolerance", &
      & 1.0e-10_dp, 1.0e-2_dp, error, context="Tolerance check")
  if (.not. allocated(error)) then
    print '(A)', "   ✓ SCCTolerance is within valid range"
  else
    print '(A)', "   ✗ SCCTolerance validation failed"
    call error%print()
    deallocate(error)
  end if

  call access%get("Hamiltonian/DFTB/MaxAngularMomentum/C", method, &
      & default="unknown")
  call hsd_validate_one_of(root, &
      & "Hamiltonian/DFTB/MaxAngularMomentum/C", &
      & [character(len=3) :: "s", "p", "d", "f"], error)
  if (.not. allocated(error)) then
    print '(A,A,A)', "   ✓ MaxAngularMomentum/C = '", method, &
        & "' is valid"
  end if
  print *

  ! ===========================================================================
  ! 9. Modify via access object and write
  ! ===========================================================================
  print '(A)', "9. Modifying configuration..."

  call hsd_clone(root, modified_root)
  call mod_access%init(modified_root)

  call mod_access%set("Driver/MaxSteps", 500)
  call mod_access%set("NewSection/EnableFeature", .true.)
  call mod_access%set("NewSection/Coefficients", &
      & [1.0_dp, 2.5_dp, 3.7_dp])

  print '(A)', "   ✓ Cloned and modified tree"
  print '(A)', &
      & "   Added NewSection with EnableFeature and Coefficients array"
  print *

  ! ===========================================================================
  ! 10. Merge configurations
  ! ===========================================================================
  print '(A)', "10. Merging configurations..."

  block
    type(hsd_node_t) :: overlay
    type(hsd_access_t) :: merge_access
    character(len=:), allocatable :: overlay_text

    overlay_text = "Driver { MaxSteps = 1000 }" // char(10) // &
        & "Analysis { ComputeBandStructure = Yes }"

    call hsd_load_string(overlay_text, overlay, error)
    if (.not. allocated(error)) then
      call hsd_clone(root, merged_root)
      call hsd_merge(merged_root, overlay)
      print '(A)', "   ✓ Merged overlay configuration"

      call merge_access%init(merged_root)
      call merge_access%get("Driver/MaxSteps", max_steps)
      print '(A,I0)', "   Merged Driver/MaxSteps = ", max_steps

      if (hsd_has_child(merged_root, &
          & "Analysis/ComputeBandStructure")) then
        print '(A)', &
            & "   ✓ New Analysis/ComputeBandStructure field added"
      end if

      call overlay%destroy()
      call merged_root%destroy()
    end if
  end block
  print *

  ! ===========================================================================
  ! 11. Write modified configuration
  ! ===========================================================================
  print '(A)', &
      & "11. Writing modified configuration to 'modified_output.hsd'..."

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
