program bench_parser
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

  integer, parameter :: N_TRIALS = 1000
  type(hsd_table) :: root
  type(hsd_error_t), allocatable :: error
  character(len=:), allocatable :: hsd_text
  integer(8) :: t1, t2, rate
  real(8) :: dt
  integer :: i

  ! Medium sized HSD string
  hsd_text = &
    'Geometry = GenFormat {' // char(10) // &
    '  TypeNames = { "C" "H" "O" }' // char(10) // &
    '}' // char(10) // &
    'Driver = ConjugateGradient {' // char(10) // &
    '  MaxSteps = 200' // char(10) // &
    '  MaxForceComponent [eV/Angstrom] = 0.001' // char(10) // &
    '  Convergence {' // char(10) // &
    '    GradElem = 1.0e-4' // char(10) // &
    '    GradNorm = 1.0e-3' // char(10) // &
    '  }' // char(10) // &
    '  OutputPrefix = "geom.out"' // char(10) // &
    '}' // char(10) // &
    'Hamiltonian = DFTB {' // char(10) // &
    '  SCC = Yes' // char(10) // &
    '  SCCTolerance = 1.0e-5' // char(10) // &
    '  MaxAngularMomentum {' // char(10) // &
    '    C = "p"' // char(10) // &
    '    H = "s"' // char(10) // &
    '    O = "p"' // char(10) // &
    '  }' // char(10) // &
    '}'

  call system_clock(count_rate=rate)

  print '(A20, A20)', "Operation", "Time (us)"

  ! Benchmark parsing
  call system_clock(t1)
  do i = 1, N_TRIALS
    call hsd_load_string(hsd_text, root, error)
    if (allocated(error)) then
       print *, "Error parsing!"
       stop 1
    end if
    call root%destroy()
  end do
  call system_clock(t2)
  dt = real(t2 - t1, 8) / real(rate, 8) / real(N_TRIALS, 8) * 1.0d6
  print '(A20, F20.2)', "Parse String", dt

  ! Benchmark dumping (reuse a parsed tree)
  call hsd_load_string(hsd_text, root, error)
  call system_clock(t1)
  do i = 1, N_TRIALS
    call hsd_dump_to_string(root, hsd_text)
  end do
  call system_clock(t2)
  dt = real(t2 - t1, 8) / real(rate, 8) / real(N_TRIALS, 8) * 1.0d6
  print '(A20, F20.2)', "Dump String", dt

  call root%destroy()

end program bench_parser
