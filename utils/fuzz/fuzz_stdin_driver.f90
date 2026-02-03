!> Standalone fuzz driver for AFL/AFL++ integration
!>
!> This program reads HSD input from stdin and parses it, suitable for
!> fuzzing with AFL, AFL++, or similar file/stdin-based fuzzers.
!>
!> Build with AFL:
!>   afl-gfortran -o fuzz_stdin fuzz_stdin_driver.f90 -L../build/src -lhsd
!>
!> Run with AFL:
!>   afl-fuzz -i corpus -o findings -- ./fuzz_stdin
program fuzz_stdin_driver
  use hsd, only : &
    & hsd_table, hsd_value, hsd_node, hsd_error_t, hsd_iterator, hsd_node_ptr, &
    & hsd_load, hsd_load_string, hsd_dump, hsd_dump_to_string, &
    & hsd_get, hsd_get_or, hsd_get_matrix, hsd_get_child, hsd_get_table, &
    & hsd_get_attrib, hsd_get_keys, hsd_get_type, hsd_set, hsd_has_child, &
    & hsd_has_attrib, hsd_is_table, hsd_is_value, hsd_is_array, hsd_child_count, &
    & hsd_require, hsd_validate_range, hsd_validate_one_of, hsd_visitor_t, &
    & hsd_accept, hsd_merge, hsd_clone, hsd_remove_child, dp, sp, &
    & VALUE_TYPE_NONE, VALUE_TYPE_ARRAY, VALUE_TYPE_STRING, VALUE_TYPE_INTEGER, &
    & VALUE_TYPE_REAL, VALUE_TYPE_LOGICAL, VALUE_TYPE_COMPLEX, &
    & HSD_STAT_OK, HSD_STAT_NOT_FOUND, HSD_STAT_SYNTAX_ERROR, &
    & HSD_STAT_UNCLOSED_TAG, HSD_STAT_UNCLOSED_ATTRIB, HSD_STAT_UNCLOSED_QUOTE, &
    & HSD_STAT_ORPHAN_TEXT, HSD_STAT_INCLUDE_CYCLE, HSD_STAT_INCLUDE_DEPTH, &
    & HSD_STAT_FILE_NOT_FOUND, HSD_STAT_IO_ERROR, HSD_STAT_TYPE_ERROR, &
    & hsd_schema_t, schema_init, schema_destroy, schema_add_field, &
    & schema_add_field_enum, schema_validate, schema_validate_strict, &
    & FIELD_REQUIRED, FIELD_OPTIONAL, FIELD_TYPE_STRING, FIELD_TYPE_INTEGER, &
    & FIELD_TYPE_REAL, FIELD_TYPE_LOGICAL, FIELD_TYPE_ARRAY, FIELD_TYPE_TABLE, &
    & new_table, new_value
  implicit none (type, external)

  character(len=1048576) :: input_buffer
  type(hsd_table) :: root
  type(hsd_error_t), allocatable :: error
  integer :: input_len, io_stat

  ! Read all available input from stdin
  input_len = 0
  do
    read(*, '(A)', iostat=io_stat, advance='no', size=input_len) input_buffer
    if (io_stat /= 0) exit
  end do

  ! Parse the input (this is what we're testing)
  if (input_len > 0) then
    call hsd_load_string(input_buffer(1:input_len), root, error)
  else
    call hsd_load_string("", root, error)
  end if

  ! Clean up
  call root%destroy()
  if (allocated(error)) deallocate(error)

end program fuzz_stdin_driver
