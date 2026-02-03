!> Test application driving Fortuno unit tests for HSD
program testapp
  use fortuno_serial, only : execute_serial_cmd_app, test_list
  use test_lexer_suite, only : lexer_tests => tests
  use test_parser_suite, only : parser_tests => tests
  use test_formatter_suite, only : formatter_tests => tests
  use test_array_suite, only : array_tests => tests
  use test_error_suite, only : error_tests => tests
  use test_api_suite, only : api_tests => tests
  use test_coverage_suite, only : coverage_tests => tests
  use test_formatter_extended_suite, only : formatter_ext_tests => tests
  use test_file_ops_suite, only : file_ops_tests => tests
  use test_edge_cases_suite, only : edge_cases_tests => tests
  use test_error_paths_suite, only : error_paths_tests
  use test_final_coverage_suite, only : final_coverage_tests
  use test_deep_coverage_suite, only : deep_coverage_tests
  use test_ultra_coverage_suite, only : ultra_coverage_tests
  use test_file_dump_coverage_suite, only : file_dump_coverage_tests
  use test_types_edge_coverage_suite, only : types_edge_coverage_tests
  use test_validation_mutator_coverage_suite, only : validation_mutator_coverage_tests
  use test_stat_coverage_suite, only : stat_coverage_tests
  use test_remaining_coverage_suite, only : remaining_coverage_tests
  use test_schema_suite, only : schema_tests => tests
  use test_fuzz_suite, only : fuzz_tests => tests
  implicit none (type, external)

  ! Execute all test suites
  ! Note: this function does not return but stops the code with the right exit code
  call execute_serial_cmd_app(test_list([&
      lexer_tests(), &
      parser_tests(), &
      formatter_tests(), &
      array_tests(), &
      error_tests(), &
      api_tests(), &
      coverage_tests(), &
      formatter_ext_tests(), &
      file_ops_tests(), &
      edge_cases_tests(), &
      error_paths_tests(), &
      final_coverage_tests(), &
      deep_coverage_tests(), &
      ultra_coverage_tests(), &
      file_dump_coverage_tests(), &
      types_edge_coverage_tests(), &
      validation_mutator_coverage_tests(), &
      stat_coverage_tests(), &
      remaining_coverage_tests(), &
      schema_tests(), &
      fuzz_tests() &
  ]))

end program testapp
