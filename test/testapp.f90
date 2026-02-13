!> Test application driving Fortuno unit tests for HSD
program testapp
  use fortuno_serial, only: execute_serial_cmd_app, test_list
  use build_env, only: build_env_init
  use test_lexer_suite, only: lexer_tests => tests
  use test_parser_suite, only: parser_tests => tests
  use test_formatter_suite, only: formatter_tests => tests
  use test_array_suite, only: array_tests => tests
  use test_error_suite, only: error_tests => tests
  use test_hash_table_suite, only: hash_table_tests => tests
  use test_api_suite, only: api_tests => tests
  use test_formatter_extended_suite, only: formatter_ext_tests => tests
  use test_file_ops_suite, only: file_ops_tests => tests
  use test_edge_cases_suite, only: edge_cases_tests => tests
  use test_error_paths_suite, only: error_paths_tests
  use test_validation_mutator_coverage_suite, only: validation_mutator_coverage_tests
  use test_schema_suite, only: schema_tests => tests
  use test_fuzz_suite, only: fuzz_tests => tests
  use count_visitor_mod, only: count_visitor
  use test_full_coverage_suite, only: full_coverage_tests => tests
  use test_dftb_integration_suite, only: dftb_integration_tests => tests
  use test_new_apis_suite, only: new_apis_tests => tests
  use test_set_processed_suite, only: set_processed_tests => tests
  use test_tree_utils_suite, only: tree_utils_tests => tests
  use test_edge_cases_io_suite, only: io_edge_cases_tests => tests
  use test_coverage_gaps_suite, only: coverage_gaps_tests => tests
  implicit none (type, external)

  ! Initialize build environment paths (no-op for CMake, sets getcwd for fpm)
  call build_env_init()
  call execute_serial_cmd_app(test_list([&
      lexer_tests(), &
      parser_tests(), &
      formatter_tests(), &
      array_tests(), &
      error_tests(), &
      hash_table_tests(), &
      api_tests(), &
      formatter_ext_tests(), &
      file_ops_tests(), &
      edge_cases_tests(), &
      error_paths_tests(), &
      validation_mutator_coverage_tests(), &
      schema_tests(), &
      fuzz_tests(), &
      full_coverage_tests(), &
      dftb_integration_tests(), &
      new_apis_tests(), &
      set_processed_tests(), &
      tree_utils_tests(), &
      io_edge_cases_tests(), &
      coverage_gaps_tests() &
  ]))

end program testapp
