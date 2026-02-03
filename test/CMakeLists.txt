# Generate build_env module with absolute paths for reliable file I/O
configure_file(
  ${CMAKE_CURRENT_SOURCE_DIR}/build_env.f90.in
  ${CMAKE_CURRENT_BINARY_DIR}/build_env.f90
  @ONLY
)

# Build environment library providing source/build directory paths
add_library(build_env STATIC ${CMAKE_CURRENT_BINARY_DIR}/build_env.f90)
target_include_directories(build_env PUBLIC ${CMAKE_CURRENT_BINARY_DIR})

# Test suites organized by category
set(TEST_CORE_SUITES
  suites/core/test_error_suite.f90
  suites/core/test_error_paths_suite.f90
  suites/core/test_array_suite.f90
  suites/core/test_edge_cases_suite.f90
)

set(TEST_IO_SUITES
  suites/io/test_lexer_suite.f90
  suites/io/test_parser_suite.f90
  suites/io/test_formatter_suite.f90
  suites/io/test_formatter_extended_suite.f90
  suites/io/test_file_ops_suite.f90
)

set(TEST_API_SUITES
  suites/api/test_api_suite.f90
  suites/api/test_schema_suite.f90
  suites/api/test_validation_mutator_coverage_suite.f90
)

set(TEST_COVERAGE_SUITES
  suites/coverage/test_coverage_suite.f90
  suites/coverage/test_deep_coverage_suite.f90
  suites/coverage/test_ultra_coverage_suite.f90
  suites/coverage/test_final_coverage_suite.f90
  suites/coverage/test_stat_coverage_suite.f90
  suites/coverage/test_remaining_coverage_suite.f90
  suites/coverage/test_file_dump_coverage_suite.f90
  suites/coverage/test_types_edge_coverage_suite.f90
  suites/coverage/test_fuzz_suite.f90
)

# Fortuno-based unit tests
add_executable(hsd_testapp
  testapp.f90
  ${TEST_CORE_SUITES}
  ${TEST_IO_SUITES}
  ${TEST_API_SUITES}
  ${TEST_COVERAGE_SUITES}
)
target_link_libraries(hsd_testapp PRIVATE hsd Fortuno::fortuno_serial build_env)
target_include_directories(hsd_testapp PRIVATE ${CMAKE_BINARY_DIR}/modules)

# Register tests with CTest via fortuno_discover_tests
fortuno_discover_tests(hsd_testapp)
