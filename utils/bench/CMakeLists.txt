# Benchmark executables
add_executable(bench_lookup bench_lookup.f90)
target_link_libraries(bench_lookup PRIVATE hsd)

add_executable(bench_copy bench_copy.f90)
target_link_libraries(bench_copy PRIVATE hsd)

add_executable(bench_parser bench_parser.f90)
target_link_libraries(bench_parser PRIVATE hsd)

add_executable(bench_access bench_access.f90)
target_link_libraries(bench_access PRIVATE hsd)

add_executable(bench_array bench_array.f90)
target_link_libraries(bench_array PRIVATE hsd)

# Use higher optimization for benchmarks if not already set
if(CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
  target_compile_options(bench_lookup PRIVATE -O3)
  target_compile_options(bench_copy PRIVATE -O3)
  target_compile_options(bench_parser PRIVATE -O3)
  target_compile_options(bench_access PRIVATE -O3)
  target_compile_options(bench_array PRIVATE -O3)
endif()

# Add to CTest (as benchmarks)
add_test(NAME bench_lookup COMMAND bench_lookup)
add_test(NAME bench_copy COMMAND bench_copy)
add_test(NAME bench_parser COMMAND bench_parser)
add_test(NAME bench_access COMMAND bench_access)
add_test(NAME bench_array COMMAND bench_array)
