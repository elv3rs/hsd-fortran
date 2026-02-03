# AFL++ Fuzz Testing for HSD-Fortran
#
# This CMakeLists.txt builds the fuzz target for the HSD parser.
# The stdin driver can be used with AFL++, AFL, or any stdin-based fuzzer.
#
# Usage:
#   cmake -B build-fuzz -S utils/fuzz
#   cmake --build build-fuzz
#   afl-fuzz -i corpus -o findings -- ./build-fuzz/hsd_fuzz_stdin

cmake_minimum_required(VERSION 3.14)
project(hsd-fuzz LANGUAGES Fortran)

# Require parent HSD library source directory
if(NOT DEFINED HSD_SOURCE_DIR)
  set(HSD_SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/../..")
endif()

get_filename_component(HSD_SOURCE_DIR "${HSD_SOURCE_DIR}" ABSOLUTE)
message(STATUS "HSD source directory: ${HSD_SOURCE_DIR}")

# Verify the source directory exists
if(NOT EXISTS "${HSD_SOURCE_DIR}/src/hsd.f90")
  message(FATAL_ERROR "Cannot find HSD source at ${HSD_SOURCE_DIR}")
endif()

# Collect HSD library sources
file(GLOB_RECURSE HSD_SOURCES
  "${HSD_SOURCE_DIR}/src/*.f90"
)

# Build HSD library for fuzzing (static)
add_library(hsd_fuzz_lib STATIC ${HSD_SOURCES})
target_include_directories(hsd_fuzz_lib PUBLIC ${CMAKE_CURRENT_BINARY_DIR})

# Fortran module output directory
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/modules)
target_include_directories(hsd_fuzz_lib PUBLIC ${CMAKE_Fortran_MODULE_DIRECTORY})

# Standalone stdin driver for AFL++/AFL fuzzing
add_executable(hsd_fuzz_stdin fuzz_stdin_driver.f90)
target_link_libraries(hsd_fuzz_stdin PRIVATE hsd_fuzz_lib)

# Copy dictionary to build directory for convenience
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/hsd.dict ${CMAKE_CURRENT_BINARY_DIR}/hsd.dict COPYONLY)

message(STATUS "")
message(STATUS "=== HSD Fuzz Build Configuration ===")
message(STATUS "Fortran Compiler: ${CMAKE_Fortran_COMPILER}")
message(STATUS "")
message(STATUS "To run fuzzing with AFL++:")
message(STATUS "  afl-fuzz -i corpus -o findings -- ./hsd_fuzz_stdin")
message(STATUS "")
message(STATUS "For instrumented builds, use afl-gfortran:")
message(STATUS "  FC=afl-gfortran cmake -B build-fuzz -S utils/fuzz")
message(STATUS "")
