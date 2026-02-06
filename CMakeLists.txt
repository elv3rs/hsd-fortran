# CMake 3.14 is the tested minimum version. The project uses modern CMake
# features including target_link_libraries with PRIVATE/PUBLIC/INTERFACE,
# generator expressions, and CMakePackageConfigHelpers which are all available
# in CMake 3.14. If you encounter issues with an older CMake, please report.
cmake_minimum_required(VERSION 3.14)

project(hsd-fortran
  VERSION 1.0.0
  LANGUAGES Fortran
  DESCRIPTION "Human-friendly Structured Data parser for Fortran"
)

# Options
option(HSD_ACCEPT_TRUE_FALSE "Accept True/False as boolean values" ON)
option(HSD_BUILD_TESTS "Build test suite" ON)
option(HSD_BUILD_EXAMPLES "Build examples" ON)
option(HSD_BUILD_BENCHMARKS "Build benchmarks" ON)
option(HSD_COVERAGE "Enable code coverage instrumentation (requires GCC)" OFF)
option(HSD_SANITIZERS "Enable sanitizers for leak/memory detection in Debug builds (requires GCC/Clang)" OFF)

# Coverage flags (must be set before defining targets)
if(HSD_COVERAGE)
  if(NOT CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
    message(FATAL_ERROR "Coverage requires GCC compiler")
  endif()
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} --coverage")
  set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} --coverage")
endif()

# Sanitizer flags (must be set before defining targets)
if(HSD_SANITIZERS)
  if(NOT (CMAKE_Fortran_COMPILER_ID MATCHES "GNU" OR CMAKE_Fortran_COMPILER_ID MATCHES "Clang"))
    message(FATAL_ERROR "Sanitizers require GCC or Clang compiler")
  endif()
  if(NOT CMAKE_BUILD_TYPE STREQUAL "Debug")
    message(WARNING "Sanitizers are most effective in Debug builds. Current build type: ${CMAKE_BUILD_TYPE}")
  endif()
  # Address sanitizer detects memory errors, leaks, buffer overflows
  # Leak sanitizer detects memory leaks
  # Undefined behavior sanitizer detects undefined behavior
  set(SANITIZER_FLAGS "-fsanitize=address -fsanitize=leak -fsanitize=undefined")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${SANITIZER_FLAGS}")
  set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} ${SANITIZER_FLAGS}")
  message(STATUS "Sanitizers enabled: address, leak, undefined")
endif()

# Add fortuno for unit testing
if(HSD_BUILD_TESTS)
  include(FetchContent)
  FetchContent_Declare(
    Fortuno
    GIT_REPOSITORY "https://github.com/elv3rs/fortuno"
    GIT_TAG "discovery"
  )
  set(FORTUNO_WITH_TESTS OFF CACHE BOOL "" FORCE)
  set(FORTUNO_WITH_EXAMPLES OFF CACHE BOOL "" FORCE)
  set(FORTUNO_INSTALL OFF CACHE BOOL "" FORCE)
  FetchContent_MakeAvailable(Fortuno)
  #list(APPEND CMAKE_MODULE_PATH ${fortuno_SOURCE_DIR}/cmake)
  include(Fortuno)
endif()

# Library
add_subdirectory(src)

# Tests
if(HSD_BUILD_TESTS)
  enable_testing()
  add_subdirectory(test)
endif()

# Examples
if(HSD_BUILD_EXAMPLES)
  add_subdirectory(example)
endif()

# Benchmarks
if(HSD_BUILD_BENCHMARKS)
  add_subdirectory(utils/bench)
endif()

# Installation
include(GNUInstallDirs)
include(CMakePackageConfigHelpers)

install(TARGETS hsd
  EXPORT hsd-targets
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
)

install(DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/modules/
  DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/hsd
)

install(EXPORT hsd-targets
  FILE hsd-targets.cmake
  NAMESPACE hsd::
  DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/hsd
)

configure_package_config_file(
  ${CMAKE_CURRENT_SOURCE_DIR}/cmake/hsd-config.cmake.in
  ${CMAKE_CURRENT_BINARY_DIR}/hsd-config.cmake
  INSTALL_DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/hsd
)

write_basic_package_version_file(
  ${CMAKE_CURRENT_BINARY_DIR}/hsd-config-version.cmake
  VERSION ${PROJECT_VERSION}
  COMPATIBILITY SameMajorVersion
)

install(FILES
  ${CMAKE_CURRENT_BINARY_DIR}/hsd-config.cmake
  ${CMAKE_CURRENT_BINARY_DIR}/hsd-config-version.cmake
  DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/hsd
)
