# HSD library sources - organized by subdirectory
# Core modules (foundation)
set(HSD_CORE_SOURCES
  core/hsd_constants.f90
  core/hsd_utils.f90
  core/hsd_error.f90
  core/hsd_hash_table.f90
)

# I/O modules (parsing and serialization)
set(HSD_IO_SOURCES
  io/hsd_token.f90
  io/hsd_lexer.f90
  io/hsd_parser.f90
  io/hsd_formatter.f90
)

# API modules (public operations)
set(HSD_API_SOURCES
  api/hsd_visitor.f90
  api/hsd_query.f90
  api/hsd_accessors.f90
  api/hsd_mutators.f90
  api/hsd_validation.f90
  api/hsd_schema.f90
)

# Type modules (data structures)
set(HSD_TYPE_SOURCES
  hsd_types.f90
)

# Main facade module
set(HSD_FACADE_SOURCES
  hsd.f90
)

# Combine all sources
set(HSD_SOURCES
  ${HSD_CORE_SOURCES}
  ${HSD_TYPE_SOURCES}
  ${HSD_IO_SOURCES}
  ${HSD_API_SOURCES}
  ${HSD_FACADE_SOURCES}
)

add_library(hsd ${HSD_SOURCES})

# Module output directory
set(HSD_MODULE_DIR ${CMAKE_CURRENT_BINARY_DIR}/../modules)
set_target_properties(hsd PROPERTIES
  Fortran_MODULE_DIRECTORY ${HSD_MODULE_DIR}
)
target_include_directories(hsd PUBLIC
  $<BUILD_INTERFACE:${HSD_MODULE_DIR}>
  $<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}/hsd>
)

# Compiler flags
if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
  target_compile_options(hsd PRIVATE
    -Wall                    # Enable most warnings
    -Wextra                  # Enable extra warnings
    -pedantic                # Strict ISO Fortran compliance
    -Wunused-parameter       # Warn about unused parameters
    -Wimplicit-interface     # Warn about implicit interfaces
    -Wno-maybe-uninitialized # Suppress false positives with intent(out)
  )
  # Runtime checks and debugging support in Debug mode
  if(CMAKE_BUILD_TYPE STREQUAL "Debug")
    target_compile_options(hsd PRIVATE
      -g                     # Debug symbols
      -fcheck=all            # All runtime checks (bounds, pointer, etc.)
      -fbacktrace            # Produce backtrace on errors
      -ffpe-trap=invalid,zero,overflow  # Trap floating point exceptions
      -finit-real=snan       # Initialize reals to signaling NaN
      -finit-integer=-999999 # Initialize integers to sentinel value
      -finit-logical=true    # Initialize logicals
    )
  endif()
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
  target_compile_options(hsd PRIVATE
    -warn all                # Enable all warnings
    -warn unused             # Warn about unused variables
    -warn interfaces         # Warn about interface mismatches
    -stand f08               # Check F2008 standard compliance
  )
  # Runtime checks and debugging support in Debug mode
  if(CMAKE_BUILD_TYPE STREQUAL "Debug")
    target_compile_options(hsd PRIVATE
      -g                     # Debug symbols
      -check all             # All runtime checks
      -traceback             # Produce backtrace on errors
      -fpe0                  # Trap floating point exceptions
      -init=snan             # Initialize to signaling NaN
    )
  endif()
endif()
