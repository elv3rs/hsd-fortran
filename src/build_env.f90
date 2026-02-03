!> Test environment configuration module (build_env compatibility layer)
!>
!> This module provides paths for test file I/O that work across different
!> build systems (CMake and fpm). It is a library-level module that can be
!> used by tests in any subdirectory.
!>
!> For fpm builds, relative paths are used (fpm runs from project root).
!> For CMake builds, the paths are overridden by the generated test/build_env.f90.
!>
!> Usage in tests:
!> ```fortran
!> use build_env, only: source_dir, build_dir
!> character(len=512) :: input_file
!> input_file = source_dir // "/test/inputs/sample.hsd"
!> ```
module build_env
  implicit none (type, external)
  private

  public :: source_dir, build_dir

  !> Path to the project source directory
  !> For fpm: "." (current working directory is project root)
  character(len=*), parameter :: source_dir = "."

  !> Path to the build directory for temporary test outputs
  !> For fpm: "build" (fpm's default build directory)
  character(len=*), parameter :: build_dir = "build"

end module build_env
