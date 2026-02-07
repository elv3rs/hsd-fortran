!> Test environment configuration module (build_env compatibility layer)
!>
!> This module provides paths for test file I/O that work across different
!> build systems (CMake and fpm). It is a library-level module that can be
!> used by tests in any subdirectory.
!>
!> For fpm builds, absolute paths are determined at runtime via getcwd.
!> For CMake builds, the paths are overridden by the generated test/build_env.f90.in.
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

  public :: source_dir, build_dir, build_env_init

  !> Path to the project source directory (set by build_env_init)
  character(len=:), allocatable :: source_dir

  !> Path to the build directory for temporary test outputs
  character(len=:), allocatable :: build_dir

contains

  !> Initialize paths using getcwd (call once from test main program)
  subroutine build_env_init()
    character(len=4096) :: cwd
    integer :: ierr

    if (allocated(source_dir)) return

    call getcwd(cwd, ierr)
    if (ierr == 0) then
      source_dir = trim(cwd)
      build_dir = trim(cwd) // "/build"
    else
      source_dir = "."
      build_dir = "build"
    end if
  end subroutine build_env_init

end module build_env
