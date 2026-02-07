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
  use, intrinsic :: iso_c_binding, only: c_char, c_ptr, c_null_char, c_size_t, c_associated, &
      & c_null_ptr
  implicit none (type, external)
  private

  public :: source_dir, build_dir, build_env_init

  !> Path to the project source directory (set by build_env_init)
  character(len=:), allocatable :: source_dir

  !> Path to the build directory for temporary test outputs
  character(len=:), allocatable :: build_dir

  !> Buffer size for getcwd
  integer, parameter :: PATH_BUF_LEN = 4096

  interface
    !> C standard library getcwd (portable, standard Fortran C binding)
    type(c_ptr) function c_getcwd(buf, size) bind(c, name="getcwd")
      import :: c_ptr, c_char, c_size_t
      implicit none (type, external)
      character(kind=c_char), intent(inout) :: buf(*)
      integer(c_size_t), value :: size
    end function c_getcwd
  end interface

contains

  !> Initialize paths using getcwd via C binding (call once from test main program)
  subroutine build_env_init()
    character(kind=c_char) :: c_buf(PATH_BUF_LEN)
    type(c_ptr) :: ret
    character(len=PATH_BUF_LEN) :: cwd
    integer :: ii

    if (allocated(source_dir)) return

    ret = c_getcwd(c_buf, int(PATH_BUF_LEN, c_size_t))
    if (c_associated(ret)) then
      cwd = " "
      do ii = 1, PATH_BUF_LEN
        if (c_buf(ii) == c_null_char) exit
        cwd(ii:ii) = c_buf(ii)
      end do
      source_dir = trim(cwd)
      build_dir = trim(cwd) // "/build"
    else
      source_dir = "."
      build_dir = "build"
    end if
  end subroutine build_env_init

end module build_env
