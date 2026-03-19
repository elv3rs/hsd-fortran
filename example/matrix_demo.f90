! HSD Library - Array and Matrix Operations Example
!
! This example demonstrates how to efficiently handle arrays and matrices
! using the hsd_access_t interface.
!
program matrix_demo
  use hsd, only: hsd_node_t, hsd_error_t, hsd_access_t, dp, &
      & hsd_load_file
  implicit none (type, external)

  type(hsd_node_t), target :: root
  type(hsd_access_t) :: access
  type(hsd_error_t), allocatable :: error

  integer, allocatable :: ints(:)
  real(dp), allocatable :: vec(:)
  real(dp), allocatable :: matrix(:,:)
  real(dp) :: direct_matrix(2,2)
  integer :: nrows, ncols

  print '(A)', "--- HSD Matrix Operations Demo ---"
  print *

  ! Load Data
  call hsd_load_file("matrix_input.hsd", root, error)
  if (allocated(error)) then
    call error%print()
    stop 1
  end if
  call access%init(root)

  ! 1. Allocatable Arrays
  print '(A)', "1. Reading allocatable arrays"

  call access%get("Integers", ints)
  if (allocated(ints)) then
    print '(A,I0,A)', "   Integers (size=", size(ints), "):"
    print *, ints
  end if

  call access%get("Reals", vec)
  if (allocated(vec)) then
    print '(A,I0,A)', "   Reals (size=", size(vec), "):"
    print *, vec
  end if
  print *

  ! 2. Reading as Matrix
  print '(A)', "2. Reading matrices"

  call access%get_matrix("TransformationMatrix", matrix, nrows, ncols)
  if (.not. access%has_errors()) then
    print '(A,I0,A,I0,A)', &
        & "   TransformationMatrix (", nrows, "x", ncols, "):"
    print '(2F10.2)', matrix(1,1:ncols)
    print '(2F10.2)', matrix(2,1:ncols)
  else
    print '(A)', "   Failed to read matrix"
    call access%clear_errors()
  end if
  print *

  ! 3. Reading into fixed-size arrays (by copy)
  print '(A)', "3. Reading into fixed-size arrays"
  direct_matrix = 0.0_dp

  if (allocated(matrix)) then
    if (size(matrix, 1) == 2 .and. size(matrix, 2) == 2) then
      direct_matrix = matrix
      print '(A)', "   Direct Matrix Read (Should match above):"
      print '(2F10.2)', direct_matrix(1,:)
      print '(2F10.2)', direct_matrix(2,:)
    else
      print '(A)', "   Matrix dimensions mismatch expected 2x2"
    end if
  end if

  ! Check for any accumulated errors
  if (access%has_errors()) then
    print '(A)', "Errors encountered:"
    call access%print_errors()
  end if

  call root%destroy()

end program matrix_demo
