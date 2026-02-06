! HSD Library - Array and Matrix Operations Example
!
! This example demonstrates how to efficiently handle arrays and matrices
! using HSD-Fortran.
!
program matrix_demo
  use hsd
  implicit none (type, external)

  type(hsd_table) :: root
  type(hsd_error_t), allocatable :: error
  
  integer, allocatable :: ints(:)
  real(dp), allocatable :: vec(:)
  real(dp), allocatable :: matrix(:,:)
  real(dp) :: direct_matrix(2,2)
  integer :: stat, nrows, ncols

  print '(A)', "--- HSD Matrix Operations Demo ---"
  print *
  
  ! Load Data
  call hsd_load("matrix_input.hsd", root, error)
  if (allocated(error)) then
     call error%print()
     stop 1
  end if

  ! 1. Allocatable Arrays
  print '(A)', "1. Reading allocatable arrays"
  
  ! Read unknown length array
  call hsd_get(root, "Integers", ints)
  if (allocated(ints)) then
     print '(A,I0,A)', "   Integers (size=", size(ints), "):"
     print *, ints
  end if
  
  call hsd_get(root, "Reals", vec)
    if (allocated(vec)) then
     print '(A,I0,A)', "   Reals (size=", size(vec), "):"
     print *, vec
  end if
  print *

  ! 2. Reading as Matrix
  print '(A)', "2. Reading matrices"
  
  ! hsd_get_matrix allows specifying shape
  ! It reads as flat array then reshapes, or validates dimensions?
  ! HSD stores data as flat arrays. hsd_get_matrix is a helper.
  
  ! Dynamic shape - usually we read as array and reshape if we know dims, 
  ! or hsd_get_matrix handles it if we provide allocatable rank-2 array?
  ! Let's check hsd_get_matrix signature/capability.
  ! The API exposes hsd_get_matrix for fixed size arrays usually or specific logic.
  
  call hsd_get_matrix(root, "TransformationMatrix", matrix, nrows, ncols, stat)
  if (stat == HSD_STAT_OK) then
     print '(A,I0,A,I0,A)', "   TransformationMatrix (", nrows, "x", ncols, "):"
     print '(2F10.2)', matrix(1,1:ncols)
     print '(2F10.2)', matrix(2,1:ncols)
  else
     print '(A)', "   Failed to read matrix"
  end if
  print *
  
  ! 3. Reading into fixed-size arrays (by copy)
  print '(A)', "3. Reading into fixed-size arrays"
  direct_matrix = 0.0_dp
  
  ! We reuse the matrix read above since hsd_get_matrix requires allocatable
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
  
  call root%destroy()

end program matrix_demo
