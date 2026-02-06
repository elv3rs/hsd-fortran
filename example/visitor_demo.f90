! HSD Library - Visitor Pattern Demo
!
! This example demonstrates the visitor pattern for tree traversal:
! - Defining a custom visitor type (see visitor_types.f90)
! - Implementing visit_table and visit_value callbacks
! - Traversing the entire tree with hsd_accept
! - Collecting statistics about the tree structure
program visitor_demo
  use hsd, only: hsd_table, hsd_error_t, hsd_load_string, hsd_accept
  use visitor_types, only: tree_printer, tree_counter
  implicit none (type, external)

  type(hsd_table) :: root
  type(hsd_error_t), allocatable :: error
  type(tree_printer) :: printer
  type(tree_counter) :: counter
  integer :: stat

  character(len=*), parameter :: sample = &
      & "Geometry {" // char(10) // &
      & "  TypeNames = C H O" // char(10) // &
      & "  Periodic = No" // char(10) // &
      & "  Lattice {" // char(10) // &
      & "    10.0 0.0 0.0" // char(10) // &
      & "    0.0 10.0 0.0" // char(10) // &
      & "    0.0 0.0 10.0" // char(10) // &
      & "  }" // char(10) // &
      & "}" // char(10) // &
      & "Driver {" // char(10) // &
      & "  MaxSteps = 100" // char(10) // &
      & "  Method = ConjugateGradient" // char(10) // &
      & "}"

  call hsd_load_string(sample, root, error)
  if (allocated(error)) then
    call error%print()
    stop 1
  end if

  ! --- 1. Print the full tree structure ---
  print '(A)', "Tree structure:"
  print '(A)', repeat("-", 40)
  call hsd_accept(root, printer, stat)
  print *

  ! --- 2. Count tables and values ---
  counter%num_tables = 0
  counter%num_values = 0
  call hsd_accept(root, counter, stat)
  print '(A,I0)', "Tables: ", counter%num_tables
  print '(A,I0)', "Values: ", counter%num_values

  call root%destroy()

end program visitor_demo