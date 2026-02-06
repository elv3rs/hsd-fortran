program bench_access
  use hsd, only : hsd_table, hsd_node, hsd_error_t, hsd_load_string, hsd_get, hsd_get_child
  implicit none (type, external)

  integer, parameter :: N_TRIALS = 100000
  type(hsd_table) :: root
  type(hsd_error_t), allocatable :: error
  integer(8) :: t1, t2, rate
  real(8) :: dt
  integer :: i, val
  class(hsd_node), pointer :: node
  character(len=:), allocatable :: hsd_text

  hsd_text = &
    'a { b { c { d { e = 42 } } } }'

  call hsd_load_string(hsd_text, root, error)
  if (allocated(error)) stop 1

  call system_clock(count_rate=rate)

  print '(A25, A20)', "Access Method", "Time/Access (ns)"

  ! Benchmark nested path access
  call system_clock(t1)
  do i = 1, N_TRIALS
    call hsd_get(root, "a/b/c/d/e", val)
    if (val /= 42) stop 2
  end do
  call system_clock(t2)
  dt = real(t2 - t1, 8) / real(rate, 8) / real(N_TRIALS, 8) * 1.0d9
  print '(A25, F20.2)', "Path (a/b/c/d/e)", dt

  ! Benchmark shallow path access
  call system_clock(t1)
  do i = 1, N_TRIALS
    call hsd_get_child(root, "a", node)
  end do
  call system_clock(t2)
  dt = real(t2 - t1, 8) / real(rate, 8) / real(N_TRIALS, 8) * 1.0d9
  print '(A25, F20.2)', "Path shallow (a)", dt

  call root%destroy()

end program bench_access
