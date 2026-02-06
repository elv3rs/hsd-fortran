program bench_array
  use hsd, only : hsd_table, hsd_error_t, hsd_load_string, hsd_get, dp
  implicit none (type, external)

  integer, parameter :: N_TRIALS = 10000
  type(hsd_table) :: root
  type(hsd_error_t), allocatable :: error
  integer(8) :: t1, t2, rate
  real(8) :: dt
  integer :: i
  real(8), allocatable :: arr(:)
  character(len=:), allocatable :: hsd_text

  ! Large array (1000 elements)
  hsd_text = "data = {"
  do i = 1, 1000
    hsd_text = hsd_text // " 1.234"
  end do
  hsd_text = hsd_text // " }"

  call system_clock(count_rate=rate)

  print '(A25, A20)', "Operation (1000 real)", "Time (us)"

  ! Benchmark first access (parsing + caching)
  call system_clock(t1)
  do i = 1, N_TRIALS / 100  ! Fewer trials because we have to re-parse
    call hsd_load_string(hsd_text, root, error)
    call hsd_get(root, "data", arr)
    call root%destroy()
  end do
  call system_clock(t2)
  dt = real(t2 - t1, 8) / real(rate, 8) / (real(N_TRIALS, 8) / 100.0_dp) * 1.0e6_dp
  print '(A25, F20.2)', "Parse + Get Array", dt

  ! Benchmark subsequent access (cached)
  call hsd_load_string(hsd_text, root, error)
  call system_clock(t1)
  do i = 1, N_TRIALS
    call hsd_get(root, "data", arr)
  end do
  call system_clock(t2)
  dt = real(t2 - t1, 8) / real(rate, 8) / real(N_TRIALS, 8) * 1.0d6
  print '(A25, F20.2)', "Cached Get Array", dt

  call root%destroy()

end program bench_array
