program bench_lookup
  use hsd, only : hsd_table, hsd_value, hsd_node, new_table, new_value
  implicit none (type, external)

  integer, parameter :: N_TRIALS = 100000
  integer, parameter :: N_SIZES = 12
  integer :: n_children_list(N_SIZES) = [2, 4, 8, 12, 16, 20, 32, 64, 128, 256, 512, 1024]
  integer :: i, j, n
  type(hsd_table) :: root
  type(hsd_value) :: val
  class(hsd_node), pointer :: child
  character(len=32) :: name
  character(len=32) :: target_name
  integer(8) :: t1, t2, rate
  real(8) :: dt

  call system_clock(count_rate=rate)

  print '(A15, A20, A15)', "Num Children", "Time/Lookup (ns)", "Hash Active?"

  do i = 1, size(n_children_list)
    n = n_children_list(i)

    call new_table(root, "root")
    do j = 1, n
      write(name, '(A,I0)') "child", j
      call new_value(val, trim(name))
      call root%add_child(val)
      call val%destroy()
    end do

    ! Search for the middle child to avoid best/worst case bias
    write(target_name, '(A,I0)') "child", n / 2

    call system_clock(t1)
    do j = 1, N_TRIALS
      call root%get_child_by_name(trim(target_name), child)
      if (.not. associated(child)) then
        print *, "Error: child not found!"
        stop 1
      end if
    end do
    call system_clock(t2)

    dt = real(t2 - t1, 8) / real(rate, 8) / real(N_TRIALS, 8) * 1.0d9  ! ns per lookup

    print '(I15, F20.2, L15)', n, dt, root%index_active

    call root%destroy()
  end do

end program bench_lookup
