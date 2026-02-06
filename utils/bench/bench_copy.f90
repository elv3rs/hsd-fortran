program bench_copy
  use hsd, only : hsd_table, hsd_value, hsd_node, new_table, new_value, hsd_clone
  implicit none (type, external)

  integer, parameter :: N_TRIALS = 100
  integer :: depths(4) = [1, 2, 3, 4]
  integer :: branching = 8
  integer :: i, j, d
  type(hsd_table) :: root, cloned
  integer(8) :: t1, t2, rate
  real(8) :: dt
  integer :: total_nodes

  call system_clock(count_rate=rate)

  print '(A10, A15, A20)', "Depth", "Nodes", "Clone+Destroy (us)"

  do i = 1, size(depths)
    d = depths(i)

    call new_table(root, "root")
    call build_tree(root, d, branching)

    total_nodes = (branching**(d+1) - 1) / (branching - 1)

    call system_clock(t1)
    do j = 1, N_TRIALS
      call hsd_clone(root, cloned)
      call cloned%destroy()
    end do
    call system_clock(t2)

    dt = real(t2 - t1, 8) / real(rate, 8) / real(N_TRIALS, 8) * 1.0d6  ! us per clone+destroy

    print '(I10, I15, F20.2)', d, total_nodes, dt

    call root%destroy()
  end do

contains

  recursive subroutine build_tree(tbl, depth, b)
    type(hsd_table), intent(inout) :: tbl
    integer, intent(in) :: depth, b
    integer :: k
    type(hsd_table) :: tmp_table
    type(hsd_value) :: val
    class(hsd_node), pointer :: child_ptr
    character(len=32) :: name

    if (depth == 1) then
      do k = 1, b
        write(name, '(A,I0)') "leaf", k
        call new_value(val, trim(name))
        call val%set_integer(k)
        call tbl%add_child(val)
        call val%destroy()
      end do
      return
    end if

    do k = 1, b
      write(name, '(A,I0)') "node", k
      call new_table(tmp_table, trim(name))
      call tbl%add_child(tmp_table)

      call tbl%get_child(tbl%num_children, child_ptr)
      select type (child_ptr)
      type is (hsd_table)
        call build_tree(child_ptr, depth - 1, b)
      end select
      ! DO NOT call tmp_table%destroy() because add_child already handled it?
      ! Actually new_table allocated children for tmp_table.
      ! tmp_table%children is allocatable. It will be freed on return.
      ! tmp_table%name_index will also be freed.
      ! This is safe.
    end do
  end subroutine build_tree

end program bench_copy
