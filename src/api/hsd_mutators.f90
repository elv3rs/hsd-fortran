!> HSD data mutators (setters)
!>
!> This module provides interfaces and implementations for modifying HSD tables.
!> It supports type-safe setting of scalars and arrays, with automatic path
!> creation for nested structures.
module hsd_mutators
  use hsd_constants, only: dp, sp
  use hsd_utils, only: to_lower, string_buffer_t
  use hsd_error, only: HSD_STAT_OK, HSD_STAT_NOT_FOUND, HSD_STAT_TYPE_ERROR
  use hsd_types, only: hsd_node, hsd_table, hsd_value, new_table, new_value
  implicit none (type, external)
  private

  ! Public interface
  public :: hsd_set

  !> Generic interface for setting values by path
  interface hsd_set
    module procedure :: hsd_set_string
    module procedure :: hsd_set_integer
    module procedure :: hsd_set_real_dp
    module procedure :: hsd_set_real_sp
    module procedure :: hsd_set_logical
    module procedure :: hsd_set_complex_dp
    module procedure :: hsd_set_integer_array
    module procedure :: hsd_set_real_dp_array
    module procedure :: hsd_set_real_sp_array
    module procedure :: hsd_set_logical_array
    module procedure :: hsd_set_complex_dp_array
    module procedure :: hsd_set_string_array
    module procedure :: hsd_set_integer_matrix
    module procedure :: hsd_set_real_dp_matrix
  end interface hsd_set

contains

  !> Set string value by path
  subroutine hsd_set_string(table, path, val, stat)
    type(hsd_table), intent(inout) :: table
    character(len=*), intent(in) :: path
    character(len=*), intent(in) :: val
    integer, intent(out), optional :: stat

    class(hsd_node), pointer :: child
    integer :: local_stat

    call get_or_create_child(table, path, child, local_stat)

    if (local_stat /= 0) then
      if (present(stat)) stat = local_stat
      return
    end if

    select type (child)
    type is (hsd_value)
      call child%set_string(val)
    class default
      if (present(stat)) stat = HSD_STAT_TYPE_ERROR
      return
    end select

    if (present(stat)) stat = HSD_STAT_OK

  end subroutine hsd_set_string

  !> Set integer value by path
  subroutine hsd_set_integer(table, path, val, stat)
    type(hsd_table), intent(inout) :: table
    character(len=*), intent(in) :: path
    integer, intent(in) :: val
    integer, intent(out), optional :: stat

    class(hsd_node), pointer :: child
    integer :: local_stat

    call get_or_create_child(table, path, child, local_stat)

    if (local_stat /= 0) then
      if (present(stat)) stat = local_stat
      return
    end if

    select type (child)
    type is (hsd_value)
      call child%set_integer(val)
    class default
      if (present(stat)) stat = HSD_STAT_TYPE_ERROR
      return
    end select

    if (present(stat)) stat = HSD_STAT_OK

  end subroutine hsd_set_integer

  !> Set double precision real value by path
  subroutine hsd_set_real_dp(table, path, val, stat)
    type(hsd_table), intent(inout) :: table
    character(len=*), intent(in) :: path
    real(dp), intent(in) :: val
    integer, intent(out), optional :: stat

    class(hsd_node), pointer :: child
    integer :: local_stat

    call get_or_create_child(table, path, child, local_stat)

    if (local_stat /= 0) then
      if (present(stat)) stat = local_stat
      return
    end if

    select type (child)
    type is (hsd_value)
      call child%set_real(val)
    class default
      if (present(stat)) stat = HSD_STAT_TYPE_ERROR
      return
    end select

    if (present(stat)) stat = HSD_STAT_OK

  end subroutine hsd_set_real_dp

  !> Set single precision real value by path
  subroutine hsd_set_real_sp(table, path, val, stat)
    type(hsd_table), intent(inout) :: table
    character(len=*), intent(in) :: path
    real(sp), intent(in) :: val
    integer, intent(out), optional :: stat

    call hsd_set_real_dp(table, path, real(val, dp), stat)

  end subroutine hsd_set_real_sp

  !> Set logical value by path
  subroutine hsd_set_logical(table, path, val, stat)
    type(hsd_table), intent(inout) :: table
    character(len=*), intent(in) :: path
    logical, intent(in) :: val
    integer, intent(out), optional :: stat

    class(hsd_node), pointer :: child
    integer :: local_stat

    call get_or_create_child(table, path, child, local_stat)

    if (local_stat /= 0) then
      if (present(stat)) stat = local_stat
      return
    end if

    select type (child)
    type is (hsd_value)
      call child%set_logical(val)
    class default
      if (present(stat)) stat = HSD_STAT_TYPE_ERROR
      return
    end select

    if (present(stat)) stat = HSD_STAT_OK

  end subroutine hsd_set_logical

  !> Set complex value by path
  subroutine hsd_set_complex_dp(table, path, val, stat)
    type(hsd_table), intent(inout) :: table
    character(len=*), intent(in) :: path
    complex(dp), intent(in) :: val
    integer, intent(out), optional :: stat

    class(hsd_node), pointer :: child
    integer :: local_stat

    call get_or_create_child(table, path, child, local_stat)

    if (local_stat /= 0) then
      if (present(stat)) stat = local_stat
      return
    end if

    select type (child)
    type is (hsd_value)
      call child%set_complex(val)
    class default
      if (present(stat)) stat = HSD_STAT_TYPE_ERROR
      return
    end select

    if (present(stat)) stat = HSD_STAT_OK

  end subroutine hsd_set_complex_dp

  !> Set integer array by path
  subroutine hsd_set_integer_array(table, path, val, stat)
    type(hsd_table), intent(inout) :: table
    character(len=*), intent(in) :: path
    integer, intent(in) :: val(:)
    integer, intent(out), optional :: stat

    class(hsd_node), pointer :: child
    integer :: local_stat, i
    character(len=32) :: buffer
    type(string_buffer_t) :: buf

    call get_or_create_child(table, path, child, local_stat)

    if (local_stat /= 0) then
      if (present(stat)) stat = local_stat
      return
    end if

    select type (child)
    type is (hsd_value)
      call buf%init(size(val) * 12)
      do i = 1, size(val)
        write(buffer, '(I0)') val(i)
        if (i > 1) call buf%append_char(' ')
        call buf%append_str(trim(adjustl(buffer)))
      end do
      call child%set_raw(buf%get_string())
    class default
      if (present(stat)) stat = HSD_STAT_TYPE_ERROR
      return
    end select

    if (present(stat)) stat = HSD_STAT_OK

  end subroutine hsd_set_integer_array

  !> Set double precision real array by path
  subroutine hsd_set_real_dp_array(table, path, val, stat)
    type(hsd_table), intent(inout) :: table
    character(len=*), intent(in) :: path
    real(dp), intent(in) :: val(:)
    integer, intent(out), optional :: stat

    class(hsd_node), pointer :: child
    integer :: local_stat, i
    character(len=32) :: buffer
    type(string_buffer_t) :: buf

    call get_or_create_child(table, path, child, local_stat)

    if (local_stat /= 0) then
      if (present(stat)) stat = local_stat
      return
    end if

    select type (child)
    type is (hsd_value)
      call buf%init(size(val) * 16)
      do i = 1, size(val)
        write(buffer, '(G0)') val(i)
        if (i > 1) call buf%append_char(' ')
        call buf%append_str(trim(adjustl(buffer)))
      end do
      call child%set_raw(buf%get_string())
    class default
      if (present(stat)) stat = HSD_STAT_TYPE_ERROR
      return
    end select

    if (present(stat)) stat = HSD_STAT_OK

  end subroutine hsd_set_real_dp_array

  !> Set single precision real array by path
  subroutine hsd_set_real_sp_array(table, path, val, stat)
    type(hsd_table), intent(inout) :: table
    character(len=*), intent(in) :: path
    real(sp), intent(in) :: val(:)
    integer, intent(out), optional :: stat

    real(dp), allocatable :: val_dp(:)

    allocate(val_dp(size(val)))
    val_dp = real(val, dp)
    call hsd_set_real_dp_array(table, path, val_dp, stat)

  end subroutine hsd_set_real_sp_array

  !> Set logical array by path
  subroutine hsd_set_logical_array(table, path, val, stat)
    type(hsd_table), intent(inout) :: table
    character(len=*), intent(in) :: path
    logical, intent(in) :: val(:)
    integer, intent(out), optional :: stat

    class(hsd_node), pointer :: child
    integer :: local_stat, i
    type(string_buffer_t) :: buf

    call get_or_create_child(table, path, child, local_stat)

    if (local_stat /= 0) then
      if (present(stat)) stat = local_stat
      return
    end if

    select type (child)
    type is (hsd_value)
      call buf%init(size(val) * 4)
      do i = 1, size(val)
        if (i > 1) call buf%append_char(' ')
        if (val(i)) then
          call buf%append_str("Yes")
        else
          call buf%append_str("No")
        end if
      end do
      call child%set_raw(buf%get_string())
    class default
      if (present(stat)) stat = HSD_STAT_TYPE_ERROR
      return
    end select

    if (present(stat)) stat = HSD_STAT_OK

  end subroutine hsd_set_logical_array

  !> Set complex array by path
  subroutine hsd_set_complex_dp_array(table, path, val, stat)
    type(hsd_table), intent(inout) :: table
    character(len=*), intent(in) :: path
    complex(dp), intent(in) :: val(:)
    integer, intent(out), optional :: stat

    class(hsd_node), pointer :: child
    integer :: local_stat, i
    character(len=64) :: buffer
    type(string_buffer_t) :: buf

    call get_or_create_child(table, path, child, local_stat)

    if (local_stat /= 0) then
      if (present(stat)) stat = local_stat
      return
    end if

    select type (child)
    type is (hsd_value)
      call buf%init(size(val) * 32)
      do i = 1, size(val)
        if (i > 1) call buf%append_char(' ')
        if (aimag(val(i)) >= 0.0_dp) then
          write(buffer, '(G0,"+",G0,"i")') real(val(i)), aimag(val(i))
        else
          write(buffer, '(G0,G0,"i")') real(val(i)), aimag(val(i))
        end if
        call buf%append_str(trim(adjustl(buffer)))
      end do
      call child%set_raw(buf%get_string())
    class default
      if (present(stat)) stat = HSD_STAT_TYPE_ERROR
      return
    end select

    if (present(stat)) stat = HSD_STAT_OK

  end subroutine hsd_set_complex_dp_array

  !> Set string array by path
  subroutine hsd_set_string_array(table, path, val, stat)
    type(hsd_table), intent(inout) :: table
    character(len=*), intent(in) :: path
    character(len=*), intent(in) :: val(:)
    integer, intent(out), optional :: stat

    class(hsd_node), pointer :: child
    integer :: local_stat, i
    type(string_buffer_t) :: buf

    call get_or_create_child(table, path, child, local_stat)

    if (local_stat /= 0) then
      if (present(stat)) stat = local_stat
      return
    end if

    select type (child)
    type is (hsd_value)
      call buf%init(size(val) * 32)
      do i = 1, size(val)
        if (i > 1) call buf%append_char(' ')
        ! Quote strings containing spaces
        if (index(val(i), ' ') > 0) then
          call buf%append_char('"')
          call buf%append_str(trim(val(i)))
          call buf%append_char('"')
        else
          call buf%append_str(trim(val(i)))
        end if
      end do
      call child%set_raw(buf%get_string())
    class default
      if (present(stat)) stat = HSD_STAT_TYPE_ERROR
      return
    end select

    if (present(stat)) stat = HSD_STAT_OK

  end subroutine hsd_set_string_array

  !> Set integer matrix by path
  subroutine hsd_set_integer_matrix(table, path, val, stat)
    type(hsd_table), intent(inout) :: table
    character(len=*), intent(in) :: path
    integer, intent(in) :: val(:,:)
    integer, intent(out), optional :: stat

    class(hsd_node), pointer :: child
    integer :: local_stat, ir, ic
    character(len=32) :: buffer
    type(string_buffer_t) :: buf

    call get_or_create_child(table, path, child, local_stat)

    if (local_stat /= 0) then
      if (present(stat)) stat = local_stat
      return
    end if

    select type (child)
    type is (hsd_value)
      call buf%init(size(val) * 12)
      do ir = 1, size(val, 1)
        if (ir > 1) call buf%append_str(new_line('a'))
        do ic = 1, size(val, 2)
          write(buffer, '(I0)') val(ir, ic)
          if (ic > 1) call buf%append_char(' ')
          call buf%append_str(trim(adjustl(buffer)))
        end do
      end do
      call child%set_raw(buf%get_string())
    class default
      if (present(stat)) stat = HSD_STAT_TYPE_ERROR
      return
    end select

    if (present(stat)) stat = HSD_STAT_OK

  end subroutine hsd_set_integer_matrix

  !> Set double precision real matrix by path
  subroutine hsd_set_real_dp_matrix(table, path, val, stat)
    type(hsd_table), intent(inout) :: table
    character(len=*), intent(in) :: path
    real(dp), intent(in) :: val(:,:)
    integer, intent(out), optional :: stat

    class(hsd_node), pointer :: child
    integer :: local_stat, ir, ic
    character(len=32) :: buffer
    type(string_buffer_t) :: buf

    call get_or_create_child(table, path, child, local_stat)

    if (local_stat /= 0) then
      if (present(stat)) stat = local_stat
      return
    end if

    select type (child)
    type is (hsd_value)
      call buf%init(size(val) * 16)
      do ir = 1, size(val, 1)
        if (ir > 1) call buf%append_str(new_line('a'))
        do ic = 1, size(val, 2)
          write(buffer, '(G0)') val(ir, ic)
          if (ic > 1) call buf%append_char(' ')
          call buf%append_str(trim(adjustl(buffer)))
        end do
      end do
      call child%set_raw(buf%get_string())
    class default
      if (present(stat)) stat = HSD_STAT_TYPE_ERROR
      return
    end select

    if (present(stat)) stat = HSD_STAT_OK

  end subroutine hsd_set_real_dp_matrix

  !> Get or create a child node by path, creating intermediate tables as needed
  subroutine get_or_create_child(table, path, child, stat)
    type(hsd_table), intent(inout), target :: table
    character(len=*), intent(in) :: path
    class(hsd_node), pointer, intent(out) :: child
    integer, intent(out), optional :: stat

    character(len=:), allocatable :: remaining, segment
    class(hsd_node), pointer :: current
    type(hsd_table), pointer :: current_table
    type(hsd_table) :: new_tbl
    type(hsd_value) :: new_val
    integer :: sep_pos, i

    child => null()
    remaining = path
    current_table => table

    do while (len_trim(remaining) > 0)
      ! Get next segment
      sep_pos = index(remaining, "/")
      if (sep_pos > 0) then
        segment = remaining(1:sep_pos-1)
        remaining = remaining(sep_pos+1:)
      else
        segment = remaining
        remaining = ""
      end if

      ! Look for existing child
      call current_table%get_child_by_name(segment, current, case_insensitive=.true.)

      if (.not. associated(current)) then
        ! Need to create node
        if (len_trim(remaining) > 0) then
          ! More path segments: create table
          call new_table(new_tbl, name=segment)
          call current_table%add_child(new_tbl)
          ! Get the newly added child
          do i = current_table%num_children, 1, -1
            call current_table%get_child(i, current)
            if (associated(current)) then
              if (allocated(current%name)) then
                if (to_lower(current%name) == to_lower(segment)) exit
              end if
            end if
          end do
        else
          ! Final segment: create value node
          call new_value(new_val, name=segment)
          call current_table%add_child(new_val)
          ! Get the newly added child
          do i = current_table%num_children, 1, -1
            call current_table%get_child(i, current)
            if (associated(current)) then
              if (allocated(current%name)) then
                if (to_lower(current%name) == to_lower(segment)) exit
              end if
            end if
          end do
          child => current
          if (present(stat)) stat = HSD_STAT_OK
          return
        end if
      end if

      ! Navigate deeper if more path remains
      if (len_trim(remaining) > 0) then
        select type (current)
        type is (hsd_table)
          current_table => current
        class default
          ! Path segment is not a table, cannot navigate
          if (present(stat)) stat = HSD_STAT_NOT_FOUND
          return
        end select
      else
        child => current
        if (present(stat)) stat = HSD_STAT_OK
        return
      end if
    end do

    if (present(stat)) stat = HSD_STAT_NOT_FOUND

  end subroutine get_or_create_child

end module hsd_mutators
