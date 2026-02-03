!> HSD data accessors (getters)
!>
!> This module provides interfaces and implementations for retrieving data
!> from HSD tables. It supports type-safe access to scalars, arrays, and
!> matrices with optional default values.
module hsd_accessors
  use hsd_constants, only: dp, sp
  use hsd_error, only: HSD_STAT_OK, HSD_STAT_NOT_FOUND, HSD_STAT_TYPE_ERROR
  use hsd_types, only: hsd_node, hsd_table, hsd_value, new_value, VALUE_TYPE_ARRAY
  implicit none (type, external)
  private

  ! Public interfaces
  public :: hsd_get, hsd_get_or, hsd_get_matrix

  !> Generic interface for getting values
  !>
  !> All procedures accept an optional `stat` parameter for error status.
  !> Use `hsd_get_or` for fallback default values when key is not found.
  interface hsd_get
    module procedure :: hsd_get_string
    module procedure :: hsd_get_integer
    module procedure :: hsd_get_real_dp
    module procedure :: hsd_get_real_sp
    module procedure :: hsd_get_logical
    module procedure :: hsd_get_complex_dp
    module procedure :: hsd_get_integer_array
    module procedure :: hsd_get_real_dp_array
    module procedure :: hsd_get_real_sp_array
    module procedure :: hsd_get_logical_array
    module procedure :: hsd_get_string_array
    module procedure :: hsd_get_complex_dp_array
  end interface hsd_get

  !> Generic interface for getting values with default fallback
  !>
  !> Returns the default value if the key is not found.
  !> stat will be HSD_STAT_NOT_FOUND when default is used, HSD_STAT_OK otherwise.
  interface hsd_get_or
    module procedure :: hsd_get_string_default
    module procedure :: hsd_get_integer_default
    module procedure :: hsd_get_real_dp_default
    module procedure :: hsd_get_real_sp_default
    module procedure :: hsd_get_logical_default
    module procedure :: hsd_get_complex_dp_default
  end interface hsd_get_or

  !> Generic interface for getting 2D matrices
  interface hsd_get_matrix
    module procedure :: hsd_get_integer_matrix
    module procedure :: hsd_get_real_dp_matrix
  end interface hsd_get_matrix

contains

  !> Get string value by path
  subroutine hsd_get_string(table, path, val, stat)
    type(hsd_table), intent(in), target :: table
    character(len=*), intent(in) :: path
    character(len=:), allocatable, intent(out) :: val
    integer, intent(out), optional :: stat

    class(hsd_node), pointer :: child
    integer :: local_stat

    call get_child_by_path(table, path, child, local_stat)

    if (local_stat /= 0 .or. .not. associated(child)) then
      if (present(stat)) stat = HSD_STAT_NOT_FOUND
      val = ""
      return
    end if

    select type (child)
    type is (hsd_value)
      call child%get_string(val, local_stat)
      if (present(stat)) stat = local_stat
    class default
      if (present(stat)) stat = HSD_STAT_TYPE_ERROR
      val = ""
    end select

  end subroutine hsd_get_string

  !> Get string value by path with default fallback
  subroutine hsd_get_string_default(table, path, val, default, stat)
    type(hsd_table), intent(in), target :: table
    character(len=*), intent(in) :: path
    character(len=:), allocatable, intent(out) :: val
    character(len=*), intent(in) :: default
    integer, intent(out), optional :: stat

    integer :: local_stat

    call hsd_get_string(table, path, val, local_stat)

    if (local_stat /= 0) then
      val = default
      if (present(stat)) stat = HSD_STAT_NOT_FOUND
    else
      if (present(stat)) stat = HSD_STAT_OK
    end if

  end subroutine hsd_get_string_default

  !> Get integer value by path
  subroutine hsd_get_integer(table, path, val, stat)
    type(hsd_table), intent(in), target :: table
    character(len=*), intent(in) :: path
    integer, intent(out) :: val
    integer, intent(out), optional :: stat

    class(hsd_node), pointer :: child
    integer :: local_stat

    call get_child_by_path(table, path, child, local_stat)

    if (local_stat /= 0 .or. .not. associated(child)) then
      if (present(stat)) stat = HSD_STAT_NOT_FOUND
      val = 0
      return
    end if

    select type (child)
    type is (hsd_value)
      call child%get_integer(val, local_stat)
      if (present(stat)) stat = local_stat
    class default
      if (present(stat)) stat = HSD_STAT_TYPE_ERROR
      val = 0
    end select

  end subroutine hsd_get_integer

  !> Get integer value by path with default fallback
  subroutine hsd_get_integer_default(table, path, val, default, stat)
    type(hsd_table), intent(in), target :: table
    character(len=*), intent(in) :: path
    integer, intent(out) :: val
    integer, intent(in) :: default
    integer, intent(out), optional :: stat

    integer :: local_stat

    call hsd_get_integer(table, path, val, local_stat)

    if (local_stat /= 0) then
      val = default
      if (present(stat)) stat = HSD_STAT_NOT_FOUND
    else
      if (present(stat)) stat = HSD_STAT_OK
    end if

  end subroutine hsd_get_integer_default

  !> Get double precision real value by path
  subroutine hsd_get_real_dp(table, path, val, stat)
    type(hsd_table), intent(in), target :: table
    character(len=*), intent(in) :: path
    real(dp), intent(out) :: val
    integer, intent(out), optional :: stat

    class(hsd_node), pointer :: child
    integer :: local_stat

    call get_child_by_path(table, path, child, local_stat)

    if (local_stat /= 0 .or. .not. associated(child)) then
      if (present(stat)) stat = HSD_STAT_NOT_FOUND
      val = 0.0_dp
      return
    end if

    select type (child)
    type is (hsd_value)
      call child%get_real(val, local_stat)
      if (present(stat)) stat = local_stat
    class default
      if (present(stat)) stat = HSD_STAT_TYPE_ERROR
      val = 0.0_dp
    end select

  end subroutine hsd_get_real_dp

  !> Get double precision real value by path with default fallback
  subroutine hsd_get_real_dp_default(table, path, val, default, stat)
    type(hsd_table), intent(in), target :: table
    character(len=*), intent(in) :: path
    real(dp), intent(out) :: val
    real(dp), intent(in) :: default
    integer, intent(out), optional :: stat

    integer :: local_stat

    call hsd_get_real_dp(table, path, val, local_stat)

    if (local_stat /= 0) then
      val = default
      if (present(stat)) stat = HSD_STAT_NOT_FOUND
    else
      if (present(stat)) stat = HSD_STAT_OK
    end if

  end subroutine hsd_get_real_dp_default

  !> Get single precision real value by path
  subroutine hsd_get_real_sp(table, path, val, stat)
    type(hsd_table), intent(in), target :: table
    character(len=*), intent(in) :: path
    real(sp), intent(out) :: val
    integer, intent(out), optional :: stat

    real(dp) :: val_dp
    integer :: local_stat

    call hsd_get_real_dp(table, path, val_dp, local_stat)
    val = real(val_dp, sp)
    if (present(stat)) stat = local_stat

  end subroutine hsd_get_real_sp

  !> Get single precision real value by path with default fallback
  subroutine hsd_get_real_sp_default(table, path, val, default, stat)
    type(hsd_table), intent(in), target :: table
    character(len=*), intent(in) :: path
    real(sp), intent(out) :: val
    real(sp), intent(in) :: default
    integer, intent(out), optional :: stat

    integer :: local_stat

    call hsd_get_real_sp(table, path, val, local_stat)

    if (local_stat /= 0) then
      val = default
      if (present(stat)) stat = HSD_STAT_NOT_FOUND
    else
      if (present(stat)) stat = HSD_STAT_OK
    end if

  end subroutine hsd_get_real_sp_default

  !> Get logical value by path
  subroutine hsd_get_logical(table, path, val, stat)
    type(hsd_table), intent(in), target :: table
    character(len=*), intent(in) :: path
    logical, intent(out) :: val
    integer, intent(out), optional :: stat

    class(hsd_node), pointer :: child
    integer :: local_stat

    call get_child_by_path(table, path, child, local_stat)

    if (local_stat /= 0 .or. .not. associated(child)) then
      if (present(stat)) stat = HSD_STAT_NOT_FOUND
      val = .false.
      return
    end if

    select type (child)
    type is (hsd_value)
      call child%get_logical(val, local_stat)
      if (present(stat)) stat = local_stat
    class default
      if (present(stat)) stat = HSD_STAT_TYPE_ERROR
      val = .false.
    end select

  end subroutine hsd_get_logical

  !> Get logical value by path with default fallback
  subroutine hsd_get_logical_default(table, path, val, default, stat)
    type(hsd_table), intent(in), target :: table
    character(len=*), intent(in) :: path
    logical, intent(out) :: val
    logical, intent(in) :: default
    integer, intent(out), optional :: stat

    integer :: local_stat

    call hsd_get_logical(table, path, val, local_stat)

    if (local_stat /= 0) then
      val = default
      if (present(stat)) stat = HSD_STAT_NOT_FOUND
    else
      if (present(stat)) stat = HSD_STAT_OK
    end if

  end subroutine hsd_get_logical_default

  !> Get complex value by path
  subroutine hsd_get_complex_dp(table, path, val, stat)
    type(hsd_table), intent(in), target :: table
    character(len=*), intent(in) :: path
    complex(dp), intent(out) :: val
    integer, intent(out), optional :: stat

    class(hsd_node), pointer :: child
    integer :: local_stat

    call get_child_by_path(table, path, child, local_stat)

    if (local_stat /= 0 .or. .not. associated(child)) then
      if (present(stat)) stat = HSD_STAT_NOT_FOUND
      val = (0.0_dp, 0.0_dp)
      return
    end if

    select type (child)
    type is (hsd_value)
      call child%get_complex(val, local_stat)
      if (present(stat)) stat = local_stat
    class default
      if (present(stat)) stat = HSD_STAT_TYPE_ERROR
      val = (0.0_dp, 0.0_dp)
    end select

  end subroutine hsd_get_complex_dp

  !> Get complex value by path with default fallback
  subroutine hsd_get_complex_dp_default(table, path, val, default, stat)
    type(hsd_table), intent(in), target :: table
    character(len=*), intent(in) :: path
    complex(dp), intent(out) :: val
    complex(dp), intent(in) :: default
    integer, intent(out), optional :: stat

    integer :: local_stat

    call hsd_get_complex_dp(table, path, val, local_stat)

    if (local_stat /= 0) then
      val = default
      if (present(stat)) stat = HSD_STAT_NOT_FOUND
    else
      if (present(stat)) stat = HSD_STAT_OK
    end if

  end subroutine hsd_get_complex_dp_default

  !> Get integer array by path (supports space/comma/newline separated values)
  subroutine hsd_get_integer_array(table, path, val, stat)
    type(hsd_table), intent(in), target :: table
    character(len=*), intent(in) :: path
    integer, allocatable, intent(out) :: val(:)
    integer, intent(out), optional :: stat

    class(hsd_node), pointer :: child
    integer :: local_stat

    call get_child_by_path(table, path, child, local_stat)

    if (local_stat /= 0 .or. .not. associated(child)) then
      if (present(stat)) stat = HSD_STAT_NOT_FOUND
      allocate(val(0))
      return
    end if

    select type (child)
    type is (hsd_value)
      call child%get_int_array(val, local_stat)
      if (present(stat)) stat = local_stat
    class default
      if (present(stat)) stat = HSD_STAT_TYPE_ERROR
      allocate(val(0))
    end select

  end subroutine hsd_get_integer_array

  !> Get double precision real array by path
  subroutine hsd_get_real_dp_array(table, path, val, stat)
    type(hsd_table), intent(in), target :: table
    character(len=*), intent(in) :: path
    real(dp), allocatable, intent(out) :: val(:)
    integer, intent(out), optional :: stat

    class(hsd_node), pointer :: child
    integer :: local_stat

    call get_child_by_path(table, path, child, local_stat)

    if (local_stat /= 0 .or. .not. associated(child)) then
      if (present(stat)) stat = HSD_STAT_NOT_FOUND
      allocate(val(0))
      return
    end if

    select type (child)
    type is (hsd_value)
      call child%get_real_array(val, local_stat)
      if (present(stat)) stat = local_stat
    class default
      if (present(stat)) stat = HSD_STAT_TYPE_ERROR
      allocate(val(0))
    end select

  end subroutine hsd_get_real_dp_array

  !> Get single precision real array by path
  subroutine hsd_get_real_sp_array(table, path, val, stat)
    type(hsd_table), intent(in), target :: table
    character(len=*), intent(in) :: path
    real(sp), allocatable, intent(out) :: val(:)
    integer, intent(out), optional :: stat

    real(dp), allocatable :: val_dp(:)
    integer :: local_stat

    call hsd_get_real_dp_array(table, path, val_dp, local_stat)

    if (local_stat /= 0) then
      if (present(stat)) stat = local_stat
      allocate(val(0))
      return
    end if

    allocate(val(size(val_dp)))
    val = real(val_dp, sp)
    if (present(stat)) stat = HSD_STAT_OK

  end subroutine hsd_get_real_sp_array

  !> Get logical array by path
  subroutine hsd_get_logical_array(table, path, val, stat)
    type(hsd_table), intent(in), target :: table
    character(len=*), intent(in) :: path
    logical, allocatable, intent(out) :: val(:)
    integer, intent(out), optional :: stat

    class(hsd_node), pointer :: child
    integer :: local_stat

    call get_child_by_path(table, path, child, local_stat)

    if (local_stat /= 0 .or. .not. associated(child)) then
      if (present(stat)) stat = HSD_STAT_NOT_FOUND
      allocate(val(0))
      return
    end if

    select type (child)
    type is (hsd_value)
      call child%get_logical_array(val, local_stat)
      if (present(stat)) stat = local_stat
    class default
      if (present(stat)) stat = HSD_STAT_TYPE_ERROR
      allocate(val(0))
    end select

  end subroutine hsd_get_logical_array

  !> Get string array by path (preserves quoted strings)
  subroutine hsd_get_string_array(table, path, val, stat)
    type(hsd_table), intent(in), target :: table
    character(len=*), intent(in) :: path
    character(len=:), allocatable, intent(out) :: val(:)
    integer, intent(out), optional :: stat

    class(hsd_node), pointer :: child
    integer :: local_stat

    call get_child_by_path(table, path, child, local_stat)

    if (local_stat /= 0 .or. .not. associated(child)) then
      if (present(stat)) stat = HSD_STAT_NOT_FOUND
      allocate(character(len=1) :: val(0))
      return
    end if

    select type (child)
    type is (hsd_value)
      call child%get_string_array(val, local_stat)
      if (present(stat)) stat = local_stat
    class default
      if (present(stat)) stat = HSD_STAT_TYPE_ERROR
      allocate(character(len=1) :: val(0))
    end select

  end subroutine hsd_get_string_array

  !> Get complex array by path
  subroutine hsd_get_complex_dp_array(table, path, val, stat)
    type(hsd_table), intent(in), target :: table
    character(len=*), intent(in) :: path
    complex(dp), allocatable, intent(out) :: val(:)
    integer, intent(out), optional :: stat

    class(hsd_node), pointer :: child
    integer :: local_stat

    call get_child_by_path(table, path, child, local_stat)

    if (local_stat /= 0 .or. .not. associated(child)) then
      if (present(stat)) stat = HSD_STAT_NOT_FOUND
      allocate(val(0))
      return
    end if

    select type (child)
    type is (hsd_value)
      call child%get_complex_array(val, local_stat)
      if (present(stat)) stat = local_stat
    class default
      if (present(stat)) stat = HSD_STAT_TYPE_ERROR
      allocate(val(0))
    end select

  end subroutine hsd_get_complex_dp_array

  !> Get 2D integer matrix by path (rows separated by newlines or semicolons)
  !> Handles both value nodes and table nodes (where content is in unnamed children)
  subroutine hsd_get_integer_matrix(table, path, val, nrows, ncols, stat)
    type(hsd_table), intent(in), target :: table
    character(len=*), intent(in) :: path
    integer, allocatable, intent(out) :: val(:,:)
    integer, intent(out) :: nrows, ncols
    integer, intent(out), optional :: stat

    class(hsd_node), pointer :: child
    integer :: local_stat

    call get_child_by_path(table, path, child, local_stat)

    if (local_stat /= 0 .or. .not. associated(child)) then
      if (present(stat)) stat = HSD_STAT_NOT_FOUND
      allocate(val(0,0))
      nrows = 0
      ncols = 0
      return
    end if

    select type (child)
    type is (hsd_value)
      call child%get_int_matrix(val, nrows, ncols, local_stat)
      if (present(stat)) stat = local_stat
    type is (hsd_table)
      ! Table nodes store matrix data as unnamed child values
      call get_int_matrix_from_table(child, val, nrows, ncols, local_stat)
      if (present(stat)) stat = local_stat
    class default
      if (present(stat)) stat = HSD_STAT_TYPE_ERROR
      allocate(val(0,0))
      nrows = 0
      ncols = 0
    end select

  end subroutine hsd_get_integer_matrix

  !> Get 2D real matrix by path
  !> Handles both value nodes and table nodes (where content is in unnamed children)
  subroutine hsd_get_real_dp_matrix(table, path, val, nrows, ncols, stat)
    type(hsd_table), intent(in), target :: table
    character(len=*), intent(in) :: path
    real(dp), allocatable, intent(out) :: val(:,:)
    integer, intent(out) :: nrows, ncols
    integer, intent(out), optional :: stat

    class(hsd_node), pointer :: child
    integer :: local_stat

    call get_child_by_path(table, path, child, local_stat)

    if (local_stat /= 0 .or. .not. associated(child)) then
      if (present(stat)) stat = HSD_STAT_NOT_FOUND
      allocate(val(0,0))
      nrows = 0
      ncols = 0
      return
    end if

    select type (child)
    type is (hsd_value)
      call child%get_real_matrix(val, nrows, ncols, local_stat)
      if (present(stat)) stat = local_stat
    type is (hsd_table)
      ! Table nodes store matrix data as unnamed child values
      call get_real_matrix_from_table(child, val, nrows, ncols, local_stat)
      if (present(stat)) stat = local_stat
    class default
      if (present(stat)) stat = HSD_STAT_TYPE_ERROR
      allocate(val(0,0))
      nrows = 0
      ncols = 0
    end select

  end subroutine hsd_get_real_dp_matrix

  !> Extract integer matrix from table with unnamed value children
  subroutine get_int_matrix_from_table(tbl, mat, nrows, ncols, stat)
    type(hsd_table), intent(in) :: tbl
    integer, allocatable, intent(out) :: mat(:,:)
    integer, intent(out) :: nrows, ncols, stat

    class(hsd_node), pointer :: child
    character(len=:), allocatable :: combined_text, str_val
    integer :: i, local_stat

    ! Combine all unnamed value children into single text
    combined_text = ""
    do i = 1, tbl%num_children
      call tbl%get_child(i, child)
      if (associated(child)) then
        select type (child)
        type is (hsd_value)
          ! Only include unnamed value nodes (raw text content)
          if (.not. allocated(child%name) .or. len_trim(child%name) == 0) then
            call child%get_string(str_val, local_stat)
            if (local_stat == 0 .and. len_trim(str_val) > 0) then
              if (len(combined_text) > 0) then
                combined_text = combined_text // char(10) // str_val
              else
                combined_text = str_val
              end if
            end if
          end if
        end select
      end if
    end do

    if (len_trim(combined_text) == 0) then
      allocate(mat(0,0))
      nrows = 0
      ncols = 0
      stat = HSD_STAT_OK
      return
    end if

    ! Parse the combined text as a matrix
    block
      type(hsd_value) :: temp_val
      call new_value(temp_val)
      call temp_val%set_raw(combined_text)
      call temp_val%get_int_matrix(mat, nrows, ncols, stat)
      call temp_val%destroy()
    end block

  end subroutine get_int_matrix_from_table

  !> Extract real matrix from table with unnamed value children
  subroutine get_real_matrix_from_table(tbl, mat, nrows, ncols, stat)
    type(hsd_table), intent(in) :: tbl
    real(dp), allocatable, intent(out) :: mat(:,:)
    integer, intent(out) :: nrows, ncols, stat

    class(hsd_node), pointer :: child
    character(len=:), allocatable :: combined_text, str_val
    integer :: i, local_stat

    ! Combine all unnamed value children into single text
    combined_text = ""
    do i = 1, tbl%num_children
      call tbl%get_child(i, child)
      if (associated(child)) then
        select type (child)
        type is (hsd_value)
          ! Only include unnamed value nodes (raw text content)
          if (.not. allocated(child%name) .or. len_trim(child%name) == 0) then
            call child%get_string(str_val, local_stat)
            if (local_stat == 0 .and. len_trim(str_val) > 0) then
              if (len(combined_text) > 0) then
                combined_text = combined_text // char(10) // str_val
              else
                combined_text = str_val
              end if
            end if
          end if
        end select
      end if
    end do

    if (len_trim(combined_text) == 0) then
      allocate(mat(0,0))
      nrows = 0
      ncols = 0
      stat = HSD_STAT_OK
      return
    end if

    ! Parse the combined text as a matrix
    block
      type(hsd_value) :: temp_val
      call new_value(temp_val)
      call temp_val%set_raw(combined_text)
      call temp_val%get_real_matrix(mat, nrows, ncols, stat)
      call temp_val%destroy()
    end block

  end subroutine get_real_matrix_from_table

  !> Helper to navigate path and get child (imported from hsd_query)
  !> This is a forward reference - actual implementation in hsd_query
  recursive subroutine get_child_by_path(table, path, child, stat)
    type(hsd_table), intent(in), target :: table
    character(len=*), intent(in) :: path
    class(hsd_node), pointer, intent(out) :: child
    integer, intent(out), optional :: stat

    character(len=:), allocatable :: remaining, segment
    class(hsd_node), pointer :: current
    integer :: sep_pos

    child => null()
    remaining = path

    ! Get first segment
    sep_pos = index(remaining, "/")
    if (sep_pos > 0) then
      segment = remaining(1:sep_pos-1)
      remaining = remaining(sep_pos+1:)
    else
      segment = remaining
      remaining = ""
    end if

    ! Find child with this name
    call table%get_child_by_name(segment, current, case_insensitive=.true.)

    if (.not. associated(current)) then
      if (present(stat)) stat = HSD_STAT_NOT_FOUND
      return
    end if

    ! If no more path, return this node
    if (len_trim(remaining) == 0) then
      child => current
      if (present(stat)) stat = HSD_STAT_OK
      return
    end if

    ! Otherwise, recurse into child table
    select type (current)
    type is (hsd_table)
      call get_child_by_path(current, remaining, child, stat)
    class default
      if (present(stat)) stat = HSD_STAT_NOT_FOUND
    end select

  end subroutine get_child_by_path

end module hsd_accessors
