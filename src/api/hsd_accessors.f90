!> HSD data accessors (getters)
!>
!> This module provides interfaces and implementations for retrieving data
!> from HSD tables. It supports type-safe access to scalars, arrays, and
!> matrices with optional default values.
module hsd_accessors
  use hsd_constants, only: dp, sp
  use hsd_error, only: HSD_STAT_OK, HSD_STAT_NOT_FOUND, HSD_STAT_TYPE_ERROR
  use hsd_types, only: hsd_node, hsd_table, hsd_value, new_value, VALUE_TYPE_ARRAY
  use hsd_query, only: hsd_get_child, hsd_get_table
  use hsd_mutators, only: hsd_set
  implicit none (type, external)
  private

  ! Public interfaces
  public :: hsd_get, hsd_get_or, hsd_get_or_set, hsd_get_matrix
  public :: hsd_get_inline_text

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

  !> Generic interface for getting values with default, writing default back to tree if absent
  !>
  !> Like `hsd_get_or`, but if the key is not found, the default value is also
  !> written back into the tree. This is critical for generating processed output
  !> (e.g., dftb_pin.hsd) that contains all defaults.
  !> stat is HSD_STAT_NOT_FOUND when default is used, HSD_STAT_OK when key existed.
  interface hsd_get_or_set
    module procedure :: hsd_get_or_set_string
    module procedure :: hsd_get_or_set_integer
    module procedure :: hsd_get_or_set_real_dp
    module procedure :: hsd_get_or_set_real_sp
    module procedure :: hsd_get_or_set_logical
    module procedure :: hsd_get_or_set_complex_dp
    module procedure :: hsd_get_or_set_integer_array
    module procedure :: hsd_get_or_set_real_dp_array
    module procedure :: hsd_get_or_set_real_sp_array
    module procedure :: hsd_get_or_set_logical_array
  end interface hsd_get_or_set

  !> Generic interface for getting 2D matrices
  interface hsd_get_matrix
    module procedure :: hsd_get_integer_matrix
    module procedure :: hsd_get_real_dp_matrix
    module procedure :: hsd_get_complex_dp_matrix
  end interface hsd_get_matrix

contains

  !> Get inline text content from a table node.
  !>
  !> Looks for a child named "#text" (the convention for inline text).
  !> If found, extracts the string value; otherwise returns NOT_FOUND.
  subroutine get_inline_text_(table, val, stat)
    type(hsd_table), intent(in), target :: table
    character(len=:), allocatable, intent(out) :: val
    integer, intent(out) :: stat

    class(hsd_node), pointer :: child

    call table%get_child_by_name("#text", child, case_insensitive=.true.)
    if (associated(child)) then
      select type (child)
      type is (hsd_value)
        call child%get_string(val, stat)
        return
      end select
    end if

    val = ""
    stat = HSD_STAT_NOT_FOUND

  end subroutine get_inline_text_

  !> Get the inline text VALUE node from a table node.
  !>
  !> Looks for a child named "#text" and returns the hsd_value pointer.
  subroutine get_inline_value_(table, val_node, stat)
    type(hsd_table), intent(in), target :: table
    type(hsd_value), pointer, intent(out) :: val_node
    integer, intent(out) :: stat

    class(hsd_node), pointer :: child

    nullify(val_node)
    call table%get_child_by_name("#text", child, case_insensitive=.true.)
    if (associated(child)) then
      select type (child)
      type is (hsd_value)
        val_node => child
        stat = HSD_STAT_OK
        return
      end select
    end if

    stat = HSD_STAT_NOT_FOUND

  end subroutine get_inline_value_

  !> Get string value by path
  subroutine hsd_get_string(table, path, val, stat)
    type(hsd_table), intent(in), target :: table
    character(len=*), intent(in) :: path
    character(len=:), allocatable, intent(out) :: val
    integer, intent(out), optional :: stat

    class(hsd_node), pointer :: child
    integer :: local_stat

    call hsd_get_child(table, path, child, local_stat)

    if (local_stat /= 0 .or. .not. associated(child)) then
      if (present(stat)) stat = HSD_STAT_NOT_FOUND
      val = ""
      return
    end if

    child%processed = .true.

    select type (child)
    type is (hsd_value)
      call child%get_string(val, local_stat)
      if (present(stat)) stat = local_stat
    type is (hsd_table)
      ! Table node: try to read inline text content (#text child)
      call get_inline_text_(child, val, local_stat)
      ! If no inline text found, this is a type error (node is a table, not a value)
      if (local_stat == HSD_STAT_NOT_FOUND) local_stat = HSD_STAT_TYPE_ERROR
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
      if (present(stat)) stat = local_stat
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

    call hsd_get_child(table, path, child, local_stat)

    if (local_stat /= 0 .or. .not. associated(child)) then
      if (present(stat)) stat = HSD_STAT_NOT_FOUND
      val = 0
      return
    end if

    child%processed = .true.

    select type (child)
    type is (hsd_value)
      call child%get_integer(val, local_stat)
      if (present(stat)) stat = local_stat
    type is (hsd_table)
      block
        type(hsd_value), pointer :: vnode
        call get_inline_value_(child, vnode, local_stat)
        if (local_stat == 0) then
          call vnode%get_integer(val, local_stat)
          if (present(stat)) stat = local_stat
        else
          if (present(stat)) stat = HSD_STAT_TYPE_ERROR
          val = 0
        end if
      end block
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
      if (present(stat)) stat = local_stat
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

    call hsd_get_child(table, path, child, local_stat)

    if (local_stat /= 0 .or. .not. associated(child)) then
      if (present(stat)) stat = HSD_STAT_NOT_FOUND
      val = 0.0_dp
      return
    end if

    child%processed = .true.

    select type (child)
    type is (hsd_value)
      call child%get_real(val, local_stat)
      if (present(stat)) stat = local_stat
    type is (hsd_table)
      block
        type(hsd_value), pointer :: vnode
        call get_inline_value_(child, vnode, local_stat)
        if (local_stat == 0) then
          call vnode%get_real(val, local_stat)
          if (present(stat)) stat = local_stat
        else
          if (present(stat)) stat = HSD_STAT_TYPE_ERROR
          val = 0.0_dp
        end if
      end block
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
      if (present(stat)) stat = local_stat
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
      if (present(stat)) stat = local_stat
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

    call hsd_get_child(table, path, child, local_stat)

    if (local_stat /= 0 .or. .not. associated(child)) then
      if (present(stat)) stat = HSD_STAT_NOT_FOUND
      val = .false.
      return
    end if

    child%processed = .true.

    select type (child)
    type is (hsd_value)
      call child%get_logical(val, local_stat)
      if (present(stat)) stat = local_stat
    type is (hsd_table)
      block
        type(hsd_value), pointer :: vnode
        call get_inline_value_(child, vnode, local_stat)
        if (local_stat == 0) then
          call vnode%get_logical(val, local_stat)
          if (present(stat)) stat = local_stat
        else
          if (present(stat)) stat = HSD_STAT_TYPE_ERROR
          val = .false.
        end if
      end block
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
      if (present(stat)) stat = local_stat
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

    call hsd_get_child(table, path, child, local_stat)

    if (local_stat /= 0 .or. .not. associated(child)) then
      if (present(stat)) stat = HSD_STAT_NOT_FOUND
      val = (0.0_dp, 0.0_dp)
      return
    end if

    child%processed = .true.

    select type (child)
    type is (hsd_value)
      call child%get_complex(val, local_stat)
      if (present(stat)) stat = local_stat
    type is (hsd_table)
      block
        type(hsd_value), pointer :: vnode
        call get_inline_value_(child, vnode, local_stat)
        if (local_stat == 0) then
          call vnode%get_complex(val, local_stat)
          if (present(stat)) stat = local_stat
        else
          if (present(stat)) stat = HSD_STAT_TYPE_ERROR
          val = (0.0_dp, 0.0_dp)
        end if
      end block
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
      if (present(stat)) stat = local_stat
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

    call hsd_get_child(table, path, child, local_stat)

    if (local_stat /= 0 .or. .not. associated(child)) then
      if (present(stat)) stat = HSD_STAT_NOT_FOUND
      allocate(val(0))
      return
    end if

    child%processed = .true.

    select type (child)
    type is (hsd_value)
      call child%get_int_array(val, local_stat)
      if (present(stat)) stat = local_stat
    type is (hsd_table)
      block
        type(hsd_value), pointer :: vnode
        call get_inline_value_(child, vnode, local_stat)
        if (local_stat == 0) then
          call vnode%get_int_array(val, local_stat)
          if (present(stat)) stat = local_stat
        else
          if (present(stat)) stat = HSD_STAT_TYPE_ERROR
          allocate(val(0))
        end if
      end block
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

    call hsd_get_child(table, path, child, local_stat)

    if (local_stat /= 0 .or. .not. associated(child)) then
      if (present(stat)) stat = HSD_STAT_NOT_FOUND
      allocate(val(0))
      return
    end if

    child%processed = .true.

    select type (child)
    type is (hsd_value)
      call child%get_real_array(val, local_stat)
      if (present(stat)) stat = local_stat
    type is (hsd_table)
      block
        type(hsd_value), pointer :: vnode
        call get_inline_value_(child, vnode, local_stat)
        if (local_stat == 0) then
          call vnode%get_real_array(val, local_stat)
          if (present(stat)) stat = local_stat
        else
          if (present(stat)) stat = HSD_STAT_TYPE_ERROR
          allocate(val(0))
        end if
      end block
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

    call hsd_get_child(table, path, child, local_stat)

    if (local_stat /= 0 .or. .not. associated(child)) then
      if (present(stat)) stat = HSD_STAT_NOT_FOUND
      allocate(val(0))
      return
    end if

    child%processed = .true.

    select type (child)
    type is (hsd_value)
      call child%get_logical_array(val, local_stat)
      if (present(stat)) stat = local_stat
    type is (hsd_table)
      block
        type(hsd_value), pointer :: vnode
        call get_inline_value_(child, vnode, local_stat)
        if (local_stat == 0) then
          call vnode%get_logical_array(val, local_stat)
          if (present(stat)) stat = local_stat
        else
          if (present(stat)) stat = HSD_STAT_TYPE_ERROR
          allocate(val(0))
        end if
      end block
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

    call hsd_get_child(table, path, child, local_stat)

    if (local_stat /= 0 .or. .not. associated(child)) then
      if (present(stat)) stat = HSD_STAT_NOT_FOUND
      allocate(character(len=1) :: val(0))
      return
    end if

    child%processed = .true.

    select type (child)
    type is (hsd_value)
      call child%get_string_array(val, local_stat)
      if (present(stat)) stat = local_stat
    type is (hsd_table)
      block
        type(hsd_value), pointer :: vnode
        call get_inline_value_(child, vnode, local_stat)
        if (local_stat == 0) then
          call vnode%get_string_array(val, local_stat)
          if (present(stat)) stat = local_stat
        else
          if (present(stat)) stat = HSD_STAT_TYPE_ERROR
          allocate(character(len=1) :: val(0))
        end if
      end block
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

    call hsd_get_child(table, path, child, local_stat)

    if (local_stat /= 0 .or. .not. associated(child)) then
      if (present(stat)) stat = HSD_STAT_NOT_FOUND
      allocate(val(0))
      return
    end if

    child%processed = .true.

    select type (child)
    type is (hsd_value)
      call child%get_complex_array(val, local_stat)
      if (present(stat)) stat = local_stat
    type is (hsd_table)
      block
        type(hsd_value), pointer :: vnode
        call get_inline_value_(child, vnode, local_stat)
        if (local_stat == 0) then
          call vnode%get_complex_array(val, local_stat)
          if (present(stat)) stat = local_stat
        else
          if (present(stat)) stat = HSD_STAT_TYPE_ERROR
          allocate(val(0))
        end if
      end block
    class default
      if (present(stat)) stat = HSD_STAT_TYPE_ERROR
      allocate(val(0))
    end select

  end subroutine hsd_get_complex_dp_array

  !> Get 2D integer matrix by path (rows separated by newlines or semicolons)
  !> Handles both value nodes and table nodes (where content is in unnamed children)
  !>
  !> If `order` is present and set to "column-major", the returned matrix is
  !> transposed so that text rows map to Fortran columns (column-major layout).
  !> Default is text-layout (row-major).
  subroutine hsd_get_integer_matrix(table, path, val, nrows, ncols, stat, order)
    type(hsd_table), intent(in), target :: table
    character(len=*), intent(in) :: path
    integer, allocatable, intent(out) :: val(:,:)
    integer, intent(out) :: nrows, ncols
    integer, intent(out), optional :: stat
    character(len=*), intent(in), optional :: order

    class(hsd_node), pointer :: child
    integer :: local_stat

    call hsd_get_child(table, path, child, local_stat)

    if (local_stat /= 0 .or. .not. associated(child)) then
      if (present(stat)) stat = HSD_STAT_NOT_FOUND
      allocate(val(0,0))
      nrows = 0
      ncols = 0
      return
    end if

    child%processed = .true.

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

    ! Transpose if column-major order requested
    if (present(order)) then
      if (order == "column-major" .and. nrows > 0 .and. ncols > 0) then
        block
          integer, allocatable :: tmp(:,:)
          integer :: swap
          allocate(tmp(ncols, nrows))
          tmp = transpose(val)
          call move_alloc(tmp, val)
          swap = nrows
          nrows = ncols
          ncols = swap
        end block
      end if
    end if

  end subroutine hsd_get_integer_matrix

  !> Get 2D real matrix by path
  !> Handles both value nodes and table nodes (where content is in unnamed children)
  !>
  !> If `order` is present and set to "column-major", the returned matrix is
  !> transposed so that text rows map to Fortran columns (column-major layout).
  !> Default is text-layout (row-major).
  subroutine hsd_get_real_dp_matrix(table, path, val, nrows, ncols, stat, order)
    type(hsd_table), intent(in), target :: table
    character(len=*), intent(in) :: path
    real(dp), allocatable, intent(out) :: val(:,:)
    integer, intent(out) :: nrows, ncols
    integer, intent(out), optional :: stat
    character(len=*), intent(in), optional :: order

    class(hsd_node), pointer :: child
    integer :: local_stat

    call hsd_get_child(table, path, child, local_stat)

    if (local_stat /= 0 .or. .not. associated(child)) then
      if (present(stat)) stat = HSD_STAT_NOT_FOUND
      allocate(val(0,0))
      nrows = 0
      ncols = 0
      return
    end if

    child%processed = .true.

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

    ! Transpose if column-major order requested
    if (present(order)) then
      if (order == "column-major" .and. nrows > 0 .and. ncols > 0) then
        block
          real(dp), allocatable :: tmp(:,:)
          integer :: swap
          allocate(tmp(ncols, nrows))
          tmp = transpose(val)
          call move_alloc(tmp, val)
          swap = nrows
          nrows = ncols
          ncols = swap
        end block
      end if
    end if

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
          ! Include unnamed, empty-named, or #text-named value nodes
          block
            logical :: is_text_child
            is_text_child = .not. allocated(child%name)
            if (.not. is_text_child) &
                & is_text_child = (len_trim(child%name) == 0 .or. child%name == "#text")
            if (is_text_child) then
              call child%get_string(str_val, local_stat)
              if (local_stat == 0 .and. len_trim(str_val) > 0) then
                if (len(combined_text) > 0) then
                  combined_text = combined_text // char(10) // str_val
                else
                  combined_text = str_val
                end if
              end if
            end if
          end block
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
          ! Include unnamed, empty-named, or #text-named value nodes
          block
            logical :: is_text_child
            is_text_child = .not. allocated(child%name)
            if (.not. is_text_child) &
                & is_text_child = (len_trim(child%name) == 0 .or. child%name == "#text")
            if (is_text_child) then
              call child%get_string(str_val, local_stat)
              if (local_stat == 0 .and. len_trim(str_val) > 0) then
                if (len(combined_text) > 0) then
                  combined_text = combined_text // char(10) // str_val
                else
                  combined_text = str_val
                end if
              end if
            end if
          end block
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

  !> Get 2D complex matrix by path
  !> Handles both value nodes and table nodes (where content is in unnamed children)
  !>
  !> If `order` is present and set to "column-major", the returned matrix is
  !> transposed so that text rows map to Fortran columns (column-major layout).
  !> Default is text-layout (row-major).
  subroutine hsd_get_complex_dp_matrix(table, path, val, nrows, ncols, stat, order)
    type(hsd_table), intent(in), target :: table
    character(len=*), intent(in) :: path
    complex(dp), allocatable, intent(out) :: val(:,:)
    integer, intent(out) :: nrows, ncols
    integer, intent(out), optional :: stat
    character(len=*), intent(in), optional :: order

    class(hsd_node), pointer :: child
    integer :: local_stat

    call hsd_get_child(table, path, child, local_stat)

    if (local_stat /= 0 .or. .not. associated(child)) then
      if (present(stat)) stat = HSD_STAT_NOT_FOUND
      allocate(val(0,0))
      nrows = 0
      ncols = 0
      return
    end if

    child%processed = .true.

    select type (child)
    type is (hsd_value)
      call child%get_complex_matrix(val, nrows, ncols, local_stat)
      if (present(stat)) stat = local_stat
    type is (hsd_table)
      ! Table nodes store matrix data as unnamed child values
      call get_complex_matrix_from_table(child, val, nrows, ncols, local_stat)
      if (present(stat)) stat = local_stat
    class default
      if (present(stat)) stat = HSD_STAT_TYPE_ERROR
      allocate(val(0,0))
      nrows = 0
      ncols = 0
    end select

    ! Transpose if column-major order requested
    if (present(order)) then
      if (order == "column-major" .and. nrows > 0 .and. ncols > 0) then
        block
          complex(dp), allocatable :: tmp(:,:)
          integer :: swap
          allocate(tmp(ncols, nrows))
          tmp = transpose(val)
          call move_alloc(tmp, val)
          swap = nrows
          nrows = ncols
          ncols = swap
        end block
      end if
    end if

  end subroutine hsd_get_complex_dp_matrix

  !> Extract complex matrix from table with unnamed value children
  subroutine get_complex_matrix_from_table(tbl, mat, nrows, ncols, stat)
    type(hsd_table), intent(in) :: tbl
    complex(dp), allocatable, intent(out) :: mat(:,:)
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
          ! Include unnamed, empty-named, or #text-named value nodes
          block
            logical :: is_text_child
            is_text_child = .not. allocated(child%name)
            if (.not. is_text_child) &
                & is_text_child = (len_trim(child%name) == 0 .or. child%name == "#text")
            if (is_text_child) then
              call child%get_string(str_val, local_stat)
              if (local_stat == 0 .and. len_trim(str_val) > 0) then
                if (len(combined_text) > 0) then
                  combined_text = combined_text // char(10) // str_val
                else
                  combined_text = str_val
                end if
              end if
            end if
          end block
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
      call temp_val%get_complex_matrix(mat, nrows, ncols, stat)
      call temp_val%destroy()
    end block

  end subroutine get_complex_matrix_from_table

  ! ===== helpers =====

  !> Mark a named child node as processed
  subroutine mark_child_processed_(table, path)
    type(hsd_table), intent(inout), target :: table
    character(len=*), intent(in) :: path

    class(hsd_node), pointer :: child
    integer :: local_stat

    call hsd_get_child(table, path, child, local_stat)
    if (local_stat == 0 .and. associated(child)) then
      child%processed = .true.
    end if

  end subroutine mark_child_processed_

  ! ===== hsd_get_or_set implementations =====

  !> Get string value with default, writing default back to tree if absent
  subroutine hsd_get_or_set_string(table, path, val, default, stat, child)
    type(hsd_table), intent(inout), target :: table
    character(len=*), intent(in) :: path
    character(len=:), allocatable, intent(out) :: val
    character(len=*), intent(in) :: default
    integer, intent(out), optional :: stat
    type(hsd_table), pointer, intent(out), optional :: child

    integer :: local_stat

    call hsd_get_string(table, path, val, local_stat)

    if (local_stat /= 0) then
      val = default
      call hsd_set(table, path, default)
      call mark_child_processed_(table, path)
      if (present(stat)) stat = local_stat
    else
      if (present(stat)) stat = HSD_STAT_OK
    end if

    if (present(child)) then
      call hsd_get_table(table, path, child)
      if (.not. associated(child)) child => table
    end if

  end subroutine hsd_get_or_set_string

  !> Get integer value with default, writing default back to tree if absent
  subroutine hsd_get_or_set_integer(table, path, val, default, stat, child)
    type(hsd_table), intent(inout), target :: table
    character(len=*), intent(in) :: path
    integer, intent(out) :: val
    integer, intent(in) :: default
    integer, intent(out), optional :: stat
    type(hsd_table), pointer, intent(out), optional :: child

    integer :: local_stat

    call hsd_get_integer(table, path, val, local_stat)

    if (local_stat /= 0) then
      val = default
      call hsd_set(table, path, default)
      call mark_child_processed_(table, path)
      if (present(stat)) stat = local_stat
    else
      if (present(stat)) stat = HSD_STAT_OK
    end if

    if (present(child)) then
      call hsd_get_table(table, path, child)
      if (.not. associated(child)) child => table
    end if

  end subroutine hsd_get_or_set_integer

  !> Get double precision real value with default, writing default back to tree if absent
  subroutine hsd_get_or_set_real_dp(table, path, val, default, stat, child)
    type(hsd_table), intent(inout), target :: table
    character(len=*), intent(in) :: path
    real(dp), intent(out) :: val
    real(dp), intent(in) :: default
    integer, intent(out), optional :: stat
    type(hsd_table), pointer, intent(out), optional :: child

    integer :: local_stat

    call hsd_get_real_dp(table, path, val, local_stat)

    if (local_stat /= 0) then
      val = default
      call hsd_set(table, path, default)
      call mark_child_processed_(table, path)
      if (present(stat)) stat = local_stat
    else
      if (present(stat)) stat = HSD_STAT_OK
    end if

    if (present(child)) then
      call hsd_get_table(table, path, child)
      if (.not. associated(child)) child => table
    end if

  end subroutine hsd_get_or_set_real_dp

  !> Get single precision real value with default, writing default back to tree if absent
  subroutine hsd_get_or_set_real_sp(table, path, val, default, stat, child)
    type(hsd_table), intent(inout), target :: table
    character(len=*), intent(in) :: path
    real(sp), intent(out) :: val
    real(sp), intent(in) :: default
    integer, intent(out), optional :: stat
    type(hsd_table), pointer, intent(out), optional :: child

    integer :: local_stat

    call hsd_get_real_sp(table, path, val, local_stat)

    if (local_stat /= 0) then
      val = default
      call hsd_set(table, path, real(default, dp))
      call mark_child_processed_(table, path)
      if (present(stat)) stat = local_stat
    else
      if (present(stat)) stat = HSD_STAT_OK
    end if

    if (present(child)) then
      call hsd_get_table(table, path, child)
      if (.not. associated(child)) child => table
    end if

  end subroutine hsd_get_or_set_real_sp

  !> Get logical value with default, writing default back to tree if absent
  subroutine hsd_get_or_set_logical(table, path, val, default, stat, child)
    type(hsd_table), intent(inout), target :: table
    character(len=*), intent(in) :: path
    logical, intent(out) :: val
    logical, intent(in) :: default
    integer, intent(out), optional :: stat
    type(hsd_table), pointer, intent(out), optional :: child

    integer :: local_stat

    call hsd_get_logical(table, path, val, local_stat)

    if (local_stat /= 0) then
      val = default
      call hsd_set(table, path, default)
      call mark_child_processed_(table, path)
      if (present(stat)) stat = local_stat
    else
      if (present(stat)) stat = HSD_STAT_OK
    end if

    if (present(child)) then
      call hsd_get_table(table, path, child)
      if (.not. associated(child)) child => table
    end if

  end subroutine hsd_get_or_set_logical

  !> Get complex value with default, writing default back to tree if absent
  subroutine hsd_get_or_set_complex_dp(table, path, val, default, stat, child)
    type(hsd_table), intent(inout), target :: table
    character(len=*), intent(in) :: path
    complex(dp), intent(out) :: val
    complex(dp), intent(in) :: default
    integer, intent(out), optional :: stat
    type(hsd_table), pointer, intent(out), optional :: child

    integer :: local_stat

    call hsd_get_complex_dp(table, path, val, local_stat)

    if (local_stat /= 0) then
      val = default
      call hsd_set(table, path, default)
      call mark_child_processed_(table, path)
      if (present(stat)) stat = local_stat
    else
      if (present(stat)) stat = HSD_STAT_OK
    end if

    if (present(child)) then
      call hsd_get_table(table, path, child)
      if (.not. associated(child)) child => table
    end if

  end subroutine hsd_get_or_set_complex_dp

  !> Get integer array with default, writing default back to tree if absent
  subroutine hsd_get_or_set_integer_array(table, path, val, default, stat, child)
    type(hsd_table), intent(inout), target :: table
    character(len=*), intent(in) :: path
    integer, allocatable, intent(out) :: val(:)
    integer, intent(in) :: default(:)
    integer, intent(out), optional :: stat
    type(hsd_table), pointer, intent(out), optional :: child

    integer :: local_stat

    call hsd_get_integer_array(table, path, val, local_stat)

    if (local_stat /= 0) then
      val = default
      call hsd_set(table, path, default)
      call mark_child_processed_(table, path)
      if (present(stat)) stat = local_stat
    else
      if (present(stat)) stat = HSD_STAT_OK
    end if

    if (present(child)) then
      call hsd_get_table(table, path, child)
      if (.not. associated(child)) child => table
    end if

  end subroutine hsd_get_or_set_integer_array

  !> Get double precision real array with default, writing default back to tree if absent
  subroutine hsd_get_or_set_real_dp_array(table, path, val, default, stat, child)
    type(hsd_table), intent(inout), target :: table
    character(len=*), intent(in) :: path
    real(dp), allocatable, intent(out) :: val(:)
    real(dp), intent(in) :: default(:)
    integer, intent(out), optional :: stat
    type(hsd_table), pointer, intent(out), optional :: child

    integer :: local_stat

    call hsd_get_real_dp_array(table, path, val, local_stat)

    if (local_stat /= 0) then
      val = default
      call hsd_set(table, path, default)
      call mark_child_processed_(table, path)
      if (present(stat)) stat = local_stat
    else
      if (present(stat)) stat = HSD_STAT_OK
    end if

    if (present(child)) then
      call hsd_get_table(table, path, child)
      if (.not. associated(child)) child => table
    end if

  end subroutine hsd_get_or_set_real_dp_array

  !> Get single precision real array with default, writing default back to tree if absent
  subroutine hsd_get_or_set_real_sp_array(table, path, val, default, stat, child)
    type(hsd_table), intent(inout), target :: table
    character(len=*), intent(in) :: path
    real(sp), allocatable, intent(out) :: val(:)
    real(sp), intent(in) :: default(:)
    integer, intent(out), optional :: stat
    type(hsd_table), pointer, intent(out), optional :: child

    integer :: local_stat

    call hsd_get_real_sp_array(table, path, val, local_stat)

    if (local_stat /= 0) then
      val = default
      call hsd_set(table, path, real(default, dp))
      call mark_child_processed_(table, path)
      if (present(stat)) stat = local_stat
    else
      if (present(stat)) stat = HSD_STAT_OK
    end if

    if (present(child)) then
      call hsd_get_table(table, path, child)
      if (.not. associated(child)) child => table
    end if

  end subroutine hsd_get_or_set_real_sp_array

  !> Get logical array with default, writing default back to tree if absent
  subroutine hsd_get_or_set_logical_array(table, path, val, default, stat, child)
    type(hsd_table), intent(inout), target :: table
    character(len=*), intent(in) :: path
    logical, allocatable, intent(out) :: val(:)
    logical, intent(in) :: default(:)
    integer, intent(out), optional :: stat
    type(hsd_table), pointer, intent(out), optional :: child

    integer :: local_stat

    call hsd_get_logical_array(table, path, val, local_stat)

    if (local_stat /= 0) then
      val = default
      call hsd_set(table, path, default)
      call mark_child_processed_(table, path)
      if (present(stat)) stat = local_stat
    else
      if (present(stat)) stat = HSD_STAT_OK
    end if

    if (present(child)) then
      call hsd_get_table(table, path, child)
      if (.not. associated(child)) child => table
    end if

  end subroutine hsd_get_or_set_logical_array


  !> Get concatenated text content of all unnamed value children.
  !>
  !> Iterates children of `table`, collecting text from unnamed or "#text"
  !> `hsd_value` nodes. Multiple values are separated by spaces.
  subroutine hsd_get_inline_text(table, text, stat)
    type(hsd_table), intent(in), target :: table
    character(len=:), allocatable, intent(out) :: text
    integer, intent(out), optional :: stat

    integer :: ii, local_stat
    class(hsd_node), pointer :: child
    character(len=:), allocatable :: piece

    text = ""
    do ii = 1, table%num_children
      call table%get_child(ii, child)
      if (.not. associated(child)) cycle
      select type (v => child)
      type is (hsd_value)
        block
          logical :: is_anon_val
          is_anon_val = .true.
          if (allocated(v%name)) then
            if (len_trim(v%name) > 0 .and. v%name /= "#text") then
              is_anon_val = .false.
            end if
          end if
        if (is_anon_val) then
          call v%get_string(piece, local_stat)
          if (local_stat == HSD_STAT_OK .and. allocated(piece)) then
            if (len(text) > 0) then
              text = text // " " // piece
            else
              text = piece
            end if
          end if
        end if
        end block
      end select
    end do

    if (present(stat)) then
      if (len(text) > 0) then
        stat = HSD_STAT_OK
      else
        stat = HSD_STAT_NOT_FOUND
      end if
    end if

  end subroutine hsd_get_inline_text

end module hsd_accessors
