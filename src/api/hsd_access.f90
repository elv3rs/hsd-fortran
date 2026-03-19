!> HSD Access Object
!>
!> Provides a configurable, error-accumulating interface for reading and writing
!> HSD tree values. This is the primary user-facing API for the HSD library.
!>
!> ## Usage
!>
!> ```fortran
!> type(hsd_node_t), target :: root
!> type(hsd_access_t) :: access
!>
!> call hsd_load_file("input.hsd", root, error)
!> call access%init(root)
!> call access%get("Geometry/Periodic", periodic, default=.false.)
!> call access%get("Hamiltonian/MaxIter", max_iter, default=100)
!> if (access%has_errors()) then
!>   call access%print_errors()
!>   stop 1
!> end if
!> ```
!>
!> ## On-Missing Behavior
!>
!> When a key is not found and a default is provided, the `on_missing` setting
!> controls what happens:
!>
!> - `HSD_ON_MISSING_SET` (default): writes the default value back to the tree,
!>   critical for generating processed output (e.g. dftb_pin.hsd).
!> - `HSD_ON_MISSING_RETURN`: returns the default without modifying the tree.
!>
!> ## Thread Safety
!>
!> The access object is NOT thread-safe. Each thread should use its own instance.
!> The root node must have the `target` attribute and must outlive the access object.
module hsd_access
  use, intrinsic :: iso_fortran_env, only: error_unit
  use hsd_constants, only: dp
  use hsd_utils, only: to_lower
  use hsd_error, only: HSD_STAT_OK, HSD_STAT_NOT_FOUND, HSD_STAT_TYPE_ERROR
  use hsd_types, only: hsd_node_t, NODE_TYPE_TABLE, NODE_TYPE_VALUE
  use hsd_api, only: hsd_get_child, hsd_set, hsd_get_matrix
  implicit none (type, external)
  private

  !> Write default value back to tree when key is missing
  integer, parameter, public :: HSD_ON_MISSING_SET = 1
  !> Return default without modifying the tree
  integer, parameter, public :: HSD_ON_MISSING_RETURN = 2

  !> Single error entry in the error stack
  type, public :: hsd_error_entry_t
    character(len=:), allocatable :: path
    integer :: stat = 0
    character(len=:), allocatable :: message
  end type hsd_error_entry_t

  !> Configurable access object for HSD trees
  !>
  !> Wraps an HSD tree root with configurable behavior (processed marking,
  !> default handling) and accumulates errors for batch checking.
  type, public :: hsd_access_t
    !> Non-owning pointer to tree root (must have target attribute)
    type(hsd_node_t), pointer :: root => null()
    !> Whether to set the processed flag on accessed nodes
    logical :: mark_processed = .true.
    !> Behavior when a key is not found and a default is provided
    integer :: on_missing = HSD_ON_MISSING_SET
    !> Error stack (grows by doubling)
    type(hsd_error_entry_t), allocatable :: errors(:)
    !> Number of errors currently in the stack
    integer :: num_errors = 0
  contains
    procedure :: init => access_init
    ! Scalar getters
    procedure, private :: get_string => access_get_string
    procedure, private :: get_integer => access_get_integer
    procedure, private :: get_real_dp => access_get_real_dp
    procedure, private :: get_logical => access_get_logical
    procedure, private :: get_complex_dp => access_get_complex_dp
    ! Array getters
    procedure, private :: get_integer_array => access_get_integer_array
    procedure, private :: get_real_dp_array => access_get_real_dp_array
    procedure, private :: get_logical_array => access_get_logical_array
    procedure, private :: get_string_array => access_get_string_array
    procedure, private :: get_complex_dp_array => access_get_complex_dp_array
    !> Generic value getter (resolves on val type and rank)
    generic :: get => get_string, get_integer, get_real_dp, get_logical, &
        & get_complex_dp, get_integer_array, get_real_dp_array, &
        & get_logical_array, get_string_array, get_complex_dp_array
    ! Matrix getters
    procedure, private :: get_int_matrix => access_get_int_matrix
    procedure, private :: get_real_matrix => access_get_real_matrix
    procedure, private :: get_complex_matrix => access_get_complex_matrix
    !> Generic matrix getter (resolves on val element type)
    generic :: get_matrix => get_int_matrix, get_real_matrix, get_complex_matrix
    ! Scalar setters
    procedure, private :: set_string => access_set_string
    procedure, private :: set_integer => access_set_integer
    procedure, private :: set_real_dp => access_set_real_dp
    procedure, private :: set_logical => access_set_logical
    procedure, private :: set_complex_dp => access_set_complex_dp
    ! Array setters
    procedure, private :: set_integer_array => access_set_integer_array
    procedure, private :: set_real_dp_array => access_set_real_dp_array
    procedure, private :: set_logical_array => access_set_logical_array
    procedure, private :: set_string_array => access_set_string_array
    procedure, private :: set_complex_dp_array => access_set_complex_dp_array
    ! Matrix setters
    procedure, private :: set_integer_matrix => access_set_integer_matrix
    procedure, private :: set_real_dp_matrix => access_set_real_dp_matrix
    procedure, private :: set_complex_dp_matrix => access_set_complex_dp_matrix
    !> Generic value/array/matrix setter (resolves on val type and rank)
    generic :: set => set_string, set_integer, set_real_dp, set_logical, &
        & set_complex_dp, set_integer_array, set_real_dp_array, &
        & set_logical_array, set_string_array, set_complex_dp_array, &
        & set_integer_matrix, set_real_dp_matrix, set_complex_dp_matrix
    ! Choice getter
    procedure :: get_choice => access_get_choice
    ! Error handling
    procedure :: has_errors => access_has_errors
    procedure :: error_count => access_error_count
    procedure :: print_errors => access_print_errors
    procedure :: clear_errors => access_clear_errors
    procedure :: get_errors => access_get_errors
    ! Internal helpers
    procedure, private :: push_error => access_push_error
    procedure, private :: resolve_value_node => access_resolve_value_node
    procedure, private :: mark_path_processed => access_mark_path_processed
  end type hsd_access_t

contains

  ! ===== Initialization =====

  !> Initialize the access object with a tree root
  subroutine access_init(self, root, mark_processed, on_missing)
    class(hsd_access_t), intent(out) :: self
    type(hsd_node_t), intent(in), target :: root
    logical, intent(in), optional :: mark_processed
    integer, intent(in), optional :: on_missing

    self%root => root
    if (present(mark_processed)) self%mark_processed = mark_processed
    if (present(on_missing)) self%on_missing = on_missing
    allocate(self%errors(16))
    self%num_errors = 0
  end subroutine access_init

  ! ===== Error Stack =====

  !> Push an error entry onto the stack
  subroutine access_push_error(self, path, stat, message)
    class(hsd_access_t), intent(inout) :: self
    character(len=*), intent(in) :: path
    integer, intent(in) :: stat
    character(len=*), intent(in) :: message

    type(hsd_error_entry_t), allocatable :: tmp(:)

    if (.not. allocated(self%errors)) allocate(self%errors(16))

    if (self%num_errors >= size(self%errors)) then
      allocate(tmp(size(self%errors) * 2))
      tmp(1:self%num_errors) = self%errors(1:self%num_errors)
      call move_alloc(tmp, self%errors)
    end if

    self%num_errors = self%num_errors + 1
    self%errors(self%num_errors)%path = path
    self%errors(self%num_errors)%stat = stat
    self%errors(self%num_errors)%message = message
  end subroutine access_push_error

  !> Check whether any errors have been accumulated
  pure function access_has_errors(self) result(has)
    class(hsd_access_t), intent(in) :: self
    logical :: has
    has = self%num_errors > 0
  end function access_has_errors

  !> Return the number of accumulated errors
  pure function access_error_count(self) result(n)
    class(hsd_access_t), intent(in) :: self
    integer :: n
    n = self%num_errors
  end function access_error_count

  !> Print all accumulated errors to a file unit (default: stderr)
  subroutine access_print_errors(self, unit)
    class(hsd_access_t), intent(in) :: self
    integer, intent(in), optional :: unit

    integer :: iu, ii

    iu = error_unit
    if (present(unit)) iu = unit

    do ii = 1, self%num_errors
      write(iu, '(A,A,A,A)') &
          & "Error at '", self%errors(ii)%path, &
          & "': ", self%errors(ii)%message
    end do
  end subroutine access_print_errors

  !> Clear all accumulated errors
  subroutine access_clear_errors(self)
    class(hsd_access_t), intent(inout) :: self
    self%num_errors = 0
  end subroutine access_clear_errors

  !> Return a copy of all accumulated errors
  subroutine access_get_errors(self, errors)
    class(hsd_access_t), intent(in) :: self
    type(hsd_error_entry_t), allocatable, intent(out) :: errors(:)

    if (self%num_errors > 0) then
      errors = self%errors(1:self%num_errors)
    else
      allocate(errors(0))
    end if
  end subroutine access_get_errors

  ! ===== Internal Helpers =====

  !> Resolve a path to a value node, handling inline text transparently
  subroutine access_resolve_value_node(self, path, val_node, stat)
    class(hsd_access_t), intent(inout) :: self
    character(len=*), intent(in) :: path
    type(hsd_node_t), pointer, intent(out) :: val_node
    integer, intent(out) :: stat

    type(hsd_node_t), pointer :: child

    nullify(val_node)
    call hsd_get_child(self%root, path, child, stat)
    if (stat /= HSD_STAT_OK .or. .not. associated(child)) then
      stat = HSD_STAT_NOT_FOUND
      return
    end if

    if (self%mark_processed) child%processed = .true.

    if (child%node_type == NODE_TYPE_VALUE) then
      val_node => child
      stat = HSD_STAT_OK
    else if (child%node_type == NODE_TYPE_TABLE) then
      ! Try to extract inline value (#text child)
      call child%get_child_by_name("#text", val_node)
      if (associated(val_node) .and. &
          & val_node%node_type == NODE_TYPE_VALUE) then
        stat = HSD_STAT_OK
      else
        nullify(val_node)
        stat = HSD_STAT_TYPE_ERROR
      end if
    else
      stat = HSD_STAT_TYPE_ERROR
    end if
  end subroutine access_resolve_value_node

  !> Mark the child at the given path as processed
  subroutine access_mark_path_processed(self, path)
    class(hsd_access_t), intent(inout) :: self
    character(len=*), intent(in) :: path

    type(hsd_node_t), pointer :: child
    integer :: local_stat

    if (.not. self%mark_processed) return
    call hsd_get_child(self%root, path, child, local_stat)
    if (local_stat == HSD_STAT_OK .and. associated(child)) &
        & child%processed = .true.
  end subroutine access_mark_path_processed

  ! ===== Scalar Getters =====

  !> Get string value by path
  subroutine access_get_string(self, path, val, default)
    class(hsd_access_t), intent(inout) :: self
    character(len=*), intent(in) :: path
    character(len=:), allocatable, intent(out) :: val
    character(len=*), intent(in), optional :: default

    type(hsd_node_t), pointer :: vnode
    integer :: local_stat

    call self%resolve_value_node(path, vnode, local_stat)
    if (local_stat == HSD_STAT_OK) then
      call vnode%get_string(val, local_stat)
      if (local_stat /= HSD_STAT_OK) then
        call self%push_error(path, HSD_STAT_TYPE_ERROR, &
            & "Cannot read as string")
        val = ""
      end if
      return
    end if

    if (present(default)) then
      val = default
      if (self%on_missing == HSD_ON_MISSING_SET) then
        call hsd_set(self%root, path, default)
        call self%mark_path_processed(path)
      end if
    else
      call self%push_error(path, HSD_STAT_NOT_FOUND, &
          & "Required string field not found")
      val = ""
    end if
  end subroutine access_get_string

  !> Get integer value by path
  subroutine access_get_integer(self, path, val, default)
    class(hsd_access_t), intent(inout) :: self
    character(len=*), intent(in) :: path
    integer, intent(out) :: val
    integer, intent(in), optional :: default

    type(hsd_node_t), pointer :: vnode
    integer :: local_stat

    call self%resolve_value_node(path, vnode, local_stat)
    if (local_stat == HSD_STAT_OK) then
      call vnode%get_integer(val, local_stat)
      if (local_stat /= HSD_STAT_OK) then
        call self%push_error(path, HSD_STAT_TYPE_ERROR, &
            & "Cannot parse as integer")
        val = 0
      end if
      return
    end if

    if (present(default)) then
      val = default
      if (self%on_missing == HSD_ON_MISSING_SET) then
        call hsd_set(self%root, path, default)
        call self%mark_path_processed(path)
      end if
    else
      call self%push_error(path, HSD_STAT_NOT_FOUND, &
          & "Required integer field not found")
      val = 0
    end if
  end subroutine access_get_integer

  !> Get double precision real value by path
  subroutine access_get_real_dp(self, path, val, default)
    class(hsd_access_t), intent(inout) :: self
    character(len=*), intent(in) :: path
    real(dp), intent(out) :: val
    real(dp), intent(in), optional :: default

    type(hsd_node_t), pointer :: vnode
    integer :: local_stat

    call self%resolve_value_node(path, vnode, local_stat)
    if (local_stat == HSD_STAT_OK) then
      call vnode%get_real(val, local_stat)
      if (local_stat /= HSD_STAT_OK) then
        call self%push_error(path, HSD_STAT_TYPE_ERROR, &
            & "Cannot parse as real")
        val = 0.0_dp
      end if
      return
    end if

    if (present(default)) then
      val = default
      if (self%on_missing == HSD_ON_MISSING_SET) then
        call hsd_set(self%root, path, default)
        call self%mark_path_processed(path)
      end if
    else
      call self%push_error(path, HSD_STAT_NOT_FOUND, &
          & "Required real field not found")
      val = 0.0_dp
    end if
  end subroutine access_get_real_dp

  !> Get logical value by path
  subroutine access_get_logical(self, path, val, default)
    class(hsd_access_t), intent(inout) :: self
    character(len=*), intent(in) :: path
    logical, intent(out) :: val
    logical, intent(in), optional :: default

    type(hsd_node_t), pointer :: vnode
    integer :: local_stat

    call self%resolve_value_node(path, vnode, local_stat)
    if (local_stat == HSD_STAT_OK) then
      call vnode%get_logical(val, local_stat)
      if (local_stat /= HSD_STAT_OK) then
        call self%push_error(path, HSD_STAT_TYPE_ERROR, &
            & "Cannot parse as logical")
        val = .false.
      end if
      return
    end if

    if (present(default)) then
      val = default
      if (self%on_missing == HSD_ON_MISSING_SET) then
        call hsd_set(self%root, path, default)
        call self%mark_path_processed(path)
      end if
    else
      call self%push_error(path, HSD_STAT_NOT_FOUND, &
          & "Required logical field not found")
      val = .false.
    end if
  end subroutine access_get_logical

  !> Get complex double precision value by path
  subroutine access_get_complex_dp(self, path, val, default)
    class(hsd_access_t), intent(inout) :: self
    character(len=*), intent(in) :: path
    complex(dp), intent(out) :: val
    complex(dp), intent(in), optional :: default

    type(hsd_node_t), pointer :: vnode
    integer :: local_stat

    call self%resolve_value_node(path, vnode, local_stat)
    if (local_stat == HSD_STAT_OK) then
      call vnode%get_complex(val, local_stat)
      if (local_stat /= HSD_STAT_OK) then
        call self%push_error(path, HSD_STAT_TYPE_ERROR, &
            & "Cannot parse as complex")
        val = (0.0_dp, 0.0_dp)
      end if
      return
    end if

    if (present(default)) then
      val = default
      if (self%on_missing == HSD_ON_MISSING_SET) then
        call hsd_set(self%root, path, default)
        call self%mark_path_processed(path)
      end if
    else
      call self%push_error(path, HSD_STAT_NOT_FOUND, &
          & "Required complex field not found")
      val = (0.0_dp, 0.0_dp)
    end if
  end subroutine access_get_complex_dp

  ! ===== Array Getters =====

  !> Get integer array by path
  subroutine access_get_integer_array(self, path, val, default)
    class(hsd_access_t), intent(inout) :: self
    character(len=*), intent(in) :: path
    integer, allocatable, intent(out) :: val(:)
    integer, intent(in), optional :: default(:)

    type(hsd_node_t), pointer :: vnode
    integer :: local_stat

    call self%resolve_value_node(path, vnode, local_stat)
    if (local_stat == HSD_STAT_OK) then
      call vnode%get_int_array(val, local_stat)
      if (local_stat /= HSD_STAT_OK) then
        call self%push_error(path, HSD_STAT_TYPE_ERROR, &
            & "Cannot parse as integer array")
        allocate(val(0))
      end if
      return
    end if

    if (present(default)) then
      val = default
      if (self%on_missing == HSD_ON_MISSING_SET) then
        call hsd_set(self%root, path, default)
        call self%mark_path_processed(path)
      end if
    else
      call self%push_error(path, HSD_STAT_NOT_FOUND, &
          & "Required integer array field not found")
      allocate(val(0))
    end if
  end subroutine access_get_integer_array

  !> Get double precision real array by path
  subroutine access_get_real_dp_array(self, path, val, default)
    class(hsd_access_t), intent(inout) :: self
    character(len=*), intent(in) :: path
    real(dp), allocatable, intent(out) :: val(:)
    real(dp), intent(in), optional :: default(:)

    type(hsd_node_t), pointer :: vnode
    integer :: local_stat

    call self%resolve_value_node(path, vnode, local_stat)
    if (local_stat == HSD_STAT_OK) then
      call vnode%get_real_array(val, local_stat)
      if (local_stat /= HSD_STAT_OK) then
        call self%push_error(path, HSD_STAT_TYPE_ERROR, &
            & "Cannot parse as real array")
        allocate(val(0))
      end if
      return
    end if

    if (present(default)) then
      val = default
      if (self%on_missing == HSD_ON_MISSING_SET) then
        call hsd_set(self%root, path, default)
        call self%mark_path_processed(path)
      end if
    else
      call self%push_error(path, HSD_STAT_NOT_FOUND, &
          & "Required real array field not found")
      allocate(val(0))
    end if
  end subroutine access_get_real_dp_array

  !> Get logical array by path
  subroutine access_get_logical_array(self, path, val, default)
    class(hsd_access_t), intent(inout) :: self
    character(len=*), intent(in) :: path
    logical, allocatable, intent(out) :: val(:)
    logical, intent(in), optional :: default(:)

    type(hsd_node_t), pointer :: vnode
    integer :: local_stat

    call self%resolve_value_node(path, vnode, local_stat)
    if (local_stat == HSD_STAT_OK) then
      call vnode%get_logical_array(val, local_stat)
      if (local_stat /= HSD_STAT_OK) then
        call self%push_error(path, HSD_STAT_TYPE_ERROR, &
            & "Cannot parse as logical array")
        allocate(val(0))
      end if
      return
    end if

    if (present(default)) then
      val = default
      if (self%on_missing == HSD_ON_MISSING_SET) then
        call hsd_set(self%root, path, default)
        call self%mark_path_processed(path)
      end if
    else
      call self%push_error(path, HSD_STAT_NOT_FOUND, &
          & "Required logical array field not found")
      allocate(val(0))
    end if
  end subroutine access_get_logical_array

  !> Get string array by path
  subroutine access_get_string_array(self, path, val, default)
    class(hsd_access_t), intent(inout) :: self
    character(len=*), intent(in) :: path
    character(len=:), allocatable, intent(out) :: val(:)
    character(len=*), intent(in), optional :: default(:)

    type(hsd_node_t), pointer :: vnode
    integer :: local_stat

    call self%resolve_value_node(path, vnode, local_stat)
    if (local_stat == HSD_STAT_OK) then
      call vnode%get_string_array(val, local_stat)
      if (local_stat /= HSD_STAT_OK) then
        call self%push_error(path, HSD_STAT_TYPE_ERROR, &
            & "Cannot parse as string array")
        allocate(character(len=0) :: val(0))
      end if
      return
    end if

    if (present(default)) then
      val = default
      if (self%on_missing == HSD_ON_MISSING_SET) then
        call hsd_set(self%root, path, default)
        call self%mark_path_processed(path)
      end if
    else
      call self%push_error(path, HSD_STAT_NOT_FOUND, &
          & "Required string array field not found")
      allocate(character(len=0) :: val(0))
    end if
  end subroutine access_get_string_array

  !> Get complex double precision array by path
  subroutine access_get_complex_dp_array(self, path, val, default)
    class(hsd_access_t), intent(inout) :: self
    character(len=*), intent(in) :: path
    complex(dp), allocatable, intent(out) :: val(:)
    complex(dp), intent(in), optional :: default(:)

    type(hsd_node_t), pointer :: vnode
    integer :: local_stat

    call self%resolve_value_node(path, vnode, local_stat)
    if (local_stat == HSD_STAT_OK) then
      call vnode%get_complex_array(val, local_stat)
      if (local_stat /= HSD_STAT_OK) then
        call self%push_error(path, HSD_STAT_TYPE_ERROR, &
            & "Cannot parse as complex array")
        allocate(val(0))
      end if
      return
    end if

    if (present(default)) then
      val = default
      if (self%on_missing == HSD_ON_MISSING_SET) then
        call hsd_set(self%root, path, default)
        call self%mark_path_processed(path)
      end if
    else
      call self%push_error(path, HSD_STAT_NOT_FOUND, &
          & "Required complex array field not found")
      allocate(val(0))
    end if
  end subroutine access_get_complex_dp_array

  ! ===== Matrix Getters =====

  !> Get integer matrix by path
  subroutine access_get_int_matrix(self, path, val, nrows, ncols, order)
    class(hsd_access_t), intent(inout) :: self
    character(len=*), intent(in) :: path
    integer, allocatable, intent(out) :: val(:,:)
    integer, intent(out) :: nrows, ncols
    character(len=*), intent(in), optional :: order

    type(hsd_node_t), pointer :: child
    integer :: local_stat
    logical :: was_processed

    ! Save processed state to restore if mark_processed is off
    was_processed = .false.
    call hsd_get_child(self%root, path, child, local_stat)
    if (local_stat == HSD_STAT_OK .and. associated(child)) &
        & was_processed = child%processed

    call hsd_get_matrix(self%root, path, val, nrows, ncols, &
        & local_stat, order)

    if (local_stat /= HSD_STAT_OK) then
      call self%push_error(path, local_stat, &
          & "Cannot read integer matrix")
      return
    end if

    if (.not. self%mark_processed .and. associated(child)) &
        & child%processed = was_processed
  end subroutine access_get_int_matrix

  !> Get double precision real matrix by path
  subroutine access_get_real_matrix(self, path, val, nrows, ncols, order)
    class(hsd_access_t), intent(inout) :: self
    character(len=*), intent(in) :: path
    real(dp), allocatable, intent(out) :: val(:,:)
    integer, intent(out) :: nrows, ncols
    character(len=*), intent(in), optional :: order

    type(hsd_node_t), pointer :: child
    integer :: local_stat
    logical :: was_processed

    was_processed = .false.
    call hsd_get_child(self%root, path, child, local_stat)
    if (local_stat == HSD_STAT_OK .and. associated(child)) &
        & was_processed = child%processed

    call hsd_get_matrix(self%root, path, val, nrows, ncols, &
        & local_stat, order)

    if (local_stat /= HSD_STAT_OK) then
      call self%push_error(path, local_stat, &
          & "Cannot read real matrix")
      return
    end if

    if (.not. self%mark_processed .and. associated(child)) &
        & child%processed = was_processed
  end subroutine access_get_real_matrix

  !> Get complex double precision matrix by path
  subroutine access_get_complex_matrix(self, path, val, nrows, ncols, &
      & order)
    class(hsd_access_t), intent(inout) :: self
    character(len=*), intent(in) :: path
    complex(dp), allocatable, intent(out) :: val(:,:)
    integer, intent(out) :: nrows, ncols
    character(len=*), intent(in), optional :: order

    type(hsd_node_t), pointer :: child
    integer :: local_stat
    logical :: was_processed

    was_processed = .false.
    call hsd_get_child(self%root, path, child, local_stat)
    if (local_stat == HSD_STAT_OK .and. associated(child)) &
        & was_processed = child%processed

    call hsd_get_matrix(self%root, path, val, nrows, ncols, &
        & local_stat, order)

    if (local_stat /= HSD_STAT_OK) then
      call self%push_error(path, local_stat, &
          & "Cannot read complex matrix")
      return
    end if

    if (.not. self%mark_processed .and. associated(child)) &
        & child%processed = was_processed
  end subroutine access_get_complex_matrix

  ! ===== Scalar Setters =====

  !> Set string value by path
  subroutine access_set_string(self, path, val)
    class(hsd_access_t), intent(inout) :: self
    character(len=*), intent(in) :: path
    character(len=*), intent(in) :: val

    integer :: local_stat

    call hsd_set(self%root, path, val, local_stat)
    if (local_stat /= HSD_STAT_OK) &
        & call self%push_error(path, local_stat, &
        & "Failed to set string value")
  end subroutine access_set_string

  !> Set integer value by path
  subroutine access_set_integer(self, path, val)
    class(hsd_access_t), intent(inout) :: self
    character(len=*), intent(in) :: path
    integer, intent(in) :: val

    integer :: local_stat

    call hsd_set(self%root, path, val, local_stat)
    if (local_stat /= HSD_STAT_OK) &
        & call self%push_error(path, local_stat, &
        & "Failed to set integer value")
  end subroutine access_set_integer

  !> Set double precision real value by path
  subroutine access_set_real_dp(self, path, val)
    class(hsd_access_t), intent(inout) :: self
    character(len=*), intent(in) :: path
    real(dp), intent(in) :: val

    integer :: local_stat

    call hsd_set(self%root, path, val, local_stat)
    if (local_stat /= HSD_STAT_OK) &
        & call self%push_error(path, local_stat, &
        & "Failed to set real value")
  end subroutine access_set_real_dp

  !> Set logical value by path
  subroutine access_set_logical(self, path, val)
    class(hsd_access_t), intent(inout) :: self
    character(len=*), intent(in) :: path
    logical, intent(in) :: val

    integer :: local_stat

    call hsd_set(self%root, path, val, local_stat)
    if (local_stat /= HSD_STAT_OK) &
        & call self%push_error(path, local_stat, &
        & "Failed to set logical value")
  end subroutine access_set_logical

  !> Set complex double precision value by path
  subroutine access_set_complex_dp(self, path, val)
    class(hsd_access_t), intent(inout) :: self
    character(len=*), intent(in) :: path
    complex(dp), intent(in) :: val

    integer :: local_stat

    call hsd_set(self%root, path, val, local_stat)
    if (local_stat /= HSD_STAT_OK) &
        & call self%push_error(path, local_stat, &
        & "Failed to set complex value")
  end subroutine access_set_complex_dp

  ! ===== Array Setters =====

  !> Set integer array by path
  subroutine access_set_integer_array(self, path, val)
    class(hsd_access_t), intent(inout) :: self
    character(len=*), intent(in) :: path
    integer, intent(in) :: val(:)

    integer :: local_stat

    call hsd_set(self%root, path, val, local_stat)
    if (local_stat /= HSD_STAT_OK) &
        & call self%push_error(path, local_stat, &
        & "Failed to set integer array")
  end subroutine access_set_integer_array

  !> Set double precision real array by path
  subroutine access_set_real_dp_array(self, path, val)
    class(hsd_access_t), intent(inout) :: self
    character(len=*), intent(in) :: path
    real(dp), intent(in) :: val(:)

    integer :: local_stat

    call hsd_set(self%root, path, val, local_stat)
    if (local_stat /= HSD_STAT_OK) &
        & call self%push_error(path, local_stat, &
        & "Failed to set real array")
  end subroutine access_set_real_dp_array

  !> Set logical array by path
  subroutine access_set_logical_array(self, path, val)
    class(hsd_access_t), intent(inout) :: self
    character(len=*), intent(in) :: path
    logical, intent(in) :: val(:)

    integer :: local_stat

    call hsd_set(self%root, path, val, local_stat)
    if (local_stat /= HSD_STAT_OK) &
        & call self%push_error(path, local_stat, &
        & "Failed to set logical array")
  end subroutine access_set_logical_array

  !> Set string array by path
  subroutine access_set_string_array(self, path, val)
    class(hsd_access_t), intent(inout) :: self
    character(len=*), intent(in) :: path
    character(len=*), intent(in) :: val(:)

    integer :: local_stat

    call hsd_set(self%root, path, val, local_stat)
    if (local_stat /= HSD_STAT_OK) &
        & call self%push_error(path, local_stat, &
        & "Failed to set string array")
  end subroutine access_set_string_array

  !> Set complex double precision array by path
  subroutine access_set_complex_dp_array(self, path, val)
    class(hsd_access_t), intent(inout) :: self
    character(len=*), intent(in) :: path
    complex(dp), intent(in) :: val(:)

    integer :: local_stat

    call hsd_set(self%root, path, val, local_stat)
    if (local_stat /= HSD_STAT_OK) &
        & call self%push_error(path, local_stat, &
        & "Failed to set complex array")
  end subroutine access_set_complex_dp_array

  ! ===== Matrix Setters =====

  !> Set integer matrix by path
  subroutine access_set_integer_matrix(self, path, val)
    class(hsd_access_t), intent(inout) :: self
    character(len=*), intent(in) :: path
    integer, intent(in) :: val(:,:)

    integer :: local_stat

    call hsd_set(self%root, path, val, local_stat)
    if (local_stat /= HSD_STAT_OK) &
        & call self%push_error(path, local_stat, &
        & "Failed to set integer matrix")
  end subroutine access_set_integer_matrix

  !> Set double precision real matrix by path
  subroutine access_set_real_dp_matrix(self, path, val)
    class(hsd_access_t), intent(inout) :: self
    character(len=*), intent(in) :: path
    real(dp), intent(in) :: val(:,:)

    integer :: local_stat

    call hsd_set(self%root, path, val, local_stat)
    if (local_stat /= HSD_STAT_OK) &
        & call self%push_error(path, local_stat, &
        & "Failed to set real matrix")
  end subroutine access_set_real_dp_matrix

  !> Set complex double precision matrix by path
  subroutine access_set_complex_dp_matrix(self, path, val)
    class(hsd_access_t), intent(inout) :: self
    character(len=*), intent(in) :: path
    complex(dp), intent(in) :: val(:,:)

    integer :: local_stat

    call hsd_set(self%root, path, val, local_stat)
    if (local_stat /= HSD_STAT_OK) &
        & call self%push_error(path, local_stat, &
        & "Failed to set complex matrix")
  end subroutine access_set_complex_dp_matrix

  ! ===== Choice Getter =====

  !> Get a polymorphic choice (first table child of a block)
  !>
  !> Returns the lowercase name of the chosen child table and a pointer to it.
  !> Pushes an error if the path doesn't exist or contains no table child.
  subroutine access_get_choice(self, path, choice_name, choice_table)
    class(hsd_access_t), intent(inout) :: self
    character(len=*), intent(in) :: path
    character(len=:), allocatable, intent(out) :: choice_name
    type(hsd_node_t), pointer, intent(out) :: choice_table

    type(hsd_node_t), pointer :: table_node, child
    integer :: local_stat, ii

    nullify(choice_table)
    choice_name = ""

    call hsd_get_child(self%root, path, table_node, local_stat)
    if (local_stat /= HSD_STAT_OK .or. .not. associated(table_node)) then
      call self%push_error(path, HSD_STAT_NOT_FOUND, &
          & "Choice block not found")
      return
    end if

    if (self%mark_processed) table_node%processed = .true.

    if (table_node%node_type /= NODE_TYPE_TABLE) then
      call self%push_error(path, HSD_STAT_TYPE_ERROR, &
          & "Choice block is not a table")
      return
    end if

    do ii = 1, table_node%num_children
      call table_node%get_child(ii, child)
      if (.not. associated(child)) cycle
      if (child%node_type == NODE_TYPE_TABLE) then
        choice_table => child
        if (allocated(child%name)) then
          choice_name = to_lower(child%name)
        end if
        if (self%mark_processed) choice_table%processed = .true.
        return
      end if
    end do

    call self%push_error(path, HSD_STAT_NOT_FOUND, &
        & "No choice found in block")
  end subroutine access_get_choice

end module hsd_access
