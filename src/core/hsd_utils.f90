!> Utility functions and types for the HSD parser
module hsd_utils
  use hsd_constants, only: dp, sp
  implicit none (type, external)
  private

  public :: string_buffer_t
  public :: to_lower

  !> Initial buffer capacity
  integer, parameter :: BUFFER_INITIAL_CAPACITY = 256

  !> String buffer for efficient string building
  !>
  !> Avoids O(n²) string concatenation by pre-allocating buffer space
  !> and growing geometrically when needed.
  type :: string_buffer_t
    character(len=:), allocatable :: buffer
    integer :: length = 0
    integer :: capacity = 0
  contains
    procedure :: init => buffer_init
    procedure :: append_char => buffer_append_char
    procedure :: append_str => buffer_append_str
    procedure :: get_string => buffer_get_string
    procedure :: clear => buffer_clear
  end type string_buffer_t

contains

  !> Initialize the string buffer with given or default capacity
  subroutine buffer_init(self, initial_capacity)
    class(string_buffer_t), intent(inout) :: self
    integer, intent(in), optional :: initial_capacity

    if (present(initial_capacity)) then
      self%capacity = initial_capacity
    else
      self%capacity = BUFFER_INITIAL_CAPACITY
    end if

    if (allocated(self%buffer)) deallocate(self%buffer)
    allocate(character(len=self%capacity) :: self%buffer)
    self%length = 0

  end subroutine buffer_init

  !> Append a single character to the buffer
  subroutine buffer_append_char(self, ch)
    class(string_buffer_t), intent(inout) :: self
    character(len=1), intent(in) :: ch

    character(len=:), allocatable :: new_buffer
    integer :: new_capacity

    ! Initialize if needed
    if (self%capacity == 0) call self%init()

    ! Grow buffer if needed (double capacity)
    if (self%length >= self%capacity) then
      new_capacity = self%capacity * 2
      allocate(character(len=new_capacity) :: new_buffer)
      new_buffer(1:self%length) = self%buffer(1:self%length)
      call move_alloc(new_buffer, self%buffer)
      self%capacity = new_capacity
    end if

    self%length = self%length + 1
    self%buffer(self%length:self%length) = ch

  end subroutine buffer_append_char

  !> Append a string to the buffer
  subroutine buffer_append_str(self, str)
    class(string_buffer_t), intent(inout) :: self
    character(len=*), intent(in) :: str

    character(len=:), allocatable :: new_buffer
    integer :: new_capacity, str_len

    str_len = len(str)
    if (str_len == 0) return  ! LCOV_EXCL_LINE

    ! Initialize if needed
    if (self%capacity == 0) call self%init()

    ! Grow buffer if needed
    if (self%length + str_len > self%capacity) then
      new_capacity = max(self%capacity * 2, self%length + str_len)
      allocate(character(len=new_capacity) :: new_buffer)
      new_buffer(1:self%length) = self%buffer(1:self%length)
      call move_alloc(new_buffer, self%buffer)
      self%capacity = new_capacity
    end if

    self%buffer(self%length+1:self%length+str_len) = str
    self%length = self%length + str_len

  end subroutine buffer_append_str

  !> Get the accumulated string
  function buffer_get_string(self) result(str)
    class(string_buffer_t), intent(in) :: self
    character(len=:), allocatable :: str

    if (self%length > 0) then
      str = self%buffer(1:self%length)
    else
      str = ""
    end if

  end function buffer_get_string

  !> Clear the buffer for reuse (keeps capacity)
  subroutine buffer_clear(self)
    class(string_buffer_t), intent(inout) :: self
    self%length = 0
  end subroutine buffer_clear

  !> Convert a string to lowercase
  pure function to_lower(str) result(lower)
    character(len=*), intent(in) :: str
    character(len=len(str)) :: lower
    integer :: i, ic

    do i = 1, len(str)
      ic = ichar(str(i:i))
      if (ic >= 65 .and. ic <= 90) then
        lower(i:i) = char(ic + 32)
      else
        lower(i:i) = str(i:i)
      end if
    end do
  end function to_lower

end module hsd_utils
