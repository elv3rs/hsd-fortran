!> Value operations for HSD types
!>
!> This submodule implements all type-bound procedures for hsd_value,
!> plus private helper routines for parsing arrays, matrices, and
!> complex numbers. See hsd_types.f90 for type definitions and
!> interface declarations.
submodule (hsd_types) hsd_value_ops
  implicit none (type, external)

contains

  ! ===================================================================
  ! Value setters
  ! ===================================================================

  !> Set string value
  module procedure value_set_string
    self%value_type = VALUE_TYPE_STRING
    self%string_value = val
  end procedure value_set_string

  !> Set integer value
  module procedure value_set_integer
    self%value_type = VALUE_TYPE_INTEGER
    self%int_value = val
  end procedure value_set_integer

  !> Set real value
  module procedure value_set_real
    self%value_type = VALUE_TYPE_REAL
    self%real_value = val
  end procedure value_set_real

  !> Set logical value
  module procedure value_set_logical
    self%value_type = VALUE_TYPE_LOGICAL
    self%logical_value = val
  end procedure value_set_logical

  !> Set complex value
  module procedure value_set_complex
    self%value_type = VALUE_TYPE_COMPLEX
    self%complex_value = val
  end procedure value_set_complex

  !> Set raw text (for arrays/matrices)
  !>
  !> Detects whether the text contains multiple values (space/comma/
  !> newline separated) and sets VALUE_TYPE_ARRAY accordingly, so that
  !> hsd_is_array returns the correct result for programmatically set
  !> arrays.
  module procedure value_set_raw

    integer :: i, token_count
    logical :: in_token

    ! Quick count of whitespace/comma-separated tokens
    token_count = 0
    in_token = .false.
    do i = 1, len(text)
      if (is_separator(text(i:i))) then
        in_token = .false.
      else
        if (.not. in_token) then
          token_count = token_count + 1
          in_token = .true.
        end if
      end if
    end do

    if (token_count > 1) then
      self%value_type = VALUE_TYPE_ARRAY
    else
      self%value_type = VALUE_TYPE_STRING
    end if
    self%raw_text = text
    self%string_value = text

  end procedure value_set_raw

  ! ===================================================================
  ! Value getters
  ! ===================================================================

  !> Get string value
  !>
  !> Returns the string representation of any typed value.
  !> For natively-typed values (integer, real, logical, complex), the
  !> value is serialized to a string on the fly.
  module procedure value_get_string

    character(len=40) :: buf

    if (allocated(self%string_value)) then
      val = self%string_value
      if (present(stat)) stat = HSD_STAT_OK
    else if (allocated(self%raw_text)) then
      val = self%raw_text
      if (present(stat)) stat = HSD_STAT_OK
    else
      ! Serialize natively-typed values to string
      select case (self%value_type)
      case (VALUE_TYPE_INTEGER)
        write(buf, '(i0)') self%int_value
        val = trim(buf)
        if (present(stat)) stat = HSD_STAT_OK
      case (VALUE_TYPE_REAL)
        write(buf, '(es23.15)') self%real_value
        val = trim(adjustl(buf))
        if (present(stat)) stat = HSD_STAT_OK
      case (VALUE_TYPE_LOGICAL)
        if (self%logical_value) then
          val = "Yes"
        else
          val = "No"
        end if
        if (present(stat)) stat = HSD_STAT_OK
      case (VALUE_TYPE_COMPLEX)
        write(buf, '(es23.15,sp,es23.15,"i")') &
            & real(self%complex_value), aimag(self%complex_value)
        val = trim(adjustl(buf))
        if (present(stat)) stat = HSD_STAT_OK
      case default
        val = ""
        if (present(stat)) stat = HSD_STAT_NOT_FOUND
      end select
    end if

  end procedure value_get_string

  !> Get integer value
  module procedure value_get_integer

    integer :: io_stat

    if (self%value_type == VALUE_TYPE_INTEGER) then
      val = self%int_value
      if (present(stat)) stat = HSD_STAT_OK
    else if (allocated(self%string_value)) then
      read(self%string_value, *, iostat=io_stat) val
      if (io_stat /= 0) then
        if (present(stat)) stat = HSD_STAT_TYPE_ERROR
      else
        if (present(stat)) stat = HSD_STAT_OK
      end if
    else
      val = 0
      if (present(stat)) stat = HSD_STAT_NOT_FOUND
    end if

  end procedure value_get_integer

  !> Get real value
  module procedure value_get_real

    integer :: io_stat

    if (self%value_type == VALUE_TYPE_REAL) then
      val = self%real_value
      if (present(stat)) stat = HSD_STAT_OK
    else if (self%value_type == VALUE_TYPE_INTEGER) then
      val = real(self%int_value, dp)
      if (present(stat)) stat = HSD_STAT_OK
    else if (allocated(self%string_value)) then
      read(self%string_value, *, iostat=io_stat) val
      if (io_stat /= 0) then
        if (present(stat)) stat = HSD_STAT_TYPE_ERROR
      else
        if (present(stat)) stat = HSD_STAT_OK
      end if
    else
      val = 0.0_dp
      if (present(stat)) stat = HSD_STAT_NOT_FOUND
    end if

  end procedure value_get_real

  !> Get logical value
  module procedure value_get_logical

    character(len=:), allocatable :: lower_val

    if (self%value_type == VALUE_TYPE_LOGICAL) then
      val = self%logical_value
      if (present(stat)) stat = HSD_STAT_OK
    else if (allocated(self%string_value)) then
      lower_val = to_lower(trim(self%string_value))
      select case (lower_val)
      case ("yes", "on", "1", "true", ".true.")
        val = .true.
        if (present(stat)) stat = HSD_STAT_OK
      case ("no", "off", "0", "false", ".false.")
        val = .false.
        if (present(stat)) stat = HSD_STAT_OK
      case default
        val = .false.
        if (present(stat)) stat = HSD_STAT_TYPE_ERROR
      end select
    else
      val = .false.
      if (present(stat)) stat = HSD_STAT_NOT_FOUND
    end if

  end procedure value_get_logical

  !> Get complex value
  !> Parses formats: 4.0+9.0i, 2.0-3.0i, (1.0,2.0), 5.0+2.0j
  module procedure value_get_complex

    if (self%value_type == VALUE_TYPE_COMPLEX) then
      val = self%complex_value
      if (present(stat)) stat = HSD_STAT_OK
    else if (allocated(self%string_value)) then
      call parse_complex(trim(self%string_value), val, stat)
    else
      val = (0.0_dp, 0.0_dp)
      if (present(stat)) stat = HSD_STAT_NOT_FOUND
    end if

  end procedure value_get_complex

  ! ===================================================================
  ! Array getters (with caching)
  ! ===================================================================

  !> Get integer array from raw text
  !> Caches the parsed result for subsequent calls
  module procedure value_get_int_array

    character(len=:), allocatable :: text
    integer :: io_stat

    ! If already parsed, return cached array
    if (allocated(self%int_array)) then
      val = self%int_array
      if (present(stat)) stat = HSD_STAT_OK
      return
    end if

    ! Get source text
    if (allocated(self%raw_text)) then
      text = self%raw_text
    else if (allocated(self%string_value)) then
      text = self%string_value
    else
      allocate(val(0))
      if (present(stat)) stat = HSD_STAT_NOT_FOUND
      return
    end if

    ! Count and parse values
    call parse_int_array(text, val, io_stat)
    if (present(stat)) stat = io_stat

    ! Cache result for next access
    if (io_stat == 0) then
      self%int_array = val
    end if

  end procedure value_get_int_array

  !> Get real array from raw text
  !> Caches the parsed result for subsequent calls
  module procedure value_get_real_array

    character(len=:), allocatable :: text
    integer :: io_stat

    ! If already parsed, return cached array
    if (allocated(self%real_array)) then
      val = self%real_array
      if (present(stat)) stat = HSD_STAT_OK
      return
    end if

    ! Get source text
    if (allocated(self%raw_text)) then
      text = self%raw_text
    else if (allocated(self%string_value)) then
      text = self%string_value
    else
      allocate(val(0))
      if (present(stat)) stat = HSD_STAT_NOT_FOUND
      return
    end if

    ! Count and parse values
    call parse_real_array(text, val, io_stat)
    if (present(stat)) stat = io_stat

    ! Cache result for next access
    if (io_stat == 0) then
      self%real_array = val
    end if

  end procedure value_get_real_array

  !> Get logical array from raw text
  !> Caches the parsed result for subsequent calls
  module procedure value_get_logical_array

    character(len=:), allocatable :: text, tokens(:)
    integer :: i, n
    logical :: parse_ok

    if (allocated(self%logical_array)) then
      val = self%logical_array
      if (present(stat)) stat = HSD_STAT_OK
      return
    end if

    if (allocated(self%raw_text)) then
      text = self%raw_text
    else if (allocated(self%string_value)) then
      text = self%string_value
    else
      allocate(val(0))
      if (present(stat)) stat = HSD_STAT_NOT_FOUND
      return
    end if

    call tokenize_string(text, tokens)
    n = size(tokens)
    allocate(val(n))
    parse_ok = .true.

    do i = 1, n
      select case (to_lower(trim(tokens(i))))
      case ("yes", "on", "1", "true", ".true.")
        val(i) = .true.
      case ("no", "off", "0", "false", ".false.")
        val(i) = .false.
      case default
        val(i) = .false.
        parse_ok = .false.
        if (present(stat)) stat = HSD_STAT_TYPE_ERROR
        return
      end select
    end do

    if (present(stat)) stat = HSD_STAT_OK

    ! Cache result for next access
    if (parse_ok) then
      self%logical_array = val
    end if

  end procedure value_get_logical_array

  !> Get complex array from raw text
  !> Caches the parsed result for subsequent calls
  module procedure value_get_complex_array

    character(len=:), allocatable :: text
    integer :: io_stat

    ! If already parsed, return cached array
    if (allocated(self%complex_array)) then
      val = self%complex_array
      if (present(stat)) stat = HSD_STAT_OK
      return
    end if

    ! Get source text
    if (allocated(self%raw_text)) then
      text = self%raw_text
    else if (allocated(self%string_value)) then
      text = self%string_value
    else
      allocate(val(0))
      if (present(stat)) stat = HSD_STAT_NOT_FOUND
      return
    end if

    ! Count and parse values
    call parse_complex_array(text, val, io_stat)
    if (present(stat)) stat = io_stat

    ! Cache result for next access
    if (io_stat == 0) then
      self%complex_array = val
    end if

  end procedure value_get_complex_array

  !> Get string array from raw text (quoted strings preserved)
  !> Caches the parsed result for subsequent calls
  module procedure value_get_string_array

    character(len=:), allocatable :: text

    if (allocated(self%string_array)) then
      val = self%string_array
      if (present(stat)) stat = HSD_STAT_OK
      return
    end if

    if (allocated(self%raw_text)) then
      text = self%raw_text
    else if (allocated(self%string_value)) then
      text = self%string_value
    else
      allocate(character(len=1) :: val(0))
      if (present(stat)) stat = HSD_STAT_NOT_FOUND
      return
    end if

    call tokenize_quoted_string(text, val)
    if (present(stat)) stat = HSD_STAT_OK

    ! Cache result for next access
    self%string_array = val

  end procedure value_get_string_array

  ! ===================================================================
  ! Matrix getters (with caching)
  ! ===================================================================

  !> Get 2D integer matrix from raw text
  !> Rows separated by newlines or semicolons.
  !> Caches the parsed result for subsequent calls.
  module procedure value_get_int_matrix

    character(len=:), allocatable :: text
    integer :: io_stat

    if (allocated(self%int_matrix)) then
      val = self%int_matrix
      nrows = self%nrows
      ncols = self%ncols
      if (present(stat)) stat = HSD_STAT_OK
      return
    end if

    if (allocated(self%raw_text)) then
      text = self%raw_text
    else if (allocated(self%string_value)) then
      text = self%string_value
    else
      allocate(val(0,0))
      nrows = 0
      ncols = 0
      if (present(stat)) stat = HSD_STAT_NOT_FOUND
      return
    end if

    call parse_int_matrix(text, val, nrows, ncols, io_stat)
    if (present(stat)) stat = io_stat

    ! Cache result for next access
    if (io_stat == 0) then
      self%int_matrix = val
      self%nrows = nrows
      self%ncols = ncols
    end if

  end procedure value_get_int_matrix

  !> Get 2D real matrix from raw text
  !> Caches the parsed result for subsequent calls.
  module procedure value_get_real_matrix

    character(len=:), allocatable :: text
    integer :: io_stat

    if (allocated(self%real_matrix)) then
      val = self%real_matrix
      nrows = self%nrows
      ncols = self%ncols
      if (present(stat)) stat = HSD_STAT_OK
      return
    end if

    if (allocated(self%raw_text)) then
      text = self%raw_text
    else if (allocated(self%string_value)) then
      text = self%string_value
    else
      allocate(val(0,0))
      nrows = 0
      ncols = 0
      if (present(stat)) stat = HSD_STAT_NOT_FOUND
      return
    end if

    call parse_real_matrix(text, val, nrows, ncols, io_stat)
    if (present(stat)) stat = io_stat

    ! Cache result for next access
    if (io_stat == 0) then
      self%real_matrix = val
      self%nrows = nrows
      self%ncols = ncols
    end if

  end procedure value_get_real_matrix

  ! ===================================================================
  ! Destructor
  ! ===================================================================

  !> Destroy value
  module procedure value_destroy

    if (allocated(self%name)) deallocate(self%name)
    if (allocated(self%attrib)) deallocate(self%attrib)
    if (allocated(self%string_value)) deallocate(self%string_value)
    if (allocated(self%raw_text)) deallocate(self%raw_text)
    if (allocated(self%int_array)) deallocate(self%int_array)
    if (allocated(self%real_array)) deallocate(self%real_array)
    if (allocated(self%logical_array)) deallocate(self%logical_array)
    if (allocated(self%string_array)) deallocate(self%string_array)
    if (allocated(self%complex_array)) deallocate(self%complex_array)
    if (allocated(self%int_matrix)) deallocate(self%int_matrix)
    if (allocated(self%real_matrix)) deallocate(self%real_matrix)

    self%value_type = VALUE_TYPE_NONE
    self%nrows = 0
    self%ncols = 0

  end procedure value_destroy

  ! ===================================================================
  ! Private helper routines
  ! ===================================================================

  !> Check if character is a separator (whitespace, comma, semicolon)
  pure function is_separator(ch) result(is_sep)
    character(len=1), intent(in) :: ch
    logical :: is_sep
    is_sep = (ch == ' ' .or. ch == char(9) .or. ch == char(10) &
        & .or. ch == char(13) .or. ch == ',' .or. ch == ';')
  end function is_separator

  !> Parse space/comma-separated integers from text
  subroutine parse_int_array(text, arr, stat)
    character(len=*), intent(in) :: text
    integer, allocatable, intent(out) :: arr(:)
    integer, intent(out) :: stat

    character(len=:), allocatable :: tokens(:)
    integer :: i, n, val, io_stat

    call tokenize_string(text, tokens)
    n = size(tokens)

    allocate(arr(n))
    do i = 1, n
      read(tokens(i), *, iostat=io_stat) val
      if (io_stat /= 0) then
        deallocate(arr)
        allocate(arr(0))
        stat = io_stat
        return
      end if
      arr(i) = val
    end do

    stat = 0

  end subroutine parse_int_array

  !> Parse space/comma-separated reals from text
  subroutine parse_real_array(text, arr, stat)
    character(len=*), intent(in) :: text
    real(dp), allocatable, intent(out) :: arr(:)
    integer, intent(out) :: stat

    character(len=:), allocatable :: tokens(:)
    integer :: i, n, io_stat
    real(dp) :: val

    call tokenize_string(text, tokens)
    n = size(tokens)

    allocate(arr(n))
    do i = 1, n
      read(tokens(i), *, iostat=io_stat) val
      if (io_stat /= 0) then
        deallocate(arr)
        allocate(arr(0))
        stat = io_stat
        return
      end if
      arr(i) = val
    end do

    stat = 0

  end subroutine parse_real_array

  !> Tokenize string by whitespace and commas
  !>
  !> Uses a two-pass approach: first counts tokens and measures max
  !> length, then allocates and fills.
  subroutine tokenize_string(text, tokens)
    character(len=*), intent(in) :: text
    character(len=:), allocatable, intent(out) :: tokens(:)

    integer :: i, start, max_len, token_count
    logical :: in_token

    ! First pass: count tokens and find max length
    token_count = 0
    max_len = 0
    in_token = .false.
    start = 1

    do i = 1, len(text)
      if (is_separator(text(i:i))) then
        if (in_token) then
          token_count = token_count + 1
          max_len = max(max_len, i - start)
          in_token = .false.
        end if
      else
        if (.not. in_token) then
          start = i
          in_token = .true.
        end if
      end if
    end do

    ! Handle last token
    if (in_token) then
      token_count = token_count + 1
      max_len = max(max_len, len(text) - start + 1)
    end if

    if (token_count == 0 .or. max_len == 0) then
      allocate(character(len=1) :: tokens(0))
      return
    end if

    ! Allocate result array
    allocate(character(len=max_len) :: tokens(token_count))

    ! Second pass: extract tokens
    token_count = 0
    in_token = .false.

    do i = 1, len(text)
      if (is_separator(text(i:i))) then
        if (in_token) then
          token_count = token_count + 1
          tokens(token_count) = text(start:i-1)
          in_token = .false.
        end if
      else
        if (.not. in_token) then
          start = i
          in_token = .true.
        end if
      end if
    end do

    ! Handle last token
    if (in_token) then
      token_count = token_count + 1
      tokens(token_count) = text(start:len(text))
    end if

  end subroutine tokenize_string

  !> Tokenize string preserving quoted sections
  !>
  !> Uses a two-pass approach to avoid O(n²) stack allocation.
  subroutine tokenize_quoted_string(text, tokens)
    character(len=*), intent(in) :: text
    character(len=:), allocatable, intent(out) :: tokens(:)

    integer :: i, start, max_len, token_count, tlen
    character(len=1) :: quote_char
    logical :: in_token, in_quote

    tlen = len_trim(text)

    ! First pass: count tokens and find max length
    token_count = 0
    max_len = 0
    in_token = .false.
    in_quote = .false.
    quote_char = ' '
    start = 1

    i = 1
    do while (i <= tlen)
      if (in_quote) then
        if (text(i:i) == quote_char) then
          token_count = token_count + 1
          max_len = max(max_len, i - start - 1)
          in_quote = .false.
          in_token = .false.
        end if
      else if (text(i:i) == '"' .or. text(i:i) == "'") then
        quote_char = text(i:i)
        in_quote = .true.
        start = i
        in_token = .true.
      else if (is_separator(text(i:i))) then
        if (in_token) then
          token_count = token_count + 1
          max_len = max(max_len, i - start)
          in_token = .false.
        end if
      else
        if (.not. in_token) then
          start = i
          in_token = .true.
        end if
      end if
      i = i + 1
    end do

    if (in_token .and. .not. in_quote) then
      token_count = token_count + 1
      max_len = max(max_len, tlen - start + 1)
    end if

    if (token_count == 0 .or. max_len == 0) then
      allocate(character(len=1) :: tokens(0))
      return
    end if

    ! Allocate result array
    allocate(character(len=max_len) :: tokens(token_count))

    ! Second pass: extract tokens
    token_count = 0
    in_token = .false.
    in_quote = .false.
    quote_char = ' '

    i = 1
    do while (i <= tlen)
      if (in_quote) then
        if (text(i:i) == quote_char) then
          token_count = token_count + 1
          if (i > start + 1) then
            tokens(token_count) = text(start+1:i-1)
          else
            tokens(token_count) = ""
          end if
          in_quote = .false.
          in_token = .false.
        end if
      else if (text(i:i) == '"' .or. text(i:i) == "'") then
        quote_char = text(i:i)
        in_quote = .true.
        start = i
        in_token = .true.
      else if (is_separator(text(i:i))) then
        if (in_token) then
          token_count = token_count + 1
          tokens(token_count) = text(start:i-1)
          in_token = .false.
        end if
      else
        if (.not. in_token) then
          start = i
          in_token = .true.
        end if
      end if
      i = i + 1
    end do

    if (in_token .and. .not. in_quote) then
      token_count = token_count + 1
      tokens(token_count) = text(start:tlen)
    end if

  end subroutine tokenize_quoted_string

  !> Parse 2D integer matrix (rows by newlines or semicolons)
  subroutine parse_int_matrix(text, mat, nrows, ncols, stat)
    character(len=*), intent(in) :: text
    integer, allocatable, intent(out) :: mat(:,:)
    integer, intent(out) :: nrows, ncols, stat

    character(len=:), allocatable :: rows(:)
    integer, allocatable :: row_vals(:)
    integer :: i, j

    call count_matrix_dims(text, rows, nrows, ncols, stat)

    if (nrows == 0 .or. ncols == 0) then
      allocate(mat(0,0))
      nrows = 0
      ncols = 0
      stat = 0
      return
    end if

    if (stat /= 0) then
      ! Ragged matrix: return error but preserve dimension info
      allocate(mat(0,0))
      return
    end if

    allocate(mat(nrows, ncols))
    mat = 0

    j = 0
    do i = 1, size(rows)
      if (len_trim(rows(i)) > 0) then
        call parse_int_array(rows(i), row_vals, stat)
        if (stat /= 0) then
          deallocate(mat)
          allocate(mat(0,0))
          nrows = 0
          ncols = 0
          return
        end if
        if (size(row_vals) > 0) then
          j = j + 1
          mat(j, 1:size(row_vals)) = row_vals
        end if
      end if
    end do

    stat = 0

  end subroutine parse_int_matrix

  !> Parse 2D real matrix
  subroutine parse_real_matrix(text, mat, nrows, ncols, stat)
    character(len=*), intent(in) :: text
    real(dp), allocatable, intent(out) :: mat(:,:)
    integer, intent(out) :: nrows, ncols, stat

    character(len=:), allocatable :: rows(:)
    real(dp), allocatable :: row_vals(:)
    integer :: i, j

    call count_matrix_dims(text, rows, nrows, ncols, stat)

    if (nrows == 0 .or. ncols == 0) then
      allocate(mat(0,0))
      nrows = 0
      ncols = 0
      stat = 0
      return
    end if

    if (stat /= 0) then
      ! Ragged matrix: return error but preserve dimension info
      allocate(mat(0,0))
      return
    end if

    if (stat /= 0 .or. nrows == 0 .or. ncols == 0) then
      allocate(mat(0,0))
      nrows = 0
      ncols = 0
      return
    end if

    allocate(mat(nrows, ncols))
    mat = 0.0_dp

    j = 0
    do i = 1, size(rows)
      if (len_trim(rows(i)) > 0) then
        call parse_real_array(rows(i), row_vals, stat)
        if (stat /= 0) then
          deallocate(mat)
          allocate(mat(0,0))
          nrows = 0
          ncols = 0
          return
        end if
        if (size(row_vals) > 0) then
          j = j + 1
          mat(j, 1:size(row_vals)) = row_vals
        end if
      end if
    end do

    stat = 0

  end subroutine parse_real_matrix

  !> Count matrix dimensions from text (rows by newlines/semicolons)
  !>
  !> Shared helper for parse_int_matrix and parse_real_matrix.
  !> Splits text into rows, counts non-empty rows, and validates
  !> that all rows have the same number of columns.
  !>
  !> @param[in]  text   Raw text to parse
  !> @param[out] rows   Rows split by newlines (for caller to parse)
  !> @param[out] nrows  Number of non-empty rows
  !> @param[out] ncols  Number of columns (from first row)
  !> @param[out] stat   0 on success, HSD_STAT_TYPE_ERROR on ragged
  subroutine count_matrix_dims(text, rows, nrows, ncols, stat)
    character(len=*), intent(in) :: text
    character(len=:), allocatable, intent(out) :: rows(:)
    integer, intent(out) :: nrows, ncols, stat

    character(len=:), allocatable :: tokens(:)
    integer :: i, row_count, col_count, first_cols

    call split_by_newlines(text, rows)
    row_count = size(rows)

    nrows = 0
    ncols = 0
    first_cols = -1
    stat = 0

    do i = 1, row_count
      if (len_trim(rows(i)) > 0) then
        call tokenize_string(rows(i), tokens)
        col_count = size(tokens)
        if (col_count > 0) then
          nrows = nrows + 1
          if (first_cols < 0) then
            first_cols = col_count
            ncols = col_count
          else if (col_count /= first_cols) then
            ! Ragged matrix: flag error but preserve dimension info
            ncols = max(ncols, col_count)
            stat = HSD_STAT_TYPE_ERROR
          end if
        end if
      end if
    end do

  end subroutine count_matrix_dims

  !> Split text by newlines
  !>
  !> Uses a two-pass approach to avoid O(n²) stack allocation.
  subroutine split_by_newlines(text, lines)
    character(len=*), intent(in) :: text
    character(len=:), allocatable, intent(out) :: lines(:)

    integer :: i, start, line_count, max_len, tlen

    tlen = len(text)

    ! First pass: count lines and find max length
    line_count = 0
    max_len = 0
    start = 1

    do i = 1, tlen
      if (text(i:i) == char(10) .or. text(i:i) == ';') then
        line_count = line_count + 1
        max_len = max(max_len, i - start)
        start = i + 1
      end if
    end do

    ! Handle last line
    if (start <= tlen) then
      line_count = line_count + 1
      max_len = max(max_len, tlen - start + 1)
    end if

    if (line_count == 0 .or. max_len == 0) then
      allocate(character(len=max(1, len(text))) :: lines(1))
      lines(1) = text
      return
    end if

    ! Allocate result array
    allocate(character(len=max_len) :: lines(line_count))

    ! Second pass: extract lines
    line_count = 0
    start = 1

    do i = 1, tlen
      if (text(i:i) == char(10) .or. text(i:i) == ';') then
        line_count = line_count + 1
        if (i > start) then
          lines(line_count) = text(start:i-1)
        else
          lines(line_count) = ""
        end if
        start = i + 1
      end if
    end do

    ! Handle last line
    if (start <= tlen) then
      line_count = line_count + 1
      lines(line_count) = text(start:tlen)
    end if

  end subroutine split_by_newlines

  !> Parse a single complex number from string
  !> Supports: 4.0+9.0i, 2.0-3.0i, (1.0,2.0), 5.0+2.0j, 3.5,
  !> pure imaginary 2.0i
  subroutine parse_complex(str, val, stat)
    character(len=*), intent(in) :: str
    complex(dp), intent(out) :: val
    integer, intent(out), optional :: stat

    character(len=:), allocatable :: work
    integer :: i, sign_pos, io_stat
    real(dp) :: re, im
    character(len=1) :: ch

    work = adjustl(trim(str))

    ! Handle empty string
    if (len_trim(work) == 0) then
      val = (0.0_dp, 0.0_dp)
      if (present(stat)) stat = HSD_STAT_TYPE_ERROR
      return
    end if

    ! Handle Fortran-style (re,im) format
    if (work(1:1) == '(') then
      i = index(work, ')')
      if (i > 2) then
        work = work(2:i-1)
        i = index(work, ',')
        if (i > 0) then
          read(work(1:i-1), *, iostat=io_stat) re
          if (io_stat /= 0) then
            val = (0.0_dp, 0.0_dp)
            if (present(stat)) stat = io_stat
            return
          end if
          read(work(i+1:), *, iostat=io_stat) im
          if (io_stat /= 0) then
            val = (0.0_dp, 0.0_dp)
            if (present(stat)) stat = io_stat
            return
          end if
          val = cmplx(re, im, dp)
          if (present(stat)) stat = HSD_STAT_OK
          return
        end if
      end if
    end if

    ! Handle a+bi or a-bi format (also handles j instead of i)
    ! Find the + or - that separates real and imaginary parts
    ! (must skip the first char and any exponent signs)
    sign_pos = 0
    do i = 2, len_trim(work)
      ch = work(i:i)
      if ((ch == '+' .or. ch == '-')) then
        ! Make sure this isn't part of an exponent
        if (i > 1) then
          if (work(i-1:i-1) /= 'e' &
              & .and. work(i-1:i-1) /= 'E' &
              & .and. work(i-1:i-1) /= 'd' &
              & .and. work(i-1:i-1) /= 'D') then
            sign_pos = i
          end if
        end if
      end if
    end do

    ! Check if last character is 'i' or 'j' (imaginary marker)
    ch = work(len_trim(work):len_trim(work))
    if (ch == 'i' .or. ch == 'I' &
        & .or. ch == 'j' .or. ch == 'J') then
      if (sign_pos > 0) then
        ! Format: a+bi or a-bi
        read(work(1:sign_pos-1), *, iostat=io_stat) re
        if (io_stat /= 0) then
          val = (0.0_dp, 0.0_dp)
          if (present(stat)) stat = io_stat
          return
        end if
        read(work(sign_pos:len_trim(work)-1), *, &
            & iostat=io_stat) im
        if (io_stat /= 0) then
          val = (0.0_dp, 0.0_dp)
          if (present(stat)) stat = io_stat
          return
        end if
        val = cmplx(re, im, dp)
        if (present(stat)) stat = HSD_STAT_OK
        return
      else
        ! Pure imaginary: bi
        read(work(1:len_trim(work)-1), *, iostat=io_stat) im
        if (io_stat /= 0) then
          val = (0.0_dp, 0.0_dp)
          if (present(stat)) stat = io_stat
          return
        end if
        val = cmplx(0.0_dp, im, dp)
        if (present(stat)) stat = HSD_STAT_OK
        return
      end if
    else
      ! Pure real number
      read(work, *, iostat=io_stat) re
      if (io_stat /= 0) then
        val = (0.0_dp, 0.0_dp)
        if (present(stat)) stat = io_stat
        return
      end if
      val = cmplx(re, 0.0_dp, dp)
      if (present(stat)) stat = HSD_STAT_OK
    end if

  end subroutine parse_complex

  !> Parse an array of complex numbers from text
  subroutine parse_complex_array(text, arr, stat)
    character(len=*), intent(in) :: text
    complex(dp), allocatable, intent(out) :: arr(:)
    integer, intent(out) :: stat

    character(len=:), allocatable :: tokens(:)
    integer :: i, n, io_stat
    complex(dp) :: val

    call tokenize_string(text, tokens)
    n = size(tokens)

    allocate(arr(n))
    do i = 1, n
      call parse_complex(tokens(i), val, io_stat)
      if (io_stat /= 0) then
        deallocate(arr)
        allocate(arr(0))
        stat = io_stat
        return
      end if
      arr(i) = val
    end do

    stat = 0

  end subroutine parse_complex_array

end submodule hsd_value_ops
