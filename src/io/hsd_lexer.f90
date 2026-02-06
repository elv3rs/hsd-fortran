!> Lexer (tokenizer) for HSD files
!>
!> This module provides the lexer that converts a character stream into
!> a sequence of tokens for the HSD parser.
module hsd_lexer
  use hsd_constants, only: hsd_max_line_length, &
      & CHAR_TAB, CHAR_BACKSLASH, CHAR_SPACE, CHAR_NEWLINE, CHAR_LBRACE, &
      & CHAR_RBRACE, CHAR_LBRACKET, CHAR_RBRACKET, CHAR_EQUAL, CHAR_SEMICOLON, &
      & CHAR_HASH, CHAR_DQUOTE, CHAR_SQUOTE, CHAR_LESS
  use hsd_utils, only: to_lower, string_buffer_t
  use hsd_token, only: hsd_token_t, TOKEN_EOF, TOKEN_STRING, &
    TOKEN_LBRACE, TOKEN_RBRACE, TOKEN_EQUAL, TOKEN_LBRACKET, TOKEN_RBRACKET, &
    TOKEN_INCLUDE_TXT, TOKEN_INCLUDE_HSD, TOKEN_SEMICOLON, TOKEN_COMMENT, &
    TOKEN_NEWLINE, TOKEN_TEXT
  use hsd_error, only: hsd_error_t, make_error, &
    HSD_STAT_OK, HSD_STAT_IO_ERROR, HSD_STAT_UNCLOSED_QUOTE, HSD_STAT_UNCLOSED_ATTRIB, &
    HSD_STAT_FILE_NOT_FOUND
  implicit none (type, external)
  private

  public :: hsd_lexer_t, new_lexer_from_file, new_lexer_from_string

  !> Lexer state
  type :: hsd_lexer_t
    !> Source filename (for error reporting)
    character(len=:), allocatable :: filename
    !> Source content
    character(len=:), allocatable :: source
    !> Current position in source
    integer :: pos = 1
    !> Current line number (1-based)
    integer :: line = 1
    !> Current column number (1-based)
    integer :: column = 1
    !> Length of source
    integer :: source_len = 0
    !> Whether we're inside an attribute context
    logical :: in_attrib = .false.
    !> Whether we're inside a quoted string
    logical :: in_quote = .false.
    !> Quote character being used
    character(len=1) :: quote_char = ''
  contains
    procedure :: next_token => lexer_next_token
    procedure :: peek_char => lexer_peek_char
    procedure :: advance => lexer_advance
    procedure :: skip_whitespace => lexer_skip_whitespace
    procedure :: read_string => lexer_read_string
    procedure :: read_text => lexer_read_text
    procedure :: read_comment => lexer_read_comment
    procedure :: is_eof => lexer_is_eof
  end type hsd_lexer_t

contains

  !> Create a new lexer from a file
  subroutine new_lexer_from_file(lexer, filename, error)
    type(hsd_lexer_t), intent(out) :: lexer
    character(len=*), intent(in) :: filename
    type(hsd_error_t), allocatable, intent(out), optional :: error

    integer :: unit_num, io_stat
    integer :: file_size
    character(len=256) :: io_msg
    logical :: file_exists

    ! Check if file exists
    inquire(file=filename, exist=file_exists)
    if (.not. file_exists) then
      if (present(error)) then
        call make_error(error, HSD_STAT_FILE_NOT_FOUND, &
          "File not found: " // trim(filename), filename)
      end if
      return
    end if

    ! Get file size
    inquire(file=filename, size=file_size)

    ! Open and read file
    open(newunit=unit_num, file=filename, status='old', action='read', &
         access='stream', form='unformatted', iostat=io_stat, iomsg=io_msg)
    if (io_stat /= 0) then
      if (present(error)) then  ! LCOV_EXCL_LINE
        call make_error(error, HSD_STAT_IO_ERROR, trim(io_msg), filename)  ! LCOV_EXCL_LINE
      end if  ! LCOV_EXCL_LINE
      return  ! LCOV_EXCL_LINE
    end if

    ! Allocate and read content
    allocate(character(len=file_size) :: lexer%source)
    read(unit_num, iostat=io_stat) lexer%source
    close(unit_num)

    if (io_stat /= 0 .and. io_stat /= -1) then  ! -1 is EOF, which is okay
      if (present(error)) then  ! LCOV_EXCL_LINE
        call make_error(error, HSD_STAT_IO_ERROR, "Error reading file", filename)  ! LCOV_EXCL_LINE
      end if  ! LCOV_EXCL_LINE
      return  ! LCOV_EXCL_LINE
    end if

    lexer%filename = filename
    lexer%source_len = len(lexer%source)
    lexer%pos = 1
    lexer%line = 1
    lexer%column = 1

  end subroutine new_lexer_from_file

  !> Create a new lexer from a string
  subroutine new_lexer_from_string(lexer, source, filename)
    type(hsd_lexer_t), intent(out) :: lexer
    character(len=*), intent(in) :: source
    character(len=*), intent(in), optional :: filename

    lexer%source = source
    lexer%source_len = len(source)
    lexer%pos = 1
    lexer%line = 1
    lexer%column = 1

    if (present(filename)) then
      lexer%filename = filename
    else
      lexer%filename = "<string>"
    end if

  end subroutine new_lexer_from_string

  !> Check if lexer is at end of file
  pure function lexer_is_eof(self) result(is_eof)
    class(hsd_lexer_t), intent(in) :: self
    logical :: is_eof
    is_eof = self%pos > self%source_len
  end function lexer_is_eof

  !> Peek at current character without advancing
  pure function lexer_peek_char(self, offset) result(ch)
    class(hsd_lexer_t), intent(in) :: self
    integer, intent(in), optional :: offset
    character(len=1) :: ch
    integer :: peek_pos

    if (present(offset)) then
      peek_pos = self%pos + offset
    else
      peek_pos = self%pos
    end if

    if (peek_pos > 0 .and. peek_pos <= self%source_len) then
      ch = self%source(peek_pos:peek_pos)
    else
      ch = char(0)  ! NUL for EOF
    end if

  end function lexer_peek_char

  !> Advance position by n characters
  subroutine lexer_advance(self, n)
    class(hsd_lexer_t), intent(inout) :: self
    integer, intent(in), optional :: n

    integer :: i, steps
    character(len=1) :: ch

    if (present(n)) then
      steps = n
    else
      steps = 1
    end if

    do i = 1, steps
      if (self%pos <= self%source_len) then
        ch = self%source(self%pos:self%pos)
        if (ch == CHAR_NEWLINE) then
          self%line = self%line + 1
          self%column = 1
        else
          self%column = self%column + 1
        end if
        self%pos = self%pos + 1
      end if
    end do

  end subroutine lexer_advance

  !> Skip whitespace characters (not newlines)
  subroutine lexer_skip_whitespace(self)
    class(hsd_lexer_t), intent(inout) :: self
    character(len=1) :: ch

    do while (.not. self%is_eof())
      ch = self%peek_char()
      if (ch == CHAR_SPACE .or. ch == CHAR_TAB) then
        call self%advance()
      else
        exit
      end if
    end do

  end subroutine lexer_skip_whitespace

  !> Read a quoted string
  subroutine lexer_read_string(self, token)
    class(hsd_lexer_t), intent(inout) :: self
    type(hsd_token_t), intent(out) :: token

    character(len=1) :: quote_char, ch
    type(string_buffer_t) :: buf
    integer :: start_line, start_col
    logical :: escaped

    start_line = self%line
    start_col = self%column
    quote_char = self%peek_char()
    call self%advance()  ! Skip opening quote

    call buf%init()
    escaped = .false.

    do while (.not. self%is_eof())
      ch = self%peek_char()

      if (escaped) then
        ! Handle escape sequences
        select case (ch)
        case ('n')
          call buf%append_char(CHAR_NEWLINE)
        case ('t')
          call buf%append_char(CHAR_TAB)
        case ('\')
          call buf%append_char(CHAR_BACKSLASH)
        case ('"')
          call buf%append_char(CHAR_DQUOTE)
        case ("'")
          call buf%append_char(CHAR_SQUOTE)
        case default
          call buf%append_char(ch)
        end select
        escaped = .false.
        call self%advance()
      else if (ch == CHAR_BACKSLASH) then
        escaped = .true.
        call self%advance()
      else if (ch == quote_char) then
        call self%advance()  ! Skip closing quote
        exit
      else
        call buf%append_char(ch)
        call self%advance()
      end if
    end do

    token%kind = TOKEN_STRING
    token%value = buf%get_string()
    token%line = start_line
    token%column = start_col

  end subroutine lexer_read_string

  !> Read unquoted text (identifier or value)
  subroutine lexer_read_text(self, token, stop_chars)
    class(hsd_lexer_t), intent(inout) :: self
    type(hsd_token_t), intent(out) :: token
    character(len=*), intent(in) :: stop_chars

    character(len=1) :: ch
    type(string_buffer_t) :: buf
    integer :: start_line, start_col

    start_line = self%line
    start_col = self%column
    call buf%init()

    do while (.not. self%is_eof())
      ch = self%peek_char()

      ! Note: escape handling is done in read_string_token, not here

      ! Check for stop characters
      if (index(stop_chars, ch) > 0) then
        exit
      end if

      ! Check for newline
      if (ch == CHAR_NEWLINE .or. ch == char(13)) then
        exit
      end if

      call buf%append_char(ch)
      call self%advance()
    end do

    token%kind = TOKEN_TEXT
    token%value = buf%get_string()
    token%line = start_line
    token%column = start_col

  end subroutine lexer_read_text

  !> Read a comment (from # to end of line)
  subroutine lexer_read_comment(self, token)
    class(hsd_lexer_t), intent(inout) :: self
    type(hsd_token_t), intent(out) :: token

    character(len=1) :: ch
    type(string_buffer_t) :: buf
    integer :: start_line, start_col

    start_line = self%line
    start_col = self%column
    call self%advance()  ! Skip #

    call buf%init()
    do while (.not. self%is_eof())
      ch = self%peek_char()
      if (ch == CHAR_NEWLINE) then
        exit
      end if
      call buf%append_char(ch)
      call self%advance()
    end do

    token%kind = TOKEN_COMMENT
    token%value = buf%get_string()
    token%line = start_line
    token%column = start_col

  end subroutine lexer_read_comment

  !> Get the next token from the source
  subroutine lexer_next_token(self, token, in_attrib)
    class(hsd_lexer_t), intent(inout) :: self
    type(hsd_token_t), intent(out) :: token
    logical, intent(in), optional :: in_attrib

    character(len=1) :: ch, ch2, ch3
    character(len=*), parameter :: general_stop = "{}[]<=""'#;"
    character(len=*), parameter :: attrib_stop = "]""'"
    character(len=:), allocatable :: stop_chars
    logical :: inside_attrib

    if (present(in_attrib)) then  ! LCOV_EXCL_START
      inside_attrib = in_attrib
    else  ! LCOV_EXCL_STOP
      inside_attrib = self%in_attrib
    end if

    if (inside_attrib) then  ! LCOV_EXCL_START
      stop_chars = attrib_stop
    else  ! LCOV_EXCL_STOP
      stop_chars = general_stop
    end if

    ! Skip whitespace
    call self%skip_whitespace()

    ! Check for EOF
    if (self%is_eof()) then
      token%kind = TOKEN_EOF
      token%line = self%line
      token%column = self%column
      return
    end if

    ch = self%peek_char()

    ! Single character tokens
    select case (ch)
    case (CHAR_NEWLINE)
      token%kind = TOKEN_NEWLINE
      token%line = self%line
      token%column = self%column
      call self%advance()
      return

    case (char(13))  ! Carriage return
      call self%advance()
      if (self%peek_char() == CHAR_NEWLINE) then
        call self%advance()
      end if
      token%kind = TOKEN_NEWLINE
      token%line = self%line
      token%column = self%column
      return

    case (CHAR_LBRACE)
      token%kind = TOKEN_LBRACE
      token%line = self%line
      token%column = self%column
      call self%advance()
      return

    case (CHAR_RBRACE)
      token%kind = TOKEN_RBRACE
      token%line = self%line
      token%column = self%column
      call self%advance()
      return

    case (CHAR_LBRACKET)
      token%kind = TOKEN_LBRACKET
      token%line = self%line
      token%column = self%column
      call self%advance()
      return

    case (CHAR_RBRACKET)
      token%kind = TOKEN_RBRACKET
      token%line = self%line
      token%column = self%column
      call self%advance()
      return

    case (CHAR_EQUAL)
      token%kind = TOKEN_EQUAL
      token%line = self%line
      token%column = self%column
      call self%advance()
      return

    case (CHAR_SEMICOLON)
      token%kind = TOKEN_SEMICOLON
      token%line = self%line
      token%column = self%column
      call self%advance()
      return

    case (CHAR_HASH)
      call self%read_comment(token)
      return

    case (CHAR_DQUOTE, CHAR_SQUOTE)
      call self%read_string(token)
      return

    case (CHAR_LESS)
      ! Check for include directives
      ch2 = self%peek_char(1)
      ch3 = self%peek_char(2)
      if (ch2 == CHAR_LESS .and. ch3 == CHAR_LESS) then
        ! <<< text include
        token%kind = TOKEN_INCLUDE_TXT
        token%line = self%line
        token%column = self%column
        call self%advance(3)
        ! Read the filename
        call self%skip_whitespace()
        if (self%peek_char() == CHAR_DQUOTE .or. self%peek_char() == CHAR_SQUOTE) then
          call self%read_string(token)
          token%kind = TOKEN_INCLUDE_TXT
        else
          call self%read_text(token, general_stop)
          token%kind = TOKEN_INCLUDE_TXT
        end if
        return
      else if (ch2 == CHAR_LESS .and. ch3 == '+') then
        ! <<+ HSD include
        token%kind = TOKEN_INCLUDE_HSD
        token%line = self%line
        token%column = self%column
        call self%advance(3)
        ! Read the filename
        call self%skip_whitespace()
        if (self%peek_char() == CHAR_DQUOTE .or. self%peek_char() == CHAR_SQUOTE) then
          call self%read_string(token)
          token%kind = TOKEN_INCLUDE_HSD
        else
          call self%read_text(token, general_stop)
          token%kind = TOKEN_INCLUDE_HSD
        end if
        return
      else
        ! Standalone '<' - treat as single-character text token
        token%kind = TOKEN_TEXT
        token%value = "<"
        token%line = self%line
        token%column = self%column
        call self%advance()
        return
      end if

    case default
      ! Fall through to read as text

    end select

    ! Default: read as text
    call self%read_text(token, stop_chars)

  end subroutine lexer_next_token

end module hsd_lexer
