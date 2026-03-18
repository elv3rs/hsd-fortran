!> Token definitions for the HSD lexer
!>
!> This module defines token types used during HSD parsing.
module hsd_token
  implicit none (type, external)
  private

  public :: hsd_token_t
  public :: TOKEN_INVALID, TOKEN_EOF, TOKEN_NEWLINE
  public :: TOKEN_LBRACE, TOKEN_RBRACE, TOKEN_LBRACKET
  public :: TOKEN_RBRACKET, TOKEN_EQUAL, TOKEN_SEMICOLON, TOKEN_STRING
  public :: TOKEN_TEXT, TOKEN_INCLUDE_TXT, TOKEN_INCLUDE_HSD
  public :: token_name

  !> Token kind constants
  !>
  !> TOKEN_INVALID is a sentinel value: it is never emitted by the lexer and
  !> serves as the error sentinel for is_valid() and token_name().
  !> Whitespace and comments are silently consumed by the lexer and never
  !> represented as tokens.
  integer, parameter :: TOKEN_INVALID = -1
  integer, parameter :: TOKEN_EOF = 0
  integer, parameter :: TOKEN_NEWLINE = 1
  integer, parameter :: TOKEN_LBRACE = 2
  integer, parameter :: TOKEN_RBRACE = 3
  integer, parameter :: TOKEN_LBRACKET = 4
  integer, parameter :: TOKEN_RBRACKET = 5
  integer, parameter :: TOKEN_EQUAL = 6
  integer, parameter :: TOKEN_SEMICOLON = 7
  integer, parameter :: TOKEN_STRING = 8
  integer, parameter :: TOKEN_TEXT = 9
  integer, parameter :: TOKEN_INCLUDE_TXT = 10
  integer, parameter :: TOKEN_INCLUDE_HSD = 11

  !> Token type with position and value
  type :: hsd_token_t
    !> Kind of token
    integer :: kind = TOKEN_EOF
    !> Token value (for strings and text)
    character(len=:), allocatable :: value
    !> Line number where token starts
    integer :: line = 0
    !> Column number where token starts
    integer :: column = 0
  contains
    procedure :: is_eof => token_is_eof
    procedure :: is_valid => token_is_valid
  end type hsd_token_t

contains

  !> Check if token is eof
  pure function token_is_eof(self) result(is_eof)
    class(hsd_token_t), intent(in) :: self
    logical :: is_eof
    is_eof = self%kind == TOKEN_EOF
  end function token_is_eof

  !> Check if token is valid (not invalid or eof)
  pure function token_is_valid(self) result(is_valid)
    class(hsd_token_t), intent(in) :: self
    logical :: is_valid
    is_valid = self%kind > TOKEN_EOF
  end function token_is_valid

  !> Get human-readable name for a token kind
  pure function token_name(kind) result(name)
    integer, intent(in) :: kind
    character(len=:), allocatable :: name

    select case (kind)
    case (TOKEN_INVALID)
      name = "invalid"
    case (TOKEN_EOF)
      name = "end of file"
    case (TOKEN_NEWLINE)
      name = "newline"
    case (TOKEN_LBRACE)
      name = "opening brace"
    case (TOKEN_RBRACE)
      name = "closing brace"
    case (TOKEN_LBRACKET)
      name = "opening bracket"
    case (TOKEN_RBRACKET)
      name = "closing bracket"
    case (TOKEN_EQUAL)
      name = "equal sign"
    case (TOKEN_SEMICOLON)
      name = "semicolon"
    case (TOKEN_STRING)
      name = "string"
    case (TOKEN_TEXT)
      name = "text"
    case (TOKEN_INCLUDE_TXT)
      name = "text include"
    case (TOKEN_INCLUDE_HSD)
      name = "HSD include"
    case default
      name = "unknown"
    end select

  end function token_name

end module hsd_token
