!> Token definitions for the HSD lexer
!>
!> This module defines token types used during HSD parsing.
module hsd_token
  implicit none (type, external)
  private

  public :: hsd_token_t, token_kind
  public :: TOKEN_INVALID, TOKEN_EOF, TOKEN_WHITESPACE, TOKEN_NEWLINE
  public :: TOKEN_COMMENT, TOKEN_LBRACE, TOKEN_RBRACE, TOKEN_LBRACKET
  public :: TOKEN_RBRACKET, TOKEN_EQUAL, TOKEN_SEMICOLON, TOKEN_STRING
  public :: TOKEN_TEXT, TOKEN_INCLUDE_TXT, TOKEN_INCLUDE_HSD
  public :: token_name

  !> Token kind enumeration
  type :: enum_token_kind
    integer :: invalid = -1
    integer :: eof = 0
    integer :: whitespace = 1
    integer :: newline = 2
    integer :: comment = 3
    integer :: lbrace = 4      ! {
    integer :: rbrace = 5      ! }
    integer :: lbracket = 6    ! [
    integer :: rbracket = 7    ! ]
    integer :: equal = 8       ! =
    integer :: semicolon = 9   ! ;
    integer :: string = 10     ! quoted string
    integer :: text = 11       ! unquoted text/identifier
    integer :: include_txt = 12  ! <<<
    integer :: include_hsd = 13  ! <<+
  end type enum_token_kind

  !> Token kind constants
  type(enum_token_kind), parameter :: token_kind = enum_token_kind()

  !> Convenience aliases
  integer, parameter :: TOKEN_INVALID = -1
  integer, parameter :: TOKEN_EOF = 0
  integer, parameter :: TOKEN_WHITESPACE = 1
  integer, parameter :: TOKEN_NEWLINE = 2
  integer, parameter :: TOKEN_COMMENT = 3
  integer, parameter :: TOKEN_LBRACE = 4
  integer, parameter :: TOKEN_RBRACE = 5
  integer, parameter :: TOKEN_LBRACKET = 6
  integer, parameter :: TOKEN_RBRACKET = 7
  integer, parameter :: TOKEN_EQUAL = 8
  integer, parameter :: TOKEN_SEMICOLON = 9
  integer, parameter :: TOKEN_STRING = 10
  integer, parameter :: TOKEN_TEXT = 11
  integer, parameter :: TOKEN_INCLUDE_TXT = 12
  integer, parameter :: TOKEN_INCLUDE_HSD = 13

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
    case (TOKEN_WHITESPACE)
      name = "whitespace"
    case (TOKEN_NEWLINE)
      name = "newline"
    case (TOKEN_COMMENT)
      name = "comment"
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
