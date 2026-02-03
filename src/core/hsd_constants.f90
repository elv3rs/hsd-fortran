!> Constants and parameters for the HSD parser
!>
!> This module defines special characters, limits, and kind parameters
!> used throughout the HSD library.
module hsd_constants
  implicit none (type, external)
  private

  public :: dp, sp, hsd_max_line_length, hsd_max_include_depth
  public :: CHAR_LBRACE, CHAR_RBRACE, CHAR_LBRACKET, CHAR_RBRACKET
  public :: CHAR_EQUAL, CHAR_HASH, CHAR_SEMICOLON, CHAR_LESS
  public :: CHAR_DQUOTE, CHAR_SQUOTE, CHAR_BACKSLASH, CHAR_NEWLINE
  public :: CHAR_SPACE, CHAR_TAB
  public :: SPECIAL_CHARS, ATTRIB_SPECIAL_CHARS, WHITESPACE_CHARS

  !> Double precision kind parameter
  integer, parameter :: dp = selected_real_kind(15, 307)

  !> Single precision kind parameter
  integer, parameter :: sp = selected_real_kind(6, 37)

  !> Maximum line length for reading HSD files
  integer, parameter :: hsd_max_line_length = 4096

  !> Maximum include depth to prevent infinite recursion
  integer, parameter :: hsd_max_include_depth = 100

  !> Opening brace character
  character(len=1), parameter :: CHAR_LBRACE = '{'

  !> Closing brace character
  character(len=1), parameter :: CHAR_RBRACE = '}'

  !> Opening bracket character (for attributes)
  character(len=1), parameter :: CHAR_LBRACKET = '['

  !> Closing bracket character (for attributes)
  character(len=1), parameter :: CHAR_RBRACKET = ']'

  !> Equal sign character
  character(len=1), parameter :: CHAR_EQUAL = '='

  !> Hash character (comment)
  character(len=1), parameter :: CHAR_HASH = '#'

  !> Semicolon character (statement terminator)
  character(len=1), parameter :: CHAR_SEMICOLON = ';'

  !> Less-than character (for includes)
  character(len=1), parameter :: CHAR_LESS = '<'

  !> Double quote character
  character(len=1), parameter :: CHAR_DQUOTE = '"'

  !> Single quote character
  character(len=1), parameter :: CHAR_SQUOTE = "'"

  !> Backslash character (escape)
  character(len=1), parameter :: CHAR_BACKSLASH = '\'

  !> Newline character
  character(len=1), parameter :: CHAR_NEWLINE = new_line('a')

  !> Space character
  character(len=1), parameter :: CHAR_SPACE = ' '

  !> Tab character
  character(len=1), parameter :: CHAR_TAB = char(9)

  !> Special characters in general parsing context
  character(len=*), parameter :: SPECIAL_CHARS = "{}[]<=""'#;"

  !> Special characters within attribute context
  character(len=*), parameter :: ATTRIB_SPECIAL_CHARS = "]""'"

  !> Whitespace characters
  character(len=*), parameter :: WHITESPACE_CHARS = " " // char(9)



end module hsd_constants
