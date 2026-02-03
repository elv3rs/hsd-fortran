!> Lexer unit tests using Fortuno framework
module test_lexer_suite
  use hsd_lexer, only : hsd_lexer_t, new_lexer_from_string
  use hsd_token, only : hsd_token_t, TOKEN_EOF, TOKEN_TEXT, TOKEN_STRING, &
      & TOKEN_LBRACE, TOKEN_RBRACE, TOKEN_EQUAL, TOKEN_LBRACKET, TOKEN_RBRACKET, &
      & TOKEN_INCLUDE_TXT, TOKEN_INCLUDE_HSD, TOKEN_COMMENT
  use fortuno_serial, only : is_equal, test => serial_case_item, check => serial_check, &
      & suite => serial_suite_item, test_list
  implicit none (type, external)

  private
  public :: tests

contains

  !> Returns all lexer tests
  function tests()
    type(test_list) :: tests

    tests = test_list([&
        suite("lexer", test_list([&
            test("simple_tokens", test_simple_tokens), &
            test("quoted_strings_double", test_quoted_strings_double), &
            test("quoted_strings_single", test_quoted_strings_single), &
            test("attributes", test_attributes), &
            test("comments", test_comments), &
            test("include_text", test_include_text), &
            test("include_hsd", test_include_hsd), &
            test("multiline_content", test_multiline_content), &
            test("escaped_chars", test_escaped_chars) &
        ])) &
    ])

  end function tests


  !> Test basic token recognition
  subroutine test_simple_tokens()
    type(hsd_lexer_t) :: lexer
    type(hsd_token_t) :: token

    call new_lexer_from_string(lexer, "tag { value = 123 }")

    call lexer%next_token(token)
    call check(is_equal(token%kind, TOKEN_TEXT), msg="First token is TEXT")
    call check(token%value == "tag", msg="First token value is 'tag'")

    call lexer%next_token(token)
    call check(is_equal(token%kind, TOKEN_LBRACE), msg="Second token is LBRACE")

    call lexer%next_token(token)
    call check(is_equal(token%kind, TOKEN_TEXT), msg="Third token is TEXT")
    call check(token%value == "value", msg="Third token value is 'value'")

    call lexer%next_token(token)
    call check(is_equal(token%kind, TOKEN_EQUAL), msg="Fourth token is EQUAL")

    call lexer%next_token(token)
    call check(is_equal(token%kind, TOKEN_TEXT), msg="Fifth token is TEXT")
    call check(token%value == "123", msg="Fifth token value is '123'")

    call lexer%next_token(token)
    call check(is_equal(token%kind, TOKEN_RBRACE), msg="Sixth token is RBRACE")

    call lexer%next_token(token)
    call check(is_equal(token%kind, TOKEN_EOF), msg="Last token is EOF")

  end subroutine test_simple_tokens


  !> Test double-quoted strings
  subroutine test_quoted_strings_double()
    type(hsd_lexer_t) :: lexer
    type(hsd_token_t) :: token

    call new_lexer_from_string(lexer, 'name = "hello world"')

    call lexer%next_token(token)  ! name
    call lexer%next_token(token)  ! =
    call lexer%next_token(token)  ! "hello world"

    call check(is_equal(token%kind, TOKEN_STRING), msg="Quoted token is STRING")
    call check(token%value == "hello world", msg="Quoted value is correct")

  end subroutine test_quoted_strings_double


  !> Test single-quoted strings
  subroutine test_quoted_strings_single()
    type(hsd_lexer_t) :: lexer
    type(hsd_token_t) :: token

    call new_lexer_from_string(lexer, "name = 'single quoted'")

    call lexer%next_token(token)  ! name
    call lexer%next_token(token)  ! =
    call lexer%next_token(token)  ! 'single quoted'

    call check(is_equal(token%kind, TOKEN_STRING), msg="Single-quoted token is STRING")
    call check(token%value == "single quoted", msg="Single-quoted value is correct")

  end subroutine test_quoted_strings_single


  !> Test attribute syntax [attrib]
  subroutine test_attributes()
    type(hsd_lexer_t) :: lexer
    type(hsd_token_t) :: token

    call new_lexer_from_string(lexer, "temp [Kelvin] = 300")

    call lexer%next_token(token)  ! temp
    call check(is_equal(token%kind, TOKEN_TEXT), msg="Tag name is TEXT")

    call lexer%next_token(token)  ! [
    call check(is_equal(token%kind, TOKEN_LBRACKET), msg="Left bracket recognized")

    call lexer%next_token(token)  ! Kelvin
    call check(is_equal(token%kind, TOKEN_TEXT), msg="Attribute is TEXT")
    call check(token%value == "Kelvin", msg="Attribute value is 'Kelvin'")

    call lexer%next_token(token)  ! ]
    call check(is_equal(token%kind, TOKEN_RBRACKET), msg="Right bracket recognized")

    call lexer%next_token(token)  ! =
    call check(is_equal(token%kind, TOKEN_EQUAL), msg="Equal sign recognized")

    call lexer%next_token(token)  ! 300
    call check(is_equal(token%kind, TOKEN_TEXT), msg="Value is TEXT")
    call check(token%value == "300", msg="Value is '300'")

  end subroutine test_attributes


  !> Test comment handling
  subroutine test_comments()
    type(hsd_lexer_t) :: lexer
    type(hsd_token_t) :: token

    call new_lexer_from_string(lexer, "tag # this is a comment" // char(10) // "next")

    call lexer%next_token(token)  ! tag
    call check(is_equal(token%kind, TOKEN_TEXT), msg="Token before comment is TEXT")

    call lexer%next_token(token)  ! # comment
    call check(is_equal(token%kind, TOKEN_COMMENT), msg="Comment token recognized")

    call lexer%next_token(token)  ! newline
    call lexer%next_token(token)  ! next
    call check(is_equal(token%kind, TOKEN_TEXT), msg="Token after comment is TEXT")
    call check(token%value == "next", msg="Token after comment has correct value")

  end subroutine test_comments


  !> Test text include syntax <<<
  subroutine test_include_text()
    type(hsd_lexer_t) :: lexer
    type(hsd_token_t) :: token

    call new_lexer_from_string(lexer, '<<< "file.txt"')

    call lexer%next_token(token)
    call check(is_equal(token%kind, TOKEN_INCLUDE_TXT), msg="<<< creates TEXT include token")
    call check(token%value == "file.txt", msg="Include filename is correct")

  end subroutine test_include_text


  !> Test HSD include syntax <<+
  subroutine test_include_hsd()
    type(hsd_lexer_t) :: lexer
    type(hsd_token_t) :: token

    call new_lexer_from_string(lexer, '<<+ "other.hsd"')

    call lexer%next_token(token)
    call check(is_equal(token%kind, TOKEN_INCLUDE_HSD), msg="<<+ creates HSD include token")
    call check(token%value == "other.hsd", msg="HSD include filename is correct")

  end subroutine test_include_hsd


  !> Test multiline content handling
  subroutine test_multiline_content()
    type(hsd_lexer_t) :: lexer
    type(hsd_token_t) :: token
    character(len=:), allocatable :: input

    input = "data {" // char(10) // &
            "  1 2 3" // char(10) // &
            "  4 5 6" // char(10) // &
            "}"

    call new_lexer_from_string(lexer, input)

    call lexer%next_token(token)  ! data
    call check(is_equal(token%kind, TOKEN_TEXT), msg="Tag name recognized")

    call lexer%next_token(token)  ! {
    call check(is_equal(token%kind, TOKEN_LBRACE), msg="Opening brace recognized")

    ! Skip through content tokens
    do while (token%kind /= TOKEN_RBRACE .and. token%kind /= TOKEN_EOF)
      call lexer%next_token(token)
    end do

    call check(is_equal(token%kind, TOKEN_RBRACE), msg="Closing brace recognized")

  end subroutine test_multiline_content


  !> Test escaped character handling
  subroutine test_escaped_chars()
    type(hsd_lexer_t) :: lexer
    type(hsd_token_t) :: token

    ! Test escaped brace
    call new_lexer_from_string(lexer, 'text = "value with \{ brace"')

    call lexer%next_token(token)  ! text
    call lexer%next_token(token)  ! =
    call lexer%next_token(token)  ! string

    call check(is_equal(token%kind, TOKEN_STRING), msg="Escaped string is STRING")

  end subroutine test_escaped_chars

end module test_lexer_suite
