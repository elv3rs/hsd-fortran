!> Fuzzing and robustness tests for HSD parser
!>
!> This test suite verifies that the parser handles malformed inputs gracefully
!> without crashing. These tests serve as regression tests for robustness issues.
module test_fuzz_suite
  use hsd
  use fortuno_serial, only: is_equal, test => serial_case_item, &
      & check => serial_check, suite => serial_suite_item, test_list
  implicit none (type, external)
  private
  public :: tests

contains

  function tests()
    type(test_list) :: tests
    tests = test_list([&
        suite("fuzz", test_list([&
            test("malformed_braces", test_malformed_braces), &
            test("malformed_brackets", test_malformed_brackets), &
            test("malformed_quotes", test_malformed_quotes), &
            test("malformed_escapes", test_malformed_escapes), &
            test("deeply_nested", test_deeply_nested), &
            test("empty_inputs", test_empty_inputs), &
            test("binary_data", test_binary_data), &
            test("unicode_chars", test_unicode_chars), &
            test("extreme_whitespace", test_extreme_whitespace), &
            test("malformed_includes", test_malformed_includes), &
            test("edge_case_names", test_edge_case_names), &
            test("large_values", test_large_values), &
            test("numeric_edge_cases", test_numeric_edge_cases), &
            test("comment_edge_cases", test_comment_edge_cases), &
            test("attribute_edge_cases", test_attribute_edge_cases), &
            test("standalone_less_than", test_standalone_less_than) &
        ]))&
    ])
  end function tests

  !> Test malformed brace patterns
  !> Focus is on robustness (no crashes), not specific error detection
  subroutine test_malformed_braces()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error

    ! Unclosed brace - parser may be lenient
    call hsd_load_string("Tag {", root, error)
    ! No crash is success
    if (allocated(error)) deallocate(error)
    call root%destroy()

    ! Extra closing brace - parser may ignore
    call hsd_load_string("Tag = 1 }", root, error)
    if (allocated(error)) deallocate(error)
    call root%destroy()

    ! Mismatched braces - parser may accept partial
    call hsd_load_string("A { B { } ", root, error)
    if (allocated(error)) deallocate(error)
    call root%destroy()

    ! Empty braces (should work)
    call hsd_load_string("Tag {}", root, error)
    call check(.not. allocated(error), msg="Empty braces should be valid")
    call root%destroy()

    ! Many nested braces
    call hsd_load_string("A{B{C{D{E{F{}}}}}}", root, error)
    call check(.not. allocated(error), msg="Nested braces should work")
    call root%destroy()

  end subroutine test_malformed_braces

  !> Test malformed bracket patterns (attributes)
  !> Focus is on robustness (no crashes), not specific error detection
  subroutine test_malformed_brackets()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error

    ! Unclosed bracket - parser may be lenient or error
    call hsd_load_string("Tag [attrib = 1", root, error)
    if (allocated(error)) deallocate(error)
    call root%destroy()

    ! Bracket in value
    call hsd_load_string("Tag = ]", root, error)
    ! This might parse as a value, depends on parser
    call root%destroy()
    if (allocated(error)) deallocate(error)

    ! Multiple brackets
    call hsd_load_string("Tag [a] [b] = 1", root, error)
    ! Parser behavior varies
    call root%destroy()
    if (allocated(error)) deallocate(error)

    ! Nested brackets (invalid)
    call hsd_load_string("Tag [[nested]] = 1", root, error)
    call root%destroy()
    if (allocated(error)) deallocate(error)

  end subroutine test_malformed_brackets

  !> Test malformed quote patterns - focus on robustness (no crashes)
  subroutine test_malformed_quotes()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error

    ! Unclosed double quote - test robustness
    call hsd_load_string('Tag = "unclosed', root, error)
    ! Parser may or may not error, but should not crash
    if (allocated(error)) deallocate(error)
    call root%destroy()

    ! Unclosed single quote - test robustness
    call hsd_load_string("Tag = 'unclosed", root, error)
    if (allocated(error)) deallocate(error)
    call root%destroy()

    ! Mixed quotes - test robustness
    call hsd_load_string('Tag = "starts' // "'" // 'different', root, error)
    if (allocated(error)) deallocate(error)
    call root%destroy()

    ! Empty quotes (should work)
    call hsd_load_string('Tag = ""', root, error)
    call check(.not. allocated(error), msg="Empty quotes should be valid")
    call root%destroy()

    ! Quote in attribute - test robustness
    call hsd_load_string('Tag [attr="quoted"] = 1', root, error)
    call root%destroy()
    if (allocated(error)) deallocate(error)

  end subroutine test_malformed_quotes

  !> Test escape sequence edge cases
  subroutine test_malformed_escapes()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error

    ! Trailing backslash
    call hsd_load_string('Tag = "text\"', root, error)
    call root%destroy()
    if (allocated(error)) deallocate(error)

    ! Backslash at end
    call hsd_load_string("Tag = text\\", root, error)
    call root%destroy()
    if (allocated(error)) deallocate(error)

    ! Double backslash
    call hsd_load_string('Tag = "text\\\\"', root, error)
    call root%destroy()
    if (allocated(error)) deallocate(error)

  end subroutine test_malformed_escapes

  !> Test deeply nested structures
  subroutine test_deeply_nested()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=:), allocatable :: deep_input
    integer :: i

    ! Build deeply nested structure (100 levels)
    deep_input = ""
    do i = 1, 100
      deep_input = deep_input // "Level" // char(48 + mod(i, 10)) // " { "
    end do
    deep_input = deep_input // "Value = 42"
    do i = 1, 100
      deep_input = deep_input // " }"
    end do

    call hsd_load_string(deep_input, root, error)
    ! Should either parse or error gracefully, not crash
    call root%destroy()
    if (allocated(error)) deallocate(error)

  end subroutine test_deeply_nested

  !> Test empty and whitespace-only inputs
  subroutine test_empty_inputs()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error

    ! Completely empty
    call hsd_load_string("", root, error)
    call check(.not. allocated(error), msg="Empty input should be valid")
    call root%destroy()

    ! Only whitespace
    call hsd_load_string("   ", root, error)
    call check(.not. allocated(error), msg="Whitespace only should be valid")
    call root%destroy()

    ! Only newlines
    call hsd_load_string(new_line('a') // new_line('a'), root, error)
    call check(.not. allocated(error), msg="Newlines only should be valid")
    call root%destroy()

    ! Only comments
    call hsd_load_string("# Just a comment", root, error)
    call check(.not. allocated(error), msg="Comment only should be valid")
    call root%destroy()

    ! Multiple empty lines with comments
    call hsd_load_string("# Comment 1" // new_line('a') // &
                         "# Comment 2" // new_line('a'), root, error)
    call check(.not. allocated(error), msg="Multiple comments should be valid")
    call root%destroy()

  end subroutine test_empty_inputs

  !> Test binary/control character handling
  subroutine test_binary_data()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=100) :: input

    ! Null character
    input = "Tag = value" // char(0) // "more"
    call hsd_load_string(input(1:16), root, error)
    call root%destroy()
    if (allocated(error)) deallocate(error)

    ! Bell character
    input = "Tag = " // char(7)
    call hsd_load_string(trim(input), root, error)
    call root%destroy()
    if (allocated(error)) deallocate(error)

    ! Form feed
    input = "Tag = value" // char(12)
    call hsd_load_string(trim(input), root, error)
    call root%destroy()
    if (allocated(error)) deallocate(error)

    ! Carriage return variations
    input = "Tag = value" // char(13) // char(10)
    call hsd_load_string(trim(input), root, error)
    call root%destroy()
    if (allocated(error)) deallocate(error)

  end subroutine test_binary_data

  !> Test Unicode and extended ASCII
  subroutine test_unicode_chars()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error

    ! High ASCII
    call hsd_load_string("Tag = " // char(200) // char(201), root, error)
    call root%destroy()
    if (allocated(error)) deallocate(error)

    ! UTF-8 sequences (may or may not be handled)
    call hsd_load_string('Tag = "Ü"', root, error)
    call root%destroy()
    if (allocated(error)) deallocate(error)

  end subroutine test_unicode_chars

  !> Test extreme whitespace patterns
  subroutine test_extreme_whitespace()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error
    character(len=1000) :: spaces

    ! Many spaces
    spaces = repeat(" ", 500)
    call hsd_load_string(trim(spaces) // "Tag = 1", root, error)
    call root%destroy()
    if (allocated(error)) deallocate(error)

    ! Tabs
    call hsd_load_string(char(9) // char(9) // "Tag = 1", root, error)
    call check(.not. allocated(error), msg="Tabs should be valid whitespace")
    call root%destroy()

    ! Mixed whitespace
    call hsd_load_string(" " // char(9) // " " // char(9) // "Tag = 1", root, error)
    call check(.not. allocated(error), msg="Mixed whitespace should be valid")
    call root%destroy()

  end subroutine test_extreme_whitespace

  !> Test malformed include directives
  subroutine test_malformed_includes()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error

    ! Include with no file
    call hsd_load_string("<<<", root, error)
    call root%destroy()
    if (allocated(error)) deallocate(error)

    ! Include with empty string
    call hsd_load_string('<<< ""', root, error)
    call root%destroy()
    if (allocated(error)) deallocate(error)

    ! Parsed include with no file
    call hsd_load_string("<<+", root, error)
    call root%destroy()
    if (allocated(error)) deallocate(error)

    ! Non-existent file
    call hsd_load_string('<<< "/nonexistent/path/file.hsd"', root, error)
    call check(allocated(error), msg="Should error on non-existent include")
    if (allocated(error)) deallocate(error)
    call root%destroy()

  end subroutine test_malformed_includes

  !> Test edge case names
  subroutine test_edge_case_names()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error

    ! Numeric-starting name
    call hsd_load_string("123Tag = 1", root, error)
    call root%destroy()
    if (allocated(error)) deallocate(error)

    ! Special characters in name
    call hsd_load_string("Tag-With-Dashes = 1", root, error)
    call root%destroy()
    if (allocated(error)) deallocate(error)

    ! Underscore name
    call hsd_load_string("_private = 1", root, error)
    call root%destroy()
    if (allocated(error)) deallocate(error)

    ! Very long name
    call hsd_load_string(repeat("A", 1000) // " = 1", root, error)
    call root%destroy()
    if (allocated(error)) deallocate(error)

  end subroutine test_edge_case_names

  !> Test large values
  subroutine test_large_values()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error

    ! Very long string value
    call hsd_load_string('Tag = "' // repeat("X", 10000) // '"', root, error)
    call root%destroy()
    if (allocated(error)) deallocate(error)

    ! Very large number
    call hsd_load_string("Tag = 999999999999999999999999", root, error)
    call root%destroy()
    if (allocated(error)) deallocate(error)

    ! Many array elements
    call hsd_load_string("Tag = { " // repeat("1 ", 1000) // "}", root, error)
    call root%destroy()
    if (allocated(error)) deallocate(error)

  end subroutine test_large_values

  !> Test numeric edge cases
  subroutine test_numeric_edge_cases()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error

    ! Scientific notation variations
    call hsd_load_string("Tag = 1e+1000", root, error)
    call root%destroy()
    if (allocated(error)) deallocate(error)

    call hsd_load_string("Tag = 1e-1000", root, error)
    call root%destroy()
    if (allocated(error)) deallocate(error)

    ! Infinity representations
    call hsd_load_string("Tag = Inf", root, error)
    call root%destroy()
    if (allocated(error)) deallocate(error)

    ! NaN
    call hsd_load_string("Tag = NaN", root, error)
    call root%destroy()
    if (allocated(error)) deallocate(error)

    ! Multiple decimals
    call hsd_load_string("Tag = 1.2.3", root, error)
    call root%destroy()
    if (allocated(error)) deallocate(error)

    ! Plus sign
    call hsd_load_string("Tag = +42", root, error)
    call root%destroy()
    if (allocated(error)) deallocate(error)

    ! Negative zero
    call hsd_load_string("Tag = -0.0", root, error)
    call root%destroy()
    if (allocated(error)) deallocate(error)

  end subroutine test_numeric_edge_cases

  !> Test comment edge cases
  subroutine test_comment_edge_cases()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error

    ! Comment with special characters
    call hsd_load_string("# Comment with {} [] = <<<", root, error)
    call check(.not. allocated(error), msg="Comment with special chars should be valid")
    call root%destroy()

    ! Comment after value
    call hsd_load_string("Tag = 1 # inline comment", root, error)
    call check(.not. allocated(error), msg="Inline comment should be valid")
    call root%destroy()

    ! Hash in string (not a comment)
    call hsd_load_string('Tag = "value # not comment"', root, error)
    call check(.not. allocated(error), msg="Hash in string should not be comment")
    call root%destroy()

    ! Very long comment
    call hsd_load_string("# " // repeat("X", 10000), root, error)
    call root%destroy()
    if (allocated(error)) deallocate(error)

  end subroutine test_comment_edge_cases

  !> Test attribute edge cases
  subroutine test_attribute_edge_cases()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error

    ! Empty attribute
    call hsd_load_string("Tag [] = 1", root, error)
    call root%destroy()
    if (allocated(error)) deallocate(error)

    ! Whitespace only attribute
    call hsd_load_string("Tag [   ] = 1", root, error)
    call root%destroy()
    if (allocated(error)) deallocate(error)

    ! Very long attribute
    call hsd_load_string("Tag [" // repeat("X", 1000) // "] = 1", root, error)
    call root%destroy()
    if (allocated(error)) deallocate(error)

    ! Attribute with special characters
    call hsd_load_string("Tag [unit=eV] = 1", root, error)
    call root%destroy()
    if (allocated(error)) deallocate(error)

  end subroutine test_attribute_edge_cases

  !> Test standalone less-than character (regression test for infinite loop bug)
  !> The '<' character when not part of '<<<' or '<<+' was causing an infinite loop
  subroutine test_standalone_less_than()
    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error

    ! Standalone '<' in value position - should not hang
    call hsd_load_string("A { B { C < 1 } }", root, error)
    ! The parser should handle this gracefully (either error or treat < as text)
    call root%destroy()
    if (allocated(error)) deallocate(error)

    ! Standalone '<' at start - should not hang
    call hsd_load_string("< { B { C = 1 } }", root, error)
    call root%destroy()
    if (allocated(error)) deallocate(error)

    ! Standalone '<' with close tag - should not hang
    call hsd_load_string("A { B { C = 1 }<}", root, error)
    call root%destroy()
    if (allocated(error)) deallocate(error)

    ! Multiple standalone '<' - should not hang
    call hsd_load_string("A < B < C < D", root, error)
    call root%destroy()
    if (allocated(error)) deallocate(error)

    ! '<' in string should work fine
    call hsd_load_string('Tag = "value < 10"', root, error)
    call check(.not. allocated(error), msg="< in quoted string should be valid")
    call root%destroy()

  end subroutine test_standalone_less_than

end module test_fuzz_suite
