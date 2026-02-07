!> HSD Formatter/Serializer
!>
!> This module provides functionality to write HSD data structures back to
!> text format.
module hsd_formatter
  use hsd_constants, only: dp, sp, CHAR_NEWLINE, CHAR_DQUOTE, CHAR_SQUOTE, CHAR_BACKSLASH
  use hsd_types, only: hsd_node, hsd_table, hsd_value, hsd_iterator, &
    VALUE_TYPE_NONE, VALUE_TYPE_ARRAY, VALUE_TYPE_STRING, &
    VALUE_TYPE_INTEGER, VALUE_TYPE_REAL, VALUE_TYPE_LOGICAL, VALUE_TYPE_COMPLEX
  use hsd_error, only: hsd_error_t, HSD_STAT_OK, HSD_STAT_IO_ERROR, make_error
  use hsd_utils, only: string_buffer_t
  implicit none (type, external)
  private

  public :: hsd_dump, hsd_dump_to_string

  !> Indentation string (2 spaces)
  character(len=*), parameter :: INDENT_STR = "  "

  !> Characters that require quoting
  character(len=*), parameter :: QUOTE_TRIGGER_CHARS = "{}[]= " // char(9)

contains

  !> Write HSD table to a file
  subroutine hsd_dump(root, filename, error)
    type(hsd_table), intent(in) :: root
    character(len=*), intent(in) :: filename
    type(hsd_error_t), allocatable, intent(out), optional :: error

    integer :: unit_num, io_stat
    character(len=256) :: io_msg

    open(newunit=unit_num, file=filename, status='replace', action='write', &
         iostat=io_stat, iomsg=io_msg)
    if (io_stat /= 0) then
      if (present(error)) then
        call make_error(error, HSD_STAT_IO_ERROR, &
          "Cannot open file for writing: " // trim(io_msg), filename)
      end if
      return
    end if

    call write_table_content(unit_num, root, 0)

    close(unit_num)

  end subroutine hsd_dump

  !> Write HSD table to a string (dynamically allocated)
  !>
  !> Uses string_buffer_t internally to avoid O(n²) concatenation.
  subroutine hsd_dump_to_string(root, output)
    type(hsd_table), intent(in) :: root
    character(len=:), allocatable, intent(out) :: output

    type(string_buffer_t) :: buf

    call buf%init(1024)
    call write_table_to_string_buf(root, 0, buf)
    output = buf%get_string()

  end subroutine hsd_dump_to_string

  !> Write table contents to unit
  recursive subroutine write_table_content(unit_num, table, indent_level)
    integer, intent(in) :: unit_num
    type(hsd_table), intent(in) :: table
    integer, intent(in) :: indent_level

    integer :: i
    class(hsd_node), pointer :: child
    character(len=:), allocatable :: indent

    indent = repeat(INDENT_STR, indent_level)

    do i = 1, table%num_children
      call table%get_child(i, child)
      if (.not. associated(child)) cycle

      select type (child)
      type is (hsd_table)
        call write_table_node(unit_num, child, indent_level)
      type is (hsd_value)
        call write_value_node(unit_num, child, indent_level)
      end select
    end do

  end subroutine write_table_content

  !> Write a table node
  recursive subroutine write_table_node(unit_num, table, indent_level)
    integer, intent(in) :: unit_num
    type(hsd_table), intent(in) :: table
    integer, intent(in) :: indent_level

    character(len=:), allocatable :: indent, attrib_str

    indent = repeat(INDENT_STR, indent_level)

    ! Build attribute string
    if (table%has_attrib()) then
      attrib_str = " [" // table%get_attrib() // "]"
    else
      attrib_str = ""
    end if

    ! Check if table has single child (for = syntax)
    if (table%num_children == 1) then
      block
        class(hsd_node), pointer :: single_child
        call table%get_child(1, single_child)

        select type (single_child)
        type is (hsd_table)
          ! Tag = ChildTag { ... }
          if (allocated(table%name) .and. len_trim(table%name) > 0) then
            write(unit_num, '(A)') indent // trim(table%name) // attrib_str // &
              " = " // trim(single_child%name) // " {"
            call write_table_content(unit_num, single_child, indent_level + 1)
            write(unit_num, '(A)') indent // "}"
          else
            ! Unnamed table, just write children
            call write_table_content(unit_num, table, indent_level)
          end if
          return

        type is (hsd_value)
          ! Tag = value (only for unnamed/anonymous children)
          if (.not. (allocated(single_child%name) .and. &
              len_trim(single_child%name) > 0)) then
            if (allocated(table%name) .and. len_trim(table%name) > 0) then
              call write_tag_value(unit_num, table%name, attrib_str, &
                                   single_child, indent_level)
            else
              call write_value_node(unit_num, single_child, indent_level)
            end if
            return
          end if
          ! Named child — fall through to regular block
        end select
      end block
    end if

    ! Regular block: Tag { ... }
    if (allocated(table%name) .and. len_trim(table%name) > 0) then
      write(unit_num, '(A)') indent // trim(table%name) // attrib_str // " {"
      call write_table_content(unit_num, table, indent_level + 1)
      write(unit_num, '(A)') indent // "}"
    else
      ! Root or unnamed table - just write content
      call write_table_content(unit_num, table, indent_level)
    end if

  end subroutine write_table_node

  !> Write a value node
  subroutine write_value_node(unit_num, val, indent_level)
    integer, intent(in) :: unit_num
    type(hsd_value), intent(in) :: val
    integer, intent(in) :: indent_level

    character(len=:), allocatable :: indent, attrib_str, value_str

    indent = repeat(INDENT_STR, indent_level)

    ! Build attribute string
    if (val%has_attrib()) then
      attrib_str = " [" // val%get_attrib() // "]"
    else
      attrib_str = ""
    end if

    ! Get value string
    value_str = format_value(val)

    ! Write
    if (allocated(val%name) .and. len_trim(val%name) > 0) then
      if (index(value_str, CHAR_NEWLINE) > 0) then
        ! Multi-line value
        write(unit_num, '(A)') indent // trim(val%name) // attrib_str // " {"
        call write_multiline(unit_num, value_str, indent_level + 1)
        write(unit_num, '(A)') indent // "}"
      else
        ! Single-line value
        write(unit_num, '(A)') indent // trim(val%name) // attrib_str // " = " // value_str
      end if
    else
      ! Anonymous value (data content)
      if (index(value_str, CHAR_NEWLINE) > 0) then
        call write_multiline(unit_num, value_str, indent_level)
      else
        write(unit_num, '(A)') indent // value_str
      end if
    end if

  end subroutine write_value_node

  !> Write tag = value
  subroutine write_tag_value(unit_num, name, attrib_str, val, indent_level)
    integer, intent(in) :: unit_num
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: attrib_str
    type(hsd_value), intent(in) :: val
    integer, intent(in) :: indent_level

    character(len=:), allocatable :: indent, value_str, val_attrib

    indent = repeat(INDENT_STR, indent_level)
    value_str = format_value(val)

    ! Combine attributes
    if (val%has_attrib()) then
      val_attrib = " [" // val%get_attrib() // "]"
    else
      val_attrib = attrib_str
    end if

    if (index(value_str, CHAR_NEWLINE) > 0) then
      ! Multi-line value
      write(unit_num, '(A)') indent // trim(name) // val_attrib // " {"
      call write_multiline(unit_num, value_str, indent_level + 1)
      write(unit_num, '(A)') indent // "}"
    else
      write(unit_num, '(A)') indent // trim(name) // val_attrib // " = " // value_str
    end if

  end subroutine write_tag_value

  !> Write multi-line content
  subroutine write_multiline(unit_num, text, indent_level)
    integer, intent(in) :: unit_num
    character(len=*), intent(in) :: text
    integer, intent(in) :: indent_level

    character(len=:), allocatable :: indent
    integer :: pos, next_pos, text_len

    indent = repeat(INDENT_STR, indent_level)
    text_len = len(text)
    pos = 1

    do while (pos <= text_len)
      next_pos = index(text(pos:), CHAR_NEWLINE)
      if (next_pos > 0) then
        next_pos = pos + next_pos - 1
        if (next_pos > pos) then
          write(unit_num, '(A)') indent // text(pos:next_pos-1)
        else
          write(unit_num, '(A)') ""
        end if
        pos = next_pos + 1
      else
        write(unit_num, '(A)') indent // text(pos:)
        exit
      end if
    end do

  end subroutine write_multiline

  !> Format a value for output
  function format_value(val) result(str)
    type(hsd_value), intent(in) :: val
    character(len=:), allocatable :: str

    character(len=64) :: buffer

    select case (val%value_type)
    case (VALUE_TYPE_LOGICAL)
      if (val%logical_value) then
        str = "Yes"
      else
        str = "No"
      end if

    case (VALUE_TYPE_INTEGER)
      write(buffer, '(I0)') val%int_value
      str = trim(adjustl(buffer))

    case (VALUE_TYPE_REAL)
      write(buffer, '(G0)') val%real_value
      str = trim(adjustl(buffer))
      ! Ensure we have a decimal point for whole numbers
      if (index(str, ".") == 0 .and. index(str, "E") == 0 .and. index(str, "e") == 0) then
        str = str // ".0"
      end if

    case (VALUE_TYPE_COMPLEX)
      block
        character(len=64) :: re_buf, im_buf
        write(re_buf, '(G0)') real(val%complex_value)
        write(im_buf, '(G0)') aimag(val%complex_value)
        str = "(" // trim(adjustl(re_buf)) // "," // trim(adjustl(im_buf)) // ")"
      end block

    case (VALUE_TYPE_STRING)
      if (allocated(val%string_value)) then
        str = quote_if_needed(val%string_value)
      else if (allocated(val%raw_text)) then
        str = val%raw_text
      else
        str = ""
      end if

    case default
      if (allocated(val%string_value)) then
        str = quote_if_needed(val%string_value)
      else if (allocated(val%raw_text)) then
        str = val%raw_text
      else
        str = ""
      end if
    end select

  end function format_value

  !> Quote a string if it contains special characters.
  !>
  !> Newlines, backslashes, tabs, and quote characters are escaped so that
  !> the result is always a single-line quoted token that round-trips cleanly.
  function quote_if_needed(str) result(quoted)
    character(len=*), intent(in) :: str
    character(len=:), allocatable :: quoted

    logical :: needs_quote
    integer :: i
    character(len=1) :: ch

    needs_quote = .false.
    do i = 1, len(str)
      ch = str(i:i)
      if (index(QUOTE_TRIGGER_CHARS, ch) > 0 &
          & .or. ch == CHAR_NEWLINE &
          & .or. ch == CHAR_BACKSLASH &
          & .or. ch == CHAR_DQUOTE &
          & .or. ch == CHAR_SQUOTE) then
        needs_quote = .true.
        exit
      end if
    end do

    if (.not. needs_quote) then
      quoted = str
    else
      quoted = CHAR_DQUOTE // escape_string(str) // CHAR_DQUOTE
    end if

  end function quote_if_needed

  !> Escape special characters in a string for HSD quoted output.
  !>
  !> Handles: backslash, double-quote, newline, tab.
  !> Uses string_buffer_t to avoid O(n^2) concatenation.
  function escape_string(str) result(escaped)
    character(len=*), intent(in) :: str
    character(len=:), allocatable :: escaped

    type(string_buffer_t) :: buf
    integer :: i
    character(len=1) :: ch

    call buf%init(len(str) + 16)
    do i = 1, len(str)
      ch = str(i:i)
      if (ch == CHAR_BACKSLASH) then
        call buf%append_str(CHAR_BACKSLASH // CHAR_BACKSLASH)
      else if (ch == CHAR_DQUOTE) then
        call buf%append_str(CHAR_BACKSLASH // CHAR_DQUOTE)
      else if (ch == CHAR_NEWLINE) then
        call buf%append_str(CHAR_BACKSLASH // "n")
      else if (ch == char(9)) then
        call buf%append_str(CHAR_BACKSLASH // "t")
      else
        call buf%append_char(ch)
      end if
    end do
    escaped = buf%get_string()

  end function escape_string

  !> Write table to string_buffer_t (for string output, avoids O(n²) concatenation)
  !>
  !> Mirrors the file-output path including the single-child `= ChildTag { }`
  !> and `Tag = value` shorthand syntax.
  recursive subroutine write_table_to_string_buf(table, indent_level, buf)
    type(hsd_table), intent(in) :: table
    integer, intent(in) :: indent_level
    type(string_buffer_t), intent(inout) :: buf

    integer :: i
    class(hsd_node), pointer :: child
    character(len=:), allocatable :: indent, attrib_str, line, value_str

    indent = repeat(INDENT_STR, indent_level)

    do i = 1, table%num_children
      call table%get_child(i, child)
      if (.not. associated(child)) cycle

      select type (child)
      type is (hsd_table)
        call write_table_node_to_buf(child, indent_level, buf)

      type is (hsd_value)
        if (child%has_attrib()) then
          attrib_str = " [" // child%get_attrib() // "]"
        else
          attrib_str = ""
        end if

        value_str = format_value(child)

        if (allocated(child%name) .and. len_trim(child%name) > 0) then
          if (index(value_str, CHAR_NEWLINE) > 0) then
            ! Multi-line value: Tag { \n content \n }
            call buf%append_str(indent // trim(child%name) // attrib_str // " {")
            call buf%append_str(CHAR_NEWLINE)
            call write_multiline_to_buf(value_str, indent_level + 1, buf)
            call buf%append_str(indent // "}")
            call buf%append_str(CHAR_NEWLINE)
          else
            line = indent // trim(child%name) // attrib_str // " = " // value_str
            call buf%append_str(line)
            call buf%append_str(CHAR_NEWLINE)
          end if
        else
          if (index(value_str, CHAR_NEWLINE) > 0) then
            call write_multiline_to_buf(value_str, indent_level, buf)
          else
            line = indent // value_str
            call buf%append_str(line)
            call buf%append_str(CHAR_NEWLINE)
          end if
        end if
      end select
    end do

  end subroutine write_table_to_string_buf

  !> Write a table node to string buffer with single-child shorthand
  recursive subroutine write_table_node_to_buf(table, indent_level, buf)
    type(hsd_table), intent(in) :: table
    integer, intent(in) :: indent_level
    type(string_buffer_t), intent(inout) :: buf

    character(len=:), allocatable :: indent, attrib_str, value_str, val_attrib

    indent = repeat(INDENT_STR, indent_level)

    ! Build attribute string
    if (table%has_attrib()) then
      attrib_str = " [" // table%get_attrib() // "]"
    else
      attrib_str = ""
    end if

    ! Check for single-child shorthand (= syntax)
    if (table%num_children == 1) then
      block
        class(hsd_node), pointer :: single_child
        call table%get_child(1, single_child)

        select type (single_child)
        type is (hsd_table)
          ! Tag = ChildTag { ... }
          if (allocated(table%name) .and. len_trim(table%name) > 0) then
            call buf%append_str(indent // trim(table%name) // attrib_str // &
              " = " // trim(single_child%name) // " {")
            call buf%append_str(CHAR_NEWLINE)
            call write_table_to_string_buf(single_child, indent_level + 1, buf)
            call buf%append_str(indent // "}")
            call buf%append_str(CHAR_NEWLINE)
          else
            call write_table_to_string_buf(table, indent_level, buf)
          end if
          return

        type is (hsd_value)
          ! Tag = value (only for unnamed/anonymous children)
          if (.not. (allocated(single_child%name) .and. &
              len_trim(single_child%name) > 0)) then
            if (allocated(table%name) .and. len_trim(table%name) > 0) then
              value_str = format_value(single_child)
              if (single_child%has_attrib()) then
                val_attrib = " [" // single_child%get_attrib() // "]"
              else
                val_attrib = attrib_str
              end if
              if (index(value_str, CHAR_NEWLINE) > 0) then
                call buf%append_str(indent // trim(table%name) // val_attrib // " {")
                call buf%append_str(CHAR_NEWLINE)
                call write_multiline_to_buf(value_str, indent_level + 1, buf)
                call buf%append_str(indent // "}")
                call buf%append_str(CHAR_NEWLINE)
              else
                call buf%append_str(indent // trim(table%name) // val_attrib // &
                  " = " // value_str)
                call buf%append_str(CHAR_NEWLINE)
              end if
            else
              value_str = format_value(single_child)
              call buf%append_str(indent // value_str)
              call buf%append_str(CHAR_NEWLINE)
            end if
            return
          end if
          ! Named child — fall through to regular block
        end select
      end block
    end if

    ! Regular block: Tag { ... }
    if (allocated(table%name) .and. len_trim(table%name) > 0) then
      call buf%append_str(indent // trim(table%name) // attrib_str // " {")
      call buf%append_str(CHAR_NEWLINE)
      call write_table_to_string_buf(table, indent_level + 1, buf)
      call buf%append_str(indent // "}")
      call buf%append_str(CHAR_NEWLINE)
    else
      call write_table_to_string_buf(table, indent_level, buf)
    end if

  end subroutine write_table_node_to_buf

  !> Write multi-line content to string buffer
  subroutine write_multiline_to_buf(text, indent_level, buf)
    character(len=*), intent(in) :: text
    integer, intent(in) :: indent_level
    type(string_buffer_t), intent(inout) :: buf

    character(len=:), allocatable :: indent
    integer :: pos, next_pos, text_len

    indent = repeat(INDENT_STR, indent_level)
    text_len = len(text)
    pos = 1

    do while (pos <= text_len)
      next_pos = index(text(pos:), CHAR_NEWLINE)
      if (next_pos > 0) then
        next_pos = pos + next_pos - 1
        if (next_pos > pos) then
          call buf%append_str(indent // text(pos:next_pos-1))
        end if
        call buf%append_str(CHAR_NEWLINE)
        pos = next_pos + 1
      else
        call buf%append_str(indent // text(pos:))
        call buf%append_str(CHAR_NEWLINE)
        exit
      end if
    end do

  end subroutine write_multiline_to_buf

end module hsd_formatter
