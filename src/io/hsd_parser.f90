!> HSD Parser
!>
!> This module provides the main parsing functionality for HSD files.
!> It converts a token stream into a tree of hsd_node_t nodes.
!> Includes cycle detection for <<+ includes.
module hsd_parser
  use hsd_constants, only: dp, hsd_max_include_depth, CHAR_NEWLINE
  use hsd_lexer, only: hsd_lexer_t, new_lexer_from_file, new_lexer_from_string, &
    hsd_token_t, TOKEN_EOF, TOKEN_STRING, &
    TOKEN_LBRACE, TOKEN_RBRACE, TOKEN_EQUAL, TOKEN_LBRACKET, TOKEN_RBRACKET, &
    TOKEN_INCLUDE_TXT, TOKEN_INCLUDE_HSD, TOKEN_SEMICOLON, &
    TOKEN_TEXT, TOKEN_NEWLINE
  use hsd_types, only: hsd_node_t, hsd_node_ptr_t, &
    new_table, new_value, VALUE_TYPE_NONE, VALUE_TYPE_ARRAY, &
    VALUE_TYPE_STRING, NODE_TYPE_TABLE, NODE_TYPE_VALUE
  use hsd_error, only: hsd_error_t, make_error, &
    HSD_STAT_OK, HSD_STAT_SYNTAX_ERROR, HSD_STAT_FILE_NOT_FOUND, &
    HSD_STAT_IO_ERROR, HSD_STAT_INCLUDE_CYCLE, HSD_STAT_INCLUDE_DEPTH, &
    HSD_STAT_UNCLOSED_ATTRIB
  use hsd_utils, only: to_lower
  implicit none (type, external)
  private

  public :: hsd_load_file, hsd_load_string

  !> Include stack item for cycle detection
  type :: include_item_t
    character(len=:), allocatable :: path
  end type include_item_t

  !> Parser state
  type :: parser_state_t
    !> Current lexer
    type(hsd_lexer_t) :: lexer
    !> Current token
    type(hsd_token_t) :: current_token
    !> Include stack for cycle detection
    type(include_item_t), allocatable :: include_stack(:)
    !> Current include depth
    integer :: include_depth = 0
    !> Base directory for relative includes
    character(len=:), allocatable :: base_dir
  contains
    procedure :: next_token => parser_next_token
    procedure :: push_include => parser_push_include
    procedure :: pop_include => parser_pop_include
    procedure :: is_include_cycle => parser_is_cycle
  end type parser_state_t

contains

  ! ---- PUBLIC API ----

  !> Load HSD from a file
  subroutine hsd_load_file(filename, root, error)
    character(len=*), intent(in) :: filename
    type(hsd_node_t), intent(out) :: root
    type(hsd_error_t), allocatable, intent(out), optional :: error

    type(parser_state_t) :: state
    type(hsd_error_t), allocatable :: local_error

    ! Always initialize root so callers get a valid (empty) table even on error
    call new_table(root)

    ! Initialize lexer
    call new_lexer_from_file(state%lexer, filename, local_error)
    if (allocated(local_error)) then
      call handle_public_error(local_error, error)
      return
    end if

    ! Initialize parser state
    state%base_dir = get_directory(filename)
    allocate(state%include_stack(hsd_max_include_depth))
    state%include_depth = 0

    ! Push current file onto include stack
    call state%push_include(filename, local_error)
    if (allocated(local_error)) then
      call handle_public_error(local_error, error)
      return
    end if

    ! Get first token and parse
    call state%next_token()
    call parse_content(state, root, local_error)
    call state%pop_include()

    call handle_public_error(local_error, error)

  end subroutine hsd_load_file

  !> Load HSD from a string
  subroutine hsd_load_string(source, root, error, filename)
    character(len=*), intent(in) :: source
    type(hsd_node_t), intent(out) :: root
    type(hsd_error_t), allocatable, intent(out), optional :: error
    character(len=*), intent(in), optional :: filename

    type(parser_state_t) :: state
    type(hsd_error_t), allocatable :: local_error

    ! Initialize lexer from string
    if (present(filename)) then
      call new_lexer_from_string(state%lexer, source, filename)
      state%base_dir = get_directory(filename)
    else
      call new_lexer_from_string(state%lexer, source)
      state%base_dir = "."
    end if

    ! Initialize parser state
    allocate(state%include_stack(hsd_max_include_depth))
    state%include_depth = 0

    ! Initialize root table
    call new_table(root)

    ! Get first token and parse
    call state%next_token()
    call parse_content(state, root, local_error)

    call handle_public_error(local_error, error)

  end subroutine hsd_load_string

  !> Map internal mandatory errors to public optional ones
  subroutine handle_public_error(internal_err, public_err)
    type(hsd_error_t), allocatable, intent(inout) :: internal_err
    type(hsd_error_t), allocatable, intent(out), optional :: public_err

    if (allocated(internal_err) .and. present(public_err)) then
      call move_alloc(internal_err, public_err)
    end if

  end subroutine handle_public_error

  ! ---- PARSER STATE ----

  !> Advance to the next token
  subroutine parser_next_token(self)
    class(parser_state_t), intent(inout) :: self
    call self%lexer%next_token(self%current_token)
  end subroutine parser_next_token

  !> Push file onto include stack
  subroutine parser_push_include(self, path, error)
    class(parser_state_t), intent(inout) :: self
    character(len=*), intent(in) :: path
    type(hsd_error_t), allocatable, intent(out) :: error

    ! Check for cycle
    if (self%is_include_cycle(path)) then
      call make_error(error, HSD_STAT_INCLUDE_CYCLE, &
        "Cyclic include detected", &
        self%lexer%filename, &
        self%current_token%line, &
        column=self%current_token%column, &
        actual=path, &
        hint="This file is already being processed in the include chain")
      return
    end if

    ! Check depth limit
    if (self%include_depth >= hsd_max_include_depth) then
      call make_error(error, HSD_STAT_INCLUDE_DEPTH, &
        "Maximum include depth exceeded", &
        self%lexer%filename, &
        self%current_token%line, &
        column=self%current_token%column, &
        actual=path, &
        hint="Reduce nesting of include directives")
      return
    end if

    ! Push onto stack
    self%include_depth = self%include_depth + 1
    self%include_stack(self%include_depth)%path = path

  end subroutine parser_push_include

  !> Pop file from include stack
  subroutine parser_pop_include(self)
    class(parser_state_t), intent(inout) :: self

    if (self%include_depth > 0) then
      if (allocated(self%include_stack(self%include_depth)%path)) then
        deallocate(self%include_stack(self%include_depth)%path)
      end if
      self%include_depth = self%include_depth - 1
    end if

  end subroutine parser_pop_include

  !> Check if path would create a cycle
  function parser_is_cycle(self, path) result(is_cycle)
    class(parser_state_t), intent(in) :: self
    character(len=*), intent(in) :: path
    logical :: is_cycle

    integer :: i

    is_cycle = .false.
    do i = 1, self%include_depth
      if (allocated(self%include_stack(i)%path)) then
        if (self%include_stack(i)%path == path) then
          is_cycle = .true.
          return
        end if
      end if
    end do

  end function parser_is_cycle

  ! ---- BUFFER HELPERS ----

  !> Flush buffered text into a value node on the parent.
  subroutine flush_text_buffer(parent, buffer, start_line)
    type(hsd_node_t), intent(inout) :: parent
    character(len=:), allocatable, intent(inout) :: buffer
    integer, intent(in) :: start_line

    if (len_trim(buffer) > 0) then
      call add_text_to_parent(parent, strip_trailing_nl(buffer), start_line)
      buffer = ""
    end if

  end subroutine flush_text_buffer

  !> Append text to the buffer, joining with space or newline as appropriate.
  subroutine append_text_buffer(buffer, text, start_line, current_line)
    character(len=:), allocatable, intent(inout) :: buffer
    character(len=*), intent(in) :: text
    integer, intent(inout) :: start_line
    integer, intent(in) :: current_line

    if (len(buffer) == 0) then
      buffer = text
      start_line = current_line
    else if (buffer(len(buffer):len(buffer)) == char(10)) then
      buffer = buffer // text
    else
      buffer = buffer // " " // text
    end if

  end subroutine append_text_buffer

  !> Collect remaining value tokens until end-of-line / EOF / semicolon / }.
  subroutine collect_value_line(state, value_text)
    type(parser_state_t), intent(inout) :: state
    character(len=:), allocatable, intent(inout) :: value_text

    do while (state%current_token%kind /= TOKEN_NEWLINE .and. &
              state%current_token%kind /= TOKEN_EOF .and. &
              state%current_token%kind /= TOKEN_SEMICOLON .and. &
              state%current_token%kind /= TOKEN_RBRACE)
      if (state%current_token%kind == TOKEN_TEXT .or. &
          state%current_token%kind == TOKEN_STRING) then
        value_text = value_text // " " // state%current_token%value
      end if
      call state%next_token()
    end do

    if (state%current_token%kind == TOKEN_SEMICOLON) call state%next_token()

  end subroutine collect_value_line

  ! ---- PARSER CORE ----

  !> Parse content (multiple tags/values)
  recursive subroutine parse_content(state, parent, error)
    type(parser_state_t), intent(inout) :: state
    type(hsd_node_t), intent(inout) :: parent
    type(hsd_error_t), allocatable, intent(out) :: error

    character(len=:), allocatable :: text_buffer
    integer :: text_start_line

    text_buffer = ""
    text_start_line = 0

    do while (.not. state%current_token%is_eof())
      select case (state%current_token%kind)
      case (TOKEN_RBRACE)
        ! End of current block — flush text and exit loop
        call flush_text_buffer(parent, text_buffer, text_start_line)
        exit

      case (TOKEN_TEXT)
        call parse_tag_or_value(state, parent, text_buffer, text_start_line, error)
        if (allocated(error)) return

      case (TOKEN_STRING)
        call append_text_buffer(text_buffer, state%current_token%value, &
          & text_start_line, state%current_token%line)
        call state%next_token()

      case (TOKEN_INCLUDE_HSD)
        call handle_hsd_include(state, parent, error)
        if (allocated(error)) return

      case (TOKEN_INCLUDE_TXT)
        call handle_text_include(state, text_buffer, error)
        if (allocated(error)) return

      case (TOKEN_NEWLINE)
        ! Preserve newlines as content separators when buffering inline text;
        ! otherwise skip them (e.g. blank lines between tags).
        if (len(text_buffer) > 0) then
          text_buffer = text_buffer // char(10)
        end if
        call state%next_token()

      case default
        call state%next_token()
      end select
    end do

    ! Flush remaining text buffer (strip trailing newlines)
    call flush_text_buffer(parent, text_buffer, text_start_line)

  end subroutine parse_content

  !> Parse a tag (possibly with value) or just data
  recursive subroutine parse_tag_or_value(state, parent, text_buffer, text_start_line, error)
    type(parser_state_t), intent(inout) :: state
    type(hsd_node_t), intent(inout) :: parent
    character(len=:), allocatable, intent(inout) :: text_buffer
    integer, intent(inout) :: text_start_line
    type(hsd_error_t), allocatable, intent(out) :: error

    character(len=:), allocatable :: tag_name, attrib, original_text
    integer :: tag_line
    logical :: is_amendment

    ! Save current state — preserve original text for data fallback, lowercase for tag use
    original_text = trim(state%current_token%value)
    tag_name = to_lower(original_text)
    tag_line = state%current_token%line
    call state%next_token()

    ! Check for amendment prefix (+Tag means merge into existing Tag)
    is_amendment = (len(tag_name) > 1 .and. tag_name(1:1) == "+")
    if (is_amendment) tag_name = tag_name(2:)

    ! Check for attribute [...]
    attrib = ""
    if (state%current_token%kind == TOKEN_LBRACKET) then
      call state%next_token()
      call parse_attribute(state, attrib, error)
      if (allocated(error)) return
    end if

    ! Determine what follows
    select case (state%current_token%kind)
    case (TOKEN_LBRACE)
      ! Block: Tag { ... } or +Tag { ... }
      call flush_text_buffer(parent, text_buffer, text_start_line)
      call state%next_token()  ! consume {
      call parse_block(state, parent, tag_name, attrib, tag_line, is_amendment, error)

    case (TOKEN_EQUAL)
      ! Assignment: Tag = value or Tag = ChildTag { ... }
      call flush_text_buffer(parent, text_buffer, text_start_line)
      call state%next_token()  ! consume =
      call parse_assignment(state, parent, tag_name, attrib, tag_line, is_amendment, error)

    case (TOKEN_NEWLINE, TOKEN_EOF, TOKEN_RBRACE, TOKEN_SEMICOLON)
      ! Just a tag name on its own — treat as text (preserve original case)
      call append_text_buffer(text_buffer, original_text, text_start_line, tag_line)

    case default
      ! Treat as part of text (preserve original case)
      call append_text_buffer(text_buffer, original_text, text_start_line, tag_line)
    end select

  end subroutine parse_tag_or_value

  !> Parse a block: Tag { ... } or +Tag { ... } (amendment)
  recursive subroutine parse_block(state, parent, tag_name, attrib, tag_line, is_amendment, error)
    type(parser_state_t), intent(inout) :: state
    type(hsd_node_t), intent(inout) :: parent
    character(len=*), intent(in) :: tag_name, attrib
    integer, intent(in) :: tag_line
    logical, intent(in) :: is_amendment
    type(hsd_error_t), allocatable, intent(out) :: error

    type(hsd_node_t) :: child_table
    type(hsd_node_t), pointer :: target

    if (is_amendment) then
      call parse_amendment_target(state, parent, tag_name, tag_line, target, error)
      if (allocated(error)) return
      call parse_content(state, target, error)
    else
      call new_table(child_table, tag_name, attrib, tag_line)
      call parse_content(state, child_table, error)
    end if

    if (allocated(error)) return
    if (state%current_token%kind == TOKEN_RBRACE) call state%next_token()
    if (.not. is_amendment) call parent%add_child(child_table)

  end subroutine parse_block

  !> Parse an assignment: Tag = value, Tag = { ... }, or Tag = ChildTag { ... }
  recursive subroutine parse_assignment(state, parent, tag_name, attrib, tag_line, &
      & is_amendment, error)
    type(parser_state_t), intent(inout) :: state
    type(hsd_node_t), intent(inout) :: parent
    character(len=*), intent(in) :: tag_name, attrib
    integer, intent(in) :: tag_line
    logical, intent(in) :: is_amendment
    type(hsd_error_t), allocatable, intent(out) :: error

    type(hsd_node_t) :: child_table, child_value
    type(hsd_token_t) :: saved_token
    character(len=:), allocatable :: value_text, child_tag_name
    logical :: child_is_amendment
    type(hsd_node_t), pointer :: existing_target

    ! Tag = { ... } — direct block
    if (state%current_token%kind == TOKEN_LBRACE) then
      call state%next_token()
      call new_table(child_table, tag_name, attrib, tag_line)
      call parse_content(state, child_table, error)
      if (allocated(error)) return
      if (state%current_token%kind == TOKEN_RBRACE) call state%next_token()
      call parent%add_child(child_table)
      return
    end if

    ! Tag = TEXT ...
    if (state%current_token%kind == TOKEN_TEXT) then
      saved_token = state%current_token
      call state%next_token()

      ! Tag = ChildTag { ... }
      if (state%current_token%kind == TOKEN_LBRACE) then
        call state%next_token()  ! consume {

        child_tag_name = to_lower(trim(saved_token%value))
        child_is_amendment = (len(child_tag_name) > 1 .and. child_tag_name(1:1) == "+")
        if (child_is_amendment) child_tag_name = child_tag_name(2:)

        if (is_amendment) then
          call parse_amendment_target(state, parent, tag_name, tag_line, existing_target, error)
          if (allocated(error)) return
          call parse_block(state, existing_target, child_tag_name, "", &
            & saved_token%line, child_is_amendment, error)
        else
          call new_table(child_table, tag_name, attrib, tag_line)
          call parse_block(state, child_table, child_tag_name, "", &
            & saved_token%line, .false., error)
          if (.not. allocated(error)) call parent%add_child(child_table)
        end if
        return
      end if

      ! Not a block — start of a value
      value_text = trim(saved_token%value)

    else if (state%current_token%kind == TOKEN_STRING) then
      ! Tag = "string value"
      value_text = state%current_token%value
      call state%next_token()

    else
      ! Empty value
      value_text = ""
    end if

    ! Gather rest of line for values
    call collect_value_line(state, value_text)

    call new_value(child_value, tag_name, attrib, tag_line)
    call child_value%set_string(trim(value_text))
    call parent%add_child(child_value)

  end subroutine parse_assignment

  !> Find an existing table child for amendment; error if not found or not a table.
  subroutine parse_amendment_target(state, parent, tag_name, line, target, error)
    type(parser_state_t), intent(in) :: state
    type(hsd_node_t), intent(inout) :: parent
    character(len=*), intent(in) :: tag_name
    integer, intent(in) :: line
    type(hsd_node_t), pointer, intent(out) :: target
    type(hsd_error_t), allocatable, intent(out) :: error

    nullify(target)
    call parent%get_child_by_name(tag_name, target)

    if (.not. associated(target)) then
      call make_error(error, HSD_STAT_SYNTAX_ERROR, &
        "Amendment target '" // tag_name // "' not found in parent", &
        state%lexer%filename, line)
    else if (target%node_type /= NODE_TYPE_TABLE) then
      call make_error(error, HSD_STAT_SYNTAX_ERROR, &
        "Amendment target '" // tag_name // "' is not a block", &
        state%lexer%filename, line)
    end if

  end subroutine parse_amendment_target

  !> Parse attribute content between [ and ]
  subroutine parse_attribute(state, attrib, error)
    type(parser_state_t), intent(inout) :: state
    character(len=:), allocatable, intent(out) :: attrib
    type(hsd_error_t), allocatable, intent(out) :: error

    attrib = ""

    do while (state%current_token%kind /= TOKEN_RBRACKET .and. &
              .not. state%current_token%is_eof())
      if (state%current_token%kind == TOKEN_TEXT .or. &
          state%current_token%kind == TOKEN_STRING) then
        if (len(attrib) > 0) then
          attrib = attrib // " " // state%current_token%value
        else
          attrib = state%current_token%value
        end if
      end if
      call state%next_token()
    end do

    ! Consume closing bracket
    if (state%current_token%kind == TOKEN_RBRACKET) then
      call state%next_token()
    else
      block
        character(len=:), allocatable :: actual_str
        if (allocated(state%current_token%value)) then
          actual_str = trim(state%current_token%value)
        else
          actual_str = "<EOF>"
        end if
        call make_error(error, HSD_STAT_UNCLOSED_ATTRIB, &
          "Unclosed attribute bracket", &
          state%lexer%filename, &
          state%current_token%line, &
          column=state%current_token%column, &
          expected="]", &
          actual=actual_str, &
          hint="Add closing ']' to complete the attribute")
      end block
    end if

  end subroutine parse_attribute

  ! ---- INCLUDES ----

  !> Handle <<+ HSD include
  recursive subroutine handle_hsd_include(state, parent, error)
    type(parser_state_t), intent(inout) :: state
    type(hsd_node_t), intent(inout) :: parent
    type(hsd_error_t), allocatable, intent(out) :: error

    character(len=:), allocatable :: include_path, abs_path
    type(parser_state_t) :: include_state

    ! Get the include filename
    include_path = trim(state%current_token%value)
    call state%next_token()

    ! Resolve relative path
    abs_path = resolve_path(state%base_dir, include_path)

    ! Check for cycle
    if (state%is_include_cycle(abs_path)) then
      call make_error(error, HSD_STAT_INCLUDE_CYCLE, &
        "Cyclic include detected in HSD include", &
        state%lexer%filename, &
        state%current_token%line, &
        column=state%current_token%column, &
        actual=abs_path, &
        hint="This file is already being processed in the include chain")
      return
    end if

    ! Push onto include stack
    call state%push_include(abs_path, error)
    if (allocated(error)) return

    ! Create new lexer for included file
    call new_lexer_from_file(include_state%lexer, abs_path, error)
    if (allocated(error)) then
      call state%pop_include()
      return
    end if

    ! Copy include stack
    include_state%include_stack = state%include_stack
    include_state%include_depth = state%include_depth
    include_state%base_dir = get_directory(abs_path)

    ! Parse included file
    call include_state%next_token()
    call parse_content(include_state, parent, error)

    ! Pop from stack
    call state%pop_include()

  end subroutine handle_hsd_include

  !> Handle <<< text include
  subroutine handle_text_include(state, text_buffer, error)
    type(parser_state_t), intent(inout) :: state
    character(len=:), allocatable, intent(inout) :: text_buffer
    type(hsd_error_t), allocatable, intent(out) :: error

    character(len=:), allocatable :: include_path, abs_path
    character(len=:), allocatable :: file_content
    integer :: unit_num, io_stat, file_size
    logical :: file_exists

    ! Get the include filename
    include_path = trim(state%current_token%value)
    call state%next_token()

    ! Resolve relative path
    abs_path = resolve_path(state%base_dir, include_path)

    ! Check file exists
    inquire(file=abs_path, exist=file_exists)
    if (.not. file_exists) then
      call make_error(error, HSD_STAT_FILE_NOT_FOUND, &
        "Text include file not found", &
        state%lexer%filename, &
        state%current_token%line, &
        column=state%current_token%column, &
        expected="readable file", &
        actual=abs_path, &
        hint="Check that the file path is correct and the file exists")
      return
    end if

    ! Read file content
    inquire(file=abs_path, size=file_size)
    allocate(character(len=file_size) :: file_content)

    open(newunit=unit_num, file=abs_path, status='old', action='read', &
         access='stream', form='unformatted', iostat=io_stat)
    if (io_stat /= 0) then
      call make_error(error, HSD_STAT_IO_ERROR, &
        "Cannot read text include file", &
        state%lexer%filename, &
        state%current_token%line, &
        column=state%current_token%column, &
        actual=abs_path, &
        hint="Check file permissions and that the file is readable")
      return
    end if

    read(unit_num, iostat=io_stat) file_content
    close(unit_num)

    ! Strip HSD comments (# to end-of-line) from included text content.
    ! This matches the inline HSD text behavior where the lexer consumes
    ! #-comments.  Without this, files included via <<< would contain
    ! comment lines that downstream readers (e.g. GenFormat) cannot handle.
    call strip_hsd_comments_(file_content)

    ! Append to text buffer
    if (len(text_buffer) > 0) then
      text_buffer = text_buffer // CHAR_NEWLINE // file_content
    else
      text_buffer = file_content
    end if

  end subroutine handle_text_include

  ! ---- TEXT UTILITIES ----

  !> Strip HSD-style comments (# to end-of-line) from text.
  !>
  !> Mimics the lexer behavior for inline HSD text: everything from an
  !> unquoted '#' to the next newline is removed, including the '#' itself.
  !> Lines that consist entirely of a comment (only whitespace before '#')
  !> are removed completely (including the trailing newline) so that
  !> downstream readers do not see spurious blank lines.
  !> Blank lines (containing only whitespace) are also removed to match
  !> the behavior of the legacy HSD parser.
  subroutine strip_hsd_comments_(text)
    character(len=:), allocatable, intent(inout) :: text

    character(len=:), allocatable :: buf
    integer :: i, n, out_pos, line_start, line_data_start
    logical :: in_comment, line_is_comment_only, line_is_blank

    if (.not. allocated(text)) return
    n = len(text)
    if (n == 0) return

    allocate(character(len=n) :: buf)
    out_pos = 0
    in_comment = .false.
    line_is_comment_only = .false.
    line_is_blank = .true.
    line_start = 1
    line_data_start = out_pos + 1

    do i = 1, n
      if (text(i:i) == char(10) .or. text(i:i) == char(13)) then
        ! End of line
        if (.not. line_is_comment_only .and. .not. line_is_blank) then
          ! Keep the newline
          out_pos = out_pos + 1
          buf(out_pos:out_pos) = text(i:i)
        else if (line_is_blank) then
          ! Blank line: remove any whitespace that was already output for this line
          out_pos = line_data_start - 1
        end if
        ! Reset for next line
        in_comment = .false.
        line_is_comment_only = .false.
        line_is_blank = .true.
        line_start = i + 1
        line_data_start = out_pos + 1
      else if (.not. in_comment .and. text(i:i) == '#') then
        ! Start of comment
        in_comment = .true.
        ! Check if only whitespace has been output for this line
        if (out_pos < line_data_start) then
          line_is_comment_only = .true.
        else
          ! Check if everything from line_data_start to out_pos is whitespace
          line_is_comment_only = .true.
          block
            integer :: j
            do j = line_data_start, out_pos
              if (buf(j:j) /= ' ' .and. buf(j:j) /= char(9)) then
                line_is_comment_only = .false.
                exit
              end if
            end do
          end block
          if (line_is_comment_only) then
            ! Remove whitespace before the comment
            out_pos = line_data_start - 1
          end if
        end if
      else if (.not. in_comment) then
        out_pos = out_pos + 1
        buf(out_pos:out_pos) = text(i:i)
        if (text(i:i) /= ' ' .and. text(i:i) /= char(9)) then
          line_is_blank = .false.
        end if
      end if
    end do

    if (out_pos > 0) then
      text = buf(1:out_pos)
    else
      text = ""
    end if

  end subroutine strip_hsd_comments_

  !> Strip trailing newlines (and spaces) from a string.
  pure function strip_trailing_nl(str) result(trimmed)
    character(len=*), intent(in) :: str
    character(len=:), allocatable :: trimmed

    integer :: last

    last = len(str)
    do while (last > 0)
      if (str(last:last) == char(10) .or. str(last:last) == char(13) &
          & .or. str(last:last) == ' ') then
        last = last - 1
      else
        exit
      end if
    end do

    if (last > 0) then
      trimmed = str(1:last)
    else
      trimmed = ""
    end if

  end function strip_trailing_nl

  !> Add text content to parent as a value node.
  !>
  !> Uses the name "#text" to match the legacy xmlf90 convention for inline
  !> text content. This allows hsd_get(table, "#text", val) to access inline
  !> text uniformly.
  subroutine add_text_to_parent(parent, text, line)
    type(hsd_node_t), intent(inout) :: parent
    character(len=*), intent(in) :: text
    integer, intent(in) :: line

    type(hsd_node_t) :: val

    call new_value(val, "#text", "", line)
    call val%set_raw(text)
    call parent%add_child(val)

  end subroutine add_text_to_parent

  !> Get directory part of a path
  pure function get_directory(path) result(dir)
    character(len=*), intent(in) :: path
    character(len=:), allocatable :: dir

    integer :: last_sep

    last_sep = index(path, "/", back=.true.)
    if (last_sep > 0) then
      dir = path(1:last_sep-1)
    else
      dir = "."
    end if

  end function get_directory

  !> Resolve a relative path against a base directory
  pure function resolve_path(base_dir, rel_path) result(abs_path)
    character(len=*), intent(in) :: base_dir
    character(len=*), intent(in) :: rel_path
    character(len=:), allocatable :: abs_path

    ! If already absolute, return as-is
    if (len(rel_path) > 0) then
      if (rel_path(1:1) == "/") then
        abs_path = rel_path
        return
      end if
    end if

    ! Combine with base directory
    if (base_dir == "." .or. len_trim(base_dir) == 0) then
      abs_path = rel_path
    else
      abs_path = trim(base_dir) // "/" // trim(rel_path)
    end if

  end function resolve_path

end module hsd_parser
