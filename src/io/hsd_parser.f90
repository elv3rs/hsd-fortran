!> HSD Parser
!>
!> This module provides the main parsing functionality for HSD files.
!> It converts a token stream into a tree of hsd_table and hsd_value nodes.
!> Includes cycle detection for <<+ includes.
module hsd_parser
  use hsd_constants, only: dp, hsd_max_include_depth, CHAR_NEWLINE
  use hsd_token, only: hsd_token_t, TOKEN_EOF, TOKEN_STRING, &
    TOKEN_LBRACE, TOKEN_RBRACE, TOKEN_EQUAL, TOKEN_LBRACKET, TOKEN_RBRACKET, &
    TOKEN_INCLUDE_TXT, TOKEN_INCLUDE_HSD, TOKEN_SEMICOLON, TOKEN_COMMENT, &
    TOKEN_TEXT, TOKEN_NEWLINE, TOKEN_WHITESPACE
  use hsd_lexer, only: hsd_lexer_t, new_lexer_from_file, new_lexer_from_string
  use hsd_types, only: hsd_node, hsd_table, hsd_value, hsd_node_ptr, &
    new_table, new_value, VALUE_TYPE_NONE, VALUE_TYPE_ARRAY, VALUE_TYPE_STRING
  use hsd_error, only: hsd_error_t, make_error, &
    HSD_STAT_OK, HSD_STAT_SYNTAX_ERROR, HSD_STAT_FILE_NOT_FOUND, &
    HSD_STAT_IO_ERROR, HSD_STAT_INCLUDE_CYCLE, HSD_STAT_INCLUDE_DEPTH, &
    HSD_STAT_UNCLOSED_ATTRIB
  use hsd_utils, only: to_lower
  implicit none (type, external)
  private

  public :: hsd_parse, hsd_parse_string

  !> Include stack item for cycle detection
  type :: include_item
    character(len=:), allocatable :: path
  end type include_item

  !> Parser state
  type :: parser_state
    !> Current lexer
    type(hsd_lexer_t) :: lexer
    !> Current token
    type(hsd_token_t) :: current_token
    !> Include stack for cycle detection
    type(include_item), allocatable :: include_stack(:)
    !> Current include depth
    integer :: include_depth = 0
    !> Base directory for relative includes
    character(len=:), allocatable :: base_dir
    !> Error if any occurred
    type(hsd_error_t), allocatable :: error
  contains
    procedure :: next_token => parser_next_token
    procedure :: skip_ws_comments => parser_skip_ws_comments
    procedure :: push_include => parser_push_include
    procedure :: pop_include => parser_pop_include
    procedure :: is_include_cycle => parser_is_cycle
  end type parser_state

contains

  !> Parse an HSD file into a tree structure
  subroutine hsd_parse(filename, root, error)
    character(len=*), intent(in) :: filename
    type(hsd_table), intent(out) :: root
    type(hsd_error_t), allocatable, intent(out), optional :: error

    type(parser_state) :: state
    type(hsd_error_t), allocatable :: local_error
    character(len=:), allocatable :: abs_path

    ! Always initialize root so callers get a valid (empty) table even on error
    call new_table(root)

    ! Get absolute path
    abs_path = get_absolute_path(filename)

    ! Initialize lexer
    call new_lexer_from_file(state%lexer, abs_path, local_error)
    if (allocated(local_error)) then
      if (present(error)) call move_alloc(local_error, error)
      return
    end if

    ! Initialize parser state
    state%base_dir = get_directory(abs_path)
    allocate(state%include_stack(hsd_max_include_depth))
    state%include_depth = 0

    ! Push current file onto include stack
    call state%push_include(abs_path, local_error)
    if (allocated(local_error)) then
      if (present(error)) call move_alloc(local_error, error)
      return
    end if

    ! Get first token
    call state%next_token()

    ! Parse content
    call parse_content(state, root, local_error)

    ! Pop include stack
    call state%pop_include()

    if (allocated(local_error)) then
      if (present(error)) call move_alloc(local_error, error)
    end if

  end subroutine hsd_parse

  !> Parse HSD from a string
  subroutine hsd_parse_string(source, root, error, filename)
    character(len=*), intent(in) :: source
    type(hsd_table), intent(out) :: root
    type(hsd_error_t), allocatable, intent(out), optional :: error
    character(len=*), intent(in), optional :: filename

    type(parser_state) :: state
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

    ! Get first token
    call state%next_token()

    ! Parse content
    call parse_content(state, root, local_error)

    if (allocated(local_error)) then
      if (present(error)) call move_alloc(local_error, error)
    end if

  end subroutine hsd_parse_string

  !> Get next meaningful token (skipping whitespace)
  subroutine parser_next_token(self)
    class(parser_state), intent(inout) :: self
    call self%lexer%next_token(self%current_token)
  end subroutine parser_next_token

  !> Skip whitespace and comments
  subroutine parser_skip_ws_comments(self)
    class(parser_state), intent(inout) :: self

    do while (self%current_token%kind == TOKEN_WHITESPACE .or. &
              self%current_token%kind == TOKEN_COMMENT .or. &
              self%current_token%kind == TOKEN_NEWLINE)
      call self%next_token()
    end do

  end subroutine parser_skip_ws_comments

  !> Push file onto include stack
  subroutine parser_push_include(self, path, error)
    class(parser_state), intent(inout) :: self
    character(len=*), intent(in) :: path
    type(hsd_error_t), allocatable, intent(out), optional :: error

    ! Check for cycle

    ! Callers (handle_hsd_include) already check for cycles before calling push_include
    if (self%is_include_cycle(path)) then
      if (present(error)) then
        call make_error(error, HSD_STAT_INCLUDE_CYCLE, &
          "Cyclic include detected", &
          self%lexer%filename, &
          self%current_token%line, &
          column=self%current_token%column, &
          actual=path, &
          hint="This file is already being processed in the include chain")
      end if
      return
    end if


    ! Check depth limit

    ! Requires 10+ unique include files to trigger; callers catch common cases
    if (self%include_depth >= hsd_max_include_depth) then
      if (present(error)) then
        call make_error(error, HSD_STAT_INCLUDE_DEPTH, &
          "Maximum include depth exceeded", &
          self%lexer%filename, &
          self%current_token%line, &
          column=self%current_token%column, &
          actual=path, &
          hint="Reduce nesting of include directives")
      end if
      return
    end if


    ! Push onto stack
    self%include_depth = self%include_depth + 1
    self%include_stack(self%include_depth)%path = path

  end subroutine parser_push_include

  !> Pop file from include stack
  subroutine parser_pop_include(self)
    class(parser_state), intent(inout) :: self

    if (self%include_depth > 0) then
      if (allocated(self%include_stack(self%include_depth)%path)) then
        deallocate(self%include_stack(self%include_depth)%path)
      end if
      self%include_depth = self%include_depth - 1
    end if

  end subroutine parser_pop_include

  !> Check if path would create a cycle
  function parser_is_cycle(self, path) result(is_cycle)
    class(parser_state), intent(in) :: self
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

  !> Parse content (multiple tags/values)
  recursive subroutine parse_content(state, parent, error)
    type(parser_state), intent(inout) :: state
    type(hsd_table), intent(inout) :: parent
    type(hsd_error_t), allocatable, intent(out), optional :: error

    type(hsd_error_t), allocatable :: local_error
    character(len=:), allocatable :: text_buffer
    integer :: text_start_line

    text_buffer = ""
    text_start_line = 0

    do while (.not. state%current_token%is_eof())
      ! Skip whitespace and comments.  When a comment is followed by a
      ! newline, consume the newline as well so that comment-only lines
      ! do not leave spurious blank lines in the text buffer.
      block
        logical :: had_comment
        had_comment = .false.
        do while (state%current_token%kind == TOKEN_WHITESPACE .or. &
                  state%current_token%kind == TOKEN_COMMENT)
          if (state%current_token%kind == TOKEN_COMMENT) had_comment = .true.
          call state%next_token()
        end do
        if (had_comment .and. state%current_token%kind == TOKEN_NEWLINE) then
          call state%next_token()
          cycle
        end if
      end block

      ! Preserve newlines as content separators when buffering inline text.
      ! Without text buffered, newlines are simply skipped (e.g. between tags).
      if (state%current_token%kind == TOKEN_NEWLINE) then
        if (len(text_buffer) > 0) then
          text_buffer = text_buffer // char(10)
        end if
        call state%next_token()
        cycle
      end if

      if (state%current_token%is_eof()) exit

      select case (state%current_token%kind)
      case (TOKEN_RBRACE)
        ! End of current block - return to parent
        ! Flush any buffered text first (strip trailing newlines)
        if (len_trim(text_buffer) > 0) then
          call add_text_to_parent(parent, strip_trailing_nl(text_buffer), text_start_line)
          text_buffer = ""
        end if
        exit

      case (TOKEN_TEXT)
        ! Could be tag name or data
        call parse_tag_or_value(state, parent, text_buffer, text_start_line, local_error)
        if (allocated(local_error)) then
          if (present(error)) call move_alloc(local_error, error)
          return
        end if

      case (TOKEN_STRING)
        ! String data
        if (len(text_buffer) > 0) then
          if (text_buffer(len(text_buffer):len(text_buffer)) == char(10)) then
            text_buffer = text_buffer // state%current_token%value
          else
            text_buffer = text_buffer // " " // state%current_token%value
          end if
        else
          text_buffer = state%current_token%value
          text_start_line = state%current_token%line
        end if
        call state%next_token()

      case (TOKEN_INCLUDE_HSD)
        ! <<+ include
        call handle_hsd_include(state, parent, local_error)
        if (allocated(local_error)) then
          if (present(error)) call move_alloc(local_error, error)
          return
        end if

      case (TOKEN_INCLUDE_TXT)
        ! <<< include
        call handle_text_include(state, text_buffer, local_error)
        if (allocated(local_error)) then
          if (present(error)) call move_alloc(local_error, error)
          return
        end if

      case (TOKEN_NEWLINE)
        ! Newlines should have been handled above the select case.
        ! This is a safety fallthrough for edge cases.
        if (len(text_buffer) > 0) then
          text_buffer = text_buffer // char(10)
        end if
        call state%next_token()

      case default
        call state%next_token()
      end select
    end do

    ! Flush remaining text buffer (strip trailing newlines)
    if (len_trim(text_buffer) > 0) then
      call add_text_to_parent(parent, strip_trailing_nl(text_buffer), text_start_line)
    end if

  end subroutine parse_content

  !> Parse a tag (possibly with value) or just data
  recursive subroutine parse_tag_or_value(state, parent, text_buffer, text_start_line, error)
    type(parser_state), intent(inout) :: state
    type(hsd_table), intent(inout) :: parent
    character(len=:), allocatable, intent(inout) :: text_buffer
    integer, intent(inout) :: text_start_line
    type(hsd_error_t), allocatable, intent(out), optional :: error

    character(len=:), allocatable :: tag_name, attrib
    character(len=:), allocatable :: original_text
    integer :: tag_line
    type(hsd_token_t) :: saved_token
    type(hsd_table) :: child_table
    type(hsd_value) :: child_value
    type(hsd_error_t), allocatable :: local_error
    character(len=:), allocatable :: value_text
    logical :: is_amendment
    class(hsd_node), pointer :: existing_child
    type(hsd_table), pointer :: existing_table

    ! Save current state — preserve original text for data fallback, lowercase for tag use
    original_text = trim(state%current_token%value)
    tag_name = to_lower(original_text)
    tag_line = state%current_token%line
    call state%next_token()

    ! Check for amendment prefix (+Tag means merge into existing Tag)
    is_amendment = .false.
    if (len(tag_name) > 1 .and. tag_name(1:1) == "+") then
      is_amendment = .true.
      tag_name = tag_name(2:)
    end if

    ! Skip whitespace (lexer already skips, defensive)
    do while (state%current_token%kind == TOKEN_WHITESPACE)
      call state%next_token()
    end do

    ! Check for attribute [...]
    attrib = ""
    if (state%current_token%kind == TOKEN_LBRACKET) then
      call state%next_token()
      call parse_attribute(state, attrib, local_error)
      if (allocated(local_error)) then
        if (present(error)) call move_alloc(local_error, error)
        return
      end if
    end if

    ! Skip whitespace again (lexer already skips, defensive)
    do while (state%current_token%kind == TOKEN_WHITESPACE)
      call state%next_token()
    end do

    ! Determine what follows
    select case (state%current_token%kind)
    case (TOKEN_LBRACE)
      ! Block: Tag { ... } or +Tag { ... } (amendment)
      ! First flush text buffer
      if (len_trim(text_buffer) > 0) then
        call add_text_to_parent(parent, trim(text_buffer), text_start_line)
        text_buffer = ""
      end if

      call state%next_token()  ! consume {

      if (is_amendment) then
        ! Amendment: find existing child and merge into it
        existing_child => null()
        call parent%get_child_by_name(tag_name, existing_child, case_insensitive=.true.)
        if (.not. associated(existing_child)) then
          if (present(error)) then
            call make_error(error, HSD_STAT_SYNTAX_ERROR, &
              "Amendment target '" // tag_name // "' not found in parent", &
              state%lexer%filename, tag_line)
          end if
          return
        end if
        select type (existing_child)
        type is (hsd_table)
          existing_table => existing_child
          call parse_content(state, existing_table, local_error)
        class default
          if (present(error)) then
            call make_error(error, HSD_STAT_SYNTAX_ERROR, &
              "Amendment target '" // tag_name // "' is not a block", &
              state%lexer%filename, tag_line)
          end if
          return
        end select
      else
        call new_table(child_table, tag_name, attrib, tag_line)
        call parse_content(state, child_table, local_error)
      end if

      if (allocated(local_error)) then
        if (present(error)) call move_alloc(local_error, error)
        return
      end if

      ! Expect closing brace
      if (state%current_token%kind == TOKEN_RBRACE) then
        call state%next_token()  ! consume }
      end if

      if (.not. is_amendment) then
        call parent%add_child(child_table)
      end if

    case (TOKEN_EQUAL)
      ! Assignment: Tag = value or Tag = ChildTag { ... }
      ! First flush text buffer
      if (len_trim(text_buffer) > 0) then
        call add_text_to_parent(parent, trim(text_buffer), text_start_line)
        text_buffer = ""
      end if

      call state%next_token()  ! consume =

      ! Skip whitespace (lexer already skips, defensive)
      do while (state%current_token%kind == TOKEN_WHITESPACE)
        call state%next_token()
      end do

      ! Check what follows =
      if (state%current_token%kind == TOKEN_LBRACE) then
        ! Tag = { ... } - direct block
        call state%next_token()
        call new_table(child_table, tag_name, attrib, tag_line)
        call parse_content(state, child_table, local_error)
        if (allocated(local_error)) then
          if (present(error)) call move_alloc(local_error, error)
          return
        end if
        if (state%current_token%kind == TOKEN_RBRACE) then
          call state%next_token()
        end if
        call parent%add_child(child_table)

      else if (state%current_token%kind == TOKEN_TEXT) then
        ! Could be: Tag = value OR Tag = ChildTag { ... }
        saved_token = state%current_token
        call state%next_token()

        ! Skip whitespace (lexer already skips, defensive)
        do while (state%current_token%kind == TOKEN_WHITESPACE)
          call state%next_token()
        end do

        if (state%current_token%kind == TOKEN_LBRACE) then
          ! Tag = ChildTag { ... } or +Tag = +ChildTag { ... }
          call state%next_token()  ! consume {

          block
            character(len=:), allocatable :: child_tag_name
            logical :: child_is_amendment
            type(hsd_table), pointer :: target_table

            child_tag_name = to_lower(trim(saved_token%value))
            child_is_amendment = .false.
            if (len(child_tag_name) > 1 .and. child_tag_name(1:1) == "+") then
              child_is_amendment = .true.
              child_tag_name = child_tag_name(2:)
            end if

            if (is_amendment) then
              ! +Tag = +ChildTag { ... } — amend existing Tag, then amend ChildTag inside
              existing_child => null()
              call parent%get_child_by_name(tag_name, existing_child, case_insensitive=.true.)
              if (.not. associated(existing_child)) then
                if (present(error)) then
                  call make_error(error, HSD_STAT_SYNTAX_ERROR, &
                    "Amendment target '" // tag_name // "' not found", &
                    state%lexer%filename, tag_line)
                end if
                return
              end if
              select type (existing_child)
              type is (hsd_table)
                existing_table => existing_child
              class default
                if (present(error)) then
                  call make_error(error, HSD_STAT_SYNTAX_ERROR, &
                    "Amendment target '" // tag_name // "' is not a block", &
                    state%lexer%filename, tag_line)
                end if
                return
              end select

              if (child_is_amendment) then
                ! Find ChildTag inside the existing Tag and merge into it
                existing_child => null()
                call existing_table%get_child_by_name(child_tag_name, existing_child, &
                    case_insensitive=.true.)
                if (.not. associated(existing_child)) then
                  if (present(error)) then
                    call make_error(error, HSD_STAT_SYNTAX_ERROR, &
                      "Amendment target '" // child_tag_name // "' not found in '" &
                      // tag_name // "'", state%lexer%filename, saved_token%line)
                  end if
                  return
                end if
                select type (existing_child)
                type is (hsd_table)
                  call parse_content(state, existing_child, local_error)
                class default
                  if (present(error)) then
                    call make_error(error, HSD_STAT_SYNTAX_ERROR, &
                      "Amendment target '" // child_tag_name // "' is not a block", &
                      state%lexer%filename, saved_token%line)
                  end if
                  return
                end select
              else
                ! Regular child inside amended parent
                block
                  type(hsd_table) :: nested_table
                  call new_table(nested_table, child_tag_name, "", saved_token%line)
                  call parse_content(state, nested_table, local_error)
                  if (allocated(local_error)) then
                    if (present(error)) call move_alloc(local_error, error)
                    return
                  end if
                  call existing_table%add_child(nested_table)
                end block
              end if
            else
              ! Normal: Tag = ChildTag { ... }
              call new_table(child_table, tag_name, attrib, tag_line)

              block
                type(hsd_table) :: nested_table
                call new_table(nested_table, child_tag_name, "", saved_token%line)
                call parse_content(state, nested_table, local_error)
                if (allocated(local_error)) then
                  if (present(error)) call move_alloc(local_error, error)
                  return
                end if
                call child_table%add_child(nested_table)
              end block

              call parent%add_child(child_table)
            end if

            if (allocated(local_error)) then
              if (present(error)) call move_alloc(local_error, error)
              return
            end if
            if (state%current_token%kind == TOKEN_RBRACE) then
              call state%next_token()
            end if
          end block

        else
          ! Tag = value (simple assignment)
          value_text = trim(saved_token%value)

          ! Collect rest of line
          do while (state%current_token%kind /= TOKEN_NEWLINE .and. &
                    state%current_token%kind /= TOKEN_EOF .and. &
                    state%current_token%kind /= TOKEN_SEMICOLON .and. &
                    state%current_token%kind /= TOKEN_RBRACE .and. &
                    state%current_token%kind /= TOKEN_COMMENT)
            if (state%current_token%kind == TOKEN_TEXT .or. &
                state%current_token%kind == TOKEN_STRING) then
              value_text = value_text // " " // state%current_token%value
            end if
            call state%next_token()
          end do

          ! Handle semicolon terminator
          if (state%current_token%kind == TOKEN_SEMICOLON) then
            call state%next_token()
          end if

          call new_value(child_value, tag_name, attrib, tag_line)
          call child_value%set_string(trim(value_text))
          call parent%add_child(child_value)
        end if

      else if (state%current_token%kind == TOKEN_STRING) then
        ! Tag = "string value" (possibly followed by more values on same line)
        value_text = state%current_token%value
        call state%next_token()

        ! Collect rest of line (handles: Tag = "val1" "val2" "val3")
        do while (state%current_token%kind /= TOKEN_NEWLINE .and. &
                  state%current_token%kind /= TOKEN_EOF .and. &
                  state%current_token%kind /= TOKEN_SEMICOLON .and. &
                  state%current_token%kind /= TOKEN_RBRACE .and. &
                  state%current_token%kind /= TOKEN_COMMENT)
          if (state%current_token%kind == TOKEN_TEXT .or. &
              state%current_token%kind == TOKEN_STRING) then
            value_text = value_text // " " // state%current_token%value
          end if
          call state%next_token()
        end do

        ! Handle semicolon terminator
        if (state%current_token%kind == TOKEN_SEMICOLON) then
          call state%next_token()
        end if

        call new_value(child_value, tag_name, attrib, tag_line)
        call child_value%set_string(trim(value_text))
        call parent%add_child(child_value)

      else
        ! Empty value
        call new_value(child_value, tag_name, attrib, tag_line)
        call child_value%set_string("")
        call parent%add_child(child_value)
      end if

    case (TOKEN_NEWLINE, TOKEN_EOF, TOKEN_RBRACE, TOKEN_SEMICOLON)
      ! Just a tag name on its own - treat as text (preserve original case)
      if (len(text_buffer) > 0) then
        if (text_buffer(len(text_buffer):len(text_buffer)) == char(10)) then
          text_buffer = text_buffer // original_text
        else
          text_buffer = text_buffer // " " // original_text
        end if
      else
        text_buffer = original_text
        text_start_line = tag_line
      end if

    case default
      ! Treat as part of text (preserve original case)
      if (len(text_buffer) > 0) then
        if (text_buffer(len(text_buffer):len(text_buffer)) == char(10)) then
          text_buffer = text_buffer // original_text
        else
          text_buffer = text_buffer // " " // original_text
        end if
      else
        text_buffer = original_text
        text_start_line = tag_line
      end if
    end select

  end subroutine parse_tag_or_value

  !> Parse attribute content between [ and ]
  subroutine parse_attribute(state, attrib, error)
    type(parser_state), intent(inout) :: state
    character(len=:), allocatable, intent(out) :: attrib
    type(hsd_error_t), allocatable, intent(out), optional :: error

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
    else if (present(error)) then
      call make_error(error, HSD_STAT_UNCLOSED_ATTRIB, &
        "Unclosed attribute bracket", &
        state%lexer%filename, &
        state%current_token%line, &
        column=state%current_token%column, &
        expected="]", &
        actual=trim(state%current_token%value), &
        hint="Add closing ']' to complete the attribute")
    end if

  end subroutine parse_attribute

  !> Handle <<+ HSD include
  recursive subroutine handle_hsd_include(state, parent, error)
    type(parser_state), intent(inout) :: state
    type(hsd_table), intent(inout) :: parent
    type(hsd_error_t), allocatable, intent(out), optional :: error

    character(len=:), allocatable :: include_path, abs_path
    type(parser_state) :: include_state
    type(hsd_error_t), allocatable :: local_error

    ! Get the include filename
    include_path = trim(state%current_token%value)
    call state%next_token()

    ! Resolve relative path
    abs_path = resolve_path(state%base_dir, include_path)

    ! Check for cycle
    if (state%is_include_cycle(abs_path)) then
      if (present(error)) then
        call make_error(error, HSD_STAT_INCLUDE_CYCLE, &
          "Cyclic include detected in HSD include", &
          state%lexer%filename, &
          state%current_token%line, &
          column=state%current_token%column, &
          actual=abs_path, &
          hint="This file is already being processed in the include chain")
      end if
      return
    end if

    ! Push onto include stack
    call state%push_include(abs_path, local_error)
    if (allocated(local_error)) then
      if (present(error)) call move_alloc(local_error, error)
      return
    end if

    ! Create new lexer for included file
    call new_lexer_from_file(include_state%lexer, abs_path, local_error)
    if (allocated(local_error)) then
      call state%pop_include()
      if (present(error)) call move_alloc(local_error, error)
      return
    end if

    ! Copy include stack
    include_state%include_stack = state%include_stack
    include_state%include_depth = state%include_depth
    include_state%base_dir = get_directory(abs_path)

    ! Parse included file
    call include_state%next_token()
    call parse_content(include_state, parent, local_error)

    ! Pop from stack
    call state%pop_include()

    if (allocated(local_error)) then
      if (present(error)) call move_alloc(local_error, error)
    end if

  end subroutine handle_hsd_include

  !> Handle <<< text include
  subroutine handle_text_include(state, text_buffer, error)
    type(parser_state), intent(inout) :: state
    character(len=:), allocatable, intent(inout) :: text_buffer
    type(hsd_error_t), allocatable, intent(out), optional :: error

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
      if (present(error)) then
        call make_error(error, HSD_STAT_FILE_NOT_FOUND, &
          "Text include file not found", &
          state%lexer%filename, &
          state%current_token%line, &
          column=state%current_token%column, &
          expected="readable file", &
          actual=abs_path, &
          hint="Check that the file path is correct and the file exists")
      end if
      return
    end if

    ! Read file content
    inquire(file=abs_path, size=file_size)
    allocate(character(len=file_size) :: file_content)

    open(newunit=unit_num, file=abs_path, status='old', action='read', &
         access='stream', form='unformatted', iostat=io_stat)
    if (io_stat /= 0) then
      if (present(error)) then
        call make_error(error, HSD_STAT_IO_ERROR, &
          "Cannot read text include file", &
          state%lexer%filename, &
          state%current_token%line, &
          column=state%current_token%column, &
          actual=abs_path, &
          hint="Check file permissions and that the file is readable")
      end if
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

  !> Strip HSD-style comments (# to end-of-line) from text.
  !>
  !> Mimics the lexer behavior for inline HSD text: everything from an
  !> unquoted '#' to the next newline is removed, including the '#' itself.
  !> Lines that consist entirely of a comment (only whitespace before '#')
  !> are removed completely (including the trailing newline) so that
  !> downstream readers do not see spurious blank lines.
  subroutine strip_hsd_comments_(text)
    character(len=:), allocatable, intent(inout) :: text

    character(len=:), allocatable :: buf
    integer :: i, n, out_pos, line_start, line_data_start
    logical :: in_comment, line_is_comment_only

    if (.not. allocated(text)) return
    n = len(text)
    if (n == 0) return

    allocate(character(len=n) :: buf)
    out_pos = 0
    in_comment = .false.
    line_is_comment_only = .false.
    line_start = 1
    line_data_start = out_pos + 1

    do i = 1, n
      if (text(i:i) == char(10) .or. text(i:i) == char(13)) then
        ! End of line
        if (.not. line_is_comment_only) then
          ! Keep the newline
          out_pos = out_pos + 1
          buf(out_pos:out_pos) = text(i:i)
        end if
        ! Reset for next line
        in_comment = .false.
        line_is_comment_only = .false.
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
      end if
    end do

    if (out_pos > 0) then
      text = buf(1:out_pos)
    else
      text = ""
    end if

  end subroutine strip_hsd_comments_

  !> Add text content to parent as a value node.
  !>
  !> Uses the name "#text" to match the legacy xmlf90 convention for inline
  !> text content. This allows hsd_get(table, "#text", val) to access inline
  !> text uniformly.
  subroutine add_text_to_parent(parent, text, line)
    type(hsd_table), intent(inout) :: parent
    character(len=*), intent(in) :: text
    integer, intent(in) :: line

    type(hsd_value) :: val

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

  !> Get absolute path by prepending the current working directory
  !>
  !> Returns the path as is (C interop removed).
  !> Originally resolved to absolute path using C getcwd, but explicit C interop
  !> has been removed.
  function get_absolute_path(path) result(abs_path)
    character(len=*), intent(in) :: path
    character(len=:), allocatable :: abs_path

    abs_path = path

  end function get_absolute_path

end module hsd_parser
