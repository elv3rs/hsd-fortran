!> DFTB+ compatibility layer for hsd-fortran
!>
!> Provides DFTB+-style HSD API (getChildValue, getChild, setChildValue, etc.)
!> on top of hsd-fortran's native types. This allows DFTB+ to replace xmlf90
!> with hsd-fortran by changing only `use` statements.
!>
!> ## Migration guide
!>
!> Replace xmlf90/hsdutils imports:
!> ```fortran
!> ! Before:
!> use dftbp_io_hsdutils, only: getChildValue, getChild, setChildValue
!> use dftbp_extlibs_xmlf90, only: fnode => fnode, string, char
!> ! After:
!> use hsd_compat, only: getChildValue, getChild, setChildValue, &
!>     & fnode => hsd_node, string, char
!> ```
module hsd_compat
  use hsd_constants, only: dp
  use hsd_error, only: hsd_error_t, HSD_STAT_OK, HSD_STAT_NOT_FOUND, &
      & HSD_STAT_TYPE_ERROR
  use hsd_types, only: hsd_node, hsd_node_ptr, hsd_iterator, &
      & new_table, new_value, NODE_TYPE_TABLE, NODE_TYPE_VALUE
  use hsd_api, only: hsd_get, hsd_get_or_set, hsd_get_matrix, &
      & hsd_get_child, hsd_get_inline_text, hsd_has_child, &
      & hsd_set, hsd_set_processed, hsd_get_children, hsd_child_ptr, &
      & hsd_has_value_children, hsd_get_name, hsd_clear_children
  use hsd_validation, only: hsd_node_context, hsd_format_error, &
      & hsd_format_warning, hsd_warn_unprocessed, MAX_WARNING_LEN
  use hsd_parser, only: hsd_load_file, hsd_load_string
  use hsd_formatter, only: hsd_dump, hsd_dump_to_string
  use, intrinsic :: iso_fortran_env, only: error_unit
  implicit none (type, external)
  private

  ! Re-export native types (consumers rename: fnode => hsd_node)
  public :: hsd_node, hsd_node_ptr, hsd_iterator, dp
  public :: new_table, new_value
  public :: NODE_TYPE_TABLE, NODE_TYPE_VALUE
  public :: HSD_STAT_OK, HSD_STAT_NOT_FOUND, HSD_STAT_TYPE_ERROR
  public :: hsd_error_t

  ! Re-export native API that DFTB+ may need directly
  public :: hsd_get_inline_text, hsd_has_child
  public :: hsd_has_value_children, hsd_get_name
  public :: hsd_load_file, hsd_load_string

  !> Compatibility string type (replaces xmlf90's type(string))
  type, public :: string
    character(len=:), allocatable :: str
  end type string

  !> Overload char() intrinsic for string type extraction
  public :: char
  interface char
    module procedure string_to_char
  end interface

  !> Assignment from character to string
  public :: assignment(=)
  interface assignment(=)
    module procedure assign_string_from_char
    module procedure assign_char_from_string
  end interface

  ! --- DFTB+ compatible interfaces ---

  public :: getChildValue
  interface getChildValue
    module procedure getChVal_logical
    module procedure getChVal_int
    module procedure getChVal_real
    module procedure getChVal_cmplx
    module procedure getChVal_string
    module procedure getChVal_intR1
    module procedure getChVal_realR1
    module procedure getChVal_cmplxR1
    module procedure getChVal_logicalR1
    module procedure getChVal_realR2
  end interface

  public :: getChild
  public :: getChildren

  public :: setChildValue
  interface setChildValue
    module procedure setChVal_logical
    module procedure setChVal_int
    module procedure setChVal_real
    module procedure setChVal_cmplx
    module procedure setChVal_string
    module procedure setChVal_intR1
    module procedure setChVal_realR1
    module procedure setChVal_cmplxR1
    module procedure setChVal_logicalR1
    module procedure setChVal_realR2
  end interface

  public :: setChild
  public :: setProcessed
  public :: setUnprocessed
  public :: warnUnprocessedNodes
  public :: getNodeHSDName
  public :: getFirstTextChild
  public :: detailedError
  public :: detailedWarning
  public :: parseHSD
  public :: dumpHSD
  public :: destroyNode

  ! DOM compat for programmatic tree building (mmapi.F90)
  public :: createElement
  public :: appendChild
  public :: createDocumentNode

contains

  ! ================================================================
  ! String compat
  ! ================================================================

  !> Extract character from string type
  pure function string_to_char(ss) result(res)
    type(string), intent(in) :: ss
    character(len=:), allocatable :: res
    if (allocated(ss%str)) then
      res = ss%str
    else
      res = ""
    end if
  end function string_to_char

  !> Assign character to string
  pure subroutine assign_string_from_char(ss, cc)
    type(string), intent(out) :: ss
    character(len=*), intent(in) :: cc
    ss%str = cc
  end subroutine assign_string_from_char

  !> Assign string to character
  pure subroutine assign_char_from_string(cc, ss)
    character(len=:), allocatable, intent(out) :: cc
    type(string), intent(in) :: ss
    if (allocated(ss%str)) then
      cc = ss%str
    else
      cc = ""
    end if
  end subroutine assign_char_from_string

  ! ================================================================
  ! getChild — DFTB+ compatible child lookup
  ! ================================================================

  !> Get child node by name with DFTB+-compatible optional arguments
  !>
  !> If requested=.true. (default) and child not found, calls detailedError.
  !> If requested=.false. and child not found, returns null pointer.
  !> If emptyIfMissing=.true. and child not found, creates an empty child.
  subroutine getChild(node, name, child, requested, modifier, &
      & emptyIfMissing)
    type(hsd_node), intent(inout), target :: node
    character(len=*), intent(in) :: name
    type(hsd_node), pointer, intent(out) :: child
    logical, intent(in), optional :: requested
    type(string), intent(inout), optional :: modifier
    logical, intent(in), optional :: emptyIfMissing

    logical :: isRequested, emptyReturn
    integer :: stat

    isRequested = .true.
    if (present(requested)) isRequested = requested
    emptyReturn = .false.
    if (present(emptyIfMissing)) emptyReturn = emptyIfMissing

    call hsd_get_child(node, name, child, stat)

    if (stat /= HSD_STAT_OK) then
      if (emptyReturn) then
        call setChild(node, name, child)
        child%processed = .true.
        return
      end if
      if (isRequested) then
        call detailedError(node, &
            & "Missing required block: '" // name // "'")
      end if
      child => null()
      return
    end if

    child%processed = .true.

    if (present(modifier)) then
      if (allocated(child%attrib)) then
        modifier%str = child%attrib
      else
        if (allocated(modifier%str)) deallocate(modifier%str)
      end if
    end if

  end subroutine getChild

  ! ================================================================
  ! getChildren — get list of children with same name
  ! ================================================================

  !> Get all children with a given name
  subroutine getChildren(node, name, children)
    type(hsd_node), intent(in), target :: node
    character(len=*), intent(in) :: name
    type(hsd_child_ptr), allocatable, intent(out) :: children(:)

    call hsd_get_children(node, name, children)
  end subroutine getChildren

  ! ================================================================
  ! getChildValue — DFTB+ compatible value extraction
  ! ================================================================

  !> Internal helper: find child, handle not-found, mark processed
  subroutine find_child_(node, name, child_ptr, found)
    type(hsd_node), intent(in), target :: node
    character(len=*), intent(in) :: name
    type(hsd_node), pointer, intent(out) :: child_ptr
    logical, intent(out) :: found

    integer :: stat

    call hsd_get_child(node, name, child_ptr, stat)
    found = (stat == HSD_STAT_OK)

    if (found) child_ptr%processed = .true.

  end subroutine find_child_

  !> Internal helper: extract modifier from child
  subroutine extract_modifier_(child_ptr, modifier)
    type(hsd_node), intent(in) :: child_ptr
    type(string), intent(inout) :: modifier

    if (allocated(child_ptr%attrib)) then
      modifier%str = child_ptr%attrib
    else
      if (allocated(modifier%str)) deallocate(modifier%str)
    end if
  end subroutine extract_modifier_

  ! --- Logical scalar ---
  subroutine getChVal_logical(node, name, val, default, modifier, &
      & child)
    type(hsd_node), intent(inout), target :: node
    character(len=*), intent(in) :: name
    logical, intent(out) :: val
    logical, intent(in), optional :: default
    type(string), intent(inout), optional :: modifier
    type(hsd_node), pointer, intent(out), optional :: child

    type(hsd_node), pointer :: cp
    logical :: found
    integer :: stat

    call find_child_(node, name, cp, found)

    if (.not. found) then
      if (present(default)) then
        val = default
        call hsd_set(node, name, default, stat)
        call hsd_get_child(node, name, cp, stat)
        cp%processed = .true.
        if (present(child)) child => cp
        return
      end if
      call detailedError(node, &
          & "Missing required field: '" // name // "'")
    end if

    call hsd_get(node, name, val, stat)
    if (stat /= HSD_STAT_OK) then
      call detailedError(cp, &
          & "Invalid logical value for: '" // name // "'")
    end if

    if (present(modifier)) call extract_modifier_(cp, modifier)
    if (present(child)) child => cp

  end subroutine getChVal_logical

  ! --- Integer scalar ---
  subroutine getChVal_int(node, name, val, default, modifier, child)
    type(hsd_node), intent(inout), target :: node
    character(len=*), intent(in) :: name
    integer, intent(out) :: val
    integer, intent(in), optional :: default
    type(string), intent(inout), optional :: modifier
    type(hsd_node), pointer, intent(out), optional :: child

    type(hsd_node), pointer :: cp
    logical :: found
    integer :: stat

    call find_child_(node, name, cp, found)

    if (.not. found) then
      if (present(default)) then
        val = default
        call hsd_set(node, name, default, stat)
        call hsd_get_child(node, name, cp, stat)
        cp%processed = .true.
        if (present(child)) child => cp
        return
      end if
      call detailedError(node, &
          & "Missing required field: '" // name // "'")
    end if

    call hsd_get(node, name, val, stat)
    if (stat /= HSD_STAT_OK) then
      call detailedError(cp, &
          & "Invalid integer value for: '" // name // "'")
    end if

    if (present(modifier)) call extract_modifier_(cp, modifier)
    if (present(child)) child => cp

  end subroutine getChVal_int

  ! --- Real scalar ---
  subroutine getChVal_real(node, name, val, default, modifier, child)
    type(hsd_node), intent(inout), target :: node
    character(len=*), intent(in) :: name
    real(dp), intent(out) :: val
    real(dp), intent(in), optional :: default
    type(string), intent(inout), optional :: modifier
    type(hsd_node), pointer, intent(out), optional :: child

    type(hsd_node), pointer :: cp
    logical :: found
    integer :: stat

    call find_child_(node, name, cp, found)

    if (.not. found) then
      if (present(default)) then
        val = default
        call hsd_set(node, name, default, stat)
        call hsd_get_child(node, name, cp, stat)
        cp%processed = .true.
        if (present(child)) child => cp
        return
      end if
      call detailedError(node, &
          & "Missing required field: '" // name // "'")
    end if

    call hsd_get(node, name, val, stat)
    if (stat /= HSD_STAT_OK) then
      call detailedError(cp, &
          & "Invalid real value for: '" // name // "'")
    end if

    if (present(modifier)) call extract_modifier_(cp, modifier)
    if (present(child)) child => cp

  end subroutine getChVal_real

  ! --- Complex scalar ---
  subroutine getChVal_cmplx(node, name, val, default, modifier, &
      & child)
    type(hsd_node), intent(inout), target :: node
    character(len=*), intent(in) :: name
    complex(dp), intent(out) :: val
    complex(dp), intent(in), optional :: default
    type(string), intent(inout), optional :: modifier
    type(hsd_node), pointer, intent(out), optional :: child

    type(hsd_node), pointer :: cp
    logical :: found
    integer :: stat

    call find_child_(node, name, cp, found)

    if (.not. found) then
      if (present(default)) then
        val = default
        call hsd_set(node, name, default, stat)
        call hsd_get_child(node, name, cp, stat)
        cp%processed = .true.
        if (present(child)) child => cp
        return
      end if
      call detailedError(node, &
          & "Missing required field: '" // name // "'")
    end if

    call hsd_get(node, name, val, stat)
    if (stat /= HSD_STAT_OK) then
      call detailedError(cp, &
          & "Invalid complex value for: '" // name // "'")
    end if

    if (present(modifier)) call extract_modifier_(cp, modifier)
    if (present(child)) child => cp

  end subroutine getChVal_cmplx

  ! --- String ---
  subroutine getChVal_string(node, name, val, default, modifier, &
      & child)
    type(hsd_node), intent(inout), target :: node
    character(len=*), intent(in) :: name
    type(string), intent(inout) :: val
    character(len=*), intent(in), optional :: default
    type(string), intent(inout), optional :: modifier
    type(hsd_node), pointer, intent(out), optional :: child

    type(hsd_node), pointer :: cp
    logical :: found
    integer :: stat
    character(len=:), allocatable :: text

    call find_child_(node, name, cp, found)

    if (.not. found) then
      if (present(default)) then
        val%str = default
        call hsd_set(node, name, default, stat)
        call hsd_get_child(node, name, cp, stat)
        cp%processed = .true.
        if (present(child)) child => cp
        return
      end if
      call detailedError(node, &
          & "Missing required field: '" // name // "'")
    end if

    call hsd_get(node, name, text, stat)
    if (stat /= HSD_STAT_OK) then
      call detailedError(cp, &
          & "Invalid string value for: '" // name // "'")
    end if
    val%str = text

    if (present(modifier)) call extract_modifier_(cp, modifier)
    if (present(child)) child => cp

  end subroutine getChVal_string

  ! --- Integer array ---
  subroutine getChVal_intR1(node, name, val, modifier, child)
    type(hsd_node), intent(inout), target :: node
    character(len=*), intent(in) :: name
    integer, allocatable, intent(out) :: val(:)
    type(string), intent(inout), optional :: modifier
    type(hsd_node), pointer, intent(out), optional :: child

    type(hsd_node), pointer :: cp
    logical :: found
    integer :: stat

    call find_child_(node, name, cp, found)

    if (.not. found) then
      call detailedError(node, &
          & "Missing required field: '" // name // "'")
    end if

    call hsd_get(node, name, val, stat)
    if (stat /= HSD_STAT_OK) then
      call detailedError(cp, &
          & "Invalid integer array for: '" // name // "'")
    end if

    if (present(modifier)) call extract_modifier_(cp, modifier)
    if (present(child)) child => cp

  end subroutine getChVal_intR1

  ! --- Real array ---
  subroutine getChVal_realR1(node, name, val, modifier, child)
    type(hsd_node), intent(inout), target :: node
    character(len=*), intent(in) :: name
    real(dp), allocatable, intent(out) :: val(:)
    type(string), intent(inout), optional :: modifier
    type(hsd_node), pointer, intent(out), optional :: child

    type(hsd_node), pointer :: cp
    logical :: found
    integer :: stat

    call find_child_(node, name, cp, found)

    if (.not. found) then
      call detailedError(node, &
          & "Missing required field: '" // name // "'")
    end if

    call hsd_get(node, name, val, stat)
    if (stat /= HSD_STAT_OK) then
      call detailedError(cp, &
          & "Invalid real array for: '" // name // "'")
    end if

    if (present(modifier)) call extract_modifier_(cp, modifier)
    if (present(child)) child => cp

  end subroutine getChVal_realR1

  ! --- Complex array ---
  subroutine getChVal_cmplxR1(node, name, val, modifier, child)
    type(hsd_node), intent(inout), target :: node
    character(len=*), intent(in) :: name
    complex(dp), allocatable, intent(out) :: val(:)
    type(string), intent(inout), optional :: modifier
    type(hsd_node), pointer, intent(out), optional :: child

    type(hsd_node), pointer :: cp
    logical :: found
    integer :: stat

    call find_child_(node, name, cp, found)

    if (.not. found) then
      call detailedError(node, &
          & "Missing required field: '" // name // "'")
    end if

    call hsd_get(node, name, val, stat)
    if (stat /= HSD_STAT_OK) then
      call detailedError(cp, &
          & "Invalid complex array for: '" // name // "'")
    end if

    if (present(modifier)) call extract_modifier_(cp, modifier)
    if (present(child)) child => cp

  end subroutine getChVal_cmplxR1

  ! --- Logical array ---
  subroutine getChVal_logicalR1(node, name, val, modifier, child)
    type(hsd_node), intent(inout), target :: node
    character(len=*), intent(in) :: name
    logical, allocatable, intent(out) :: val(:)
    type(string), intent(inout), optional :: modifier
    type(hsd_node), pointer, intent(out), optional :: child

    type(hsd_node), pointer :: cp
    logical :: found
    integer :: stat

    call find_child_(node, name, cp, found)

    if (.not. found) then
      call detailedError(node, &
          & "Missing required field: '" // name // "'")
    end if

    call hsd_get(node, name, val, stat)
    if (stat /= HSD_STAT_OK) then
      call detailedError(cp, &
          & "Invalid logical array for: '" // name // "'")
    end if

    if (present(modifier)) call extract_modifier_(cp, modifier)
    if (present(child)) child => cp

  end subroutine getChVal_logicalR1

  ! --- Real matrix ---
  subroutine getChVal_realR2(node, name, val, nrow, ncol, modifier, &
      & child)
    type(hsd_node), intent(inout), target :: node
    character(len=*), intent(in) :: name
    real(dp), allocatable, intent(out) :: val(:,:)
    integer, intent(out) :: nrow, ncol
    type(string), intent(inout), optional :: modifier
    type(hsd_node), pointer, intent(out), optional :: child

    type(hsd_node), pointer :: cp
    logical :: found
    integer :: stat

    call find_child_(node, name, cp, found)

    if (.not. found) then
      call detailedError(node, &
          & "Missing required field: '" // name // "'")
    end if

    call hsd_get_matrix(node, name, val, nrow, ncol, stat)
    if (stat /= HSD_STAT_OK) then
      call detailedError(cp, &
          & "Invalid real matrix for: '" // name // "'")
    end if

    if (present(modifier)) call extract_modifier_(cp, modifier)
    if (present(child)) child => cp

  end subroutine getChVal_realR2

  ! ================================================================
  ! setChildValue — DFTB+ compatible value setting
  ! ================================================================

  ! --- Logical scalar ---
  subroutine setChVal_logical(node, name, val, replace, child, &
      & modifier)
    type(hsd_node), intent(inout), target :: node
    character(len=*), intent(in) :: name
    logical, intent(in) :: val
    logical, intent(in), optional :: replace
    type(hsd_node), pointer, intent(out), optional :: child
    character(len=*), intent(in), optional :: modifier

    call set_child_common_(node, name, replace)
    call hsd_set(node, name, val)
    call finalize_set_(node, name, modifier, child)

  end subroutine setChVal_logical

  ! --- Integer scalar ---
  subroutine setChVal_int(node, name, val, replace, child, modifier)
    type(hsd_node), intent(inout), target :: node
    character(len=*), intent(in) :: name
    integer, intent(in) :: val
    logical, intent(in), optional :: replace
    type(hsd_node), pointer, intent(out), optional :: child
    character(len=*), intent(in), optional :: modifier

    call set_child_common_(node, name, replace)
    call hsd_set(node, name, val)
    call finalize_set_(node, name, modifier, child)

  end subroutine setChVal_int

  ! --- Real scalar ---
  subroutine setChVal_real(node, name, val, replace, child, modifier)
    type(hsd_node), intent(inout), target :: node
    character(len=*), intent(in) :: name
    real(dp), intent(in) :: val
    logical, intent(in), optional :: replace
    type(hsd_node), pointer, intent(out), optional :: child
    character(len=*), intent(in), optional :: modifier

    call set_child_common_(node, name, replace)
    call hsd_set(node, name, val)
    call finalize_set_(node, name, modifier, child)

  end subroutine setChVal_real

  ! --- Complex scalar ---
  subroutine setChVal_cmplx(node, name, val, replace, child, &
      & modifier)
    type(hsd_node), intent(inout), target :: node
    character(len=*), intent(in) :: name
    complex(dp), intent(in) :: val
    logical, intent(in), optional :: replace
    type(hsd_node), pointer, intent(out), optional :: child
    character(len=*), intent(in), optional :: modifier

    call set_child_common_(node, name, replace)
    call hsd_set(node, name, val)
    call finalize_set_(node, name, modifier, child)

  end subroutine setChVal_cmplx

  ! --- String ---
  subroutine setChVal_string(node, name, val, replace, child, &
      & modifier)
    type(hsd_node), intent(inout), target :: node
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: val
    logical, intent(in), optional :: replace
    type(hsd_node), pointer, intent(out), optional :: child
    character(len=*), intent(in), optional :: modifier

    call set_child_common_(node, name, replace)
    call hsd_set(node, name, val)
    call finalize_set_(node, name, modifier, child)

  end subroutine setChVal_string

  ! --- Integer array ---
  subroutine setChVal_intR1(node, name, val, replace, child, &
      & modifier)
    type(hsd_node), intent(inout), target :: node
    character(len=*), intent(in) :: name
    integer, intent(in) :: val(:)
    logical, intent(in), optional :: replace
    type(hsd_node), pointer, intent(out), optional :: child
    character(len=*), intent(in), optional :: modifier

    call set_child_common_(node, name, replace)
    call hsd_set(node, name, val)
    call finalize_set_(node, name, modifier, child)

  end subroutine setChVal_intR1

  ! --- Real array ---
  subroutine setChVal_realR1(node, name, val, replace, child, &
      & modifier)
    type(hsd_node), intent(inout), target :: node
    character(len=*), intent(in) :: name
    real(dp), intent(in) :: val(:)
    logical, intent(in), optional :: replace
    type(hsd_node), pointer, intent(out), optional :: child
    character(len=*), intent(in), optional :: modifier

    call set_child_common_(node, name, replace)
    call hsd_set(node, name, val)
    call finalize_set_(node, name, modifier, child)

  end subroutine setChVal_realR1

  ! --- Complex array ---
  subroutine setChVal_cmplxR1(node, name, val, replace, child, &
      & modifier)
    type(hsd_node), intent(inout), target :: node
    character(len=*), intent(in) :: name
    complex(dp), intent(in) :: val(:)
    logical, intent(in), optional :: replace
    type(hsd_node), pointer, intent(out), optional :: child
    character(len=*), intent(in), optional :: modifier

    call set_child_common_(node, name, replace)
    call hsd_set(node, name, val)
    call finalize_set_(node, name, modifier, child)

  end subroutine setChVal_cmplxR1

  ! --- Logical array ---
  subroutine setChVal_logicalR1(node, name, val, replace, child, &
      & modifier)
    type(hsd_node), intent(inout), target :: node
    character(len=*), intent(in) :: name
    logical, intent(in) :: val(:)
    logical, intent(in), optional :: replace
    type(hsd_node), pointer, intent(out), optional :: child
    character(len=*), intent(in), optional :: modifier

    call set_child_common_(node, name, replace)
    call hsd_set(node, name, val)
    call finalize_set_(node, name, modifier, child)

  end subroutine setChVal_logicalR1

  ! --- Real matrix ---
  subroutine setChVal_realR2(node, name, val, replace, child, &
      & modifier)
    type(hsd_node), intent(inout), target :: node
    character(len=*), intent(in) :: name
    real(dp), intent(in) :: val(:,:)
    logical, intent(in), optional :: replace
    type(hsd_node), pointer, intent(out), optional :: child
    character(len=*), intent(in), optional :: modifier

    call set_child_common_(node, name, replace)
    call hsd_set(node, name, val)
    call finalize_set_(node, name, modifier, child)

  end subroutine setChVal_realR2

  ! ================================================================
  ! setChildValue helpers
  ! ================================================================

  !> Remove existing child if replace is requested
  subroutine set_child_common_(node, name, replace)
    type(hsd_node), intent(inout), target :: node
    character(len=*), intent(in) :: name
    logical, intent(in), optional :: replace

    logical :: do_replace
    integer :: stat

    do_replace = .false.
    if (present(replace)) do_replace = replace

    if (do_replace .and. hsd_has_child(node, name)) then
      call node%remove_child_by_name(name, stat)
    end if

  end subroutine set_child_common_

  !> Set modifier and return child pointer after set
  subroutine finalize_set_(node, name, modifier, child)
    type(hsd_node), intent(in), target :: node
    character(len=*), intent(in) :: name
    character(len=*), intent(in), optional :: modifier
    type(hsd_node), pointer, intent(out), optional :: child

    type(hsd_node), pointer :: cp
    integer :: stat

    call hsd_get_child(node, name, cp, stat)
    if (stat == HSD_STAT_OK) then
      cp%processed = .true.
      if (present(modifier)) cp%attrib = modifier
    end if
    if (present(child)) then
      if (stat == HSD_STAT_OK) then
        child => cp
      else
        child => null()
      end if
    end if

  end subroutine finalize_set_

  ! ================================================================
  ! setChild — create/get empty child block
  ! ================================================================

  !> Create or get an empty child block
  subroutine setChild(node, name, child, modifier)
    type(hsd_node), intent(inout), target :: node
    character(len=*), intent(in) :: name
    type(hsd_node), pointer, intent(out) :: child
    character(len=*), intent(in), optional :: modifier

    type(hsd_node) :: new_child
    integer :: stat

    call hsd_get_child(node, name, child, stat)

    if (stat /= HSD_STAT_OK) then
      call new_table(new_child, name)
      if (present(modifier)) new_child%attrib = modifier
      call node%add_child(new_child)
      call hsd_get_child(node, name, child, stat)
    end if

    if (associated(child)) child%processed = .true.

  end subroutine setChild

  ! ================================================================
  ! Processing tracking
  ! ================================================================

  !> Mark node and optionally all descendants as processed
  subroutine setProcessed(node, recursive)
    type(hsd_node), intent(inout), target :: node
    logical, intent(in), optional :: recursive

    call hsd_set_processed(node, recursive)
  end subroutine setProcessed

  !> Mark node as unprocessed
  subroutine setUnprocessed(node)
    type(hsd_node), intent(inout) :: node
    node%processed = .false.
  end subroutine setUnprocessed

  !> Warn about unprocessed nodes in the tree
  !>
  !> If tIgnoreUnprocessed is .true., does nothing.
  !> Writes warnings to stderr and optionally returns the list.
  subroutine warnUnprocessedNodes(node, tIgnoreUnprocessed)
    type(hsd_node), intent(in) :: node
    logical, intent(in), optional :: tIgnoreUnprocessed

    logical :: doIgnore
    character(len=MAX_WARNING_LEN), allocatable :: warnings(:)
    integer :: ii

    doIgnore = .false.
    if (present(tIgnoreUnprocessed)) doIgnore = tIgnoreUnprocessed
    if (doIgnore) return

    call hsd_warn_unprocessed(node, warnings)

    do ii = 1, size(warnings)
      write(error_unit, '(A,A)') "Warning: ", trim(warnings(ii))
    end do

  end subroutine warnUnprocessedNodes

  ! ================================================================
  ! Error reporting
  ! ================================================================

  !> Report detailed error with node context and stop
  subroutine detailedError(node, msg)
    type(hsd_node), intent(in) :: node
    character(len=*), intent(in) :: msg

    character(len=:), allocatable :: formatted

    call hsd_format_error(node, msg, formatted)
    write(error_unit, '(A)') formatted
    error stop 1
  end subroutine detailedError

  !> Report detailed warning with node context
  subroutine detailedWarning(node, msg)
    type(hsd_node), intent(in) :: node
    character(len=*), intent(in) :: msg

    character(len=:), allocatable :: formatted

    call hsd_format_warning(node, msg, formatted)
    write(error_unit, '(A)') formatted
  end subroutine detailedWarning

  ! ================================================================
  ! Node info
  ! ================================================================

  !> Get the HSD name of a node (replaces DFTB+'s getNodeHSDName)
  subroutine getNodeHSDName(node, name)
    type(hsd_node), intent(in) :: node
    character(len=:), allocatable, intent(out) :: name

    call hsd_get_name(node, name)
  end subroutine getNodeHSDName

  !> Get concatenated text from value children (replaces getFirstTextChild)
  subroutine getFirstTextChild(node, text)
    type(hsd_node), intent(in), target :: node
    character(len=:), allocatable, intent(out) :: text

    integer :: stat

    if (node%node_type == NODE_TYPE_VALUE) then
      if (allocated(node%string_value)) then
        text = node%string_value
      else
        text = ""
      end if
    else
      call hsd_get_inline_text(node, text, stat)
      if (stat /= HSD_STAT_OK) text = ""
    end if
  end subroutine getFirstTextChild

  ! ================================================================
  ! Parse / Dump wrappers
  ! ================================================================

  !> Parse HSD file into tree (replaces DFTB+'s parseHSD)
  subroutine parseHSD(rootName, filename, root)
    character(len=*), intent(in) :: rootName
    character(len=*), intent(in) :: filename
    type(hsd_node), intent(out) :: root

    type(hsd_node) :: parsed
    type(hsd_error_t), allocatable :: error

    call hsd_load_file(filename, parsed, error)
    if (allocated(error)) then
      write(error_unit, '(A)') "Error parsing " // filename // ": " &
          & // error%message
      error stop 1
    end if

    ! Wrap parsed content under rootName
    call new_table(root, rootName)
    call move_children_(parsed, root)

  end subroutine parseHSD

  !> Dump tree to HSD format (replaces DFTB+'s dumpHSD)
  subroutine dumpHSD(root, filename)
    type(hsd_node), intent(in) :: root
    character(len=*), intent(in) :: filename

    type(hsd_error_t), allocatable :: error

    call hsd_dump(root, filename, error)
  end subroutine dumpHSD

  ! ================================================================
  ! DOM compat for programmatic tree building
  ! ================================================================

  !> Create a new document node (empty root table)
  subroutine createDocumentNode(doc)
    type(hsd_node), intent(out) :: doc
    call new_table(doc, name="document")
  end subroutine createDocumentNode

  !> Create a named element (table node)
  function createElement(name) result(node)
    character(len=*), intent(in) :: name
    type(hsd_node) :: node
    call new_table(node, name)
  end function createElement

  !> Append a child node to a parent
  subroutine appendChild(parent, child)
    type(hsd_node), intent(inout) :: parent
    type(hsd_node), intent(in) :: child
    call parent%add_child(child)
  end subroutine appendChild

  !> Destroy a node and all its children
  subroutine destroyNode(node)
    type(hsd_node), intent(inout) :: node
    call node%destroy()
  end subroutine destroyNode

  ! ================================================================
  ! Internal helpers
  ! ================================================================

  !> Move all children from source to dest
  subroutine move_children_(source, dest)
    type(hsd_node), intent(inout) :: source
    type(hsd_node), intent(inout) :: dest

    integer :: ii
    type(hsd_node), pointer :: child

    do ii = 1, source%num_children
      call source%get_child(ii, child)
      if (associated(child)) call dest%add_child(child)
    end do

  end subroutine move_children_

end module hsd_compat
