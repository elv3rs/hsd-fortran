!> HSD query and navigation operations
!>
!> This module provides functionality for navigating HSD tree structures,
!> introspecting node types, and performing tree operations like merging
!> and cloning.
module hsd_query
  use hsd_utils, only: to_lower
  use hsd_error, only: HSD_STAT_OK, HSD_STAT_NOT_FOUND, HSD_STAT_TYPE_ERROR
  use hsd_types, only: hsd_node, hsd_table, hsd_value, new_table, new_value, &
    VALUE_TYPE_NONE, VALUE_TYPE_ARRAY
  implicit none (type, external)
  private

  ! Public procedures
  public :: hsd_get_child, hsd_get_table
  public :: hsd_has_child
  public :: hsd_remove_child
  public :: hsd_get_type, hsd_is_table, hsd_is_value, hsd_is_array
  public :: hsd_child_count, hsd_get_keys
  public :: hsd_get_attrib, hsd_has_attrib
  public :: hsd_merge, hsd_clone

contains

  !> Check if a table has a child with given name
  function hsd_has_child(table, name, case_insensitive) result(has)
    type(hsd_table), intent(in), target :: table
    character(len=*), intent(in) :: name
    logical, intent(in), optional :: case_insensitive
    logical :: has

    class(hsd_node), pointer :: child
    integer :: stat

    if (index(name, "/") > 0) then
      ! Path-based lookup - navigate through nested tables
      call hsd_get_child(table, name, child, stat)
      has = (stat == HSD_STAT_OK .and. associated(child))
    else
      has = table%has_child(name, case_insensitive)
    end if

  end function hsd_has_child

  !> Remove a child from a table by name
  !>
  !> Supports path-based navigation with "/" separator for nested tables.
  !> The last component of the path is the child to remove.
  subroutine hsd_remove_child(table, path, stat, case_insensitive)
    type(hsd_table), intent(inout) :: table
    character(len=*), intent(in) :: path
    integer, intent(out), optional :: stat
    logical, intent(in), optional :: case_insensitive

    class(hsd_node), pointer :: parent_node
    type(hsd_table), pointer :: parent_table
    character(len=:), allocatable :: child_name, parent_path
    integer :: last_slash, local_stat

    ! Find the last slash to separate parent path from child name
    last_slash = index(path, "/", back=.true.)

    if (last_slash > 0) then
      parent_path = path(1:last_slash-1)
      child_name = path(last_slash+1:)

      ! Get the parent table
      call hsd_get_child(table, parent_path, parent_node, local_stat)
      if (local_stat /= HSD_STAT_OK .or. .not. associated(parent_node)) then
        if (present(stat)) stat = HSD_STAT_NOT_FOUND
        return
      end if

      select type (parent_node)
      type is (hsd_table)
        parent_table => parent_node
        call parent_table%remove_child_by_name(child_name, local_stat, case_insensitive)
        if (present(stat)) stat = local_stat
      class default
        if (present(stat)) stat = HSD_STAT_TYPE_ERROR
      end select
    else
      ! No path separator - remove directly from the root table
      child_name = path
      call table%remove_child_by_name(child_name, local_stat, case_insensitive)
      if (present(stat)) stat = local_stat
    end if

  end subroutine hsd_remove_child

  !> Get the type of a value at the given path
  !>
  !> Returns one of: VALUE_TYPE_NONE (not found or is table), VALUE_TYPE_STRING,
  !> VALUE_TYPE_INTEGER, VALUE_TYPE_REAL, VALUE_TYPE_LOGICAL, VALUE_TYPE_ARRAY,
  !> VALUE_TYPE_COMPLEX
  function hsd_get_type(table, path) result(val_type)
    type(hsd_table), intent(in), target :: table
    character(len=*), intent(in) :: path
    integer :: val_type

    class(hsd_node), pointer :: child
    integer :: local_stat

    val_type = VALUE_TYPE_NONE
    call hsd_get_child(table, path, child, local_stat)

    if (local_stat /= 0 .or. .not. associated(child)) return

    select type (child)
    type is (hsd_value)
      val_type = child%value_type
    end select

  end function hsd_get_type

  !> Check if the node at path is a table (container)
  function hsd_is_table(table, path) result(is_tbl)
    type(hsd_table), intent(in), target :: table
    character(len=*), intent(in) :: path
    logical :: is_tbl

    class(hsd_node), pointer :: child
    integer :: local_stat

    is_tbl = .false.
    call hsd_get_child(table, path, child, local_stat)

    if (local_stat /= 0 .or. .not. associated(child)) return

    select type (child)
    type is (hsd_table)
      is_tbl = .true.
    end select

  end function hsd_is_table

  !> Check if the node at path is a value (leaf)
  function hsd_is_value(table, path) result(is_val)
    type(hsd_table), intent(in), target :: table
    character(len=*), intent(in) :: path
    logical :: is_val

    class(hsd_node), pointer :: child
    integer :: local_stat

    is_val = .false.
    call hsd_get_child(table, path, child, local_stat)

    if (local_stat /= 0 .or. .not. associated(child)) return

    select type (child)
    type is (hsd_value)
      is_val = .true.
    end select

  end function hsd_is_value

  !> Check if the node at path contains array data
  function hsd_is_array(table, path) result(is_arr)
    type(hsd_table), intent(in), target :: table
    character(len=*), intent(in) :: path
    logical :: is_arr

    is_arr = (hsd_get_type(table, path) == VALUE_TYPE_ARRAY)

  end function hsd_is_array

  !> Get the number of children in a table at the given path
  !>
  !> Returns 0 if path not found or is not a table
  function hsd_child_count(table, path) result(count)
    type(hsd_table), intent(in), target :: table
    character(len=*), intent(in) :: path
    integer :: count

    class(hsd_node), pointer :: child
    integer :: local_stat

    count = 0

    if (len_trim(path) == 0) then
      ! Empty path means the root table itself
      count = table%num_children
      return
    end if

    call hsd_get_child(table, path, child, local_stat)

    if (local_stat /= 0 .or. .not. associated(child)) return

    select type (child)
    type is (hsd_table)
      count = child%num_children
    end select

  end function hsd_child_count

  !> Get the keys (child names) from a table at the given path
  subroutine hsd_get_keys(table, path, keys, stat)
    type(hsd_table), intent(in), target :: table
    character(len=*), intent(in) :: path
    character(len=:), allocatable, intent(out) :: keys(:)
    integer, intent(out), optional :: stat

    class(hsd_node), pointer :: child
    integer :: local_stat

    if (present(stat)) stat = HSD_STAT_OK

    if (len_trim(path) == 0) then
      ! Empty path means the root table itself
      call table%get_keys(keys)
      return
    end if

    call hsd_get_child(table, path, child, local_stat)

    if (local_stat /= 0 .or. .not. associated(child)) then
      allocate(character(len=1) :: keys(0))
      if (present(stat)) stat = HSD_STAT_NOT_FOUND
      return
    end if

    select type (child)
    type is (hsd_table)
      call child%get_keys(keys)
    class default
      allocate(character(len=1) :: keys(0))
      if (present(stat)) stat = HSD_STAT_TYPE_ERROR
    end select

  end subroutine hsd_get_keys

  !> Get a child node by path (using / as separator)
  subroutine hsd_get_child(table, path, child, stat)
    type(hsd_table), intent(in), target :: table
    character(len=*), intent(in) :: path
    class(hsd_node), pointer, intent(out) :: child
    integer, intent(out), optional :: stat

    child => null()
    ! stat will be overriden by subroutine below.
    if (present(stat)) stat = HSD_STAT_OK

    ! Delegate to recursive helper
    call get_first_child_table(table, path, child, stat)

  end subroutine hsd_get_child

  !> Helper to navigate path and get child
  recursive subroutine get_first_child_table(table, path, child, stat)
    type(hsd_table), intent(in), target :: table
    character(len=*), intent(in) :: path
    class(hsd_node), pointer, intent(out) :: child
    integer, intent(out), optional :: stat

    character(len=:), allocatable :: remaining, segment
    class(hsd_node), pointer :: current
    integer :: sep_pos

    child => null()
    remaining = path

    ! Get first segment
    sep_pos = index(remaining, "/")
    if (sep_pos > 0) then
      segment = remaining(1:sep_pos-1)
      remaining = remaining(sep_pos+1:)
    else
      segment = remaining
      remaining = ""
    end if

    ! Find child with this name
    call table%get_child_by_name(segment, current, case_insensitive=.true.)

    if (.not. associated(current)) then
      if (present(stat)) stat = HSD_STAT_NOT_FOUND
      return
    end if

    ! If no more path, return this node
    if (len_trim(remaining) == 0) then
      child => current
      if (present(stat)) stat = HSD_STAT_OK
      return
    end if

    ! Otherwise, recurse into child table
    select type (current)
    type is (hsd_table)
      call get_first_child_table(current, remaining, child, stat)
    class default
      if (present(stat)) stat = HSD_STAT_NOT_FOUND
    end select

  end subroutine get_first_child_table

  !> Get a table child by path
  subroutine hsd_get_table(table, path, child_table, stat)
    type(hsd_table), intent(in), target :: table
    character(len=*), intent(in) :: path
    type(hsd_table), pointer, intent(out) :: child_table
    integer, intent(out), optional :: stat

    class(hsd_node), pointer :: child

    child_table => null()
    call hsd_get_child(table, path, child, stat)

    if (associated(child)) then
      select type (child)
      type is (hsd_table)
        child_table => child
      class default
        if (present(stat)) stat = HSD_STAT_NOT_FOUND
      end select
    end if

  end subroutine hsd_get_table

  !> Get an attribute from a node at the given path
  !>
  !> Example: For `LatticeConstant [Angstrom] = 5.4`, the attribute is "Angstrom"
  subroutine hsd_get_attrib(table, path, attrib, stat)
    type(hsd_table), intent(in), target :: table
    character(len=*), intent(in) :: path
    character(len=:), allocatable, intent(out) :: attrib
    integer, intent(out), optional :: stat

    class(hsd_node), pointer :: child
    integer :: local_stat

    call hsd_get_child(table, path, child, local_stat)

    if (local_stat /= 0 .or. .not. associated(child)) then
      if (present(stat)) stat = HSD_STAT_NOT_FOUND
      attrib = ""
      return
    end if

    ! Node exists - return OK regardless of whether attribute is set
    if (allocated(child%attrib)) then
      attrib = child%attrib
    else
      attrib = ""
    end if
    if (present(stat)) stat = HSD_STAT_OK

  end subroutine hsd_get_attrib

  !> Check if a node at the given path has an attribute
  function hsd_has_attrib(table, path) result(has)
    type(hsd_table), intent(in), target :: table
    character(len=*), intent(in) :: path
    logical :: has

    class(hsd_node), pointer :: child
    integer :: local_stat

    has = .false.
    call hsd_get_child(table, path, child, local_stat)

    if (local_stat /= 0 .or. .not. associated(child)) return

    has = allocated(child%attrib)

  end function hsd_has_attrib

  !> Merge two HSD tables (overlay pattern)
  !>
  !> Values from `overlay` are merged into `base`. If a key exists in both,
  !> the value from `overlay` takes precedence (unless it's a table,
  !> in which case they are merged recursively).
  recursive subroutine hsd_merge(base, overlay, stat)
    type(hsd_table), intent(inout) :: base
    type(hsd_table), intent(in) :: overlay
    integer, intent(out), optional :: stat

    class(hsd_node), pointer :: overlay_child, base_child
    type(hsd_table) :: cloned_table
    type(hsd_value) :: cloned_value
    integer :: i, local_stat

    if (present(stat)) stat = HSD_STAT_OK

    ! Iterate over overlay children
    do i = 1, overlay%num_children
      call overlay%get_child(i, overlay_child)
      if (.not. associated(overlay_child)) cycle
      if (.not. allocated(overlay_child%name)) cycle

      ! Check if base has this child
      call base%get_child_by_name(overlay_child%name, base_child, case_insensitive=.true.)

      if (.not. associated(base_child)) then
        ! Child doesn't exist in base - clone and add it
        select type (overlay_child)
        type is (hsd_table)
          call clone_table(overlay_child, cloned_table)
          call base%add_child(cloned_table)
        type is (hsd_value)
          call clone_value(overlay_child, cloned_value)
          call base%add_child(cloned_value)
        end select
      else
        ! Child exists - handle based on type
        select type (overlay_child)
        type is (hsd_table)
          ! If both are tables, merge recursively
          select type (base_child)
          type is (hsd_table)
            call hsd_merge(base_child, overlay_child, local_stat)
            if (present(stat) .and. local_stat /= HSD_STAT_OK) stat = local_stat
          class default
            ! Base is not a table but overlay is - skip (could log warning)
          end select
        type is (hsd_value)
          ! Overlay value replaces base value
          select type (base_child)
          type is (hsd_value)
            call clone_value(overlay_child, cloned_value)
            ! Replace the value content
            base_child%value_type = cloned_value%value_type
            if (allocated(cloned_value%string_value)) then
              base_child%string_value = cloned_value%string_value
            end if
            base_child%int_value = cloned_value%int_value
            base_child%real_value = cloned_value%real_value
            base_child%logical_value = cloned_value%logical_value
            base_child%complex_value = cloned_value%complex_value
            if (allocated(cloned_value%raw_text)) then
              base_child%raw_text = cloned_value%raw_text
            end if
            if (allocated(cloned_value%int_array)) then
              if (allocated(base_child%int_array)) deallocate(base_child%int_array)
              allocate(base_child%int_array, source=cloned_value%int_array)
            end if
            if (allocated(cloned_value%real_array)) then
              if (allocated(base_child%real_array)) deallocate(base_child%real_array)
              allocate(base_child%real_array, source=cloned_value%real_array)
            end if
            if (allocated(cloned_value%logical_array)) then
              if (allocated(base_child%logical_array)) deallocate(base_child%logical_array)
              allocate(base_child%logical_array, source=cloned_value%logical_array)
            end if
            if (allocated(cloned_value%string_array)) then
              if (allocated(base_child%string_array)) deallocate(base_child%string_array)
              allocate(base_child%string_array, source=cloned_value%string_array)
            end if
            if (allocated(cloned_value%complex_array)) then
              if (allocated(base_child%complex_array)) deallocate(base_child%complex_array)
              allocate(base_child%complex_array, source=cloned_value%complex_array)
            end if
            if (allocated(cloned_value%int_matrix)) then
              if (allocated(base_child%int_matrix)) deallocate(base_child%int_matrix)
              allocate(base_child%int_matrix, source=cloned_value%int_matrix)
            end if
            if (allocated(cloned_value%real_matrix)) then
              if (allocated(base_child%real_matrix)) deallocate(base_child%real_matrix)
              allocate(base_child%real_matrix, source=cloned_value%real_matrix)
            end if
            base_child%nrows = cloned_value%nrows
            base_child%ncols = cloned_value%ncols
          class default
            ! Type mismatch - skip
          end select
        end select
      end if
    end do

  end subroutine hsd_merge

  !> Clone a table (deep copy)
  recursive subroutine clone_table(source, dest)
    type(hsd_table), intent(in) :: source
    type(hsd_table), intent(out) :: dest

    class(hsd_node), pointer :: child
    type(hsd_table) :: cloned_subtable
    type(hsd_value) :: cloned_value
    integer :: i

    call new_table(dest, name=source%name)
    if (allocated(source%attrib)) dest%attrib = source%attrib
    dest%line = source%line

    do i = 1, source%num_children
      call source%get_child(i, child)
      if (.not. associated(child)) cycle

      select type (child)
      type is (hsd_table)
        call clone_table(child, cloned_subtable)
        call dest%add_child(cloned_subtable)
      type is (hsd_value)
        call clone_value(child, cloned_value)
        call dest%add_child(cloned_value)
      end select
    end do

  end subroutine clone_table

  !> Clone a value (deep copy)
  subroutine clone_value(source, dest)
    type(hsd_value), intent(in) :: source
    type(hsd_value), intent(out) :: dest

    call new_value(dest, name=source%name)
    if (allocated(source%attrib)) dest%attrib = source%attrib
    dest%line = source%line
    dest%value_type = source%value_type

    if (allocated(source%string_value)) dest%string_value = source%string_value
    dest%int_value = source%int_value
    dest%real_value = source%real_value
    dest%logical_value = source%logical_value
    dest%complex_value = source%complex_value

    if (allocated(source%raw_text)) dest%raw_text = source%raw_text
    if (allocated(source%int_array)) allocate(dest%int_array, source=source%int_array)
    if (allocated(source%real_array)) allocate(dest%real_array, source=source%real_array)
    if (allocated(source%logical_array)) allocate(dest%logical_array, source=source%logical_array)
    if (allocated(source%string_array)) allocate(dest%string_array, source=source%string_array)
    if (allocated(source%complex_array)) allocate(dest%complex_array, source=source%complex_array)
    if (allocated(source%int_matrix)) allocate(dest%int_matrix, source=source%int_matrix)
    if (allocated(source%real_matrix)) allocate(dest%real_matrix, source=source%real_matrix)
    dest%nrows = source%nrows
    dest%ncols = source%ncols

  end subroutine clone_value

  !> Deep clone an entire HSD table tree
  subroutine hsd_clone(source, dest, stat)
    type(hsd_table), intent(in) :: source
    type(hsd_table), intent(out) :: dest
    integer, intent(out), optional :: stat

    call clone_table(source, dest)
    if (present(stat)) stat = HSD_STAT_OK

  end subroutine hsd_clone

end module hsd_query
