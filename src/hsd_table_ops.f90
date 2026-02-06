!> Table and iterator operations for HSD types
!>
!> This submodule implements all type-bound procedures for hsd_table
!> and hsd_iterator. See hsd_types.f90 for type definitions and
!> interface declarations.
submodule (hsd_types) hsd_table_ops
  implicit none (type, external)

contains

  !> Build the hash index for O(1) child lookup
  !>
  !> This is called automatically when adding children.
  !> Can also be called explicitly to pre-build the index.
  module procedure table_build_index

    integer :: i

    call self%name_index%init(self%num_children * 2)

    do i = 1, self%num_children
      if (allocated(self%children(i)%node)) then
        if (allocated(self%children(i)%node%name)) then
          call self%name_index%insert(self%children(i)%node%name, i)
        end if
      end if
    end do

    self%index_active = .true.

  end procedure table_build_index

  !> Invalidate the hash index (called when children are removed)
  module procedure table_invalidate_index

    if (self%index_active) then
      call self%name_index%clear()
      self%index_active = .false.
    end if

  end procedure table_invalidate_index

  !> Add a child node to the table
  !>
  !> Creates a deep copy of the child node and adds it to the table.
  !> The table takes ownership of the copy and will deallocate it when
  !> the table is destroyed or the child is removed.
  !>
  !> @param[inout] self  The table to add the child to
  !> @param[in]    child The child node to copy and add
  !>
  !> @note The caller retains ownership of the original `child` argument.
  !>       The copy mechanism uses `allocate(source=child)` which performs
  !>       a deep copy of all components, including allocatable arrays.
  !>
  !> ## Performance
  !>
  !> Uses a hash index for O(1) name lookups.
  module procedure table_add_child

    type(hsd_node_ptr), allocatable :: tmp(:)
    integer :: new_capacity

    ! Grow array if needed
    if (self%num_children >= self%capacity) then
      new_capacity = self%capacity * 2
      allocate(tmp(new_capacity))
      tmp(1:self%num_children) = self%children(1:self%num_children)
      call move_alloc(tmp, self%children)
      self%capacity = new_capacity
    end if

    ! Add child
    self%num_children = self%num_children + 1
    allocate(self%children(self%num_children)%node, source=child)

    ! Update hash index
    if (.not. self%index_active) then
      call self%build_index()
    else if (allocated(child%name)) then
      call self%name_index%insert(child%name, self%num_children)
    end if

  end procedure table_add_child

  !> Get child by index
  !>
  !> Returns a pointer to the child at the given index. The pointer is
  !> owned by the table - do NOT deallocate it. The pointer becomes
  !> invalid if the child is removed or the table is destroyed.
  !>
  !> @param[in]  self   The table to search
  !> @param[in]  index  1-based index (1 to num_children)
  !> @param[out] child  Pointer to child, or null() if out of range
  module procedure table_get_child

    child => null()
    if (index >= 1 .and. index <= self%num_children) then
      if (allocated(self%children(index)%node)) then
        child => self%children(index)%node
      end if
    end if

  end procedure table_get_child

  !> Get child by name
  !>
  !> Returns a pointer to the first child with the given name.
  !> Uses O(1) hash lookup for all table sizes.
  !>
  !> @param[in]  self             The table to search
  !> @param[in]  name             Name to search for
  !> @param[out] child            Pointer to child, or null()
  !> @param[in]  case_insensitive If .true., ignore case
  module procedure table_get_child_by_name

    integer :: idx
    logical :: ignore_case, found

    child => null()
    ignore_case = .false.
    if (present(case_insensitive)) ignore_case = case_insensitive

    if (self%index_active) then
      ! O(1) hash-based lookup
      if (ignore_case) then
        idx = self%name_index%lookup_case_insensitive(name, found)
      else
        idx = self%name_index%lookup(name, found)
      end if

      if (found .and. idx >= 1 .and. idx <= self%num_children) then
        if (allocated(self%children(idx)%node)) then
          child => self%children(idx)%node
        end if
      end if
    else
      ! Linear fallback when hash index is not active
      do idx = 1, self%num_children
        if (.not. allocated(self%children(idx)%node)) cycle
        if (.not. allocated(self%children(idx)%node%name)) cycle
        if (ignore_case) then
          found = to_lower(self%children(idx)%node%name) &
              & == to_lower(name)
        else
          found = self%children(idx)%node%name == name
        end if
        if (found) then
          child => self%children(idx)%node
          return
        end if
      end do
    end if

  end procedure table_get_child_by_name

  !> Check if table has a child with given name
  module procedure table_has_child

    class(hsd_node), pointer :: child

    call self%get_child_by_name(name, child, case_insensitive)
    has = associated(child)

  end procedure table_has_child

  !> Get number of children
  module procedure table_num_children
    n = self%num_children
  end procedure table_num_children

  !> Get list of all child names
  module procedure table_get_keys

    integer :: i, max_len

    ! Find maximum key length
    max_len = 0
    do i = 1, self%num_children
      if (allocated(self%children(i)%node)) then
        if (allocated(self%children(i)%node%name)) then
          max_len = max(max_len, len(self%children(i)%node%name))
        end if
      end if
    end do

    ! Allocate and fill keys
    if (max_len > 0) then
      allocate(character(len=max_len) :: keys(self%num_children))
      do i = 1, self%num_children
        if (allocated(self%children(i)%node)) then
          if (allocated(self%children(i)%node%name)) then
            keys(i) = self%children(i)%node%name
          else
            keys(i) = ""
          end if
        end if
      end do
    else
      allocate(character(len=1) :: keys(0))
    end if

  end procedure table_get_keys

  !> Remove child at given index
  !>
  !> Removes and deallocates the child at the given index. Children
  !> after the removed one are shifted to fill the gap. Any pointers
  !> to the removed child become invalid after this call.
  !>
  !> @param[inout] self   The table to modify
  !> @param[in]    index  1-based index of the child to remove
  !> @param[out]   stat   Optional status code
  module procedure table_remove_child

    integer :: i

    if (index < 1 .or. index > self%num_children) then
      if (present(stat)) stat = HSD_STAT_NOT_FOUND
      return
    end if

    ! Destroy the child node
    if (allocated(self%children(index)%node)) then
      call self%children(index)%node%destroy()
      deallocate(self%children(index)%node)
    end if

    ! Shift remaining children down
    do i = index, self%num_children - 1
      call move_alloc(self%children(i + 1)%node, self%children(i)%node)
    end do

    self%num_children = self%num_children - 1

    ! Rebuild index as indices have shifted
    if (self%num_children > 0) then
      call self%build_index()
    else
      call self%invalidate_index()
    end if

    if (present(stat)) stat = HSD_STAT_OK

  end procedure table_remove_child

  !> Remove child by name
  module procedure table_remove_child_by_name

    integer :: idx
    logical :: ignore_case, found

    ignore_case = .false.
    if (present(case_insensitive)) ignore_case = case_insensitive

    if (self%index_active) then
      if (ignore_case) then
        idx = self%name_index%lookup_case_insensitive(name, found)
      else
        idx = self%name_index%lookup(name, found)
      end if
      if (found) then
        call self%remove_child(idx, stat)
        return
      end if
    end if

    if (present(stat)) stat = HSD_STAT_NOT_FOUND

  end procedure table_remove_child_by_name

  !> Destroy table and all children
  !>
  !> Recursively deallocates all child nodes and frees all allocated
  !> memory. Must be called explicitly to avoid memory leaks.
  module procedure table_destroy

    integer :: i

    ! Destroy hash index
    call self%name_index%destroy()
    self%index_active = .false.

    do i = 1, self%num_children
      if (allocated(self%children(i)%node)) then
        call self%children(i)%node%destroy()
        deallocate(self%children(i)%node)
      end if
    end do

    if (allocated(self%children)) deallocate(self%children)
    if (allocated(self%name)) deallocate(self%name)
    if (allocated(self%attrib)) deallocate(self%attrib)

    self%num_children = 0
    self%capacity = 0

  end procedure table_destroy

  !> Initialize iterator for a table
  module procedure iterator_init

    self%table => table
    self%pos = 0

  end procedure iterator_init

  !> Advance to next child and return it
  !> Returns .false. if no more children
  module procedure iterator_next

    child => null()
    has_more = .false.

    if (.not. associated(self%table)) return

    self%pos = self%pos + 1
    if (self%pos <= self%table%num_children) then
      if (allocated(self%table%children(self%pos)%node)) then
        child => self%table%children(self%pos)%node
        has_more = .true.
      end if
    end if

  end procedure iterator_next

  !> Reset iterator to beginning
  module procedure iterator_reset
    self%pos = 0
  end procedure iterator_reset

  !> Check if there are more children without advancing
  module procedure iterator_has_next

    has_more = .false.
    if (associated(self%table)) then
      has_more = self%pos < self%table%num_children
    end if

  end procedure iterator_has_next

end submodule hsd_table_ops
