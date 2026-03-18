!> Table and iterator operations for HSD types
!>
!> This submodule implements all type-bound procedures for table
!> and iterator operations on hsd_node_t. See hsd_types.f90 for type
!> definitions and interface declarations.
submodule (hsd_types) hsd_table_ops
  implicit none (type, external)

contains

  !> Add a child node to the table
  !>
  !> Creates a deep copy of the child node and adds it to the table.
  !> The table takes ownership of the copy and will deallocate it when
  !> the table is destroyed or the child is removed.
  module procedure table_add_child

    type(hsd_node_ptr_t), allocatable :: tmp(:)
    integer :: new_size, ii

    ! Initialize if table was never set up via new_table
    if (.not. allocated(self%children)) then
      allocate(self%children(4))
      self%num_children = 0
    end if

    ! Grow array if needed
    if (self%num_children >= size(self%children)) then
      new_size = size(self%children) * 2
      allocate(tmp(new_size))
      ! Move pointers (shallow copy) — node objects stay at same address
      do ii = 1, self%num_children
        tmp(ii)%node => self%children(ii)%node
        self%children(ii)%node => null()
      end do
      deallocate(self%children)
      call move_alloc(tmp, self%children)
    end if

    ! Add child (allocate a copy on the heap, store pointer)
    self%num_children = self%num_children + 1
    allocate(self%children(self%num_children)%node, source=child)

  end procedure table_add_child

  !> Get child by index
  !>
  !> Returns a pointer to the child at the given index. The pointer is
  !> owned by the table - do NOT deallocate it. The pointer becomes
  !> invalid if the child is removed or the table is destroyed.
  module procedure table_get_child

    child => null()
    if (index >= 1 .and. index <= self%num_children) then
      if (associated(self%children(index)%node)) then
        child => self%children(index)%node
      end if
    end if

  end procedure table_get_child

  !> Get child by name (linear search, returns last match)
  module procedure table_get_child_by_name

    integer :: idx
    character(len=:), allocatable :: lower_name

    child => null()
    lower_name = to_lower(name)

    ! Search from end to return last occurrence (override semantics)
    do idx = self%num_children, 1, -1
      if (.not. associated(self%children(idx)%node)) cycle
      if (.not. allocated(self%children(idx)%node%name)) cycle
      if (to_lower(self%children(idx)%node%name) == lower_name) then
        child => self%children(idx)%node
        return
      end if
    end do

  end procedure table_get_child_by_name

  !> Check if table has a child with given name
  module procedure table_has_child

    type(hsd_node_t), pointer :: child

    call self%get_child_by_name(name, child)
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
      if (associated(self%children(i)%node)) then
        if (allocated(self%children(i)%node%name)) then
          max_len = max(max_len, &
              & len(self%children(i)%node%name))
        end if
      end if
    end do

    ! Allocate and fill keys
    if (max_len > 0) then
      allocate(character(len=max_len) :: &
          & keys(self%num_children))
      do i = 1, self%num_children
        if (associated(self%children(i)%node)) then
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
  module procedure table_remove_child

    integer :: i

    if (index < 1 .or. index > self%num_children) then
      if (present(stat)) stat = HSD_STAT_NOT_FOUND
      return
    end if

    ! Destroy the child node
    if (associated(self%children(index)%node)) then
      call self%children(index)%node%destroy()
      deallocate(self%children(index)%node)
    end if

    ! Shift remaining children down (move pointers)
    do i = index, self%num_children - 1
      self%children(i)%node => self%children(i + 1)%node
    end do
    self%children(self%num_children)%node => null()

    self%num_children = self%num_children - 1

    if (present(stat)) stat = HSD_STAT_OK

  end procedure table_remove_child

  !> Remove child by name (linear search, removes last match)
  module procedure table_remove_child_by_name

    integer :: idx
    character(len=:), allocatable :: lower_name

    lower_name = to_lower(name)

    ! Search from end to match last occurrence (override semantics)
    do idx = self%num_children, 1, -1
      if (.not. associated(self%children(idx)%node)) cycle
      if (.not. allocated(self%children(idx)%node%name)) cycle
      if (to_lower(self%children(idx)%node%name) == &
          & lower_name) then
        call self%remove_child(idx, stat)
        return
      end if
    end do

    if (present(stat)) stat = HSD_STAT_NOT_FOUND

  end procedure table_remove_child_by_name

  !> Destroy node and all children
  !>
  !> Recursively deallocates all child nodes and frees all allocated
  !> memory. Must be called explicitly to avoid memory leaks.
  module procedure node_destroy

    integer :: i

    ! Destroy children if this is a table node
    if (self%node_type == NODE_TYPE_TABLE) then
      do i = 1, self%num_children
        if (associated(self%children(i)%node)) then
          call self%children(i)%node%destroy()
          deallocate(self%children(i)%node)
        end if
      end do
      if (allocated(self%children)) deallocate(self%children)
      self%num_children = 0
    end if

    ! Clean up value fields
    if (allocated(self%string_value)) &
        & deallocate(self%string_value)
    self%value_type = VALUE_TYPE_NONE

    ! Clean up common fields
    if (allocated(self%name)) deallocate(self%name)
    if (allocated(self%attrib)) deallocate(self%attrib)

  end procedure node_destroy

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
      if (associated( &
          & self%table%children(self%pos)%node)) then
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
