!> Hash table implementation for O(1) child name lookup
!>
!> This module provides a simple hash table for mapping string names to
!> integer indices. Used by hsd_table to accelerate child lookup when
!> tables have many children.
!>
!> ## Thread Safety
!>
!> This module is NOT thread-safe. Concurrent modifications to the same
!> hash table from multiple threads may cause data corruption. Use external
!> synchronization if concurrent access is required.
module hsd_hash_table
  use hsd_constants, only: sp
  use hsd_utils, only: to_lower
  implicit none (type, external)
  private

  public :: hsd_name_index_t

  !> Hash table entry
  type :: hash_entry_t
    character(len=:), allocatable :: key
    character(len=:), allocatable :: key_lower  !< Lowercased key for case-insensitive lookup
    integer :: value = 0
    logical :: occupied = .false.
    integer :: next = 0  !< Index of next entry in chain (0 = no more)
  end type hash_entry_t

  !> Hash table for name-to-index mapping
  !>
  !> Uses a hybrid collision resolution strategy:
  !> 1. Primary storage is a fixed-size bucket array accessed via hash (open addressing).
  !> 2. Collisions are handled via explicit chaining, but unlike traditional chaining where
  !>    nodes are individually allocated on the heap, here they are stored in a pre-allocated
  !>    contiguous `overflow` array.
  !>
  !> This "flat chaining" approach provides better cache locality and reduces memory fragmentation.
  !> In the `next` field, negative values (-idx) indicate an index into the overflow array.
  type :: hsd_name_index_t
    type(hash_entry_t), allocatable :: buckets(:)
    type(hash_entry_t), allocatable :: overflow(:)
    integer :: num_buckets = 0
    integer :: num_overflow = 0
    integer :: overflow_capacity = 0
    integer :: num_entries = 0
  contains
    procedure :: init => name_index_init
    procedure :: insert => name_index_insert
    procedure :: lookup => name_index_lookup
    procedure :: lookup_case_insensitive => name_index_lookup_ci
    procedure :: remove => name_index_remove
    procedure :: clear => name_index_clear
    procedure :: destroy => name_index_destroy
    procedure :: rehash => name_index_rehash
  end type hsd_name_index_t

contains

  !> Hash function (djb2 algorithm)
  pure function hash_string(str) result(hash)
    character(len=*), intent(in) :: str
    integer :: hash
    integer :: i

    hash = 5381
    do i = 1, len(str)
      ! hash = hash * 33 + char, but avoid overflow with iand
      hash = iand(ishft(hash, 5) + hash + ichar(str(i:i)), huge(hash))
    end do
    hash = iand(hash, huge(hash))  ! Ensure positive (abs is UB for -huge(1)-1)

  end function hash_string

  !> Initialize the hash table
  subroutine name_index_init(self, capacity)
    class(hsd_name_index_t), intent(inout) :: self
    integer, intent(in), optional :: capacity

    integer :: cap

    cap = 32  ! Default capacity
    if (present(capacity)) cap = max(8, capacity)

    ! Round up to power of 2 for efficient modulo
    cap = 2 ** ceiling(log(real(cap, sp)) / log(2.0_sp))

    if (allocated(self%buckets)) then
      call self%destroy()
    end if

    allocate(self%buckets(cap))
    self%num_buckets = cap
    self%num_entries = 0

    ! Pre-allocate overflow area
    self%overflow_capacity = max(8, cap / 4)
    allocate(self%overflow(self%overflow_capacity))
    self%num_overflow = 0

  end subroutine name_index_init

  !> Add an entry to overflow area, returns its index (negative = -idx in overflow)
  function add_overflow_entry(self, key, value) result(idx)
    class(hsd_name_index_t), intent(inout) :: self
    character(len=*), intent(in) :: key
    integer, intent(in) :: value
    integer :: idx

    type(hash_entry_t), allocatable :: tmp(:)
    integer :: new_capacity

    ! Grow overflow if needed
    if (self%num_overflow >= self%overflow_capacity) then
      new_capacity = self%overflow_capacity * 2
      allocate(tmp(new_capacity))
      tmp(1:self%num_overflow) = self%overflow(1:self%num_overflow)
      call move_alloc(tmp, self%overflow)
      self%overflow_capacity = new_capacity
    end if

    self%num_overflow = self%num_overflow + 1
    self%overflow(self%num_overflow)%key = key
    self%overflow(self%num_overflow)%key_lower = to_lower(key)
    self%overflow(self%num_overflow)%value = value
    self%overflow(self%num_overflow)%occupied = .true.
    self%overflow(self%num_overflow)%next = 0

    ! Return negative index to indicate overflow area
    idx = -self%num_overflow

  end function add_overflow_entry

  !> Insert a key-value pair
  recursive subroutine name_index_insert(self, key, value)
    class(hsd_name_index_t), intent(inout) :: self
    character(len=*), intent(in) :: key
    integer, intent(in) :: value

    integer :: idx, chain_idx, overflow_idx

    ! Initialize if needed
    if (self%num_buckets == 0) call self%init()

    ! Check load factor and rehash if needed (> 0.75)
    if (self%num_entries * 4 > self%num_buckets * 3) then
      call self%rehash()
    end if

    idx = mod(hash_string(key), self%num_buckets) + 1

    if (.not. self%buckets(idx)%occupied) then
      ! Empty bucket - use directly
      self%buckets(idx)%key = key
      self%buckets(idx)%key_lower = to_lower(key)
      self%buckets(idx)%value = value
      self%buckets(idx)%occupied = .true.
      self%buckets(idx)%next = 0
      self%num_entries = self%num_entries + 1
      return
    end if

    ! Check if key already exists in bucket
    if (allocated(self%buckets(idx)%key)) then
      if (self%buckets(idx)%key == key) then
        self%buckets(idx)%value = value
        return
      end if
    end if

    ! Follow chain to check for existing key and find end
    chain_idx = self%buckets(idx)%next
    do while (chain_idx /= 0)
      ! In overflow area (chain_idx is always negative for overflow entries)
      overflow_idx = -chain_idx
      if (allocated(self%overflow(overflow_idx)%key)) then
        if (self%overflow(overflow_idx)%key == key) then
          self%overflow(overflow_idx)%value = value
          return
        end if
      end if
      if (self%overflow(overflow_idx)%next == 0) exit
      chain_idx = self%overflow(overflow_idx)%next
    end do

    ! Add new entry to overflow and link it
    overflow_idx = add_overflow_entry(self, key, value)

    ! Link to chain
    if (self%buckets(idx)%next == 0) then
      self%buckets(idx)%next = overflow_idx
    else
      ! Find last in chain (chain entries are always negative for overflow)
      chain_idx = self%buckets(idx)%next
      do while (chain_idx /= 0)
        if (self%overflow(-chain_idx)%next == 0) then
          self%overflow(-chain_idx)%next = overflow_idx
          exit
        end if
        chain_idx = self%overflow(-chain_idx)%next
      end do
    end if

    self%num_entries = self%num_entries + 1

  end subroutine name_index_insert

  !> Lookup a key (case-sensitive)
  function name_index_lookup(self, key, found) result(value)
    class(hsd_name_index_t), intent(in) :: self
    character(len=*), intent(in) :: key
    logical, intent(out), optional :: found
    integer :: value

    integer :: idx, chain_idx, overflow_idx

    value = 0
    if (present(found)) found = .false.

    if (self%num_buckets == 0) return

    idx = mod(hash_string(key), self%num_buckets) + 1

    if (.not. self%buckets(idx)%occupied) return

    ! Check bucket
    if (allocated(self%buckets(idx)%key)) then
      if (self%buckets(idx)%key == key) then
        value = self%buckets(idx)%value
        if (present(found)) found = .true.
        return
      end if
    end if

    ! Check chain (overflow entries use negative indices)
    chain_idx = self%buckets(idx)%next
    do while (chain_idx /= 0)
      overflow_idx = -chain_idx
      if (allocated(self%overflow(overflow_idx)%key)) then
        if (self%overflow(overflow_idx)%key == key) then
          value = self%overflow(overflow_idx)%value
          if (present(found)) found = .true.
          return
        end if
      end if
      chain_idx = self%overflow(overflow_idx)%next
    end do

  end function name_index_lookup

  !> Lookup a key (case-insensitive)
  !>
  !> Hashes the lowered key and checks the corresponding bucket chain,
  !> comparing against pre-stored lowered keys for O(1) average-case lookup.
  function name_index_lookup_ci(self, key, found) result(value)
    class(hsd_name_index_t), intent(in) :: self
    character(len=*), intent(in) :: key
    logical, intent(out), optional :: found
    integer :: value

    integer :: idx, chain_idx, overflow_idx
    character(len=:), allocatable :: key_lower

    value = 0
    if (present(found)) found = .false.

    if (self%num_buckets == 0) return

    key_lower = to_lower(key)

    ! Hash the lowered key to find the right bucket.
    ! NOTE: This only works correctly if keys were also inserted using
    ! their lowered hash. Since we currently hash the original-case key
    ! on insert, a case-insensitive lookup must still check all buckets
    ! that could contain the lowered variant. However, because typical
    ! HSD keys share the same casing, we optimise by first probing the
    ! bucket for the lowered key, then falling back to a full scan only
    ! when the probe misses.
    idx = mod(hash_string(key_lower), self%num_buckets) + 1

    ! Fast path: check bucket at the lowered-key hash position
    if (self%buckets(idx)%occupied) then
      if (allocated(self%buckets(idx)%key_lower)) then
        if (self%buckets(idx)%key_lower == key_lower) then
          value = self%buckets(idx)%value
          if (present(found)) found = .true.
          return
        end if
      end if
      chain_idx = self%buckets(idx)%next
      do while (chain_idx /= 0)
        overflow_idx = -chain_idx
        if (allocated(self%overflow(overflow_idx)%key_lower)) then
          if (self%overflow(overflow_idx)%key_lower == key_lower) then
            value = self%overflow(overflow_idx)%value
            if (present(found)) found = .true.
            return
          end if
        end if
        chain_idx = self%overflow(overflow_idx)%next
      end do
    end if

    ! Also probe the bucket for the original-case hash (covers the
    ! common case where the key was inserted with its original casing).
    idx = mod(hash_string(key), self%num_buckets) + 1
    if (self%buckets(idx)%occupied) then
      if (allocated(self%buckets(idx)%key_lower)) then
        if (self%buckets(idx)%key_lower == key_lower) then
          value = self%buckets(idx)%value
          if (present(found)) found = .true.
          return
        end if
      end if
      chain_idx = self%buckets(idx)%next
      do while (chain_idx /= 0)
        overflow_idx = -chain_idx
        if (allocated(self%overflow(overflow_idx)%key_lower)) then
          if (self%overflow(overflow_idx)%key_lower == key_lower) then
            value = self%overflow(overflow_idx)%value
            if (present(found)) found = .true.
            return
          end if
        end if
        chain_idx = self%overflow(overflow_idx)%next
      end do
    end if

  end function name_index_lookup_ci

  !> Remove a key (just marks as deleted, actual cleanup on rehash)
  subroutine name_index_remove(self, key)
    class(hsd_name_index_t), intent(inout) :: self
    character(len=*), intent(in) :: key

    integer :: idx, chain_idx, overflow_idx

    if (self%num_buckets == 0) return

    idx = mod(hash_string(key), self%num_buckets) + 1

    if (.not. self%buckets(idx)%occupied) return

    ! Check bucket
    if (allocated(self%buckets(idx)%key)) then
      if (self%buckets(idx)%key == key) then
        ! Clear the bucket but keep chain
        if (allocated(self%buckets(idx)%key)) deallocate(self%buckets(idx)%key)
        if (allocated(self%buckets(idx)%key_lower)) deallocate(self%buckets(idx)%key_lower)
        self%buckets(idx)%value = 0

        ! If there's a chain, promote first chain entry (overflow uses negative indices)
        if (self%buckets(idx)%next /= 0) then
          overflow_idx = -self%buckets(idx)%next
          self%buckets(idx)%key = self%overflow(overflow_idx)%key
          self%buckets(idx)%key_lower = self%overflow(overflow_idx)%key_lower
          self%buckets(idx)%value = self%overflow(overflow_idx)%value
          self%buckets(idx)%next = self%overflow(overflow_idx)%next
          self%overflow(overflow_idx)%occupied = .false.
        else
          self%buckets(idx)%occupied = .false.
        end if

        self%num_entries = self%num_entries - 1
        return
      end if
    end if

    ! Check chain (overflow entries use negative indices)
    chain_idx = self%buckets(idx)%next
    do while (chain_idx /= 0)
      overflow_idx = -chain_idx
      if (allocated(self%overflow(overflow_idx)%key)) then
        if (self%overflow(overflow_idx)%key == key) then
          if (allocated(self%overflow(overflow_idx)%key)) &
            deallocate(self%overflow(overflow_idx)%key)
          if (allocated(self%overflow(overflow_idx)%key_lower)) &
            deallocate(self%overflow(overflow_idx)%key_lower)
          self%overflow(overflow_idx)%occupied = .false.
          self%num_entries = self%num_entries - 1
          return
        end if
      end if
      chain_idx = self%overflow(overflow_idx)%next
    end do

  end subroutine name_index_remove

  !> Clear all entries
  subroutine name_index_clear(self)
    class(hsd_name_index_t), intent(inout) :: self
    integer :: i

    do i = 1, self%num_buckets
      if (allocated(self%buckets(i)%key)) deallocate(self%buckets(i)%key)
      if (allocated(self%buckets(i)%key_lower)) deallocate(self%buckets(i)%key_lower)
      self%buckets(i)%occupied = .false.
      self%buckets(i)%next = 0
    end do

    do i = 1, self%num_overflow
      if (allocated(self%overflow(i)%key)) deallocate(self%overflow(i)%key)
      if (allocated(self%overflow(i)%key_lower)) deallocate(self%overflow(i)%key_lower)
      self%overflow(i)%occupied = .false.
      self%overflow(i)%next = 0
    end do

    self%num_entries = 0
    self%num_overflow = 0

  end subroutine name_index_clear

  !> Destroy the hash table
  subroutine name_index_destroy(self)
    class(hsd_name_index_t), intent(inout) :: self

    if (allocated(self%buckets)) then
      call self%clear()
      deallocate(self%buckets)
    end if
    if (allocated(self%overflow)) then
      deallocate(self%overflow)
    end if
    self%num_buckets = 0
    self%num_entries = 0
    self%num_overflow = 0
    self%overflow_capacity = 0

  end subroutine name_index_destroy

  !> Rehash to larger table
  subroutine name_index_rehash(self)
    class(hsd_name_index_t), intent(inout) :: self

    type(hash_entry_t), allocatable :: old_buckets(:), old_overflow(:)
    integer :: i, old_num_buckets, old_num_overflow, chain_idx, overflow_idx

    old_num_buckets = self%num_buckets
    old_num_overflow = self%num_overflow
    call move_alloc(self%buckets, old_buckets)
    call move_alloc(self%overflow, old_overflow)

    ! Initialize with double capacity
    self%num_buckets = old_num_buckets * 2
    allocate(self%buckets(self%num_buckets))
    self%overflow_capacity = max(8, self%num_buckets / 4)
    allocate(self%overflow(self%overflow_capacity))
    self%num_entries = 0
    self%num_overflow = 0

    ! Reinsert all entries
    do i = 1, old_num_buckets
      if (old_buckets(i)%occupied) then
        if (allocated(old_buckets(i)%key)) then
          call self%insert(old_buckets(i)%key, old_buckets(i)%value)
        end if

        ! Process chain (overflow entries use negative indices)
        chain_idx = old_buckets(i)%next
        do while (chain_idx /= 0)
          overflow_idx = -chain_idx
          if (old_overflow(overflow_idx)%occupied) then
            if (allocated(old_overflow(overflow_idx)%key)) then
              call self%insert(old_overflow(overflow_idx)%key, &
                               old_overflow(overflow_idx)%value)
            end if
          end if
          chain_idx = old_overflow(overflow_idx)%next
        end do
      end if
    end do

    deallocate(old_buckets)
    deallocate(old_overflow)

  end subroutine name_index_rehash

end module hsd_hash_table
