!> HSD Schema Validation
!>
!> This module provides declarative schema validation for HSD data structures.
!> Schemas define the expected structure of HSD documents including required
!> fields, optional fields, type constraints, value ranges, and enumerations.
!>
!> ## Example Usage
!>
!> ```fortran
!> use hsd
!> use hsd_schema
!>
!> type(hsd_schema_t) :: schema
!> type(hsd_table) :: root
!> type(hsd_error_t), allocatable :: errors(:)
!>
!> ! Define schema
!> call schema_init(schema)
!> call schema_add_field(schema, "Geometry", FIELD_REQUIRED, FIELD_TYPE_TABLE)
!> call schema_add_field(schema, "Geometry/Periodic", FIELD_OPTIONAL, FIELD_TYPE_LOGICAL)
!> call schema_add_field(schema, "Hamiltonian", FIELD_REQUIRED, FIELD_TYPE_TABLE)
!> call schema_add_field(schema, "Hamiltonian/MaxSCCIterations", FIELD_OPTIONAL, &
!>                       FIELD_TYPE_INTEGER, min_int=1, max_int=1000)
!> call schema_add_field(schema, "Hamiltonian/Mixer", FIELD_OPTIONAL, FIELD_TYPE_STRING, &
!>                       allowed_values=["Broyden", "Anderson", "Simple"])
!>
!> ! Load and validate
!> call hsd_load("input.hsd", root, error)
!> call schema_validate(schema, root, errors)
!>
!> if (size(errors) > 0) then
!>   do i = 1, size(errors)
!>     call errors(i)%print()
!>   end do
!> end if
!>
!> call schema_destroy(schema)
!> ```
!>
!> ## Thread Safety
!>
!> Schema objects are NOT thread-safe. Do not share a schema object between
!> threads without external synchronization. However, validation of different
!> HSD trees with the same (immutable) schema is thread-safe.
module hsd_schema
  use hsd_constants, only: dp
  use hsd_utils, only: to_lower
  use hsd_error, only: hsd_error_t, make_error, HSD_STAT_OK, HSD_STAT_NOT_FOUND, &
    HSD_STAT_TYPE_ERROR, HSD_STAT_SCHEMA_ERROR
  use hsd_types, only: hsd_node, hsd_table, hsd_value, &
    VALUE_TYPE_NONE, VALUE_TYPE_STRING, VALUE_TYPE_INTEGER, &
    VALUE_TYPE_REAL, VALUE_TYPE_LOGICAL, VALUE_TYPE_ARRAY, VALUE_TYPE_COMPLEX
  use hsd_query, only: hsd_get_child, hsd_has_child, hsd_child_count
  implicit none (type, external)
  private

  ! Public types
  public :: hsd_schema_t, hsd_field_def_t

  ! Public constants
  public :: FIELD_REQUIRED, FIELD_OPTIONAL
  public :: FIELD_TYPE_ANY, FIELD_TYPE_STRING, FIELD_TYPE_INTEGER
  public :: FIELD_TYPE_REAL, FIELD_TYPE_LOGICAL, FIELD_TYPE_TABLE
  public :: FIELD_TYPE_ARRAY, FIELD_TYPE_COMPLEX

  ! Public procedures
  public :: schema_init, schema_destroy
  public :: schema_add_field, schema_add_field_enum
  public :: schema_validate, schema_validate_strict

  !> Field requirement constants
  integer, parameter :: FIELD_REQUIRED = 1
  integer, parameter :: FIELD_OPTIONAL = 2

  !> Field type constants (map to VALUE_TYPE_* for values, plus TABLE)
  integer, parameter :: FIELD_TYPE_ANY = -1
  integer, parameter :: FIELD_TYPE_STRING = VALUE_TYPE_STRING
  integer, parameter :: FIELD_TYPE_INTEGER = VALUE_TYPE_INTEGER
  integer, parameter :: FIELD_TYPE_REAL = VALUE_TYPE_REAL
  integer, parameter :: FIELD_TYPE_LOGICAL = VALUE_TYPE_LOGICAL
  integer, parameter :: FIELD_TYPE_ARRAY = VALUE_TYPE_ARRAY
  integer, parameter :: FIELD_TYPE_COMPLEX = VALUE_TYPE_COMPLEX
  integer, parameter :: FIELD_TYPE_TABLE = 100  ! Special marker for table

  !> Wrapper type for allocatable-length strings in arrays
  type :: alloc_string_t
    character(len=:), allocatable :: val
  end type alloc_string_t

  !> Field definition
  type :: hsd_field_def_t
    !> Path to the field (e.g., "Geometry/Periodic")
    character(len=:), allocatable :: path
    !> Whether field is required or optional
    integer :: requirement = FIELD_OPTIONAL
    !> Expected type (FIELD_TYPE_*)
    integer :: field_type = FIELD_TYPE_ANY
    !> Minimum integer value (if applicable)
    integer :: min_int = -huge(1)
    !> Maximum integer value (if applicable)
    integer :: max_int = huge(1)
    !> Minimum real value (if applicable)
    real(dp) :: min_real = -huge(1.0_dp)
    !> Maximum real value (if applicable)
    real(dp) :: max_real = huge(1.0_dp)
    !> Allowed string values (for enum validation) — no length limit
    type(alloc_string_t), allocatable :: allowed_values(:)
    !> Number of allowed values
    integer :: num_allowed = 0
    !> Whether range constraints are active
    logical :: has_int_range = .false.
    logical :: has_real_range = .false.
    !> Description for error messages
    character(len=:), allocatable :: description
  end type hsd_field_def_t

  !> Schema definition
  type :: hsd_schema_t
    !> Field definitions
    type(hsd_field_def_t), allocatable :: fields(:)
    !> Number of fields defined
    integer :: num_fields = 0
    !> Capacity of fields array
    integer :: capacity = 0
    !> Schema name/description
    character(len=:), allocatable :: name
    !> Whether to allow unknown fields (default: yes)
    logical :: allow_unknown = .true.
  contains
    procedure :: init => schema_init_method
    procedure :: destroy => schema_destroy_method
    procedure :: add_field => schema_add_field_method
    procedure :: validate => schema_validate_method
  end type hsd_schema_t

contains

  !> Initialize a schema
  subroutine schema_init(schema, name, allow_unknown)
    type(hsd_schema_t), intent(out) :: schema
    character(len=*), intent(in), optional :: name
    logical, intent(in), optional :: allow_unknown

    call schema%init(name, allow_unknown)

  end subroutine schema_init

  !> Initialize a schema (method)
  subroutine schema_init_method(self, name, allow_unknown)
    class(hsd_schema_t), intent(out) :: self
    character(len=*), intent(in), optional :: name
    logical, intent(in), optional :: allow_unknown

    self%capacity = 16
    allocate(self%fields(self%capacity))
    self%num_fields = 0

    if (present(name)) self%name = name
    if (present(allow_unknown)) self%allow_unknown = allow_unknown

  end subroutine schema_init_method

  !> Destroy a schema
  subroutine schema_destroy(schema)
    type(hsd_schema_t), intent(inout) :: schema
    call schema%destroy()
  end subroutine schema_destroy

  !> Destroy a schema (method)
  subroutine schema_destroy_method(self)
    class(hsd_schema_t), intent(inout) :: self
    integer :: i

    if (allocated(self%fields)) then
      do i = 1, self%num_fields
        if (allocated(self%fields(i)%path)) deallocate(self%fields(i)%path)
        if (allocated(self%fields(i)%description)) deallocate(self%fields(i)%description)
        if (allocated(self%fields(i)%allowed_values)) deallocate(self%fields(i)%allowed_values)
      end do
      deallocate(self%fields)
    end if
    if (allocated(self%name)) deallocate(self%name)
    self%num_fields = 0
    self%capacity = 0

  end subroutine schema_destroy_method

  !> Add a field definition to the schema
  subroutine schema_add_field(schema, path, requirement, field_type, &
      min_int, max_int, min_real, max_real, description)
    type(hsd_schema_t), intent(inout) :: schema
    character(len=*), intent(in) :: path
    integer, intent(in) :: requirement
    integer, intent(in), optional :: field_type
    integer, intent(in), optional :: min_int, max_int
    real(dp), intent(in), optional :: min_real, max_real
    character(len=*), intent(in), optional :: description

    type(hsd_field_def_t), allocatable :: tmp(:)
    integer :: new_capacity

    ! Initialize if needed
    if (schema%capacity == 0) call schema%init()

    ! Grow array if needed
    if (schema%num_fields >= schema%capacity) then
      new_capacity = schema%capacity * 2
      allocate(tmp(new_capacity))
      tmp(1:schema%num_fields) = schema%fields(1:schema%num_fields)
      call move_alloc(tmp, schema%fields)
      schema%capacity = new_capacity
    end if

    ! Add field definition
    schema%num_fields = schema%num_fields + 1
    schema%fields(schema%num_fields)%path = path
    schema%fields(schema%num_fields)%requirement = requirement

    if (present(field_type)) then
      schema%fields(schema%num_fields)%field_type = field_type
    end if

    if (present(min_int)) then
      schema%fields(schema%num_fields)%min_int = min_int
      schema%fields(schema%num_fields)%has_int_range = .true.
    end if
    if (present(max_int)) then
      schema%fields(schema%num_fields)%max_int = max_int
      schema%fields(schema%num_fields)%has_int_range = .true.
    end if

    if (present(min_real)) then
      schema%fields(schema%num_fields)%min_real = min_real
      schema%fields(schema%num_fields)%has_real_range = .true.
    end if
    if (present(max_real)) then
      schema%fields(schema%num_fields)%max_real = max_real
      schema%fields(schema%num_fields)%has_real_range = .true.
    end if

    if (present(description)) then
      schema%fields(schema%num_fields)%description = description
    end if

  end subroutine schema_add_field

  !> Add a field with enumerated allowed values
  subroutine schema_add_field_enum(schema, path, requirement, allowed_values, description)
    type(hsd_schema_t), intent(inout) :: schema
    character(len=*), intent(in) :: path
    integer, intent(in) :: requirement
    character(len=*), intent(in) :: allowed_values(:)
    character(len=*), intent(in), optional :: description

    integer :: i, n

    call schema_add_field(schema, path, requirement, FIELD_TYPE_STRING, &
                          description=description)

    n = size(allowed_values)
    allocate(schema%fields(schema%num_fields)%allowed_values(n))
    do i = 1, n
      schema%fields(schema%num_fields)%allowed_values(i)%val = trim(allowed_values(i))
    end do
    schema%fields(schema%num_fields)%num_allowed = n

  end subroutine schema_add_field_enum

  !> Add a field to schema (method wrapper)
  subroutine schema_add_field_method(self, path, requirement, field_type, &
      min_int, max_int, min_real, max_real, description)
    class(hsd_schema_t), intent(inout) :: self
    character(len=*), intent(in) :: path
    integer, intent(in) :: requirement
    integer, intent(in), optional :: field_type
    integer, intent(in), optional :: min_int, max_int
    real(dp), intent(in), optional :: min_real, max_real
    character(len=*), intent(in), optional :: description

    call schema_add_field(self, path, requirement, field_type, &
                          min_int, max_int, min_real, max_real, description)

  end subroutine schema_add_field_method

  !> Validate an HSD tree against the schema
  !>
  !> Returns an array of all validation errors found.
  subroutine schema_validate(schema, root, errors)
    type(hsd_schema_t), intent(in) :: schema
    type(hsd_table), intent(in), target :: root
    type(hsd_error_t), allocatable, intent(out) :: errors(:)

    type(hsd_error_t), allocatable :: error_list(:)
    type(hsd_error_t), allocatable :: error
    integer :: i, num_errors

    allocate(error_list(schema%num_fields))
    num_errors = 0

    do i = 1, schema%num_fields
      call validate_field(root, schema%fields(i), error)
      if (allocated(error)) then
        num_errors = num_errors + 1
        error_list(num_errors) = error
        deallocate(error)
      end if
    end do

    ! Return only actual errors
    if (num_errors > 0) then
      allocate(errors(num_errors))
      errors = error_list(1:num_errors)
    else
      allocate(errors(0))
    end if

  end subroutine schema_validate

  !> Validate an HSD tree strictly (also checks for unknown fields)
  !>
  !> Performs all checks from schema_validate, then walks every node in the
  !> tree and reports any children whose path is **not** declared in the
  !> schema.  Only the first level of children under `root` (and their
  !> sub-paths) is checked; nested children are checked recursively.
  subroutine schema_validate_strict(schema, root, errors)
    type(hsd_schema_t), intent(in) :: schema
    type(hsd_table), intent(in), target :: root
    type(hsd_error_t), allocatable, intent(out) :: errors(:)

    type(hsd_error_t), allocatable :: base_errors(:)
    type(hsd_error_t), allocatable :: unknown_errors(:)
    type(hsd_error_t), allocatable :: combined(:)
    integer :: num_unknown, cap_unknown

    ! Run standard validation first
    call schema_validate(schema, root, base_errors)

    ! Walk the tree to find unknown fields (dynamically growable)
    cap_unknown = 32
    allocate(unknown_errors(cap_unknown))
    num_unknown = 0
    call collect_unknown_fields(schema, root, "", unknown_errors, num_unknown, cap_unknown)

    ! Combine both error lists
    if (num_unknown > 0) then
      allocate(combined(size(base_errors) + num_unknown))
      if (size(base_errors) > 0) combined(1:size(base_errors)) = base_errors
      combined(size(base_errors)+1:) = unknown_errors(1:num_unknown)
      call move_alloc(combined, errors)
    else
      call move_alloc(base_errors, errors)
    end if

  end subroutine schema_validate_strict

  !> Recursively collect children not declared in the schema
  recursive subroutine collect_unknown_fields(schema, table, prefix, &
      errors, num_errors, capacity)
    type(hsd_schema_t), intent(in) :: schema
    type(hsd_table), intent(in) :: table
    character(len=*), intent(in) :: prefix
    type(hsd_error_t), intent(inout), allocatable :: errors(:)
    integer, intent(inout) :: num_errors
    integer, intent(inout) :: capacity

    class(hsd_node), pointer :: child
    character(len=:), allocatable :: child_path
    type(hsd_error_t), allocatable :: tmp_error
    type(hsd_error_t), allocatable :: tmp_arr(:)
    integer :: i, new_cap
    logical :: found

    do i = 1, table%num_children
      call table%get_child(i, child)
      if (.not. associated(child)) cycle
      if (.not. allocated(child%name)) cycle
      if (len_trim(child%name) == 0) cycle

      ! Build the full path for this child
      if (len_trim(prefix) > 0) then
        child_path = prefix // "/" // trim(child%name)
      else
        child_path = trim(child%name)
      end if

      ! Check whether this path is declared in the schema
      found = path_in_schema(schema, child_path)

      if (.not. found) then
        ! Grow array if needed
        if (num_errors >= capacity) then
          new_cap = capacity * 2
          allocate(tmp_arr(new_cap))
          tmp_arr(1:num_errors) = errors(1:num_errors)
          call move_alloc(tmp_arr, errors)
          capacity = new_cap
        end if
        num_errors = num_errors + 1
        call make_error(tmp_error, HSD_STAT_SCHEMA_ERROR, &
          "Unknown field '" // child_path // "' not declared in schema")
        errors(num_errors) = tmp_error
        deallocate(tmp_error)
      end if

      ! Recurse into sub-tables
      select type (child)
      type is (hsd_table)
        call collect_unknown_fields(schema, child, child_path, errors, num_errors, capacity)
      end select
    end do

  end subroutine collect_unknown_fields

  !> Check if a path (case-insensitive) matches any schema field path
  !> Also returns true if the path is a prefix of a schema field
  !> (i.e. intermediate table on the way to a declared leaf).
  pure function path_in_schema(schema, path) result(found)
    type(hsd_schema_t), intent(in) :: schema
    character(len=*), intent(in) :: path
    logical :: found

    character(len=:), allocatable :: lower_path, lower_field
    integer :: i, path_len

    found = .false.
    lower_path = to_lower(path)
    path_len = len(lower_path)

    do i = 1, schema%num_fields
      lower_field = to_lower(schema%fields(i)%path)

      ! Exact match
      if (lower_field == lower_path) then
        found = .true.
        return
      end if

      ! Path is a prefix of a declared field (intermediate table)
      if (len(lower_field) > path_len) then
        if (lower_field(1:path_len) == lower_path .and. &
            lower_field(path_len+1:path_len+1) == "/") then
          found = .true.
          return
        end if
      end if
    end do

  end function path_in_schema

  !> Validate method wrapper
  subroutine schema_validate_method(self, root, errors)
    class(hsd_schema_t), intent(in) :: self
    type(hsd_table), intent(in), target :: root
    type(hsd_error_t), allocatable, intent(out) :: errors(:)

    call schema_validate(self, root, errors)

  end subroutine schema_validate_method

  !> Validate a single field against its definition
  subroutine validate_field(root, field_def, error)
    type(hsd_table), intent(in), target :: root
    type(hsd_field_def_t), intent(in) :: field_def
    type(hsd_error_t), allocatable, intent(out) :: error

    class(hsd_node), pointer :: child
    integer :: stat, actual_type, int_val
    real(dp) :: real_val
    character(len=:), allocatable :: str_val

    ! Check if field exists
    call hsd_get_child(root, field_def%path, child, stat)

    if (stat /= HSD_STAT_OK .or. .not. associated(child)) then
      if (field_def%requirement == FIELD_REQUIRED) then
        call make_error(error, HSD_STAT_NOT_FOUND, &
          "Required field '" // field_def%path // "' not found")
      end if
      return
    end if

    ! Check type
    if (field_def%field_type /= FIELD_TYPE_ANY) then
      select type (child)
      type is (hsd_table)
        if (field_def%field_type /= FIELD_TYPE_TABLE) then
          call make_error(error, HSD_STAT_TYPE_ERROR, &
            "Field '" // field_def%path // "' is a table, expected " // &
            get_type_name(field_def%field_type))
          return
        end if

      type is (hsd_value)
        actual_type = child%value_type

        if (field_def%field_type == FIELD_TYPE_TABLE) then
          call make_error(error, HSD_STAT_TYPE_ERROR, &
            "Field '" // field_def%path // "' is a value, expected table")
          return
        end if

        ! For type checking, we verify that the value can be converted to the expected type.
        ! This matches HSD's behavior: values are strings, converted on access.
        select case (field_def%field_type)
        case (FIELD_TYPE_INTEGER)
          call child%get_integer(int_val, stat)
          if (stat /= HSD_STAT_OK) then
            call make_error(error, HSD_STAT_TYPE_ERROR, &
              "Field '" // field_def%path // "' cannot be converted to integer")
            return
          end if
          ! Validate integer range
          if (field_def%has_int_range) then
            if (int_val < field_def%min_int .or. int_val > field_def%max_int) then
              call make_range_error(error, field_def%path, &
                  int_val, field_def%min_int, field_def%max_int)
              return
            end if
          end if

        case (FIELD_TYPE_REAL)
          call child%get_real(real_val, stat)
          if (stat /= HSD_STAT_OK) then
            call make_error(error, HSD_STAT_TYPE_ERROR, &
              "Field '" // field_def%path // "' cannot be converted to real")
            return
          end if
          ! Validate real range
          if (field_def%has_real_range) then
            if (real_val < field_def%min_real .or. real_val > field_def%max_real) then
              call make_range_error_real(error, field_def%path, &
                  real_val, field_def%min_real, field_def%max_real)
              return
            end if
          end if

        case (FIELD_TYPE_LOGICAL)
          block
            logical :: log_val
            call child%get_logical(log_val, stat)
            if (stat /= HSD_STAT_OK) then
              call make_error(error, HSD_STAT_TYPE_ERROR, &
                "Field '" // field_def%path // "' cannot be converted to logical")
              return
            end if
          end block

        case (FIELD_TYPE_STRING)
          call child%get_string(str_val, stat)
          if (stat /= HSD_STAT_OK) then
            call make_error(error, HSD_STAT_TYPE_ERROR, &
              "Field '" // field_def%path // "' cannot be converted to string")
            return
          end if
          ! Validate enum values
          if (field_def%num_allowed > 0) then
            if (.not. is_allowed_value(str_val, field_def)) then
              call make_enum_error(error, field_def%path, str_val, field_def)
              return
            end if
          end if

        case (FIELD_TYPE_ARRAY)
          if (actual_type /= VALUE_TYPE_ARRAY .and. &
              .not. allocated(child%int_array) .and. &
              .not. allocated(child%real_array) .and. &
              .not. allocated(child%raw_text)) then
            call make_error(error, HSD_STAT_TYPE_ERROR, &
              "Field '" // field_def%path // "' is not an array")
            return
          end if

        case (FIELD_TYPE_COMPLEX)
          if (actual_type /= VALUE_TYPE_COMPLEX) then
            call make_error(error, HSD_STAT_TYPE_ERROR, &
              "Field '" // field_def%path // "' is not a complex value")
            return
          end if

        case default
          ! Unknown field type - no validation needed
          continue

        end select
      end select
    end if

  end subroutine validate_field

  !> Check if a value is in the allowed list
  pure function is_allowed_value(val, field_def) result(allowed)
    character(len=*), intent(in) :: val
    type(hsd_field_def_t), intent(in) :: field_def
    logical :: allowed

    integer :: i
    character(len=:), allocatable :: val_lower

    allowed = .false.
    val_lower = to_lower(trim(val))

    do i = 1, field_def%num_allowed
      if (to_lower(trim(field_def%allowed_values(i)%val)) == val_lower) then
        allowed = .true.
        return
      end if
    end do

  end function is_allowed_value

  !> Get human-readable type name
  pure function get_type_name(type_id) result(name)
    integer, intent(in) :: type_id
    character(len=:), allocatable :: name

    select case (type_id)
    case (FIELD_TYPE_ANY)
      name = "any"
    case (FIELD_TYPE_STRING)
      name = "string"
    case (FIELD_TYPE_INTEGER)
      name = "integer"
    case (FIELD_TYPE_REAL)
      name = "real"
    case (FIELD_TYPE_LOGICAL)
      name = "logical"
    case (FIELD_TYPE_ARRAY)
      name = "array"
    case (FIELD_TYPE_COMPLEX)
      name = "complex"
    case (FIELD_TYPE_TABLE)
      name = "table"
    case default
      name = "unknown"
    end select

  end function get_type_name

  !> Make integer range error
  subroutine make_range_error(error, path, val, min_val, max_val)
    type(hsd_error_t), allocatable, intent(out) :: error
    character(len=*), intent(in) :: path
    integer, intent(in) :: val, min_val, max_val

    character(len=64) :: val_str, min_str, max_str

    write(val_str, '(I0)') val
    write(min_str, '(I0)') min_val
    write(max_str, '(I0)') max_val

    call make_error(error, HSD_STAT_SCHEMA_ERROR, &
      "Field '" // path // "' value " // trim(val_str) // &
      " is outside range [" // trim(min_str) // ", " // trim(max_str) // "]")

  end subroutine make_range_error

  !> Make real range error
  subroutine make_range_error_real(error, path, val, min_val, max_val)
    type(hsd_error_t), allocatable, intent(out) :: error
    character(len=*), intent(in) :: path
    real(dp), intent(in) :: val, min_val, max_val

    character(len=64) :: val_str, min_str, max_str

    write(val_str, '(G0)') val
    write(min_str, '(G0)') min_val
    write(max_str, '(G0)') max_val

    call make_error(error, HSD_STAT_SCHEMA_ERROR, &
      "Field '" // path // "' value " // trim(val_str) // &
      " is outside range [" // trim(min_str) // ", " // trim(max_str) // "]")

  end subroutine make_range_error_real

  !> Make enum error
  subroutine make_enum_error(error, path, val, field_def)
    type(hsd_error_t), allocatable, intent(out) :: error
    character(len=*), intent(in) :: path, val
    type(hsd_field_def_t), intent(in) :: field_def

    character(len=1024) :: allowed_list
    integer :: i

    allowed_list = ""
    do i = 1, field_def%num_allowed
      if (i > 1) allowed_list = trim(allowed_list) // ", "
      allowed_list = trim(allowed_list) // "'" // trim(field_def%allowed_values(i)%val) // "'"
    end do

    call make_error(error, HSD_STAT_SCHEMA_ERROR, &
      "Field '" // path // "' value '" // val // &
      "' is not one of: " // trim(allowed_list))

  end subroutine make_enum_error

end module hsd_schema
