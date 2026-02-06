!> HSD - Human-friendly Structured Data for Fortran
!>
!> This is the main public API module for the HSD library.
!> It re-exports all necessary types and procedures for working with HSD data.
!>
!> The API is organized into several focused submodules:
!> - hsd_accessors: Data retrieval (hsd_get, hsd_get_or, hsd_get_matrix)
!> - hsd_mutators: Data modification (hsd_set)
!> - hsd_query: Navigation and tree operations (hsd_get_child, hsd_merge, hsd_clone)
!> - hsd_validation: Data validation (hsd_require, hsd_validate_*)
!> - hsd_schema: Declarative schema validation
!>
!> ## Thread Safety
!>
!> The HSD library is designed for single-threaded use but supports certain
!> concurrent access patterns:
!>
!> - **Thread-safe**: Parsing different files concurrently, reading from a
!>   shared tree (read-only), validating with a shared schema
!> - **NOT thread-safe**: Modifying a shared tree, using shared iterators,
!>   parsing to the same tree
!>
!> For detailed thread safety information, see docs/thread_safety.rst
!>
!> ## Example usage
!>
!> ```fortran
!> use hsd
!> type(hsd_table) :: root
!> type(hsd_error_t), allocatable :: error
!> integer :: value
!>
!> call hsd_load("input.hsd", root, error)
!> if (allocated(error)) then
!>   call error%print()
!>   stop 1
!> end if
!>
!> call hsd_get(root, "some/path/to/value", value)
!> ```
module hsd
  ! Core infrastructure
  use hsd_constants, only: dp, sp
  use hsd_error, only: hsd_error_t, &
    HSD_STAT_OK, HSD_STAT_SYNTAX_ERROR, HSD_STAT_UNCLOSED_TAG, &
    HSD_STAT_UNCLOSED_ATTRIB, HSD_STAT_UNCLOSED_QUOTE, HSD_STAT_ORPHAN_TEXT, &
    HSD_STAT_INCLUDE_CYCLE, HSD_STAT_INCLUDE_DEPTH, HSD_STAT_FILE_NOT_FOUND, &
    HSD_STAT_IO_ERROR, HSD_STAT_TYPE_ERROR, HSD_STAT_NOT_FOUND, HSD_STAT_SCHEMA_ERROR
  use hsd_types, only: hsd_node, hsd_table, hsd_value, hsd_node_ptr, hsd_iterator, &
    new_table, new_value, &
    VALUE_TYPE_NONE, VALUE_TYPE_STRING, VALUE_TYPE_INTEGER, &
    VALUE_TYPE_REAL, VALUE_TYPE_LOGICAL, VALUE_TYPE_ARRAY, VALUE_TYPE_COMPLEX
  use hsd_parser, only: hsd_parse, hsd_parse_string
  use hsd_formatter, only: hsd_dump, hsd_dump_to_string
  use hsd_visitor, only: hsd_visitor_t, hsd_accept

  ! Specialized API modules
  use hsd_accessors, only: hsd_get, hsd_get_or, hsd_get_matrix
  use hsd_mutators, only: hsd_set
  use hsd_query, only: hsd_get_child, hsd_get_table, hsd_has_child, &
    hsd_remove_child, hsd_get_type, hsd_is_table, hsd_is_value, hsd_is_array, &
    hsd_child_count, hsd_get_keys, hsd_get_attrib, hsd_has_attrib, &
    hsd_merge, hsd_clone
  use hsd_validation, only: hsd_require, hsd_validate_range, hsd_validate_one_of, &
    hsd_get_with_unit
  use hsd_schema, only: hsd_schema_t, hsd_field_def_t, &
    FIELD_REQUIRED, FIELD_OPTIONAL, &
    FIELD_TYPE_ANY, FIELD_TYPE_STRING, FIELD_TYPE_INTEGER, &
    FIELD_TYPE_REAL, FIELD_TYPE_LOGICAL, FIELD_TYPE_TABLE, &
    FIELD_TYPE_ARRAY, FIELD_TYPE_COMPLEX, &
    schema_init, schema_destroy, schema_add_field, schema_add_field_enum, &
    schema_validate, schema_validate_strict

  implicit none (type, external)
  private

  ! Re-export public types and constants
  public :: dp, sp
  public :: hsd_error_t
  public :: HSD_STAT_OK, HSD_STAT_SYNTAX_ERROR, HSD_STAT_UNCLOSED_TAG
  public :: HSD_STAT_UNCLOSED_ATTRIB, HSD_STAT_UNCLOSED_QUOTE, HSD_STAT_ORPHAN_TEXT
  public :: HSD_STAT_INCLUDE_CYCLE, HSD_STAT_INCLUDE_DEPTH, HSD_STAT_FILE_NOT_FOUND
  public :: HSD_STAT_IO_ERROR, HSD_STAT_TYPE_ERROR, HSD_STAT_NOT_FOUND
  public :: hsd_node, hsd_table, hsd_value, hsd_node_ptr, hsd_iterator
  public :: new_table, new_value
  public :: VALUE_TYPE_NONE, VALUE_TYPE_STRING, VALUE_TYPE_INTEGER
  public :: VALUE_TYPE_REAL, VALUE_TYPE_LOGICAL, VALUE_TYPE_ARRAY
  public :: VALUE_TYPE_COMPLEX

  ! Re-export I/O procedures
  public :: hsd_load, hsd_load_string
  public :: hsd_dump, hsd_dump_to_string

  ! Re-export data accessors (from hsd_accessors)
  public :: hsd_get, hsd_get_or, hsd_get_matrix

  ! Re-export data mutators (from hsd_mutators)
  public :: hsd_set

  ! Re-export query operations (from hsd_query)
  public :: hsd_get_child, hsd_get_table, hsd_has_child
  public :: hsd_remove_child
  public :: hsd_get_type, hsd_is_table, hsd_is_value, hsd_is_array
  public :: hsd_child_count, hsd_get_keys
  public :: hsd_get_attrib, hsd_has_attrib
  public :: hsd_merge, hsd_clone

  ! Re-export validation (from hsd_validation)
  public :: hsd_require, hsd_validate_range, hsd_validate_one_of
  public :: hsd_get_with_unit

  ! Re-export schema validation (from hsd_schema)
  public :: hsd_schema_t, hsd_field_def_t
  public :: FIELD_REQUIRED, FIELD_OPTIONAL
  public :: FIELD_TYPE_ANY, FIELD_TYPE_STRING, FIELD_TYPE_INTEGER
  public :: FIELD_TYPE_REAL, FIELD_TYPE_LOGICAL, FIELD_TYPE_TABLE
  public :: FIELD_TYPE_ARRAY, FIELD_TYPE_COMPLEX, HSD_STAT_SCHEMA_ERROR
  public :: schema_init, schema_destroy, schema_add_field, schema_add_field_enum
  public :: schema_validate, schema_validate_strict

  ! Re-export visitor pattern
  public :: hsd_visitor_t, hsd_accept

contains

  !> Load HSD from a file (convenience wrapper)
  subroutine hsd_load(filename, root, error)
    character(len=*), intent(in) :: filename
    type(hsd_table), intent(out) :: root
    type(hsd_error_t), allocatable, intent(out), optional :: error

    call hsd_parse(filename, root, error)

  end subroutine hsd_load

  !> Load HSD from a string (convenience wrapper)
  subroutine hsd_load_string(source, root, error, filename)
    character(len=*), intent(in) :: source
    type(hsd_table), intent(out) :: root
    type(hsd_error_t), allocatable, intent(out), optional :: error
    character(len=*), intent(in), optional :: filename

    call hsd_parse_string(source, root, error, filename)

  end subroutine hsd_load_string

end module hsd
