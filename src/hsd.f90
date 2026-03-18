!> HSD - Human-friendly Structured Data for Fortran
!>
!> This is the main public API module for the HSD library.
!> It re-exports all necessary types and procedures for working with HSD data.
!>
!> The API is organized into several focused submodules:
!> - hsd_api: Core API (accessors, mutators, query)
!> - hsd_validation: Data validation (hsd_require, hsd_validate_*)
!>
!> ## Thread Safety
!>
!> The HSD library is designed for single-threaded use but supports certain
!> concurrent access patterns:
!>
!> - **Thread-safe**: Parsing different files concurrently
!> - **NOT thread-safe**: Reading from a shared tree modifies the internal
!>   `processed` flag used for validation, modifying a shared tree,
!>   using shared iterators, parsing to the same tree
!> - **Workaround**: If strict thread safety is needed for validation logic,
!>   synchronize access.
!>
!> For detailed thread safety information, see docs/thread_safety.rst
!>
!> ## Example usage
!>
!> ```fortran
!> use hsd
!> type(hsd_node_t) :: root
!> type(hsd_error_t), allocatable :: error
!> integer :: value
!>
!> call hsd_load_file("input.hsd", root, error)
!> if (allocated(error)) then
!>   call error%print()
!>   stop 1
!> end if
!>
!> call hsd_get(root, "some/path/to/value", value)
!> ```
module hsd
  ! Core infrastructure
  use hsd_constants, only: dp
  use hsd_utils, only: string_buffer_t
  use hsd_error, only: hsd_error_t, &
    HSD_STAT_OK, HSD_STAT_SYNTAX_ERROR, HSD_STAT_UNCLOSED_TAG, &
    HSD_STAT_UNCLOSED_ATTRIB, HSD_STAT_UNCLOSED_QUOTE, HSD_STAT_ORPHAN_TEXT, &
    HSD_STAT_INCLUDE_CYCLE, HSD_STAT_INCLUDE_DEPTH, HSD_STAT_FILE_NOT_FOUND, &
    HSD_STAT_IO_ERROR, HSD_STAT_TYPE_ERROR, HSD_STAT_NOT_FOUND
  use hsd_types, only: hsd_node_t, hsd_node_ptr_t, hsd_iterator_t, &
    new_table, new_value, &
    NODE_TYPE_TABLE, NODE_TYPE_VALUE, &
    VALUE_TYPE_NONE, VALUE_TYPE_STRING, VALUE_TYPE_INTEGER, &
    VALUE_TYPE_REAL, VALUE_TYPE_LOGICAL, VALUE_TYPE_ARRAY, VALUE_TYPE_COMPLEX
  use hsd_parser, only: hsd_load_file, hsd_load_string
  use hsd_formatter, only: hsd_dump, hsd_dump_to_string

  ! Unified API module
  use hsd_api, only: &
    ! Accessors
    hsd_get, hsd_get_or_set, hsd_get_matrix, hsd_get_inline_text, &
    ! Mutators
    hsd_set, hsd_clear_children, &
    ! Query
    hsd_get_child, hsd_get_table, hsd_has_child, &
    hsd_remove_child, hsd_get_type, hsd_is_table, hsd_is_value, hsd_is_array, &
    hsd_child_count, hsd_get_keys, hsd_get_attrib, hsd_has_attrib, hsd_set_attrib, &
    hsd_rename_child, hsd_get_choice, hsd_get_children, &
    hsd_get_child_tables, &
    hsd_merge, hsd_clone, hsd_table_equal, hsd_set_processed, &
    hsd_has_value_children, hsd_get_name

  use hsd_validation, only: hsd_require, hsd_validate_range, hsd_validate_one_of, &
    hsd_get_with_unit, hsd_get_array_with_unit, hsd_get_matrix_with_unit, &
    hsd_node_context, hsd_format_error, hsd_format_warning, &
    hsd_warn_unprocessed, MAX_WARNING_LEN

  implicit none (type, external)
  private

  ! Re-export public types and constants
  public :: dp
  public :: string_buffer_t
  public :: hsd_error_t
  public :: HSD_STAT_OK, HSD_STAT_SYNTAX_ERROR, HSD_STAT_UNCLOSED_TAG
  public :: HSD_STAT_UNCLOSED_ATTRIB, HSD_STAT_UNCLOSED_QUOTE, HSD_STAT_ORPHAN_TEXT
  public :: HSD_STAT_INCLUDE_CYCLE, HSD_STAT_INCLUDE_DEPTH, HSD_STAT_FILE_NOT_FOUND
  public :: HSD_STAT_IO_ERROR, HSD_STAT_TYPE_ERROR, HSD_STAT_NOT_FOUND
  public :: hsd_node_t, hsd_node_ptr_t, hsd_iterator_t
  public :: NODE_TYPE_TABLE, NODE_TYPE_VALUE
  public :: new_table, new_value
  public :: VALUE_TYPE_NONE, VALUE_TYPE_STRING, VALUE_TYPE_INTEGER
  public :: VALUE_TYPE_REAL, VALUE_TYPE_LOGICAL, VALUE_TYPE_ARRAY
  public :: VALUE_TYPE_COMPLEX

  ! Re-export I/O procedures
  public :: hsd_load_file, hsd_load_string
  public :: hsd_dump, hsd_dump_to_string

  ! Re-export Unified API
  public :: hsd_get, hsd_get_or_set, hsd_get_matrix
  public :: hsd_get_inline_text
  public :: hsd_set, hsd_clear_children
  public :: hsd_get_child, hsd_get_table, hsd_has_child
  public :: hsd_remove_child
  public :: hsd_get_type, hsd_is_table, hsd_is_value, hsd_is_array
  public :: hsd_child_count, hsd_get_keys
  public :: hsd_get_attrib, hsd_has_attrib, hsd_set_attrib
  public :: hsd_rename_child, hsd_get_choice
  public :: hsd_get_children
  public :: hsd_get_child_tables
  public :: hsd_merge, hsd_clone
  public :: hsd_table_equal
  public :: hsd_set_processed
  public :: hsd_has_value_children, hsd_get_name

  ! Re-export validation (from hsd_validation)
  public :: hsd_require, hsd_validate_range, hsd_validate_one_of
  public :: hsd_get_with_unit
  public :: hsd_get_array_with_unit, hsd_get_matrix_with_unit
  public :: hsd_node_context, hsd_format_error, hsd_format_warning
  public :: hsd_warn_unprocessed, MAX_WARNING_LEN

end module hsd
