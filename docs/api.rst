API Reference
=============

This document provides a complete reference for all public procedures and types in HSD-Fortran.

Module: hsd
-----------

The main module that re-exports all public types and procedures. This is the only module
you need to use in most cases.

.. code-block:: fortran

   use hsd

Types
-----

Core Types
~~~~~~~~~~

``hsd_table``
^^^^^^^^^^^^^

A container node that holds child nodes. This is the main type used for parsed HSD documents.

.. code-block:: fortran

   type(hsd_table) :: root

``hsd_value``
^^^^^^^^^^^^^

A leaf node containing a scalar value or array.

.. code-block:: fortran

   type(hsd_value) :: val

``hsd_error_t``
^^^^^^^^^^^^^^^

Error information returned from parsing and validation operations.

.. code-block:: fortran

   type :: hsd_error_t
     integer :: code                           ! Error code (HSD_STAT_*)
     character(len=:), allocatable :: message  ! Human-readable message
     character(len=:), allocatable :: filename ! Source file
     integer :: line_start, line_end           ! Line range
     integer :: column                         ! Column number
     character(len=:), allocatable :: expected ! What was expected
     character(len=:), allocatable :: actual   ! What was found
     character(len=:), allocatable :: hint     ! Helpful suggestion
   contains
     procedure :: print                        ! Print formatted error
   end type

``hsd_iterator``
^^^^^^^^^^^^^^^^

Iterator for traversing table children.

.. code-block:: fortran

   type(hsd_iterator) :: iter

``hsd_schema_t``
^^^^^^^^^^^^^^^^

Schema definition for input validation.

.. code-block:: fortran

   type(hsd_schema_t) :: schema

``hsd_visitor_t``
^^^^^^^^^^^^^^^^^

Base type for visitor pattern implementation.

.. code-block:: fortran

   type, abstract :: hsd_visitor_t
   contains
     procedure(visit_table_iface), deferred :: visit_table
     procedure(visit_value_iface), deferred :: visit_value
   end type

Precision Constants
~~~~~~~~~~~~~~~~~~~

.. code-block:: fortran

   integer, parameter :: dp = kind(1.0d0)  ! Double precision
   integer, parameter :: sp = kind(1.0)    ! Single precision

Value Type Constants
~~~~~~~~~~~~~~~~~~~~

.. code-block:: fortran

   integer, parameter :: VALUE_TYPE_NONE    = 0
   integer, parameter :: VALUE_TYPE_STRING  = 1
   integer, parameter :: VALUE_TYPE_INTEGER = 2
   integer, parameter :: VALUE_TYPE_REAL    = 3
   integer, parameter :: VALUE_TYPE_LOGICAL = 4
   integer, parameter :: VALUE_TYPE_ARRAY   = 5
   integer, parameter :: VALUE_TYPE_COMPLEX = 6

Error Codes
~~~~~~~~~~~

.. code-block:: fortran

   integer, parameter :: HSD_STAT_OK             = 0   ! Success
   integer, parameter :: HSD_STAT_SYNTAX_ERROR   = 1   ! Generic syntax error
   integer, parameter :: HSD_STAT_UNCLOSED_TAG   = 2   ! Block not closed
   integer, parameter :: HSD_STAT_UNCLOSED_ATTRIB = 3  ! Attribute bracket not closed
   integer, parameter :: HSD_STAT_UNCLOSED_QUOTE = 4   ! String quote not closed
   integer, parameter :: HSD_STAT_ORPHAN_TEXT    = 5   ! Text outside any block
   integer, parameter :: HSD_STAT_INCLUDE_CYCLE  = 6   ! Circular include detected
   integer, parameter :: HSD_STAT_INCLUDE_DEPTH  = 7   ! Too many nested includes
   integer, parameter :: HSD_STAT_FILE_NOT_FOUND = 8   ! File doesn't exist
   integer, parameter :: HSD_STAT_IO_ERROR       = 9   ! I/O operation failed
   integer, parameter :: HSD_STAT_TYPE_ERROR     = 10  ! Type conversion failed
   integer, parameter :: HSD_STAT_NOT_FOUND      = 11  ! Key not found in tree
   integer, parameter :: HSD_STAT_SCHEMA_ERROR   = 20  ! Schema validation failed

Schema Constants
~~~~~~~~~~~~~~~~

.. code-block:: fortran

   ! Required/optional
   integer, parameter :: FIELD_REQUIRED = 1
   integer, parameter :: FIELD_OPTIONAL = 0

   ! Field types
   integer, parameter :: FIELD_TYPE_ANY     = 0
   integer, parameter :: FIELD_TYPE_STRING  = 1
   integer, parameter :: FIELD_TYPE_INTEGER = 2
   integer, parameter :: FIELD_TYPE_REAL    = 3
   integer, parameter :: FIELD_TYPE_LOGICAL = 4
   integer, parameter :: FIELD_TYPE_TABLE   = 5
   integer, parameter :: FIELD_TYPE_ARRAY   = 6
   integer, parameter :: FIELD_TYPE_COMPLEX = 7

I/O Procedures
--------------

hsd_load
~~~~~~~~

Load and parse an HSD file.

.. code-block:: fortran

   subroutine hsd_load(filename, root, error)
     character(len=*), intent(in) :: filename
     type(hsd_table), intent(out) :: root
     type(hsd_error_t), allocatable, intent(out), optional :: error
   end subroutine

hsd_load_string
~~~~~~~~~~~~~~~

Parse HSD from a string.

.. code-block:: fortran

   subroutine hsd_load_string(source, root, error, filename)
     character(len=*), intent(in) :: source
     type(hsd_table), intent(out) :: root
     type(hsd_error_t), allocatable, intent(out), optional :: error
     character(len=*), intent(in), optional :: filename  ! For error messages
   end subroutine

hsd_dump
~~~~~~~~

Write a tree to a file.

.. code-block:: fortran

   subroutine hsd_dump(root, filename, stat)
     type(hsd_table), intent(in) :: root
     character(len=*), intent(in) :: filename
     integer, intent(out), optional :: stat
   end subroutine

hsd_dump_to_string
~~~~~~~~~~~~~~~~~~

Serialize a tree to a string.

.. code-block:: fortran

   subroutine hsd_dump_to_string(root, output)
     type(hsd_table), intent(in) :: root
     character(len=:), allocatable, intent(out) :: output
   end subroutine

Accessor Procedures
-------------------

hsd_get
~~~~~~~

Get a value at the specified path. Supports multiple output types via generic interface.

.. code-block:: fortran

   ! Scalar types
   subroutine hsd_get(root, path, value, stat)
     type(hsd_table), intent(in) :: root
     character(len=*), intent(in) :: path
     <type>, intent(out) :: value  ! integer, real(dp), real(sp), logical, character
     integer, intent(out), optional :: stat
   end subroutine

   ! Array types
   subroutine hsd_get(root, path, array, stat)
     type(hsd_table), intent(in) :: root
     character(len=*), intent(in) :: path
     <type>, allocatable, intent(out) :: array(:)  ! integer, real(dp), logical, character
     integer, intent(out), optional :: stat
   end subroutine

**Examples:**

.. code-block:: fortran

   integer :: count
   real(dp) :: temperature
   character(len=:), allocatable :: name
   integer, allocatable :: values(:)

   call hsd_get(root, "Settings/Count", count, stat)
   call hsd_get(root, "Physics/Temperature", temperature, stat)
   call hsd_get(root, "Geometry/Name", name, stat)
   call hsd_get(root, "Data/Values", values, stat)

hsd_get_or
~~~~~~~~~~

Get a value with a default fallback if the path doesn't exist.

.. code-block:: fortran

   subroutine hsd_get_or(root, path, value, default, stat)
     type(hsd_table), intent(in) :: root
     character(len=*), intent(in) :: path
     <type>, intent(out) :: value
     <type>, intent(in) :: default
     integer, intent(out), optional :: stat  ! HSD_STAT_NOT_FOUND if default used
   end subroutine

**Example:**

.. code-block:: fortran

   call hsd_get_or(root, "Timeout", timeout, default=3600, stat=stat)

hsd_get_matrix
~~~~~~~~~~~~~~

Get a 2D array from multi-line data.

.. code-block:: fortran

   subroutine hsd_get_matrix(root, path, matrix, stat)
     type(hsd_table), intent(in) :: root
     character(len=*), intent(in) :: path
     real(dp), allocatable, intent(out) :: matrix(:,:)
     integer, intent(out), optional :: stat
   end subroutine

Query Procedures
----------------

hsd_has_child
~~~~~~~~~~~~~

Check if a path exists in the tree.

.. code-block:: fortran

   logical function hsd_has_child(root, path)
     type(hsd_table), intent(in) :: root
     character(len=*), intent(in) :: path
   end function

hsd_is_table
~~~~~~~~~~~~

Check if the node at path is a table (container).

.. code-block:: fortran

   logical function hsd_is_table(root, path)
     type(hsd_table), intent(in) :: root
     character(len=*), intent(in) :: path
   end function

hsd_is_value
~~~~~~~~~~~~

Check if the node at path is a value (leaf).

.. code-block:: fortran

   logical function hsd_is_value(root, path)
     type(hsd_table), intent(in) :: root
     character(len=*), intent(in) :: path
   end function

hsd_is_array
~~~~~~~~~~~~

Check if the value at path contains an array.

.. code-block:: fortran

   logical function hsd_is_array(root, path)
     type(hsd_table), intent(in) :: root
     character(len=*), intent(in) :: path
   end function

hsd_child_count
~~~~~~~~~~~~~~~

Get the number of children in a table.

.. code-block:: fortran

   integer function hsd_child_count(root, path)
     type(hsd_table), intent(in) :: root
     character(len=*), intent(in) :: path  ! Empty string for root
   end function

hsd_get_keys
~~~~~~~~~~~~

Get all child key names.

.. code-block:: fortran

   subroutine hsd_get_keys(root, path, keys)
     type(hsd_table), intent(in) :: root
     character(len=*), intent(in) :: path
     character(len=:), allocatable, intent(out) :: keys(:)
   end subroutine

hsd_get_type
~~~~~~~~~~~~

Get the value type at a path.

.. code-block:: fortran

   integer function hsd_get_type(root, path)
     type(hsd_table), intent(in) :: root
     character(len=*), intent(in) :: path
   end function
   ! Returns VALUE_TYPE_* constant

hsd_get_attrib
~~~~~~~~~~~~~~

Get the attribute (e.g., unit) at a path.

.. code-block:: fortran

   subroutine hsd_get_attrib(root, path, attrib, stat)
     type(hsd_table), intent(in) :: root
     character(len=*), intent(in) :: path
     character(len=:), allocatable, intent(out) :: attrib
     integer, intent(out), optional :: stat
   end subroutine

hsd_has_attrib
~~~~~~~~~~~~~~

Check if a node has an attribute.

.. code-block:: fortran

   logical function hsd_has_attrib(root, path)
     type(hsd_table), intent(in) :: root
     character(len=*), intent(in) :: path
   end function

hsd_get_child
~~~~~~~~~~~~~

Get a child node by name.

.. code-block:: fortran

   subroutine hsd_get_child(root, name, child, stat)
     type(hsd_table), intent(in) :: root
     character(len=*), intent(in) :: name
     class(hsd_node), pointer, intent(out) :: child
     integer, intent(out), optional :: stat
   end subroutine

hsd_get_table
~~~~~~~~~~~~~

Get a child table by path.

.. code-block:: fortran

   subroutine hsd_get_table(root, path, table, stat)
     type(hsd_table), intent(in) :: root
     character(len=*), intent(in) :: path
     type(hsd_table), pointer, intent(out) :: table
     integer, intent(out), optional :: stat
   end subroutine

Mutation Procedures
-------------------

hsd_set
~~~~~~~

Set a value at the specified path (creates intermediate tables as needed).

.. code-block:: fortran

   subroutine hsd_set(root, path, value)
     type(hsd_table), intent(inout) :: root
     character(len=*), intent(in) :: path
     <type>, intent(in) :: value  ! integer, real(dp), logical, character, arrays
   end subroutine

hsd_remove_child
~~~~~~~~~~~~~~~~

Remove a child node.

.. code-block:: fortran

   subroutine hsd_remove_child(root, path, stat)
     type(hsd_table), intent(inout) :: root
     character(len=*), intent(in) :: path
     integer, intent(out), optional :: stat
   end subroutine

Tree Operations
---------------

hsd_clone
~~~~~~~~~

Create a deep copy of a tree.

.. code-block:: fortran

   subroutine hsd_clone(source, dest)
     type(hsd_table), intent(in) :: source
     type(hsd_table), intent(out) :: dest
   end subroutine

hsd_merge
~~~~~~~~~

Merge source into target (source values override target on conflict).

.. code-block:: fortran

   subroutine hsd_merge(target, source)
     type(hsd_table), intent(inout) :: target
     type(hsd_table), intent(in) :: source
   end subroutine

Validation Procedures
---------------------

hsd_require
~~~~~~~~~~~

Get a required value (returns error if missing).

.. code-block:: fortran

   subroutine hsd_require(root, path, value, error)
     type(hsd_table), intent(in) :: root
     character(len=*), intent(in) :: path
     <type>, intent(out) :: value
     type(hsd_error_t), allocatable, intent(out) :: error
   end subroutine

hsd_validate_range
~~~~~~~~~~~~~~~~~~

Validate that a value is within a range.

.. code-block:: fortran

   subroutine hsd_validate_range(value, min_val, max_val, error)
     <numeric_type>, intent(in) :: value
     <numeric_type>, intent(in), optional :: min_val, max_val
     type(hsd_error_t), allocatable, intent(out) :: error
   end subroutine

hsd_validate_one_of
~~~~~~~~~~~~~~~~~~~

Validate that a value is in a set of allowed values.

.. code-block:: fortran

   subroutine hsd_validate_one_of(value, allowed, error)
     character(len=*), intent(in) :: value
     character(len=*), intent(in) :: allowed(:)
     type(hsd_error_t), allocatable, intent(out) :: error
   end subroutine

hsd_get_with_unit
~~~~~~~~~~~~~~~~~

Get a value and its unit.

.. code-block:: fortran

   subroutine hsd_get_with_unit(root, path, value, unit, stat)
     type(hsd_table), intent(in) :: root
     character(len=*), intent(in) :: path
     real(dp), intent(out) :: value
     character(len=*), intent(out) :: unit
     integer, intent(out), optional :: stat
   end subroutine

Schema Validation
-----------------

schema_init
~~~~~~~~~~~

Initialize a schema.

.. code-block:: fortran

   subroutine schema_init(schema)
     type(hsd_schema_t), intent(out) :: schema
   end subroutine

schema_destroy
~~~~~~~~~~~~~~

Clean up a schema.

.. code-block:: fortran

   subroutine schema_destroy(schema)
     type(hsd_schema_t), intent(inout) :: schema
   end subroutine

schema_add_field
~~~~~~~~~~~~~~~~

Add a field definition to the schema.

.. code-block:: fortran

   subroutine schema_add_field(schema, path, field_type, required)
     type(hsd_schema_t), intent(inout) :: schema
     character(len=*), intent(in) :: path
     integer, intent(in) :: field_type  ! FIELD_TYPE_* constant
     integer, intent(in) :: required    ! FIELD_REQUIRED or FIELD_OPTIONAL
   end subroutine

schema_add_field_enum
~~~~~~~~~~~~~~~~~~~~~

Add a field with enumerated allowed values.

.. code-block:: fortran

   subroutine schema_add_field_enum(schema, path, allowed_values, required)
     type(hsd_schema_t), intent(inout) :: schema
     character(len=*), intent(in) :: path
     character(len=*), intent(in) :: allowed_values(:)
     integer, intent(in) :: required
   end subroutine

schema_validate
~~~~~~~~~~~~~~~

Validate a tree against the schema.

.. code-block:: fortran

   subroutine schema_validate(schema, root, error)
     type(hsd_schema_t), intent(in) :: schema
     type(hsd_table), intent(in) :: root
     type(hsd_error_t), allocatable, intent(out) :: error
   end subroutine

schema_validate_strict
~~~~~~~~~~~~~~~~~~~~~~

Strict validation (fails on unknown fields).

.. code-block:: fortran

   subroutine schema_validate_strict(schema, root, error)
     type(hsd_schema_t), intent(in) :: schema
     type(hsd_table), intent(in) :: root
     type(hsd_error_t), allocatable, intent(out) :: error
   end subroutine

Visitor Pattern
---------------

hsd_accept
~~~~~~~~~~

Apply a visitor to the tree.

.. code-block:: fortran

   subroutine hsd_accept(root, visitor)
     type(hsd_table), intent(in) :: root
     class(hsd_visitor_t), intent(inout) :: visitor
   end subroutine

Constructor Functions
---------------------

new_table
~~~~~~~~~

Create a new table node.

.. code-block:: fortran

   function new_table(name) result(table)
     character(len=*), intent(in) :: name
     type(hsd_table) :: table
   end function

new_value
~~~~~~~~~

Create a new value node.

.. code-block:: fortran

   function new_value(name, val) result(value)
     character(len=*), intent(in) :: name
     <type>, intent(in) :: val
     type(hsd_value) :: value
   end function
