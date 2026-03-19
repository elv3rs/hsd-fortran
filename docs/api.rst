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

``hsd_node_t``
^^^^^^^^^^^^

The unified node type used for all nodes in the HSD tree. The ``node_type`` field
discriminates between table (container) nodes and value (leaf) nodes:

- ``NODE_TYPE_TABLE`` — a container node that holds child nodes
- ``NODE_TYPE_VALUE`` — a leaf node containing a scalar value or array

.. code-block:: fortran

   type(hsd_node_t) :: root    ! a table node (after hsd_load)
   type(hsd_node_t) :: val     ! a value node (after new_value)

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

``hsd_iterator_t``
^^^^^^^^^^^^^^^^

Iterator for traversing table children.

.. code-block:: fortran

   type(hsd_iterator_t) :: iter

``hsd_access_t``
^^^^^^^^^^^^^^^^

High-level access object for reading and writing values in the tree.
Accumulates errors internally so they can be checked after a batch of operations.

.. code-block:: fortran

   type(hsd_access_t) :: access




Precision Constants
~~~~~~~~~~~~~~~~~~~

.. code-block:: fortran

   integer, parameter :: dp = kind(1.0d0)  ! Double precision


Node Type Constants
~~~~~~~~~~~~~~~~~~~

.. code-block:: fortran

   integer, parameter :: NODE_TYPE_TABLE = 1   ! Container node
   integer, parameter :: NODE_TYPE_VALUE = 2   ! Leaf node

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

I/O Procedures
--------------

hsd_load
~~~~~~~~

Load and parse an HSD file.

.. code-block:: fortran

   subroutine hsd_load_file(filename, root, error)
     character(len=*), intent(in) :: filename
     type(hsd_node_t), intent(out) :: root
     type(hsd_error_t), allocatable, intent(out), optional :: error
   end subroutine

hsd_load_string
~~~~~~~~~~~~~~~

Parse HSD from a string.

.. code-block:: fortran

   subroutine hsd_load_string(source, root, error, filename)
     character(len=*), intent(in) :: source
     type(hsd_node_t), intent(out) :: root
     type(hsd_error_t), allocatable, intent(out), optional :: error
     character(len=*), intent(in), optional :: filename  ! For error messages
   end subroutine

hsd_dump
~~~~~~~~

Write a tree to a file.

.. code-block:: fortran

   subroutine hsd_dump(root, filename, error)
     type(hsd_node_t), intent(in) :: root
     character(len=*), intent(in) :: filename
     type(hsd_error_t), allocatable, intent(out), optional :: error
   end subroutine

hsd_dump_to_string
~~~~~~~~~~~~~~~~~~

Serialize a tree to a string.

.. code-block:: fortran

   subroutine hsd_dump_to_string(root, output)
     type(hsd_node_t), intent(in) :: root
     character(len=:), allocatable, intent(out) :: output
   end subroutine

Accessor Procedures
-------------------

hsd_access_t (Access Object)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The primary API for reading and writing values is the ``hsd_access_t`` object.
It wraps a root node and accumulates errors internally.

.. code-block:: fortran

   type :: hsd_access_t
   contains
     procedure :: init          ! Initialize with a root node
     procedure :: get           ! Get a value at a path (generic)
     procedure :: get_matrix    ! Get a 2D array from multi-line data
     procedure :: set           ! Set a value at a path (generic)
     procedure :: has_errors    ! Check if any errors accumulated
     procedure :: print_errors  ! Print all accumulated errors
   end type

**Initialization:**

.. code-block:: fortran

   subroutine init(self, root, on_missing, mark_processed)
     type(hsd_node_t), intent(in), target :: root
     integer, intent(in), optional :: on_missing       ! HSD_ON_MISSING_SET (default) or HSD_ON_MISSING_RETURN
     logical, intent(in), optional :: mark_processed   ! default .true.
   end subroutine

- ``on_missing=HSD_ON_MISSING_SET`` (default): when a key is missing and a default is provided,
  the default value is written back into the tree.
- ``on_missing=HSD_ON_MISSING_RETURN``: the default is returned to the caller without
  modifying the tree.
- ``mark_processed``: when ``.true.`` (default), accessed nodes are marked as processed.

**access%get:**

Get a value at the specified path. Supports multiple output types via generic interface.

.. code-block:: fortran

   ! Scalar types
   call access%get(path, value)
   call access%get(path, value, default=fallback)

   ! Array types
   call access%get(path, array)
   call access%get(path, array, default=fallback_array)

Supported types: ``integer``, ``real(dp)``, ``logical``, ``character``, and allocatable arrays thereof.

**Examples:**

.. code-block:: fortran

   integer :: count
   real(dp) :: temperature
   character(len=:), allocatable :: name
   integer, allocatable :: values(:)

   call access%get("Settings/Count", count)
   call access%get("Physics/Temperature", temperature, default=300.0_dp)
   call access%get("Geometry/Name", name)
   call access%get("Data/Values", values)

**access%get_matrix:**

Get a 2D array from multi-line data.

.. code-block:: fortran

   call access%get_matrix(path, matrix, nrows, ncols)

**access%set:**

Set a value at the specified path (creates intermediate tables as needed).

.. code-block:: fortran

   call access%set(path, value)

Supported types: ``integer``, ``real(dp)``, ``logical``, ``character``, and arrays thereof.

**Error handling:**

.. code-block:: fortran

   if (access%has_errors()) then
     call access%print_errors()
   end if

Legacy Free Functions (hsd_api)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The free-function accessors ``hsd_get``, ``hsd_set``, ``hsd_get_or_set``, and
``hsd_get_matrix`` are still available via ``use hsd_api`` but are no longer
exported from ``use hsd``. New code should use ``hsd_access_t`` instead.

hsd_get_inline_text
~~~~~~~~~~~~~~~~~~~

Get concatenated text content of all unnamed value children. Iterates
children of the table, collecting text from unnamed or ``#text``
value nodes (``node_type == NODE_TYPE_VALUE``). Multiple values are separated by spaces.

How ``#text`` is created:

When a table block contains inline text (not a named ``Key = Value`` child),
the parser stores that inline text as an internal value child named
``#text``. For example, ``Tag { 42 }`` creates a ``Tag`` table with one
``#text`` child containing ``42``; ``Tag { 42; Sub = x }`` creates both the
``#text`` child (``42``) and the named ``Sub`` child.

.. code-block:: fortran

   subroutine hsd_get_inline_text(table, text, stat)
     type(hsd_node_t), intent(in), target :: table
     character(len=:), allocatable, intent(out) :: text
     integer, intent(out), optional :: stat
   end subroutine

**Example:**

.. code-block:: fortran

   character(len=:), allocatable :: text
   integer :: stat

   call hsd_get_inline_text(root, text, stat)
   if (stat == HSD_STAT_OK) print *, "Inline text: ", text

Query Procedures
----------------

hsd_has_child
~~~~~~~~~~~~~

Check if a path exists in the tree.

.. code-block:: fortran

   logical function hsd_has_child(root, path)
     type(hsd_node_t), intent(in) :: root
     character(len=*), intent(in) :: path
   end function

hsd_is_table
~~~~~~~~~~~~

Check if the node at path is a table (container).

.. code-block:: fortran

   logical function hsd_is_table(root, path)
     type(hsd_node_t), intent(in) :: root
     character(len=*), intent(in) :: path
   end function

hsd_is_value
~~~~~~~~~~~~

Check if the node at path is a value (leaf).

.. code-block:: fortran

   logical function hsd_is_value(root, path)
     type(hsd_node_t), intent(in) :: root
     character(len=*), intent(in) :: path
   end function

hsd_is_array
~~~~~~~~~~~~

Check if the value at path contains an array.

.. code-block:: fortran

   logical function hsd_is_array(root, path)
     type(hsd_node_t), intent(in) :: root
     character(len=*), intent(in) :: path
   end function

hsd_child_count
~~~~~~~~~~~~~~~

Get the number of children in a table.

.. code-block:: fortran

   integer function hsd_child_count(root, path)
     type(hsd_node_t), intent(in) :: root
     character(len=*), intent(in) :: path  ! Empty string for root
   end function

hsd_get_keys
~~~~~~~~~~~~

Get all child key names.

.. code-block:: fortran

   subroutine hsd_get_keys(root, path, keys)
     type(hsd_node_t), intent(in) :: root
     character(len=*), intent(in) :: path
     character(len=:), allocatable, intent(out) :: keys(:)
   end subroutine

hsd_get_type
~~~~~~~~~~~~

Get the value type at a path.

.. code-block:: fortran

   integer function hsd_get_type(root, path)
     type(hsd_node_t), intent(in) :: root
     character(len=*), intent(in) :: path
   end function
   ! Returns VALUE_TYPE_* constant

hsd_get_attrib
~~~~~~~~~~~~~~

Get the attribute (e.g., unit) at a path.

.. code-block:: fortran

   subroutine hsd_get_attrib(root, path, attrib, stat)
     type(hsd_node_t), intent(in) :: root
     character(len=*), intent(in) :: path
     character(len=:), allocatable, intent(out) :: attrib
     integer, intent(out), optional :: stat
   end subroutine

hsd_has_attrib
~~~~~~~~~~~~~~

Check if a node has an attribute.

.. code-block:: fortran

   logical function hsd_has_attrib(root, path)
     type(hsd_node_t), intent(in) :: root
     character(len=*), intent(in) :: path
   end function

hsd_get_child
~~~~~~~~~~~~~

Get a child node by name.

.. code-block:: fortran

   subroutine hsd_get_child(root, name, child, stat)
     type(hsd_node_t), intent(in) :: root
     character(len=*), intent(in) :: name
     type(hsd_node_t), pointer, intent(out) :: child
     integer, intent(out), optional :: stat
   end subroutine

hsd_get_table
~~~~~~~~~~~~~

Get a child table by path.

.. code-block:: fortran

   subroutine hsd_get_table(root, path, table, stat)
     type(hsd_node_t), intent(in) :: root
     character(len=*), intent(in) :: path
     type(hsd_node_t), pointer, intent(out) :: table
     integer, intent(out), optional :: stat
   end subroutine

hsd_get_children
~~~~~~~~~~~~~~~~

Collect all children matching a (possibly path-qualified) name.

.. code-block:: fortran

   subroutine hsd_get_children(root, path, children, stat)
     type(hsd_node_t), intent(in), target :: root
     character(len=*), intent(in) :: path
     type(hsd_node_ptr_t), allocatable, intent(out) :: children(:)
     integer, intent(out), optional :: stat
   end subroutine

hsd_get_child_tables
~~~~~~~~~~~~~~~~~~~~

Collect all matching table children. Both ``hsd_get_children`` and
``hsd_get_child_tables`` use ``hsd_node_ptr_t`` with a ``%node`` pointer component.

.. code-block:: fortran

   subroutine hsd_get_child_tables(root, path, children, stat)
     type(hsd_node_t), intent(in), target :: root
     character(len=*), intent(in) :: path
     type(hsd_node_ptr_t), allocatable, intent(out) :: children(:)
     integer, intent(out), optional :: stat
   end subroutine

hsd_has_value_children
~~~~~~~~~~~~~~~~~~~~~~

Check whether a table has any value children (inline data).

.. code-block:: fortran

   logical function hsd_has_value_children(table)
     type(hsd_node_t), intent(in), target :: table
   end function

**Example:**

.. code-block:: fortran

   if (hsd_has_value_children(root)) then
     print *, "Table contains inline values"
   end if

hsd_get_name
~~~~~~~~~~~~

Get the lowercased name of a node. If the node's name is unset or blank,
returns the ``default`` string (which itself defaults to ``""`` if not
provided).

.. code-block:: fortran

   subroutine hsd_get_name(node, name, default)
     type(hsd_node_t), intent(in) :: node
     character(len=:), allocatable, intent(out) :: name
     character(len=*), intent(in), optional :: default
   end subroutine

**Example:**

.. code-block:: fortran

   character(len=:), allocatable :: name

   call hsd_get_name(node, name)
   call hsd_get_name(node, name, default="#text")

Mutation Procedures
-------------------

Values are typically modified via ``access%set`` (see `hsd_access_t`_ above).
The following tree-level mutation procedures remain public:

hsd_remove_child
~~~~~~~~~~~~~~~~

Remove a child node.

.. code-block:: fortran

   subroutine hsd_remove_child(root, path, stat)
     type(hsd_node_t), intent(inout) :: root
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
     type(hsd_node_t), intent(in) :: source
     type(hsd_node_t), intent(out) :: dest
   end subroutine

hsd_merge
~~~~~~~~~

Merge source into target (source values override target on conflict).

.. code-block:: fortran

   subroutine hsd_merge(target, source)
     type(hsd_node_t), intent(inout) :: target
     type(hsd_node_t), intent(in) :: source
   end subroutine

Validation Procedures
---------------------

hsd_require
~~~~~~~~~~~

Check that a required field exists (optionally check its type).

.. code-block:: fortran

   subroutine hsd_require(table, path, error, expected_type, context)
     type(hsd_node_t), intent(in), target :: table
     character(len=*), intent(in) :: path
     type(hsd_error_t), allocatable, intent(out) :: error
     integer, intent(in), optional :: expected_type  ! FIELD_TYPE_* constant
     character(len=*), intent(in), optional :: context
   end subroutine

hsd_validate_range
~~~~~~~~~~~~~~~~~~

Validate that a numeric value at a path is within a range.

.. code-block:: fortran

   subroutine hsd_validate_range(table, path, min_val, max_val, error, context)
     type(hsd_node_t), intent(in), target :: table
     character(len=*), intent(in) :: path
     real(dp), intent(in) :: min_val, max_val
     type(hsd_error_t), allocatable, intent(out) :: error
     character(len=*), intent(in), optional :: context
   end subroutine

hsd_validate_one_of
~~~~~~~~~~~~~~~~~~~

Validate that a string value at a path is one of the allowed choices.

.. code-block:: fortran

   subroutine hsd_validate_one_of(table, path, choices, error, context)
     type(hsd_node_t), intent(in), target :: table
     character(len=*), intent(in) :: path
     character(len=*), intent(in) :: choices(:)
     type(hsd_error_t), allocatable, intent(out) :: error
     character(len=*), intent(in), optional :: context
   end subroutine

hsd_get_with_unit
~~~~~~~~~~~~~~~~~

Get a value with unit conversion via a user-supplied converter function.

.. code-block:: fortran

   subroutine hsd_get_with_unit(table, path, val, target_unit, converter, stat)
     type(hsd_node_t), intent(in), target :: table
     character(len=*), intent(in) :: path
     real(dp), intent(out) :: val
     character(len=*), intent(in) :: target_unit
     interface
       pure function converter(value, from_unit, to_unit) result(converted)
         import :: dp
         real(dp), intent(in) :: value
         character(len=*), intent(in) :: from_unit, to_unit
         real(dp) :: converted
       end function
     end interface
     integer, intent(out), optional :: stat
   end subroutine





Constructor Functions
---------------------

new_table
~~~~~~~~~

Create a new table node.

.. code-block:: fortran

   function new_table(name) result(table)
     character(len=*), intent(in) :: name
     type(hsd_node_t) :: table
   end function

new_value
~~~~~~~~~

Create a new value node.

.. code-block:: fortran

   function new_value(name, val) result(value)
     character(len=*), intent(in) :: name
     <type>, intent(in) :: val
     type(hsd_node_t) :: value
   end function
