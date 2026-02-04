API Reference
=============

The complete API documentation is generated from source code using FORD (FORtran Documenter).

**View the full API documentation:** `FORD API Documentation <../ford/index.html>`_

Key Modules
-----------

The library is organized into several modules:

Main Module
~~~~~~~~~~~

**hsd**
   The main entry point that re-exports all public types and procedures.
   This is the only module you need to import in most cases.

   .. code-block:: fortran

      use hsd

High-Level API (src/api/)
~~~~~~~~~~~~~~~~~~~~~~~~~~

**hsd_accessors**
   Type-safe value retrieval with path navigation.
   
   - ``hsd_get()`` - Retrieve values with type checking
   - ``hsd_get_or()`` - Retrieve values with defaults
   - ``hsd_get_matrix()`` - Retrieve 2D arrays

**hsd_mutators**
   Value modification operations.
   
   - ``hsd_set()`` - Set values in the tree

**hsd_query**
   Tree introspection and manipulation.
   
   - ``hsd_has_child()`` - Check for child existence
   - ``hsd_is_table()`` / ``hsd_is_value()`` - Type checking
   - ``hsd_merge()`` - Merge trees
   - ``hsd_clone()`` - Deep copy nodes

**hsd_validation**
   Value validation helpers.
   
   - ``hsd_require()`` - Enforce value presence
   - ``hsd_validate_range()`` - Check numeric ranges

**hsd_schema**
   Declarative schema-based validation.
   
   - ``hsd_schema_t`` - Schema definition type
   - ``hsd_validate()`` - Validate tree against schema

**hsd_visitor**
   Visitor pattern for tree traversal.
   
   - ``hsd_visitor_t`` - Abstract base for visitors
   - ``hsd_accept()`` - Apply visitor to tree

I/O Layer (src/io/)
~~~~~~~~~~~~~~~~~~~

**hsd_lexer**
   Tokenizes HSD source text.

**hsd_parser**
   Builds tree from tokens, handles includes.
   
   - ``hsd_load()`` - Parse file into tree
   - ``hsd_parse()`` - Parse string into tree

**hsd_formatter**
   Serializes tree back to HSD format.
   
   - ``hsd_dump()`` - Write tree to file
   - ``hsd_format()`` - Convert tree to string

Core Infrastructure (src/core/)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

**hsd_constants**
   Precision constants (``dp``, ``sp``).

**hsd_error**
   Error types and status codes.
   
   - ``hsd_error_t`` - Detailed error information
   - ``HSD_STAT_*`` - Error code constants

**hsd_hash_table**
   O(1) child lookup for tables.

Data Structures (hsd_types)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

**Types:**

- ``hsd_node`` - Abstract base for all nodes
- ``hsd_table`` - Container node with children
- ``hsd_value`` - Leaf node with scalar/array data
- ``hsd_iterator`` - Iterator for traversing children

Quick Reference
---------------

Common Operations
~~~~~~~~~~~~~~~~~

.. code-block:: fortran

   use hsd
   implicit none
   
   type(hsd_table) :: root
   type(hsd_error_t), allocatable :: error
   integer :: value, stat
   
   ! Load a file
   call hsd_load("input.hsd", root, error)
   if (allocated(error)) then
     call error%print()
     stop 1
   end if
   
   ! Get a value
   call hsd_get(root, "section/key", value, stat)
   
   ! Get with default
   call hsd_get_or(root, "section/optional", value, default=42, stat=stat)
   
   ! Set a value
   call hsd_set(root, "section/key", 123)
   
   ! Check existence
   if (hsd_has_child(root, "section")) then
     ! ...
   end if
   
   ! Save to file
   call hsd_dump(root, "output.hsd")

Error Handling
~~~~~~~~~~~~~~

Most operations provide optional ``stat`` parameters for error handling:

.. code-block:: fortran

   integer :: stat
   
   call hsd_get(root, "path", value, stat)
   if (stat /= HSD_STAT_OK) then
     print *, "Error:", stat
   end if

Error codes are defined in ``hsd_error`` module:

- ``HSD_STAT_OK`` (0) - Success
- ``HSD_STAT_NOT_FOUND`` (11) - Key not found
- ``HSD_STAT_TYPE_ERROR`` (10) - Type mismatch
- ``HSD_STAT_SYNTAX_ERROR`` (1) - Parse error
- See FORD documentation for complete list

For More Details
----------------

See the `complete FORD API documentation <../ford/index.html>`_ for:

- Detailed procedure signatures
- Type definitions
- Source code
- Module dependency graphs
- Full documentation comments
