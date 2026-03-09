.. _tree_storage:

Internal HSD Tree Storage
=========================

This document explains how the HSD library represents parsed data in memory. Understanding
the tree structure is essential for contributors working on the library internals and helpful
for users who need to inspect or programmatically build HSD trees.

Overview
--------

HSD represents data as a **tree of nodes**. Every parsed input file becomes a tree with a
single root ``hsd_table`` and arbitrarily nested children. There are exactly two concrete
node types:

- **hsd_table** — a container (branch) node that holds an ordered list of child nodes
- **hsd_value** — a leaf node that holds a single typed datum (scalar, array, or matrix)

Both extend the abstract base type ``hsd_node``.

.. code-block:: text

   root (hsd_table)
   ├── Geometry (hsd_table)
   │   ├── TypeNames (hsd_value: string array ["Si", "O"])
   │   └── Periodic (hsd_value: logical .true.)
   ├── Hamiltonian (hsd_table)
   │   ├── SCC (hsd_value: logical .true.)
   │   └── MaxSccIterations (hsd_value: integer 100)
   └── Analysis (hsd_table)
       └── ProjectStates (hsd_table)
           └── ...


The Abstract Base: ``hsd_node``
-------------------------------

Every node in the tree carries four common fields defined on ``hsd_node``:

.. list-table::
   :header-rows: 1
   :widths: 20 20 60

   * - Field
     - Type
     - Description
   * - ``name``
     - ``character(:), allocatable``
     - The tag name (e.g. ``"Geometry"``, ``"SCC"``). Always stored.
   * - ``attrib``
     - ``character(:), allocatable``
     - Optional attribute string (e.g. a unit like ``"angstrom"``). Not all nodes have one.
   * - ``line``
     - ``integer``
     - Source line number where this node was defined. Used for error messages. Defaults to 0 for programmatically created nodes.
   * - ``processed``
     - ``logical``
     - Whether the host application has accessed this node. See :ref:`processed_flag` below.

``hsd_node`` is abstract and declares a single deferred method ``destroy()``.


Table Nodes: ``hsd_table``
--------------------------

``hsd_table`` extends ``hsd_node`` and acts as a **container** for child nodes.

Internal storage
~~~~~~~~~~~~~~~~

.. list-table::
   :header-rows: 1
   :widths: 25 20 55

   * - Field
     - Type
     - Description
   * - ``children(:)``
     - ``hsd_node_ptr, allocatable``
     - Array of pointers to child nodes (polymorphic: each can be ``hsd_table`` or ``hsd_value``).
   * - ``num_children``
     - ``integer``
     - Number of children currently stored (≤ ``capacity``).
   * - ``capacity``
     - ``integer``
     - Allocated length of the ``children`` array. Grows by doubling when full.
   * - ``name_index``
     - ``hsd_name_index_t``
     - Hash table mapping lowercase child names → array indices. Enables O(1) lookup by name.
   * - ``index_active``
     - ``logical``
     - Whether the hash index is currently valid. Invalidated by add/remove operations.

The ``children`` array stores ``hsd_node_ptr`` wrappers (a type containing a single
``class(hsd_node), pointer`` component). Pointers are used rather than allocatables so that
when the children array is reallocated during growth, existing pointers obtained via
``get_child`` / ``get_child_by_name`` remain valid.

Growth strategy
~~~~~~~~~~~~~~~

When a child is added and ``num_children == capacity``:

1. A new array with doubled capacity is allocated.
2. Existing pointer entries are copied (pointer assignment, not deep copy).
3. The old array is deallocated.

Initial capacity starts at 4 and doubles: 4 → 8 → 16 → 32 → ...

Hash index
~~~~~~~~~~

For tables with more than a handful of children, lookup by name uses a hash table
(``hsd_name_index_t``) that maps **lowercase** node names to their index in the
``children`` array. The index is built lazily on the first name-based lookup and
invalidated when children are added or removed.


Value Nodes: ``hsd_value``
--------------------------

``hsd_value`` extends ``hsd_node`` and stores a single datum. It supports multiple
types via a tagged-union pattern: the ``value_type`` field indicates which storage
field holds the active data.

Type enumeration
~~~~~~~~~~~~~~~~

.. list-table::
   :header-rows: 1
   :widths: 30 10 60

   * - Constant
     - Value
     - Storage field(s) used
   * - ``VALUE_TYPE_NONE``
     - 0
     - No data stored (empty/uninitialized value node).
   * - ``VALUE_TYPE_STRING``
     - 1
     - ``string_value`` (``character(:), allocatable``)
   * - ``VALUE_TYPE_INTEGER``
     - 2
     - ``int_value`` (``integer``) for scalars; ``int_array(:)`` and ``int_matrix(:,:)`` for arrays/matrices.
   * - ``VALUE_TYPE_REAL``
     - 3
     - ``real_value`` (``real(dp)``) for scalars; ``real_array(:)`` and ``real_matrix(:,:)`` for arrays/matrices.
   * - ``VALUE_TYPE_LOGICAL``
     - 4
     - ``logical_value`` (``logical``) for scalars; ``logical_array(:)`` for arrays.
   * - ``VALUE_TYPE_ARRAY``
     - 5
     - ``raw_text`` (``character(:), allocatable``) — unparsed text; typed arrays populated on first access.
   * - ``VALUE_TYPE_COMPLEX``
     - 6
     - ``complex_value`` (``complex(dp)``) for scalars; ``complex_array(:)`` and ``complex_matrix(:,:)`` for arrays/matrices.

All storage fields coexist on the type but only the ones corresponding to
``value_type`` are meaningful. After parsing, a value node with ``value_type = VALUE_TYPE_ARRAY``
initially stores only ``raw_text``. The typed array fields are populated lazily on first
accessor call (see :ref:`cache_on_read` below).

Scalar vs array storage
~~~~~~~~~~~~~~~~~~~~~~~

- **Scalars**: Stored directly (``int_value``, ``real_value``, ``logical_value``, ``complex_value``, ``string_value``).
- **Rank-1 arrays**: ``int_array(:)``, ``real_array(:)``, ``logical_array(:)``, ``string_array(:)``, ``complex_array(:)``.
- **Rank-2 matrices**: ``int_matrix(:,:)``, ``real_matrix(:,:)``, ``complex_matrix(:,:)`` with ``nrows`` and ``ncols`` tracking dimensions.

All array/matrix fields are ``allocatable``.


.. _cache_on_read:

Cache-on-Read Behavior
~~~~~~~~~~~~~~~~~~~~~~

Array and matrix accessor methods (e.g. ``get_int_array``, ``get_real_matrix``) follow a
**cache-on-read** pattern:

1. On the **first call**, the raw text (``raw_text``) is parsed into the appropriate
   typed array (e.g. ``int_array``) and the result is stored.
2. On **subsequent calls**, the cached array is returned directly without reparsing.

This means these accessor methods require ``intent(inout)`` even though they are
logically read-only — they mutate internal cache state.

**Thread safety implication**: concurrent first-access to the same ``hsd_value``
from multiple threads is a data race. Once all caches are populated (by a single-threaded
warmup pass), concurrent reads are safe. See :doc:`thread_safety` for details.


.. _processed_flag:

The ``processed`` Flag
----------------------

Every ``hsd_node`` carries a ``processed`` logical flag (default: ``.false.``).
This mechanism allows the host application (e.g. DFTB+) to detect **misspelled or
unrecognized keywords** in the input:

1. During parsing, all nodes start with ``processed = .false.``.
2. When the application reads a value via ``hsd_get``, ``hsd_get_or_set``, or
   ``hsd_get_child``, the accessed node is marked ``processed = .true.``.
3. After the application has finished processing input, it calls
   ``hsd_warn_unprocessed(root)`` to emit warnings for any children that were
   never accessed — these are likely typos or unsupported keywords.

This pattern replaces the need for a strict schema: the application defines what it
accepts by what it reads.


Memory Ownership
-----------------

The HSD tree uses a **copy-on-add** ownership model:

- **``add_child(child)``**: Performs ``allocate(source=child)`` to deep-copy the
  node. The caller retains ownership of the original and can safely destroy or reuse it.

- **``get_child(index, ptr)``** / **``get_child_by_name(name, ptr)``**: Returns a
  **pointer** to a node owned by the table. The caller must NOT deallocate it.

- **``remove_child(index)``** / **``remove_child_by_name(name)``**: Deallocates the
  removed node. Any previously obtained pointers to that node become invalid.

- **``destroy()``**: Recursively deallocates all children and their subtrees. Must be
  called explicitly on the root when the tree is no longer needed.

.. code-block:: fortran

   type(hsd_table) :: root
   type(hsd_error_t), allocatable :: err

   ! Parse creates a tree — caller owns root
   call hsd_parse("input.hsd", root, err)

   ! ... use the tree ...

   ! Caller must destroy when done
   call root%destroy()


.. _node_ptr_wrapper:

The ``hsd_node_ptr`` Wrapper
-----------------------------

Children are stored in an array of ``hsd_node_ptr`` — a simple wrapper type:

.. code-block:: fortran

   type :: hsd_node_ptr
     class(hsd_node), pointer :: node => null()
   end type

This indirection is necessary because Fortran does not support arrays of polymorphic
allocatables. Using pointers (rather than allocatables) ensures that when the ``children``
array is reallocated during table growth, existing pointers obtained via ``get_child``
remain valid — only the array of ``hsd_node_ptr`` wrappers is moved, not the nodes
themselves.
