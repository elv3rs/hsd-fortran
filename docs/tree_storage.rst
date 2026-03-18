.. _tree_storage:

Internal HSD Tree Storage
=========================

This document explains how the HSD library represents parsed data in memory. Understanding
the tree structure is essential for contributors working on the library internals and helpful
for users who need to inspect or programmatically build HSD trees.

Overview
--------

HSD represents data as a **tree of nodes**. Every parsed input file becomes a tree with a
single root ``hsd_node`` (with ``node_type = NODE_TYPE_TABLE``) and arbitrarily nested
children. There is a single unified node type ``hsd_node`` with a ``node_type`` discriminator:

- **NODE_TYPE_TABLE** — a container (branch) node that holds an ordered list of child nodes
- **NODE_TYPE_VALUE** — a leaf node that holds a single typed datum (scalar, array, or matrix)

.. code-block:: text

   root (hsd_node, NODE_TYPE_TABLE)
   ├── Geometry (hsd_node, NODE_TYPE_TABLE)
   │   ├── TypeNames (hsd_node, NODE_TYPE_VALUE: string array ["Si", "O"])
   │   └── Periodic (hsd_node, NODE_TYPE_VALUE: logical .true.)
   ├── Hamiltonian (hsd_node, NODE_TYPE_TABLE)
   │   ├── SCC (hsd_node, NODE_TYPE_VALUE: logical .true.)
   │   └── MaxSccIterations (hsd_node, NODE_TYPE_VALUE: integer 100)
   └── Analysis (hsd_node, NODE_TYPE_TABLE)
       └── ProjectStates (hsd_node, NODE_TYPE_TABLE)
           └── ...


The Unified Type: ``hsd_node``
-------------------------------

Every node in the tree is a ``type(hsd_node)``. The ``node_type`` field discriminates
between table and value nodes. All nodes carry these common fields:

.. list-table::
   :header-rows: 1
   :widths: 20 20 60

   * - Field
     - Type
     - Description
   * - ``node_type``
     - ``integer``
     - Discriminator: ``NODE_TYPE_TABLE`` (1) or ``NODE_TYPE_VALUE`` (2).
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

``hsd_node`` is a concrete type with a ``destroy()`` method that recursively deallocates
children for table nodes.


Table Nodes (``node_type = NODE_TYPE_TABLE``)
----------------------------------------------

When ``node_type = NODE_TYPE_TABLE``, the node acts as a **container** for child nodes.

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
     - Array of pointers to child nodes (each wrapped ``type(hsd_node), pointer`` can be a table or value node).
   * - ``num_children``
     - ``integer``
     - Number of children currently stored (≤ ``size(children)``).

The ``children`` array stores ``hsd_node_ptr`` wrappers (a type containing a single
``type(hsd_node), pointer`` component). Pointers are used rather than allocatables so that
when the children array is reallocated during growth, existing pointers obtained via
``get_child`` / ``get_child_by_name`` remain valid.

Growth strategy
~~~~~~~~~~~~~~~

When a child is added and ``num_children == size(children)``:

1. A new array with doubled size is allocated.
2. Existing pointer entries are copied (pointer assignment, not deep copy).
3. The old array is deallocated.

Initial array size starts at 4 and doubles: 4 → 8 → 16 → 32 → ...

Child lookup
~~~~~~~~~~~~

Lookup by name uses a simple **linear search** over the children array, comparing
node names directly. This is appropriate for configuration file parsing where tables
typically have a small number of children.


Value Nodes (``node_type = NODE_TYPE_VALUE``)
----------------------------------------------

When ``node_type = NODE_TYPE_VALUE``, the node stores a single datum. It supports multiple
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
     - ``string_value`` (stores formatted integer) for scalars and arrays/matrices.
   * - ``VALUE_TYPE_REAL``
     - 3
     - ``string_value`` (stores formatted real) for scalars and arrays/matrices.
   * - ``VALUE_TYPE_LOGICAL``
     - 4
     - ``string_value`` (stores "Yes"/"No") for scalars and arrays.
   * - ``VALUE_TYPE_ARRAY``
     - 5
     - ``string_value`` (``character(:), allocatable``) — unparsed text; arrays are parsed on demand.
   * - ``VALUE_TYPE_COMPLEX``
     - 6
     - ``string_value`` (stores formatted complex) for scalars and arrays/matrices.

All storage fields coexist on the type but only the ones corresponding to
``value_type`` are meaningful. After parsing, a value node with ``value_type = VALUE_TYPE_ARRAY``
stores only ``string_value``.

Scalar vs array storage
~~~~~~~~~~~~~~~~~~~~~~~

- **Scalars**: Stored as strings in ``string_value``.
- **Arrays & Matrices**: Stored as strings in ``string_value`` and parsed on demand.

.. _parse_on_demand:

Parse-on-Demand Behavior
~~~~~~~~~~~~~~~~~~~~~~~~

Array and matrix accessor methods (e.g. ``get_int_array``, ``get_real_matrix``) follow a
**parse-on-demand** pattern:

1. Every call parses the ``string_value`` into a temporary typed array.
2. The parsed array is returned to the caller.
3. No cached version is stored in the node.

This means these accessor methods are truly read-only (``intent(in)``) and thread-safe
for concurrent access, at the cost of reparsing overhead on repeated access.


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

   type(hsd_node) :: root
   type(hsd_error_t), allocatable :: err

   ! Parse creates a tree — caller owns root
   call hsd_load_file("input.hsd", root, err)

   ! ... use the tree ...

   ! Caller must destroy when done
   call root%destroy()


.. _node_ptr_wrapper:

The ``hsd_node_ptr`` Wrapper
-----------------------------

Children are stored in an array of ``hsd_node_ptr`` — a simple wrapper type:

.. code-block:: fortran

   type :: hsd_node_ptr
     type(hsd_node), pointer :: node => null()
   end type

This indirection is necessary because Fortran does not support arrays of pointer
types directly. Using pointers (rather than allocatables) ensures that when the ``children``
array is reallocated during table growth, existing pointers obtained via ``get_child``
remain valid — only the array of ``hsd_node_ptr`` wrappers is moved, not the nodes
themselves.
