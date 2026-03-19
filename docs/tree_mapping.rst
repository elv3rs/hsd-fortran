.. _tree_mapping:

From HSD Text to Tree: A Visual Guide
======================================

This document walks through a concrete HSD configuration and shows exactly how the
parser transforms it into the internal tree of ``hsd_node_t`` nodes. It bridges
the :doc:`hsd_format` (syntax) and :doc:`tree_storage` (data structures) documentation.

.. contents:: On this page
   :local:
   :depth: 2


Core Concepts at a Glance
--------------------------

Before diving in, here are the two building blocks of every HSD tree:

**Table node** (``NODE_TYPE_TABLE``)
  A container (branch) that holds an ordered list of child nodes. Every ``{ }``
  block in the HSD text becomes a table node. The invisible root of every parsed
  file is also a table.

**Value node** (``NODE_TYPE_VALUE``)
  A leaf that stores a single datum as a string (parsed into the requested type on
  access). Scalars, arrays, and multi-line matrix data are all value nodes.

Both node kinds share the same Fortran type (``hsd_node_t``) and are
distinguished by the ``node_type`` field. See :ref:`tree_storage` for the
full field reference.


A Sample Configuration
----------------------

Consider this HSD input file:

.. code-block:: text

   Driver = ConjugateGradient {
     MaxSteps = 200
     MaxForce [eV/Angstrom] = 0.001
   }

   Hamiltonian = DFTB {
     SCC = Yes
     Filling = Fermi {
       Temperature [Kelvin] = 300.0
     }
     KPointsAndWeights {
       4 0 0
       0 4 0
       0 0 4
     }
   }

   Analysis {
     PrintForces = Yes
   }


The Resulting Tree
------------------

After ``hsd_load_file`` (or ``hsd_load_string``), the parser produces this tree:

.. code-block:: text

   root (TABLE, name="")
   │
   ├── driver (TABLE, name="driver")
   │   └── conjugategradient (TABLE, name="conjugategradient")
   │       ├── maxsteps (VALUE, name="maxsteps", string_value="200")
   │       └── maxforce (VALUE, name="maxforce", attrib="eV/Angstrom",
   │                     string_value="0.001")
   │
   ├── hamiltonian (TABLE, name="hamiltonian")
   │   └── dftb (TABLE, name="dftb")
   │       ├── scc (VALUE, name="scc", string_value="Yes")
   │       ├── filling (TABLE, name="filling")
   │       │   └── fermi (TABLE, name="fermi")
   │       │       └── temperature (VALUE, name="temperature",
   │       │                        attrib="Kelvin", string_value="300.0")
   │       └── kpointsandweights (TABLE, name="kpointsandweights")
   │           └── (VALUE, name="#text",
   │                string_value="4 0 0\n0 4 0\n0 0 4")
   │
   └── analysis (TABLE, name="analysis")
       └── printforces (VALUE, name="printforces", string_value="Yes")

Key observations:

1. **All tag names are lowercased.** ``Driver`` → ``"driver"``, ``SCC`` → ``"scc"``.
   Lookups are therefore case-insensitive.

2. **The root is an unnamed table** that the parser creates automatically.

3. **Typed blocks produce two levels.** ``Driver = ConjugateGradient { ... }``
   creates a ``driver`` table whose sole child is a ``conjugategradient`` table.
   See :ref:`typed_blocks` below.

4. **Multi-line data becomes a ``#text`` value.** The ``KPointsAndWeights`` block
   has no named children — its content is stored as a single anonymous value node
   named ``"#text"``.


How Each Syntax Feature Maps to the Tree
-----------------------------------------

.. _simple_assignment:

Simple assignment (``Tag = value``)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: text

   MaxSteps = 200

Creates a single **value node**:

.. code-block:: text

   maxsteps (VALUE, string_value="200")

The value is always stored as a string. When you call ``hsd_get(root, "Driver/.../MaxSteps", n, stat)``,
the library parses ``"200"`` into an integer on the fly.


.. _typed_blocks:

Typed blocks (``Tag = Type { ... }``)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: text

   Hamiltonian = DFTB {
     SCC = Yes
   }

This is syntactic sugar. The parser creates **two nested tables**:

.. code-block:: text

   hamiltonian (TABLE)
   └── dftb (TABLE)
       └── scc (VALUE, string_value="Yes")

The outer table (``hamiltonian``) has exactly one child: the inner table
(``dftb``), which holds the block's content. This lets ``hsd_get(root,
"Hamiltonian/DFTB/SCC", val, stat)`` navigate both levels naturally.

When serialized back to HSD, single-child tables are automatically
compressed to the ``Tag = Type { ... }`` short form.


.. _plain_blocks:

Plain blocks (``Tag { ... }``)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: text

   Analysis {
     PrintForces = Yes
   }

Creates a single **table node** with one level:

.. code-block:: text

   analysis (TABLE)
   └── printforces (VALUE, string_value="Yes")


.. _attributes:

Attributes (``Tag [attr] = value``)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: text

   MaxForce [eV/Angstrom] = 0.001

The text inside ``[...]`` is stored in the node's ``attrib`` field:

.. code-block:: text

   maxforce (VALUE, attrib="eV/Angstrom", string_value="0.001")

Attributes can appear on both value nodes and table nodes:

.. code-block:: text

   Lattice [Angstrom] {
     10.0 0.0 0.0
     0.0 10.0 0.0
     0.0 0.0 10.0
   }

.. code-block:: text

   lattice (TABLE, attrib="Angstrom")
   └── (VALUE, name="#text", string_value="10.0 0.0 0.0\n0.0 10.0 0.0\n0.0 0.0 10.0")


.. _multiline_data:

Multi-line / inline data (``#text`` nodes)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When a block contains raw data instead of named children:

.. code-block:: text

   KPointsAndWeights {
     4 0 0
     0 4 0
     0 0 4
   }

The content is collected into a single anonymous **value node** named ``"#text"``:

.. code-block:: text

   kpointsandweights (TABLE)
   └── (VALUE, name="#text", string_value="4 0 0\n0 4 0\n0 0 4")

The ``"#text"`` name follows the legacy xmlf90 convention from DFTB+. Use
``hsd_get_inline_text(table, text, stat)`` to retrieve it, or access it directly
via ``hsd_get(root, "KPointsAndWeights", matrix, stat)`` with the matrix or
array accessors which resolve ``#text`` children transparently.


.. _arrays:

Arrays (space or comma separated)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: text

   Values = 1 2 3 4 5
   Coords = 1.0, 2.5, 3.0
   Elements = C H O N

Each creates a single **value node** whose ``string_value`` holds the
full text:

.. code-block:: text

   values  (VALUE, string_value="1 2 3 4 5")
   coords  (VALUE, string_value="1.0, 2.5, 3.0")
   elements (VALUE, string_value="C H O N")

The typed array is parsed on demand when you call ``hsd_get`` with an array
argument.


.. _case_folding:

Case-insensitive tag names
~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: text

   Temperature = 300
   temperature = 500   # Overwrites the previous one

Both tags are stored as ``"temperature"``. Since the second occurrence comes
later, ``hsd_get`` returns ``"500"`` (last-wins semantics). Both nodes are
preserved in the tree and can be seen when iterating children.


.. _duplicate_keys:

Duplicate keys
~~~~~~~~~~~~~~

.. code-block:: text

   Option = alpha
   Option = beta

Both values are kept in the tree as separate children:

.. code-block:: text

   parent (TABLE)
   ├── option (VALUE, string_value="alpha")
   └── option (VALUE, string_value="beta")

``hsd_get`` returns the **last** occurrence (``"beta"``). To see all
occurrences, iterate the parent's children with ``hsd_iterator_t``.


.. _semicolons:

Semicolons as statement separators
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Semicolons act like newlines, allowing multiple statements on one line:

.. code-block:: text

   A = 1; B = 2; C = 3

Is identical to:

.. code-block:: text

   A = 1
   B = 2
   C = 3

All three become sibling value nodes.


.. _includes:

File includes
~~~~~~~~~~~~~

**Parsed include** (``<<+``) — the included file is parsed as HSD and its
top-level nodes are merged into the current block:

.. code-block:: text

   <<+ "defaults.hsd"
   Driver { MaxSteps = 100 }

If ``defaults.hsd`` contains ``Driver { MaxSteps = 50 }``, the tree will
have two ``driver`` table nodes; the explicitly written one (``MaxSteps = 100``)
takes precedence via last-wins lookup.

**Text include** (``<<<``) — the file is read as raw text and injected as a
``#text`` value node:

.. code-block:: text

   Geometry {
     <<< "coords.xyz"
   }

.. code-block:: text

   geometry (TABLE)
   └── (VALUE, name="#text", string_value=<contents of coords.xyz>)

Include paths are resolved relative to the including file's directory.
Circular includes are detected and produce an error.


.. _amendments:

Amendments (``+Tag { ... }``)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Amendments merge additional children into an **existing** table:

.. code-block:: text

   Driver {
     MaxSteps = 100
   }

   +Driver {
     Tolerance = 1.0e-6
   }

After parsing, the tree has a single ``driver`` table with both children:

.. code-block:: text

   driver (TABLE)
   ├── maxsteps  (VALUE, string_value="100")
   └── tolerance (VALUE, string_value="1.0e-6")

The ``+`` prefix tells the parser to find the existing ``driver`` node in
the parent table and parse the new block's content into it, rather than
creating a separate table. If no existing node is found, the parser reports
an error.


.. _comments:

Comments
~~~~~~~~

.. code-block:: text

   # Full-line comment
   MaxSteps = 100  # Inline comment

Comments are stripped during lexing and leave **no trace** in the tree.


Putting It All Together
-----------------------

Here is a more complex example that exercises most features:

.. code-block:: text

   # Typed block with attribute
   Hamiltonian = DFTB {
     SCC = Yes
     SCCTolerance = 1.0e-5

     MaxAngularMomentum {
       C = "p"
       H = "s"
     }

     Filling = Fermi {
       Temperature [Kelvin] = 300.0
     }

     KPointsAndWeights {
       4 0 0
       0 4 0
       0 0 4
     }
   }

   +Hamiltonian {
     +DFTB {
       ReadInitialCharges = Yes
     }
   }

The final in-memory tree:

.. code-block:: text

   root (TABLE)
   └── hamiltonian (TABLE)
       └── dftb (TABLE)
           ├── scc              (VALUE, string_value="Yes")
           ├── scctolerance     (VALUE, string_value="1.0e-5")
           ├── maxangularmomentum (TABLE)
           │   ├── c            (VALUE, string_value="p")
           │   └── h            (VALUE, string_value="s")
           ├── filling (TABLE)
           │   └── fermi (TABLE)
           │       └── temperature (VALUE, attrib="Kelvin",
           │                        string_value="300.0")
           ├── kpointsandweights (TABLE)
           │   └── (VALUE, name="#text",
           │        string_value="4 0 0\n0 4 0\n0 0 4")
           └── readinitialcharges (VALUE, string_value="Yes")

Notice that the ``+Hamiltonian { +DFTB { ... } }`` amendment added
``readinitialcharges`` directly into the existing ``dftb`` table —
no duplicate tables were created.


Accessing the Tree from Fortran
-------------------------------

With the tree above loaded into ``root``:

.. code-block:: fortran

   use hsd
   implicit none

   type(hsd_node_t) :: root
   type(hsd_error_t), allocatable :: error
   integer :: stat
   real(dp) :: temperature
   logical :: scc
   real(dp), allocatable :: kpoints(:,:)

   call hsd_load_file("input.hsd", root, error)

   ! Scalar value — string "300.0" is parsed to real(dp)
   call hsd_get(root, "Hamiltonian/DFTB/Filling/Fermi/Temperature", &
     & temperature, stat)

   ! Logical — "Yes" is parsed to .true.
   call hsd_get(root, "Hamiltonian/DFTB/SCC", scc, stat)

   ! Matrix — the #text node is parsed into a 2D array
   call hsd_get_matrix(root, "Hamiltonian/DFTB/KPointsAndWeights", &
     & kpoints, stat)
   ! kpoints is now shape (3, 3)

   call root%destroy()

Path navigation with ``/`` separators walks table children by name.
Typed blocks add one extra path segment (``Hamiltonian/DFTB/...`` rather
than just ``Hamiltonian/...``).


Summary Table
-------------

.. list-table::
   :header-rows: 1
   :widths: 35 30 35

   * - HSD Syntax
     - Tree Result
     - Access Pattern
   * - ``Tag = value``
     - Value node
     - ``hsd_get(root, "Tag", v, stat)``
   * - ``Tag { children }``
     - Table node
     - ``hsd_get_child(root, "Tag", child, stat)``
   * - ``Tag = Type { children }``
     - Nested tables (Tag → Type)
     - ``hsd_get(root, "Tag/Type/...", ...)``
   * - ``Tag [unit] = value``
     - Value node with ``attrib``
     - ``hsd_get_attrib(root, "Tag", unit, stat)``
   * - ``Tag { raw data }``
     - Table with ``#text`` child
     - ``hsd_get_inline_text(table, text, stat)``
   * - ``<<+ "file.hsd"``
     - Parsed HSD merged in
     - (transparent)
   * - ``<<< "file.txt"``
     - ``#text`` value node
     - ``hsd_get_inline_text(...)``
   * - ``+Tag { ... }``
     - Merge into existing table
     - (transparent)
   * - ``A = 1; B = 2``
     - Two sibling nodes
     - (same as newline-separated)
   * - ``# comment``
     - Nothing (stripped)
     - —
