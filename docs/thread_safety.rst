Thread Safety Guide
====================

This document describes the thread safety characteristics of the HSD-Fortran library.

Overview
--------

HSD-Fortran is designed primarily for single-threaded use. Most operations are **NOT thread-safe** by default. However, certain usage patterns are safe for concurrent access.

Thread Safety Summary
---------------------

.. table::

    ====================================== ============= ================================
    Operation                              Thread-Safe   Notes
    ====================================== ============= ================================
    Parsing different files                ✅ Yes        Independent trees can be parsed concurrently
    Reading the same tree                  ⚠️ Conditional First access to ``hsd_value`` getters mutates internal caches; safe after all caches are populated
    Modifying the same tree                ❌ No         Requires external synchronization
    Parsing to the same tree               ❌ No         Undefined behavior
    Using shared iterators                 ❌ No         Each thread needs its own iterator
    ====================================== ============= ================================

Detailed Analysis
-----------------

Safe Operations (No Synchronization Needed)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

**1. Parsing independent files concurrently**

.. code-block:: fortran

    ! Thread 1
    call hsd_load("config1.hsd", root1, error1)
    
    ! Thread 2 (can run concurrently)
    call hsd_load("config2.hsd", root2, error2)

**2. Reading from a shared tree (after cache warmup)**

.. warning::

    ``hsd_value`` nodes use lazy caching: the first call to a typed getter
    (e.g. ``get_int``, ``get_real``) parses and stores the result internally.
    This **mutates** the node and is therefore **NOT thread-safe** if two
    threads access the same value concurrently for the first time.

    To make concurrent reads safe, populate all caches in a single-threaded
    context before sharing the tree ("cache warmup").

.. code-block:: fortran

    ! Step 1 — warm up caches in the master thread
    call hsd_get(shared_root, "path/to/value", val)
    call hsd_get(shared_root, "path/to/other", val2)
    ! ... access every value node once ...

    ! Step 2 — now multiple threads can safely read the same tree
    !$omp parallel
      call hsd_get(shared_root, "path/to/value", val)
      call hsd_has_child(shared_root, "some/path")
      call hsd_child_count(shared_root, "table/path")
    !$omp end parallel

**3. Cloning a tree for thread-local use**

.. code-block:: fortran

    ! Clone once per thread, then use freely
    call hsd_clone(shared_root, thread_local_copy)
    ! Now modify thread_local_copy safely

Unsafe Operations (Require External Synchronization)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

**1. Modifying a shared tree**

.. code-block:: fortran

    ! UNSAFE without synchronization:
    call hsd_set(shared_root, "path", value)
    call root%add_child(child)
    call root%remove_child(1)

**2. Using shared iterators**

.. code-block:: fortran

    ! Each thread must have its own iterator instance
    type(hsd_iterator) :: my_iterator  ! Thread-local
    call my_iterator%init(shared_table)

**3. Building/invalidating hash index**

The hash index used for O(1) child lookup is modified during:

- ``add_child()`` which modifies the data structure and hash index
- ``remove_child()`` (invalidates index)
- ``build_index()`` (explicit rebuild)

Module-Specific Notes
---------------------

hsd_types
~~~~~~~~~

- ``hsd_table`` and ``hsd_value`` types are NOT thread-safe for modification
- ``hsd_value`` typed getters (``get_int``, ``get_real``, ``get_logical``, etc.)
  **mutate internal caches** on first access and are therefore NOT thread-safe
  for concurrent first reads. After all caches are populated ("warmed up"),
  subsequent reads are safe.
- ``hsd_table`` read operations (``get_child``, ``child_count``, etc.) are
  thread-safe if no concurrent writes
- The internal hash index (``name_index``) is NOT thread-safe

hsd_parser
~~~~~~~~~~

- ``hsd_parse()`` and ``hsd_parse_string()`` are thread-safe for different output trees
- Include file processing is NOT thread-safe if files share the same include paths

hsd_formatter
~~~~~~~~~~~~~

- ``hsd_dump()`` is thread-safe for trees whose value caches are populated
- ``hsd_dump_to_string()`` is thread-safe for trees whose value caches are populated
- Both may trigger cache mutation on ``hsd_value`` nodes if caches are not yet populated

hsd_schema
~~~~~~~~~~

- Schema objects can be safely shared for validation (read-only)
- ``schema_validate()`` is thread-safe for read-only trees and schemas
- Schema modification (add_field) is NOT thread-safe

hsd_hash_table
~~~~~~~~~~~~~~

- All operations are NOT thread-safe
- The hash table is an internal implementation detail

OpenMP Considerations
---------------------

When using OpenMP:

.. code-block:: fortran

    !$omp parallel private(local_root, local_error)
      call hsd_load(filenames(omp_get_thread_num() + 1), local_root, local_error)
      ! Process local_root...
      call local_root%destroy()
    !$omp end parallel

For shared reading with OpenMP:

.. code-block:: fortran

    ! Parse once in serial region
    call hsd_load("shared.hsd", shared_root, error)

    ! Warm up all value caches in serial region
    call hsd_get(shared_root, "value", my_val)
    ! ... access every value node once ...

    !$omp parallel
      ! Safe: caches already populated, read-only access
      call hsd_get(shared_root, "value", my_val)
    !$omp end parallel

    call shared_root%destroy()

Coarray Fortran Considerations
------------------------------

Each image should maintain its own HSD trees:

.. code-block:: fortran

    type(hsd_table) :: local_root  ! Each image has its own copy

    call hsd_load("config.hsd", local_root, error)  ! Each image loads independently
    ! No cross-image sharing of hsd_table objects

Recommendations
---------------

1. **Parse configuration files in the master thread** and access every value
   node once (cache warmup), then share read-only
2. **Clone trees** if multiple threads need to modify
3. **Use thread-local iterators** when traversing shared trees
4. **Avoid modifying shared trees** without external locks

Future Considerations
---------------------

Future versions may include:

- Optional mutex protection for thread-safe modification
- Thread-local storage for iterators
- Atomic reference counting for shared trees

Implementation Notes
--------------------

Why No Built-in Synchronization?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

1. **Performance**: Mutex overhead is significant for small operations
2. **Flexibility**: Users can choose their synchronization strategy
3. **Fortran compatibility**: Standard Fortran has limited synchronization primitives
4. **Use case**: Most HSD use is configuration parsing (single-threaded)

Hash Table Thread Safety
~~~~~~~~~~~~~~~~~~~~~~~~

The O(1) child lookup hash table is implemented in ``hsd_hash_table.f90``. It is intentionally NOT thread-safe because:

1. Hash table modifications (insert/remove) are rare after parsing
2. Synchronization would penalize the common read-only case
3. Users who need concurrent modification should clone the tree

If thread-safe modification is required, users should:

1. Use external mutex/lock around modification operations
2. Clone the tree for each thread
3. Rebuild the tree from thread-local modifications
