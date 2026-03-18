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
    Reading the same tree                  ✅ Yes        Concurrent reads are safe (caches removed)
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
    call hsd_load_file("config1.hsd", root1, error1)
    
    ! Thread 2 (can run concurrently)
    call hsd_load_file("config2.hsd", root2, error2)

**2. Reading from a shared tree**

.. note::

    Since version 1.0.0, value nodes no longer use internal caching.
    Array values are parsed on demand from raw text. However, getter
    methods (``hsd_get``, etc.) modify the ``processed`` flag of nodes for
    validation purposes. While often benign, this constitutes a data race
    if accessed concurrently. Strict thread safety requires synchronization
    or ignoring validation results.

.. code-block:: fortran

    ! Safe: multiple threads can read the same tree concurrently
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

**3. Adding/removing children**

The children array is modified during:

- ``add_child()`` which modifies the data structure
- ``remove_child()`` (shifts children)

Module-Specific Notes
---------------------

hsd_types
~~~~~~~~~

- ``hsd_node`` is NOT thread-safe for modification
- Value node getters are **thread-safe** (no internal state mutation)
- Table node read operations (``get_child``, ``child_count``, etc.) are
  thread-safe if no concurrent writes

hsd_parser
~~~~~~~~~~

- ``hsd_load_file()`` and ``hsd_load_string()`` are thread-safe for different output trees
- Include file processing is NOT thread-safe if files share the same include paths

hsd_formatter
~~~~~~~~~~~~~

- ``hsd_dump()`` is thread-safe for read-only trees
- ``hsd_dump_to_string()`` is thread-safe for read-only trees

OpenMP Considerations
---------------------

When using OpenMP:

.. code-block:: fortran

    !$omp parallel private(local_root, local_error)
      call hsd_load_file(filenames(omp_get_thread_num() + 1), local_root, local_error)
      ! Process local_root...
      call local_root%destroy()
    !$omp end parallel

For shared reading with OpenMP:

.. code-block:: fortran

    ! Parse once in serial region
    call hsd_load_file("shared.hsd", shared_root, error)

    !$omp parallel
      ! Safe: read-only access
      call hsd_get(shared_root, "value", my_val)
    !$omp end parallel

    call shared_root%destroy()

Coarray Fortran Considerations
------------------------------

Each image should maintain its own HSD trees:

.. code-block:: fortran

    type(hsd_node) :: local_root  ! Each image has its own copy

    call hsd_load_file("config.hsd", local_root, error)  ! Each image loads independently
    ! No cross-image sharing of hsd_node objects

Recommendations
---------------

1. **Parse configuration files in the master thread**, then share read-only
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
