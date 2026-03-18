User Guide
==========

This guide covers the main features of HSD-Fortran with practical examples.

Basic Usage
-----------

Loading HSD Files
~~~~~~~~~~~~~~~~~

The most common operation is loading an HSD file into a tree structure:

.. code-block:: fortran

   use hsd
   implicit none

   type(hsd_node) :: root
   type(hsd_error_t), allocatable :: error

   ! Load from file
   call hsd_load_file("config.hsd", root, error)

   if (allocated(error)) then
     call error%print()  ! Print formatted error message
     stop 1
   end if

You can also parse HSD from a string:

.. code-block:: fortran

   character(len=*), parameter :: hsd_content = &
     'Driver { MaxSteps = 100 }'

   call hsd_load_string(hsd_content, root, error)

Accessing Values
~~~~~~~~~~~~~~~~

Use ``hsd_get`` to retrieve values from the tree using path notation:

.. code-block:: fortran

   integer :: max_steps, stat
   real(dp) :: temperature
   logical :: scc_enabled
   character(len=:), allocatable :: method

   ! Access nested values with "/" separator
   call hsd_get(root, "Driver/MaxSteps", max_steps, stat)
   call hsd_get(root, "Hamiltonian/DFTB/Temperature", temperature, stat)
   call hsd_get(root, "Hamiltonian/DFTB/SCC", scc_enabled, stat)
   call hsd_get(root, "Hamiltonian/Method", method, stat)

   ! Check status
   if (stat /= HSD_STAT_OK) then
     print *, "Value not found or type error"
   end if

Default Values
~~~~~~~~~~~~~~

Use ``hsd_get`` with status checks when you want a default value if the key is missing:

.. code-block:: fortran

   integer :: timeout, seed
   real(dp) :: tolerance

   call hsd_get(root, "Driver/Timeout", timeout, stat=stat)
   if (stat == HSD_STAT_NOT_FOUND) timeout = 3600

   call hsd_get(root, "Options/RandomSeed", seed, stat=stat)
   if (stat == HSD_STAT_NOT_FOUND) seed = 12345

   call hsd_get(root, "Tolerance", tolerance, stat=stat)
   if (stat == HSD_STAT_NOT_FOUND) tolerance = 1.0e-6_dp

   ! stat will be HSD_STAT_NOT_FOUND if default was used
   if (stat == HSD_STAT_NOT_FOUND) then
     print *, "Using default value"
   end if

Working with Arrays
-------------------

HSD supports arrays with space or comma-separated values:

.. code-block:: text

   # In HSD file
   Values = 1 2 3 4 5
   Coords = 1.0, 2.5, 3.0
   Elements = C H O N

Reading arrays in Fortran:

.. code-block:: fortran

   integer, allocatable :: int_arr(:)
   real(dp), allocatable :: real_arr(:)
   character(len=:), allocatable :: str_arr(:)

   call hsd_get(root, "Values", int_arr, stat)
   call hsd_get(root, "Coords", real_arr, stat)
   call hsd_get(root, "Elements", str_arr, stat)

Working with Matrices
~~~~~~~~~~~~~~~~~~~~~

Multi-line data in HSD represents matrices:

.. code-block:: text

   KPoints {
     4 0 0
     0 4 0
     0 0 4
   }

Use ``hsd_get_matrix`` to read 2D arrays:

.. code-block:: fortran

   real(dp), allocatable :: kpoints(:,:)
   integer :: stat

   call hsd_get_matrix(root, "KPoints", kpoints, stat)
   ! kpoints is now a 3x3 array

Type Introspection
------------------

Query the structure before accessing values:

.. code-block:: fortran

   ! Check if a path exists
   if (hsd_has_child(root, "Hamiltonian/DFTB")) then
     print *, "DFTB block exists"
   end if

   ! Check node type
   if (hsd_is_table(root, "Hamiltonian")) then
     print *, "Hamiltonian is a table (has children)"
   end if

   if (hsd_is_value(root, "Driver/MaxSteps")) then
     print *, "MaxSteps is a leaf value"
   end if

   ! Check if value is an array
   if (hsd_is_array(root, "Values")) then
     print *, "Values contains an array"
   end if

   ! Count children
   print *, "Number of children:", hsd_child_count(root, "Hamiltonian")

   ! Get all child keys
   character(len=:), allocatable :: keys(:)
   call hsd_get_keys(root, "Hamiltonian", keys)

Working with Attributes (Units)
-------------------------------

HSD supports attributes, commonly used for physical units:

.. code-block:: text

   Temperature [Kelvin] = 300.0
   MaxForce [eV/Angstrom] = 0.001

Access attributes in Fortran:

.. code-block:: fortran

   character(len=:), allocatable :: unit
   real(dp) :: temperature

   ! Get the attribute (unit)
   call hsd_get_attrib(root, "Temperature", unit, stat)
   if (stat == HSD_STAT_OK) then
     print *, "Unit:", unit  ! "Kelvin"
   end if

   ! Check if attribute exists
   if (hsd_has_attrib(root, "Temperature")) then
     print *, "Temperature has a unit specified"
   end if

   ! Get value with unit conversion (requires a converter function)
   ! call hsd_get_with_unit(root, "Temperature", temperature, "Kelvin", my_converter, stat)

Modifying Trees
---------------

Use ``hsd_set`` to modify values:

.. code-block:: fortran

   ! Set scalar values
   call hsd_set(root, "Driver/MaxSteps", 200)
   call hsd_set(root, "Hamiltonian/DFTB/SCC", .true.)
   call hsd_set(root, "Name", "my_calculation")

   ! Set arrays
   integer :: values(5) = [1, 2, 3, 4, 5]
   call hsd_set(root, "NewValues", values)

Remove children:

.. code-block:: fortran

   call hsd_remove_child(root, "OldSection")

Saving HSD Files
----------------

Write the tree back to a file:

.. code-block:: fortran

   ! Write to file
   call hsd_dump(root, "output.hsd")

   ! Or get as string
   character(len=:), allocatable :: output
   call hsd_dump_to_string(root, output)
   print *, output

Tree Operations
---------------

Cloning Trees
~~~~~~~~~~~~~

Create an independent copy of a tree:

.. code-block:: fortran

   type(hsd_node) :: copy

   call hsd_clone(root, copy)
   ! Modify copy without affecting root

Merging Trees
~~~~~~~~~~~~~

Combine two trees (source overwrites target for conflicts):

.. code-block:: fortran

   type(hsd_node) :: defaults, user_config, merged

   call hsd_load_file("defaults.hsd", defaults, error)
   call hsd_load_file("user.hsd", user_config, error)

   call hsd_clone(defaults, merged)
   call hsd_merge(merged, user_config)  ! User settings override defaults

Iterating Children
------------------

To process all children of a table, including duplicate keys, use the ``hsd_iterator``:

.. code-block:: fortran

   type(hsd_iterator) :: it
   type(hsd_node), pointer :: node
   integer :: val, stat

   call it%init(root)
   do while (it%next(node))
     print *, "Found node: ", node%name

     ! Check node type and extract value
     if (node%node_type == NODE_TYPE_VALUE) then
       if (node%name == "MyKey") then
          call node%get_integer(val, stat)
          print *, "Value:", val
       end if
     end if
   end do

This is particularly useful when handling duplicate keys, as ``hsd_get`` only returns the last occurrence.

Validation Helpers
~~~~~~~~~~~~~~~~~~

Additional validation utilities:

.. code-block:: fortran

   type(hsd_error_t), allocatable :: error

   ! Require a field to exist (optionally check type)
   call hsd_require(root, "Driver/MaxSteps", error, &
     expected_type=FIELD_TYPE_INTEGER)
   if (allocated(error)) stop 1

   ! Validate numeric range (reads value from tree)
   call hsd_validate_range(root, "Driver/MaxSteps", &
     min_val=1.0_dp, max_val=10000.0_dp, error=error)

   ! Validate against allowed values (reads value from tree)
   call hsd_validate_one_of(root, "Driver/Method", &
     [character(len=10) :: "option1", "option2", "option3"], error)



Best Practices
--------------

1. **Always check for errors** after parsing and validation
2. **Validate inputs using hsd_require and hsd_validate_***
3. **Clone trees** before modifying if you need the original
4. **Use path notation** for cleaner code instead of manual tree traversal
5. **Validate ranges** for numeric inputs
6. **Provide defaults** for optional configuration values

.. note::

   All code snippets above are excerpts. Compilable programs demonstrating
   these features can be found under ``example/``:

   - ``simple_read.f90`` — loading, accessing, modifying, merging, validation
   - ``matrix_demo.f90`` — arrays and matrices

Example: Complete Configuration Parser
--------------------------------------

.. code-block:: fortran

   subroutine load_config(filename, config, error)
     character(len=*), intent(in) :: filename
     type(my_config), intent(out) :: config
     type(hsd_error_t), allocatable, intent(out) :: error

     type(hsd_node) :: root
     integer :: stat

     ! Load file
     call hsd_load_file(filename, root, error)
     if (allocated(error)) return

     ! Extract values
     call hsd_get(root, "MaxIterations", config%max_iter, stat)
     call hsd_get(root, "Tolerance", config%tolerance, stat=stat)
     if (stat == HSD_STAT_NOT_FOUND) config%tolerance = 1.0e-6_dp
     call hsd_get(root, "Method", config%method, stat=stat)
     if (stat == HSD_STAT_NOT_FOUND) config%method = "default"

     ! Validate ranges (takes table + path)
     call hsd_validate_range(root, "MaxIterations", 1.0_dp, 100000.0_dp, error)

   end subroutine
