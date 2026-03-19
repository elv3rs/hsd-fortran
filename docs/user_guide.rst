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

   type(hsd_node_t), target :: root
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

Use the ``hsd_access_t`` object to retrieve values from the tree using path notation.
Errors are accumulated internally and can be checked after all accesses:

.. code-block:: fortran

   type(hsd_node_t), target :: root
   type(hsd_access_t) :: access
   integer :: max_steps
   real(dp) :: temperature
   logical :: scc_enabled
   character(len=:), allocatable :: method

   call hsd_load_file("config.hsd", root, error)
   call access%init(root)

   ! Access nested values with "/" separator
   call access%get("Driver/MaxSteps", max_steps)
   call access%get("Hamiltonian/DFTB/Temperature", temperature)
   call access%get("Hamiltonian/DFTB/SCC", scc_enabled)
   call access%get("Hamiltonian/Method", method)

   ! Check for accumulated errors
   if (access%has_errors()) then
     call access%print_errors()
   end if

Default Values
~~~~~~~~~~~~~~

Pass a ``default`` argument to ``access%get`` to provide a fallback when the key is
missing. By default (``on_missing=HSD_ON_MISSING_SET``), the default value is also
written back into the tree:

.. code-block:: fortran

   integer :: timeout, seed
   real(dp) :: tolerance

   call access%get("Driver/Timeout", timeout, default=3600)
   call access%get("Options/RandomSeed", seed, default=12345)
   call access%get("Tolerance", tolerance, default=1.0e-6_dp)

To return defaults without modifying the tree, initialize with
``on_missing=HSD_ON_MISSING_RETURN``:

.. code-block:: fortran

   call access%init(root, on_missing=HSD_ON_MISSING_RETURN)

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

   call access%get("Values", int_arr)
   call access%get("Coords", real_arr)
   call access%get("Elements", str_arr)

Working with Matrices
~~~~~~~~~~~~~~~~~~~~~

Multi-line data in HSD represents matrices:

.. code-block:: text

   KPoints {
     4 0 0
     0 4 0
     0 0 4
   }

Use ``access%get_matrix`` to read 2D arrays:

.. code-block:: fortran

   real(dp), allocatable :: kpoints(:,:)
   integer :: nrows, ncols

   call access%get_matrix("KPoints", kpoints, nrows, ncols)
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

Use ``access%set`` to modify values:

.. code-block:: fortran

   ! Set scalar values
   call access%set("Driver/MaxSteps", 200)
   call access%set("Hamiltonian/DFTB/SCC", .true.)
   call access%set("Name", "my_calculation")

   ! Set arrays
   integer :: values(5) = [1, 2, 3, 4, 5]
   call access%set("NewValues", values)

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

   type(hsd_node_t) :: copy

   call hsd_clone(root, copy)
   ! Modify copy without affecting root

Merging Trees
~~~~~~~~~~~~~

Combine two trees (source overwrites target for conflicts):

.. code-block:: fortran

   type(hsd_node_t) :: defaults, user_config, merged

   call hsd_load_file("defaults.hsd", defaults, error)
   call hsd_load_file("user.hsd", user_config, error)

   call hsd_clone(defaults, merged)
   call hsd_merge(merged, user_config)  ! User settings override defaults

Iterating Children
------------------

To process all children of a table, including duplicate keys, use the ``hsd_iterator_t``:

.. code-block:: fortran

   type(hsd_iterator_t) :: it
   type(hsd_node_t), pointer :: node
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

This is particularly useful when handling duplicate keys, as ``access%get`` only returns the last occurrence.

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

     type(hsd_node_t), target :: root
     type(hsd_access_t) :: access

     ! Load file
     call hsd_load_file(filename, root, error)
     if (allocated(error)) return

     ! Extract values (defaults are written back to tree)
     call access%init(root)
     call access%get("MaxIterations", config%max_iter)
     call access%get("Tolerance", config%tolerance, default=1.0e-6_dp)
     call access%get("Method", config%method, default="default")

     ! Check for accumulated errors
     if (access%has_errors()) then
       call access%print_errors()
     end if

     ! Validate ranges (takes table + path)
     call hsd_validate_range(root, "MaxIterations", 1.0_dp, 100000.0_dp, error)

   end subroutine
