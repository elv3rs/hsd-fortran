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

   type(hsd_table) :: root
   type(hsd_error_t), allocatable :: error

   ! Load from file
   call hsd_load("config.hsd", root, error)

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

Use ``hsd_get_or`` when you want a default value if the key is missing:

.. code-block:: fortran

   integer :: timeout, seed
   real(dp) :: tolerance

   ! Returns default if path not found
   call hsd_get_or(root, "Driver/Timeout", timeout, default=3600, stat=stat)
   call hsd_get_or(root, "Options/RandomSeed", seed, default=12345, stat=stat)
   call hsd_get_or(root, "Tolerance", tolerance, default=1.0e-6_dp, stat=stat)

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

   type(hsd_table) :: copy

   call hsd_clone(root, copy)
   ! Modify copy without affecting root

Merging Trees
~~~~~~~~~~~~~

Combine two trees (source overwrites target for conflicts):

.. code-block:: fortran

   type(hsd_table) :: defaults, user_config, merged

   call hsd_load("defaults.hsd", defaults, error)
   call hsd_load("user.hsd", user_config, error)

   call hsd_clone(defaults, merged)
   call hsd_merge(merged, user_config)  ! User settings override defaults

Schema Validation
-----------------

Define and validate input schemas:

.. code-block:: fortran

   use hsd
   implicit none

   type(hsd_schema_t) :: schema
   type(hsd_error_t), allocatable :: errors(:)

   ! Initialize schema
   call schema_init(schema)

   ! Add required fields (note: requirement comes before field_type)
   call schema_add_field(schema, "Driver/MaxSteps", &
     FIELD_REQUIRED, field_type=FIELD_TYPE_INTEGER)

   call schema_add_field(schema, "Hamiltonian/DFTB/SCC", &
     FIELD_REQUIRED, field_type=FIELD_TYPE_LOGICAL)

   ! Add optional field with allowed values
   call schema_add_field_enum(schema, "Driver/Method", &
     allowed_values=[character(len=20) :: &
       "ConjugateGradient", "SteepestDescent", "FIRE"], &
     required=FIELD_OPTIONAL)

   ! Validate (returns array of errors)
   call schema_validate(schema, root, errors)
   if (size(errors) > 0) then
     print *, "Validation failed:", errors(1)%message
   end if

   ! Strict validation (fails on unknown fields)
   ! Note: currently equivalent to schema_validate
   call schema_validate_strict(schema, root, errors)

   ! Cleanup
   call schema_destroy(schema)

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

Visitor Pattern
---------------

Traverse the entire tree with custom logic:

.. code-block:: fortran

   type, extends(hsd_visitor_t) :: tree_printer
   contains
     procedure :: visit_table => print_table
     procedure :: visit_value => print_value
   end type

   type(tree_printer) :: printer

   call hsd_accept(root, printer)

   contains

   subroutine print_table(self, table, path, depth, stat)
     class(tree_printer), intent(inout) :: self
     type(hsd_table), intent(in) :: table
     character(len=*), intent(in) :: path
     integer, intent(in) :: depth
     integer, intent(out), optional :: stat
     
     print *, repeat("  ", depth), "Table: ", trim(path)
     if (present(stat)) stat = 0
   end subroutine

   subroutine print_value(self, value, path, depth, stat)
     class(tree_printer), intent(inout) :: self
     type(hsd_value), intent(in) :: value
     character(len=*), intent(in) :: path
     integer, intent(in) :: depth
     integer, intent(out), optional :: stat
     
     print *, repeat("  ", depth), "Value: ", trim(path), " = ", value%get_string()
     if (present(stat)) stat = 0
   end subroutine

Best Practices
--------------

1. **Always check for errors** after parsing and validation
2. **Use schemas** for complex input validation
3. **Clone trees** before modifying if you need the original
4. **Use path notation** for cleaner code instead of manual tree traversal
5. **Validate ranges** for numeric inputs
6. **Provide defaults** for optional configuration values

Example: Complete Configuration Parser
--------------------------------------

.. code-block:: fortran

   subroutine load_config(filename, config, error)
     character(len=*), intent(in) :: filename
     type(my_config), intent(out) :: config
     type(hsd_error_t), allocatable, intent(out) :: error

     type(hsd_table) :: root
     type(hsd_schema_t) :: schema
     type(hsd_error_t), allocatable :: errors(:)
     integer :: stat

     ! Load file
     call hsd_load(filename, root, error)
     if (allocated(error)) return

     ! Setup schema (requirement before field_type)
     call schema_init(schema)
     call schema_add_field(schema, "MaxIterations", FIELD_REQUIRED, FIELD_TYPE_INTEGER)
     call schema_add_field(schema, "Tolerance", FIELD_OPTIONAL, FIELD_TYPE_REAL)
     call schema_add_field(schema, "Method", FIELD_OPTIONAL, FIELD_TYPE_STRING)

     ! Validate (returns errors array)
     call schema_validate(schema, root, errors)
     call schema_destroy(schema)
     if (size(errors) > 0) then
       allocate(error, source=errors(1))
       return
     end if

     ! Extract values
     call hsd_get(root, "MaxIterations", config%max_iter, stat)
     call hsd_get_or(root, "Tolerance", config%tolerance, default=1.0e-6_dp, stat=stat)
     call hsd_get_or(root, "Method", config%method, default="default", stat=stat)

     ! Validate ranges (takes table + path)
     call hsd_validate_range(root, "MaxIterations", 1.0_dp, 100000.0_dp, error)

   end subroutine
