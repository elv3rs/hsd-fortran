Error Message Quick Reference
==============================

Error Type Structure
--------------------

.. code-block:: fortran

    type :: hsd_error_t
      integer :: code                          ! Error code (HSD_STAT_*)
      character(len=:), allocatable :: message ! Human-readable message
      character(len=:), allocatable :: filename ! Where error occurred
      integer :: line_start, line_end          ! Line range
      integer :: column                        ! Column number (NEW)
      character(len=:), allocatable :: expected ! Expected value (NEW)
      character(len=:), allocatable :: actual   ! Actual value found (NEW)
      character(len=:), allocatable :: hint     ! Helpful suggestion (NEW)
    end type

Creating Errors with Context
-----------------------------

Basic Error
~~~~~~~~~~~

.. code-block:: fortran

    call make_error(error, HSD_STAT_SYNTAX_ERROR, &
      "Error message", "file.hsd", line_num)

Error with Full Context
~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: fortran

    call make_error(error, HSD_STAT_UNCLOSED_ATTRIB, &
      "Unclosed attribute bracket", &
      filename="file.hsd", &
      line_start=5, &
      column=12, &
      expected="]", &
      actual="=", &
      hint="Add closing ']' to complete the attribute")

Syntax Error Helper
~~~~~~~~~~~~~~~~~~~

.. code-block:: fortran

    call make_syntax_error(error, &
      "Unexpected token", &
      filename="file.hsd", &
      line=10, &
      column=5, &
      expected="}", &
      actual="{", &
      hint="Check block nesting")

Type Error Helper
~~~~~~~~~~~~~~~~~

.. code-block:: fortran

    call make_type_error(error, &
      "Cannot convert to integer", &
      filename="file.hsd", &
      line=20, &
      expected="integer", &
      actual="'hello'", &
      hint="Provide a numeric value")

Error Codes
-----------

.. list-table::
   :widths: 5 30 40
   :header-rows: 1

   * - Code
     - Constant
     - Meaning
   * - 0
     - ``HSD_STAT_OK``
     - No error
   * - 1
     - ``HSD_STAT_SYNTAX_ERROR``
     - Generic syntax error
   * - 2
     - ``HSD_STAT_UNCLOSED_TAG``
     - Block not closed
   * - 3
     - ``HSD_STAT_UNCLOSED_ATTRIB``
     - Attribute bracket not closed
   * - 4
     - ``HSD_STAT_UNCLOSED_QUOTE``
     - String quote not closed
   * - 5
     - ``HSD_STAT_ORPHAN_TEXT``
     - Text outside any block
   * - 6
     - ``HSD_STAT_INCLUDE_CYCLE``
     - Circular include detected
   * - 7
     - ``HSD_STAT_INCLUDE_DEPTH``
     - Too many nested includes
   * - 8
     - ``HSD_STAT_FILE_NOT_FOUND``
     - File doesn't exist
   * - 9
     - ``HSD_STAT_IO_ERROR``
     - I/O operation failed
   * - 10
     - ``HSD_STAT_TYPE_ERROR``
     - Type conversion failed
   * - 11
     - ``HSD_STAT_NOT_FOUND``
     - Key not found in tree

Usage Examples
--------------

Handling Errors
~~~~~~~~~~~~~~~

.. code-block:: fortran

    type(hsd_table) :: root
    type(hsd_error_t), allocatable :: error

    call hsd_load("config.hsd", root, error)

    if (allocated(error)) then
      ! Print to stdout
      call error%print()
      
      ! Or print to specific unit
      call error%print(unit=6)
      
      ! Access error details
      if (error%code == HSD_STAT_SYNTAX_ERROR) then
        print *, "Syntax error at line", error%line_start
        if (allocated(error%hint)) then
          print *, "Hint:", error%hint
        end if
      end if
      
      deallocate(error)
    end if

Error Output Format
~~~~~~~~~~~~~~~~~~~

.. code-block:: text

    Error in 'config.hsd' at line 5, column 12: Unclosed attribute bracket
      Expected: ]
      Got:      =
      Hint: Add closing ']' to complete the attribute

Common Error Scenarios
----------------------

Unclosed Bracket
~~~~~~~~~~~~~~~~~

.. code-block:: text

    value [unit=eV = 1.5  # Missing ]

Error shows: Expected ``]``, got ``=``, with hint to add closing bracket

Include Cycle
~~~~~~~~~~~~~

.. code-block:: text

    # file_a.hsd
    <<+ file_b.hsd

    # file_b.hsd  
    <<+ file_a.hsd

Error shows: Cyclic include detected with hint about include chain

File Not Found
~~~~~~~~~~~~~~

.. code-block:: text

    <<+ /nonexistent/file.hsd

Error shows: File path in ``actual`` field with hint to check path

Type Mismatch
~~~~~~~~~~~~~

.. code-block:: fortran

    ! HSD: name = hello_world
    integer :: val
    call hsd_get(root, "name", val, stat)
    ! stat = HSD_STAT_TYPE_ERROR

Best Practices
--------------

1. **Always check for errors** after parsing
2. **Use hints** to guide users toward solutions
3. **Provide context** (expected vs actual) when creating custom errors
4. **Include column numbers** for precise error location
5. **Clean up** by deallocating errors when done

Coding Conventions for ``stat`` Parameters
-------------------------------------------

When implementing procedures with optional status output parameters, follow these conventions to ensure consistent error handling across the codebase.

The Pattern
~~~~~~~~~~~

Status parameters should be declared as:

.. code-block:: fortran

    integer, intent(out), optional :: stat

**Critical Rule:** The ``stat`` parameter MUST be set on **all code paths** before the subroutine returns. If the caller passes a variable and the subroutine returns without setting it, the variable will contain undefined/garbage values.

Recommended Approaches
~~~~~~~~~~~~~~~~~~~~~~

**Approach 1: Early Initialization (Preferred)**

Initialize ``stat`` to ``HSD_STAT_OK`` at the start, then override only on error paths:

.. code-block:: fortran

    subroutine my_operation(table, path, result, stat)
      type(hsd_table), intent(in) :: table
      character(len=*), intent(in) :: path
      integer, intent(out) :: result
      integer, intent(out), optional :: stat

      integer :: local_stat

      ! Set success at start - only override on error
      if (present(stat)) stat = HSD_STAT_OK

      call some_helper(table, path, result, local_stat)
      if (local_stat /= 0) then
        if (present(stat)) stat = HSD_STAT_NOT_FOUND
        return
      end if

      ! ... more code ...
      ! No need to set stat at end - already HSD_STAT_OK

    end subroutine my_operation

**Approach 2: Set on Every Path**

Explicitly set ``stat`` before every ``return`` and at the end:

.. code-block:: fortran

    subroutine my_operation(table, path, result, stat)
      type(hsd_table), intent(in) :: table
      character(len=*), intent(in) :: path
      integer, intent(out) :: result
      integer, intent(out), optional :: stat

      class(hsd_node), pointer :: child
      integer :: local_stat

      call get_child(table, path, child, local_stat)

      if (local_stat /= 0 .or. .not. associated(child)) then
        if (present(stat)) stat = HSD_STAT_NOT_FOUND
        result = 0
        return  ! stat is set before return
      end if

      select type (child)
      type is (hsd_value)
        call child%get_integer(result, local_stat)
        if (present(stat)) stat = local_stat  ! Always set
      class default
        if (present(stat)) stat = HSD_STAT_TYPE_ERROR  ! Always set
        result = 0
      end select

    end subroutine my_operation

Common Mistakes to Avoid
~~~~~~~~~~~~~~~~~~~~~~~~

❌ **Missing stat on early return:**

.. code-block:: fortran

    if (error_condition) then
      return  ! BUG: stat not set!
    end if

❌ **Missing branch in select type:**

.. code-block:: fortran

    select type (node)
    type is (hsd_value)
      if (present(stat)) stat = HSD_STAT_OK
    ! BUG: class default branch doesn't set stat!
    end select

❌ **Forgetting the implicit return at end:**

.. code-block:: fortran

    subroutine example(stat)
      integer, intent(out), optional :: stat
      ! ... code that might not set stat ...
    end subroutine  ! BUG: stat might not be set

Using Local Status Variables
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Use a non-optional local variable when calling helper procedures, then propagate to the optional stat:

.. code-block:: fortran

    integer :: local_stat  ! Always receives status

    call helper_procedure(args, local_stat)  ! Pass non-optional
    if (present(stat)) stat = local_stat     ! Propagate to optional

This avoids the need for each helper to check ``present(stat)`` and ensures you always have a status value to work with locally.
