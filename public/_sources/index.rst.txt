HSD-Fortran Documentation
==========================

.. image:: https://img.shields.io/badge/license-BSD--2--Clause--Patent-blue.svg
   :alt: License

HSD-Fortran is a lightweight, dependency-free Human-friendly Structured Data (HSD) parser for Fortran.

HSD is a data format similar to JSON and YAML, but designed to minimize the effort for humans 
to read and write it. It was originally developed as the input format for `DFTB+ <https://github.com/dftbplus/dftbplus>`_.

Features
--------

- **Parsing** HSD files into a tree structure with comprehensive error reporting
- **Serialization** of data structures back to HSD format
- **Include support** with cycle detection (``<<+`` for HSD, ``<<<`` for text)
- **Path-based accessors** for convenient data retrieval (e.g., ``"section/subsection/value"``)
- **Schema validation** for declarative input validation
- **Type introspection** to query node types before access
- **Tree operations** including merge, clone, and visitor pattern traversal
- **Thread-safe** read access to parsed trees

Quick Start
-----------

.. code-block:: fortran

   program example
     use hsd
     implicit none
     
     type(hsd_table) :: root
     type(hsd_error_t), allocatable :: error
     integer :: max_steps, stat
     real(dp) :: temperature
     
     ! Load HSD file
     call hsd_load("input.hsd", root, error)
     if (allocated(error)) then
       call error%print()
       stop 1
     end if
     
     ! Access values with path navigation
     call hsd_get(root, "Driver/MaxSteps", max_steps, stat)
     call hsd_get(root, "Hamiltonian/DFTB/Temperature", temperature, stat)
     
     ! Modify and save
     call hsd_set(root, "Driver/MaxSteps", 200)
     call hsd_dump(root, "output.hsd")
     
   end program example

Example HSD input:

.. code-block:: text

   Driver = ConjugateGradient {
     MaxSteps = 100
   }

   Hamiltonian = DFTB {
     SCC = Yes
     Temperature [Kelvin] = 300.0
   }


Contents
--------

.. toctree::
   :maxdepth: 2
   :caption: User Documentation

   installation
   user_guide
   hsd_format
   error_handling
   thread_safety
   fuzzing

.. toctree::
   :maxdepth: 2
   :caption: Reference

   api


Indices and tables
==================

* :ref:`genindex`
* :ref:`search`
