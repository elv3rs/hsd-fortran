Installation
============

Requirements
------------

- Fortran 2008 compatible compiler:

  - GFortran ≥ 7.0
  - Intel Fortran (ifort) ≥ 18.0
  - Intel Fortran (ifx)

- CMake ≥ 3.14

Building with CMake
-------------------

Basic Build
~~~~~~~~~~~

.. code-block:: bash

   # Configure and build
   cmake -B build
   cmake --build build

   # Run tests
   ctest --test-dir build

   # Install
   cmake --install build --prefix /path/to/install

Debug Build with Coverage
~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: bash

   cmake -B build -DCMAKE_BUILD_TYPE=Debug -DHSD_COVERAGE=ON
   cmake --build build
   ctest --test-dir build

   # Generate coverage report (requires lcov)
   lcov --capture --directory build/src --output-file build/coverage.info
   genhtml build/coverage.info --output-directory build/coverage_html

CMake Options
~~~~~~~~~~~~~

The following options can be passed to CMake with ``-D<OPTION>=<VALUE>``:

.. list-table::
   :header-rows: 1
   :widths: 30 15 55

   * - Option
     - Default
     - Description
   * - ``HSD_ACCEPT_TRUE_FALSE``
     - ``ON``
     - Accept ``True``/``False`` as boolean values (in addition to ``Yes``/``No``)
   * - ``HSD_BUILD_TESTS``
     - ``ON``
     - Build the test suite
   * - ``HSD_BUILD_EXAMPLES``
     - ``ON``
     - Build example programs
   * - ``HSD_COVERAGE``
     - ``OFF``
     - Enable code coverage instrumentation (requires GCC)

Using as a CMake Subproject
~~~~~~~~~~~~~~~~~~~~~~~~~~~

You can include HSD-Fortran in your CMake project using ``add_subdirectory``:

.. code-block:: cmake

   add_subdirectory(external/hsd-fortran)
   target_link_libraries(your_target PRIVATE hsd)

Or using ``FetchContent``:

.. code-block:: cmake

   include(FetchContent)
   FetchContent_Declare(
     hsd
     GIT_REPOSITORY https://github.com/dftbplus/hsd-fortran.git
     GIT_TAG main
   )
   FetchContent_MakeAvailable(hsd)
   target_link_libraries(your_target PRIVATE hsd)

Building with fpm
-----------------

The library supports the `Fortran Package Manager (fpm) <https://fpm.fortran-lang.org/>`_:

.. code-block:: bash

   # Build
   fpm build

   # Run tests
   fpm test

   # Run example
   fpm run --example simple_read

Using as an fpm Dependency
~~~~~~~~~~~~~~~~~~~~~~~~~~

Add to your ``fpm.toml``:

.. code-block:: toml

   [dependencies]
   hsd.git = "https://github.com/dftbplus/hsd-fortran.git"

Verifying Installation
----------------------

After building, verify the installation by running the test suite:

.. code-block:: bash

   ctest --test-dir build --verbose

All tests should pass. If you encounter issues, please check:

1. Your compiler version meets the minimum requirements
2. CMake version is 3.14 or higher
3. The build directory is clean (try removing and recreating it)

For fpm, run:

.. code-block:: bash

   fpm test

Running the Example
-------------------

After building, run the included example:

.. code-block:: bash

   # CMake build
   cd build/example
   ./simple_read

   # fpm
   fpm run --example simple_read

The example demonstrates the main features of the library using a sample HSD input file.
