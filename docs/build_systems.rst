Build Systems and Debug Configuration
======================================

This document explains the build systems available for HSD-Fortran and how to use debug modes with comprehensive runtime checks and leak detection.

Build Systems
-------------

HSD-Fortran supports two build systems:

1. **CMake** (recommended for development and testing)
2. **fpm** (Fortran Package Manager - for simpler builds)

CMake Build System
------------------

Quick Start
~~~~~~~~~~~

.. code-block:: bash

    # Configure and build
    cmake -B build -DCMAKE_BUILD_TYPE=Debug
    cmake --build build

    # Run tests
    ctest --test-dir build --output-on-failure

Build Types
~~~~~~~~~~~

- **Debug**: Full debugging support with runtime checks
- **Release**: Optimized build
- **RelWithDebInfo**: Optimized with debug symbols
- **MinSizeRel**: Optimized for size

CMake Options
~~~~~~~~~~~~~

.. table::

    ============================================ ========== ============================================
    Option                                       Default    Description
    ============================================ ========== ============================================
    ``HSD_ACCEPT_TRUE_FALSE``                    ``ON``     Accept ``True``/``False`` as boolean values
    ``HSD_BUILD_TESTS``                          ``ON``     Build test suite
    ``HSD_BUILD_EXAMPLES``                       ``ON``     Build example programs
    ``HSD_BUILD_BENCHMARKS``                     ``ON``     Build benchmark programs
    ``HSD_COVERAGE``                             ``OFF``    Enable code coverage (requires GCC)
    ``HSD_SANITIZERS``                           ``OFF``    Enable sanitizers for leak/memory detection
    ============================================ ========== ============================================

Debug Mode Features
~~~~~~~~~~~~~~~~~~~

When building in Debug mode (``-DCMAKE_BUILD_TYPE=Debug``), the following features are automatically enabled:

**GCC (gfortran)**

- ``-g``: Debug symbols
- ``-fcheck=all``: All runtime checks (bounds, pointer, memory, etc.)
- ``-fbacktrace``: Backtrace on errors
- ``-ffpe-trap=invalid,zero,overflow``: Trap floating point exceptions
- ``-finit-real=snan``: Initialize reals to signaling NaN
- ``-finit-integer=-999999``: Initialize integers to sentinel value
- ``-finit-logical=true``: Initialize logicals

**Intel (ifort)**

- ``-g``: Debug symbols
- ``-check all``: All runtime checks
- ``-traceback``: Backtrace on errors
- ``-fpe0``: Trap floating point exceptions
- ``-init=snan``: Initialize to signaling NaN

Sanitizers (Leak Detection)
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Enable sanitizers for comprehensive memory leak and error detection:

.. code-block:: bash

    cmake -B build-sanitize -DCMAKE_BUILD_TYPE=Debug -DHSD_SANITIZERS=ON
    cmake --build build-sanitize
    ctest --test-dir build-sanitize

Sanitizers enabled:

- **AddressSanitizer**: Detects memory errors, buffer overflows, use-after-free
- **LeakSanitizer**: Detects memory leaks
- **UndefinedBehaviorSanitizer**: Detects undefined behavior

**Note**: Sanitizers require the runtime library (libasan) and may slow down execution significantly.

Code Coverage
~~~~~~~~~~~~~

Enable coverage reporting for test suite:

.. code-block:: bash

    cmake -B build -DCMAKE_BUILD_TYPE=Debug -DHSD_COVERAGE=ON
    cmake --build build
    ctest --test-dir build

    # Generate coverage report
    lcov --capture --directory build/src --output-file build/coverage.info --ignore-errors inconsistent
    genhtml build/coverage.info --output-directory build/coverage_html

FPM Build System
----------------

Setup and Limitations
~~~~~~~~~~~~~~~~~~~~~

⚠️ **fpm 0.10.x doesn't recursively scan subdirectories**: Test suites are organized in ``test/suites/{api,core,io,coverage}/``, but fpm can't discover them automatically.

**Workaround**: Symlinks are created in ``test/`` pointing to the actual test files in subdirectories.
These symlinks are listed in ``test/.gitignore`` and need to be recreated if test files are added.

.. code-block:: bash

    # Create symlinks (from project root)
    cd test
    for f in suites/api/*.f90 suites/core/*.f90 suites/io/*.f90 suites/coverage/*.f90; do
      ln -sf "$f" "$(basename $f)"
    done

**Note**: This is only needed for fpm. CMake explicitly lists all source files and doesn't require symlinks.

Build and Test
~~~~~~~~~~~~~~

.. code-block:: bash

    # Build with debug profile (includes runtime checks)
    fpm build --profile debug

    # Run tests
    fpm test --profile debug

    # Build example
    fpm run simple_read --profile debug

Debug Flags in FPM
~~~~~~~~~~~~~~~~~~

FPM's debug profile automatically includes:

- ``-g``: Debug symbols
- ``-fcheck=bounds``: Bounds checking
- ``-fcheck=array-temps``: Array temporary checks
- ``-fbacktrace``: Backtrace on errors

Custom Debug Flags
~~~~~~~~~~~~~~~~~~

For even more comprehensive debugging including sanitizers:

.. code-block:: bash

    fpm build --flag "-g -Wall -Wextra -fcheck=all -fbacktrace -fsanitize=address -fsanitize=leak"
    fpm test --flag "-g -Wall -Wextra -fcheck=all -fbacktrace -fsanitize=address -fsanitize=leak"

Recommended Workflow
--------------------

Development Workflow
~~~~~~~~~~~~~~~~~~~~

1. Use CMake with Debug mode for development
2. Enable sanitizers when investigating memory issues
3. Run full test suite regularly

.. code-block:: bash

    # Daily development
    cmake -B build -DCMAKE_BUILD_TYPE=Debug
    cmake --build build
    ctest --test-dir build

    # When investigating leaks/crashes
    cmake -B build-sanitize -DCMAKE_BUILD_TYPE=Debug -DHSD_SANITIZERS=ON
    cmake --build build-sanitize
    ctest --test-dir build-sanitize

Quick Testing Workflow
~~~~~~~~~~~~~~~~~~~~~~

Use fpm for quick iterations when not debugging:

.. code-block:: bash

    fpm test --profile debug

Pre-Release Workflow
~~~~~~~~~~~~~~~~~~~~

1. Run tests with sanitizers enabled
2. Check code coverage
3. Build in Release mode and verify performance

.. code-block:: bash

    # Sanitizer check
    cmake -B build-sanitize -DCMAKE_BUILD_TYPE=Debug -DHSD_SANITIZERS=ON
    cmake --build build-sanitize
    ctest --test-dir build-sanitize

    # Coverage check
    cmake -B build-coverage -DCMAKE_BUILD_TYPE=Debug -DHSD_COVERAGE=ON
    cmake --build build-coverage
    ctest --test-dir build-coverage
    # Generate and review coverage report

    # Release build
    cmake -B build-release -DCMAKE_BUILD_TYPE=Release
    cmake --build build-release
    ctest --test-dir build-release

Interpreting Debug Output
--------------------------

Runtime Check Failures
~~~~~~~~~~~~~~~~~~~~~~

When runtime checks fail, you'll see detailed error messages:

.. code-block:: text

    At line 123 of file src/hsd_types.f90
    Fortran runtime error: Array reference out of bounds
    Backtrace for this error:
    #0  0x7f... in function_name

Sanitizer Output
~~~~~~~~~~~~~~~~

Sanitizers provide detailed reports:

.. code-block:: text

    =================================================================
    ==12345==ERROR: LeakSanitizer: detected memory leaks

    Direct leak of 64 byte(s) in 1 object(s) allocated from:
        #0 0x... in malloc
        #1 0x... in my_function

Floating Point Exceptions
~~~~~~~~~~~~~~~~~~~~~~~~~~

FPE traps will halt execution on invalid operations:

.. code-block:: text

    Program received signal SIGFPE: Floating-point exception - erroneous arithmetic operation.
    Backtrace for this signal:
    #0  0x... in divide_by_zero

Troubleshooting
---------------

Sanitizer Runtime Errors
~~~~~~~~~~~~~~~~~~~~~~~~

If you get sanitizer runtime errors, ensure libasan is installed:

.. code-block:: bash

    # Ubuntu/Debian
    sudo apt-get install libasan6

    # Fedora
    sudo dnf install libasan

FPM Test Failures
~~~~~~~~~~~~~~~~~

If fpm tests fail with "module not found", re-create the symlinks:

.. code-block:: bash

    cd test
    for f in suites/api/*.f90 suites/core/*.f90 suites/io/*.f90 suites/coverage/*.f90; do
      ln -sf "$f" "$(basename $f)"
    done

Coverage Generation Issues
~~~~~~~~~~~~~~~~~~~~~~~~~~

Ensure lcov is installed:

.. code-block:: bash

    sudo apt-get install lcov

Summary
-------

- **CMake** is the primary build system with full feature support
- **FPM** provides a simpler alternative for quick builds
- **Debug builds** include comprehensive runtime checks by default
- **Sanitizers** can detect memory leaks and errors not caught by standard checks
- Both build systems are tested and working with all 381 tests passing
