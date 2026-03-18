Build Systems
=============

HSD-Fortran supports **CMake** (recommended) and **fpm**.

CMake
-----

.. code-block:: bash

    cmake -B build -DCMAKE_BUILD_TYPE=Debug
    cmake --build build
    ctest --test-dir build --output-on-failure
    cmake --install build --prefix /path/to/install

**Options**

.. table::

    ============================== ========== ===========================================
    Option                         Default    Description
    ============================== ========== ===========================================
    ``HSD_ACCEPT_TRUE_FALSE``      ``ON``     Accept ``True``/``False`` as booleans
    ``HSD_BUILD_TESTS``            ``ON``     Build test suite
    ``HSD_BUILD_EXAMPLES``         ``ON``     Build example programs
    ``HSD_COVERAGE``               ``OFF``    Enable code coverage (GCC only)
    ``HSD_SANITIZERS``             ``OFF``    Enable address/leak/UB sanitizers
    ============================== ========== ===========================================

Debug builds automatically enable runtime checks (``-fcheck=all``, ``-fbacktrace``, FPE traps, sentinel initialisation). For memory leak detection add ``-DHSD_SANITIZERS=ON``; for coverage add ``-DHSD_COVERAGE=ON``.

fpm
---

.. code-block:: bash

    fpm build --profile debug
    fpm test --profile debug

.. note::

    fpm 0.10.x doesn't scan subdirectories, so symlinks in ``test/`` point to files
    under ``test/suites/``. Recreate them if new test files are added:

    .. code-block:: bash

        cd test
        for f in suites/api/*.f90 suites/core/*.f90 suites/io/*.f90 suites/coverage/*.f90; do
          ln -sf "$f" "$(basename $f)"
        done

