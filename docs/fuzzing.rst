Fuzz Testing
============

HSD-Fortran includes infrastructure for coverage-guided fuzz testing using `AFL++ <https://github.com/AFLplusplus/AFLplusplus>`_. Fuzz testing automatically generates thousands of malformed inputs to find edge cases, crashes, and bugs that manual testing might miss.

Prerequisites
-------------

Install AFL++ on Ubuntu/Debian:

.. code-block:: bash

   sudo apt-get install afl++

For QEMU mode (binary-level instrumentation), you'll need to build QEMU support:

.. code-block:: bash

   cd /tmp
   git clone --depth 1 https://github.com/AFLplusplus/AFLplusplus.git
   cd AFLplusplus && make source-only
   cd qemu_mode && ./build_qemu_support.sh
   sudo cp ../afl-qemu-trace /usr/bin/

Building the Fuzz Target
------------------------

The fuzz testing infrastructure is in the ``utils/fuzz/`` directory:

.. code-block:: bash

   cd hsd-fortran
   
   # Build with regular gfortran
   cmake -B build-fuzz -S utils/fuzz
   cmake --build build-fuzz

This creates ``build-fuzz/hsd_fuzz_stdin``, a standalone driver that reads HSD input from stdin.

Running the Fuzzer
------------------

Basic Usage
^^^^^^^^^^^

.. code-block:: bash

   cd build-fuzz
   
   # Copy seed corpus and dictionary
   cp -r ../utils/fuzz/corpus .
   cp ../utils/fuzz/hsd.dict .
   
   # Create output directory
   mkdir -p findings
   
   # Run AFL++ with QEMU mode (coverage-guided)
   afl-fuzz -Q -i corpus -o findings -x hsd.dict -- ./hsd_fuzz_stdin

AFL++ will display a status screen showing:

- Execution speed (execs/sec)
- Code coverage (map density)
- Crashes and hangs found
- Corpus growth over time

Press ``Ctrl+C`` to stop.

Using the Dictionary
^^^^^^^^^^^^^^^^^^^^

The ``hsd.dict`` file contains HSD-specific tokens (``{``, ``}``, ``<<<``, ``Yes``, etc.) that help AFL++ generate syntactically meaningful inputs:

.. code-block:: bash

   afl-fuzz -Q -i corpus -o findings -x hsd.dict -- ./hsd_fuzz_stdin

Parallel Fuzzing
^^^^^^^^^^^^^^^^

To utilize multiple CPU cores:

.. code-block:: bash

   # Terminal 1 - Main fuzzer
   afl-fuzz -Q -i corpus -o findings -M main -- ./hsd_fuzz_stdin
   
   # Terminal 2+ - Secondary fuzzers
   afl-fuzz -Q -i corpus -o findings -S fuzzer2 -- ./hsd_fuzz_stdin
   afl-fuzz -Q -i corpus -o findings -S fuzzer3 -- ./hsd_fuzz_stdin

Understanding Output
--------------------

AFL++ creates these directories in ``findings/``:

.. list-table::
   :header-rows: 1
   :widths: 20 80

   * - Directory
     - Contents
   * - ``queue/``
     - Inputs that discovered new code paths
   * - ``crashes/``
     - Inputs that caused crashes
   * - ``hangs/``
     - Inputs that caused timeouts

Reproducing Issues
^^^^^^^^^^^^^^^^^^

To reproduce a crash:

.. code-block:: bash

   ./hsd_fuzz_stdin < findings/crashes/id:000000*
   
   # With a debugger
   gdb -ex run -ex bt --args ./hsd_fuzz_stdin < findings/crashes/id:000000*

Corpus Management
-----------------

Minimizing the Corpus
^^^^^^^^^^^^^^^^^^^^^

After fuzzing, minimize the corpus to remove redundant inputs:

.. code-block:: bash

   afl-cmin -Q -i findings/queue -o corpus_min -- ./hsd_fuzz_stdin

Minimizing a Crash
^^^^^^^^^^^^^^^^^^

Reduce a crashing input to its minimal form:

.. code-block:: bash

   afl-tmin -Q -i findings/crashes/id:000000 -o crash_min -- ./hsd_fuzz_stdin

Seed Corpus
-----------

The ``utils/fuzz/corpus/`` directory contains seed inputs covering various HSD features:

- ``minimal.hsd`` - Simple key-value
- ``section.hsd`` - Nested sections
- ``array.hsd`` - Array values
- ``attrib.hsd`` - Attributes with units
- ``bool.hsd`` - Boolean values
- ``string.hsd`` - Quoted strings
- ``nested.hsd`` - Deeply nested structure
- ``comment.hsd`` - Comments
- ``empty.hsd`` - Empty input
- ``float.hsd`` - Floating-point numbers

Add more seeds relevant to your use case for better coverage.

Troubleshooting
---------------

"afl-qemu-trace not found"
^^^^^^^^^^^^^^^^^^^^^^^^^^

QEMU mode isn't installed. See Prerequisites above to build it.

"Hmm, your system is configured to send core dump notifications"
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. code-block:: bash

   sudo sh -c 'echo core > /proc/sys/kernel/core_pattern'

Low Execution Speed
^^^^^^^^^^^^^^^^^^^

- Use a RAM disk: ``mount -t tmpfs -o size=1G tmpfs /tmp/fuzz``
- QEMU mode is 2-5x slower than native instrumentation; this is expected
- Typical speed for HSD parsing: 100-300 exec/s in QEMU mode

AFL++ Shows 0 Paths
^^^^^^^^^^^^^^^^^^^

- Ensure you're using QEMU mode (``-Q`` flag) for Fortran code
- Check that ``afl-qemu-trace`` is installed correctly
