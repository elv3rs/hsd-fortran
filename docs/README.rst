HSD-Fortran Documentation
==========================

This directory contains the Sphinx documentation for HSD-Fortran.

Building the Documentation
---------------------------

Requirements
~~~~~~~~~~~~

Install Sphinx and the required theme:

.. code-block:: bash

    pip install sphinx sphinx-rtd-theme myst-parser

Build HTML
~~~~~~~~~~

.. code-block:: bash

    cd docs
    sphinx-build -b html . _build/html

Or use the Makefile (if available):

.. code-block:: bash

    make html

View Locally
~~~~~~~~~~~~

Open ``_build/html/index.html`` in your browser.

Documentation Structure
-----------------------

- ``index.rst`` - Main documentation page
- ``installation.rst`` - Installation and build instructions
- ``user_guide.rst`` - Tutorial and usage examples
- ``hsd_format.rst`` - HSD format specification
- ``api.rst`` - API reference
- ``error_handling.rst`` - Error handling guide
- ``thread_safety.rst`` - Thread safety information
- ``conf.py`` - Sphinx configuration

Writing Documentation
---------------------

- Use reStructuredText (``.rst``) for structured documentation
- Markdown (``.md``) files are supported via MyST parser
- Code examples should be tested before inclusion
- Follow the existing style for consistency

Adding New Pages
----------------

1. Create a new ``.rst`` file
2. Add it to the appropriate ``toctree`` in ``index.rst``
3. Rebuild the documentation
