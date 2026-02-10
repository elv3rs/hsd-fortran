HSD-Fortran Documentation
==========================

This directory contains the Sphinx documentation for HSD-Fortran.

Building the Documentation
---------------------------

The documentation consists of the Sphinx documentation (user guides, tutorials) built from this directory.

Requirements
~~~~~~~~~~~~

Install documentation dependencies:

.. code-block:: bash

    # Option 1: Using pip
    pip install -r requirements.txt
    
    # Option 2: Using uv (faster)
    uv venv .venv
    source .venv/bin/activate  # On Windows: .venv\Scripts\activate
    uv pip install -r requirements.txt

Build Documentation
~~~~~~~~~~~~~~~~~~~

To build the Sphinx documentation:

.. code-block:: bash

    # 1. Build Sphinx documentation
    sphinx-build -b html docs public
    
    # 2. Open in browser
    open public/index.html
    open public/index.html  # macOS
    # or: xdg-open public/index.html  # Linux
    # or: start public/index.html  # Windows

Build Only Sphinx
~~~~~~~~~~~~~~~~~

.. code-block:: bash

    sphinx-build -b html docs _build/html
    open _build/html/index.html

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
