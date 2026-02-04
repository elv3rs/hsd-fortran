HSD-Fortran Documentation
==========================

This directory contains the Sphinx documentation for HSD-Fortran.

Building the Documentation
---------------------------

The documentation consists of two parts:

1. **Sphinx documentation** (user guides, tutorials) - built from this directory
2. **FORD API documentation** (generated from source code) - built from ``ford.md`` in the project root

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

**Note:** FORD 6.x is used for compatibility with Python 3.12. FORD 7.x has known issues with search index generation.

Build Complete Documentation
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

To build both Sphinx and FORD documentation:

.. code-block:: bash

    # 1. Build FORD API documentation
    ford ford.md
    # Note: FORD 6.x outputs to 'doc' directory, rename it:
    mv doc ford_docs
    
    # 2. Build Sphinx documentation
    sphinx-build -b html docs public
    
    # 3. Merge FORD docs into Sphinx output
    mkdir -p public/ford
    cp -r ford_docs/* public/ford/
    
    # 4. Open in browser
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
