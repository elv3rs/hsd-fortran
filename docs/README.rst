:orphan:

HSD-Fortran Documentation
==========================

This directory contains the Sphinx documentation for HSD-Fortran.

Building
--------

.. code-block:: bash

    pip install -r docs/requirements.txt
    sphinx-build -b html docs public
    open public/index.html

Structure
---------

- ``index.rst`` — Main page and table of contents
- ``installation.rst`` — Installation and build instructions
- ``build_systems.rst`` — Build systems and debug configuration
- ``user_guide.rst`` — Tutorial and usage examples
- ``hsd_format.rst`` — HSD format specification
- ``api.rst`` — API reference
- ``error_handling.rst`` — Error handling guide
- ``tree_storage.rst`` — Internal tree storage details
- ``conf.py`` — Sphinx configuration
