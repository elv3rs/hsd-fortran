HSD Format Reference
====================

HSD (Human-friendly Structured Data) is a data format designed to be easy for humans to read and write
while supporting hierarchical data structures similar to JSON or YAML.

This document describes the HSD syntax and features supported by HSD-Fortran.

Basic Syntax
------------

Structure
~~~~~~~~~

HSD uses a block-based structure with curly braces:

.. code-block:: text

   TagName {
     ChildTag = value
     NestedBlock {
       AnotherValue = 123
     }
   }

Key concepts:

- **Tags** are identifiers that name data elements
- **Blocks** are delimited by ``{`` and ``}``
- **Values** follow an ``=`` sign
- **Whitespace** is generally ignored (spaces, tabs, newlines)

Comments
~~~~~~~~

Comments start with ``#`` and continue to the end of the line:

.. code-block:: text

   # This is a comment
   Driver {
     MaxSteps = 100  # This is an inline comment
   }

Data Types
----------

Strings
~~~~~~~

Strings can be unquoted (if they contain no special characters) or quoted:

.. code-block:: text

   # Unquoted string
   Name = hello

   # Quoted string (required for spaces or special characters)
   Description = "Hello, World!"
   Path = "/path/to/file"

   # Empty string
   Empty = ""

Integers
~~~~~~~~

Standard integer notation:

.. code-block:: text

   Count = 42
   Negative = -10
   Large = 1000000

Real Numbers
~~~~~~~~~~~~

Real numbers support standard and scientific notation:

.. code-block:: text

   Value = 3.14159
   Small = 1.0e-5
   Large = 1.5E+10
   Precise = 2.99792458d8  # Fortran 'd' exponent also supported

Booleans
~~~~~~~~

HSD supports multiple boolean formats:

.. code-block:: text

   # Primary format (recommended)
   Enabled = Yes
   Disabled = No

   # Alternative formats
   Flag1 = On
   Flag2 = Off
   Flag3 = 1
   Flag4 = 0

   # Optional (if HSD_ACCEPT_TRUE_FALSE is enabled)
   Flag5 = True
   Flag6 = False

.. note::
   When writing HSD output, the library always uses ``Yes``/``No`` format.

Arrays
~~~~~~

Arrays are space-separated or comma-separated values:

.. code-block:: text

   # Space-separated
   Integers = 1 2 3 4 5
   
   # Comma-separated
   Reals = 1.0, 2.5, 3.0
   
   # Mixed separators
   Values = 1, 2 3, 4
   
   # String arrays
   Elements = C H O N
   Paths = "file1.dat" "file2.dat"

Complex Numbers
~~~~~~~~~~~~~~~

Complex numbers use parentheses:

.. code-block:: text

   Complex = (1.0, 2.0)
   ComplexArray = (1.0, 0.0) (0.0, 1.0)

Matrices (Multi-line Data)
~~~~~~~~~~~~~~~~~~~~~~~~~~

Multi-line values within a block are treated as matrix data:

.. code-block:: text

   Lattice {
     10.0  0.0  0.0
      0.0 10.0  0.0
      0.0  0.0 10.0
   }

   KPoints {
     0.0 0.0 0.0  1.0
     0.5 0.0 0.0  2.0
     0.5 0.5 0.0  4.0
   }

Each line represents a row; columns are space-separated.

Advanced Syntax
---------------

Typed Blocks (Short Form)
~~~~~~~~~~~~~~~~~~~~~~~~~

Assign a type to a block using the short form:

.. code-block:: text

   # Long form
   Hamiltonian {
     Type = DFTB
     SCC = Yes
   }

   # Equivalent short form
   Hamiltonian = DFTB {
     SCC = Yes
   }

This is commonly used to specify which variant of a block is being configured.

Attributes (Units)
~~~~~~~~~~~~~~~~~~

Values can have attributes, typically used for physical units:

.. code-block:: text

   Temperature [Kelvin] = 300.0
   MaxForce [eV/Angstrom] = 0.001
   Lattice [Angstrom] {
     10.0 0.0 0.0
     0.0 10.0 0.0
     0.0 0.0 10.0
   }

Attributes are enclosed in square brackets ``[...]`` after the tag name.

File Includes
~~~~~~~~~~~~~

HSD supports including content from other files:

**Parsed include** (``<<+``): The included file is parsed as HSD:

.. code-block:: text

   # Include another HSD file
   <<+ "common_settings.hsd"

   Driver {
     <<+ "driver_defaults.hsd"
     MaxSteps = 100  # Can override included values
   }

**Text include** (``<<<``): The included file is read as raw text:

.. code-block:: text

   Geometry {
     <<< "coordinates.xyz"
   }

Include Path Resolution
^^^^^^^^^^^^^^^^^^^^^^^

- Relative paths are resolved relative to the including file's directory
- Absolute paths are used as-is
- Circular includes are detected and result in an error

Escaping and Special Characters
-------------------------------

Quoted Strings
~~~~~~~~~~~~~~

Within quoted strings:

- Use ``""`` for a literal quote character
- Newlines and other whitespace are preserved

.. code-block:: text

   Message = "He said ""Hello"" to me"
   MultiLine = "Line 1
   Line 2"

Identifier Rules
~~~~~~~~~~~~~~~~

Tag names (identifiers) must:

- Start with a letter or underscore
- Contain only letters, numbers, and underscores
- Be case-sensitive

.. code-block:: text

   # Valid
   MaxSteps = 100
   max_steps = 100
   _private = 1

   # These are different tags
   Temperature = 300
   temperature = 300  # Different from Temperature

Grammar Summary
---------------

.. code-block:: text

   document     := block*
   block        := tag ['=' (value | type)] [attribute] ('{' content '}' | ε)
   content      := (block | value | include)*
   tag          := identifier
   type         := identifier
   attribute    := '[' text ']'
   value        := string | number | boolean | array | complex
   string       := quoted_string | unquoted_word
   number       := integer | real
   boolean      := 'Yes' | 'No' | 'On' | 'Off' | '1' | '0' | 'True' | 'False'
   array        := value ((',' | ' ') value)*
   complex      := '(' real ',' real ')'
   include      := '<<+' quoted_string | '<<<' quoted_string
   comment      := '#' text_to_eol

Error Messages
--------------

The parser provides detailed error messages for syntax errors:

.. code-block:: text

   Error in 'config.hsd' at line 5, column 12: Unclosed attribute bracket
     Expected: ]
     Got:      =
     Hint: Add closing ']' to complete the attribute

Common errors include:

- ``Unclosed tag``: Missing closing ``}``
- ``Unclosed attribute``: Missing closing ``]``
- ``Unclosed quote``: Missing closing ``"``
- ``Orphan text``: Text appearing outside any block
- ``Include cycle``: Circular file includes
- ``Include depth exceeded``: Too many nested includes

Compatibility Notes
-------------------

DFTB+ Compatibility
~~~~~~~~~~~~~~~~~~~

HSD-Fortran is designed to be compatible with DFTB+ input files. Key notes:

- All standard DFTB+ HSD syntax is supported
- The ``<<+`` and ``<<<`` include directives work identically
- Boolean values ``Yes``/``No`` are the canonical format

Differences from JSON/YAML
~~~~~~~~~~~~~~~~~~~~~~~~~~

Unlike JSON:

- No colons between keys and values (uses ``=``)
- Comments are supported
- Unquoted strings are allowed
- Arrays don't need brackets

Unlike YAML:

- Uses explicit braces for structure (not indentation-based)
- Simpler, more predictable parsing
- No complex features like anchors/aliases
