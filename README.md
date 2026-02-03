# hsd-fortran

[![License](https://img.shields.io/badge/license-BSD--2--Clause--Patent-blue.svg)](LICENSE)
[![Fortitude](https://img.shields.io/endpoint?url=https://raw.githubusercontent.com/PlasmaFAIR/fortitude/main/docs/assets/badge/v0.json)](https://github.com/PlasmaFAIR/fortitude)
[![Tests](https://github.com/elv3rs/hsd-fortran/actions/workflows/tests.yml/badge.svg)](https://github.com/elv3rs/hsd-fortran/actions/workflows/tests.yml)
[![Linting](https://github.com/elv3rs/hsd-fortran/actions/workflows/lint.yml/badge.svg)](https://github.com/elv3rs/hsd-fortran/actions/workflows/lint.yml)
[![Docs & Coverage](https://github.com/elv3rs/hsd-fortran/actions/workflows/docs.yml/badge.svg)](https://github.com/elv3rs/hsd-fortran/actions/workflows/docs.yml)
[![Coverage](https://elv3rs.github.io/hsd-fortran/coverage/coverage.svg)](https://elv3rs.github.io/hsd-fortran/coverage/index.html)

A lightweight, dependency-free HSD (Human-friendly Structured Data) parser for Fortran.

## Overview

HSD is a data format similar to JSON and YAML, but designed to minimize the effort for humans to read and write it. It was originally developed as the input format for [DFTB+](https://github.com/dftbplus/dftbplus).

This library provides:

- **Parsing** HSD files into a tree structure with full error reporting
- **Serialization** of data structures back to HSD format
- **Include support** with cycle detection (`<<+` for HSD, `<<<` for text)
- **Path-based accessors** for convenient data retrieval (`"section/subsection/value"`)
- **Schema validation** for declarative input validation
- **Type introspection** to query node types before access
- **Tree operations** including merge, clone, and visitor pattern traversal

## Quick Example

```fortran
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
  call hsd_get(root, "Hamiltonian/DFTB/Filling/Fermi/Temperature", temperature, stat)
  
  ! Use defaults for optional values
  call hsd_get_or(root, "Driver/Timeout", max_steps, default=3600, stat=stat)
  
  ! Type introspection
  if (hsd_is_table(root, "Hamiltonian/DFTB")) then
    print *, "Child count:", hsd_child_count(root, "Hamiltonian/DFTB")
  end if
  
  ! Modify and save
  call hsd_set(root, "Driver/MaxSteps", 200)
  call hsd_dump(root, "output.hsd")
  
end program example
```

Example input file:

```hsd
Driver = ConjugateGradient {
  MaxSteps = 100
}

Hamiltonian = DFTB {
  SCC = Yes
  Filling = Fermi {
    Temperature [Kelvin] = 300.0
  }
}
```

## Installation

### Requirements

- Fortran 2008 compatible compiler (gfortran ≥ 7, ifort ≥ 18, ifx)
- CMake ≥ 3.14

### Build with CMake

```bash
cmake -B build
cmake --build build
ctest --test-dir build  # Run tests
cmake --install build --prefix /path/to/install
```

### CMake Options

| Option | Default | Description |
|--------|---------|-------------|
| `HSD_ACCEPT_TRUE_FALSE` | `ON` | Accept `True`/`False` as boolean values |
| `HSD_BUILD_TESTS` | `ON` | Build test suite |
| `HSD_BUILD_EXAMPLES` | `ON` | Build examples |
| `HSD_COVERAGE` | `OFF` | Enable code coverage (GCC only) |

### Build with fpm

```bash
fpm build
fpm test
fpm run --example simple_read
```

## API Overview

### I/O Operations

| Procedure | Description |
|-----------|-------------|
| `hsd_load(file, root, error)` | Parse HSD file into tree |
| `hsd_load_string(str, root, error)` | Parse HSD string |
| `hsd_dump(root, file)` | Write tree to file |
| `hsd_dump_to_string(root, str)` | Serialize tree to string |

### Data Accessors

| Procedure | Description |
|-----------|-------------|
| `hsd_get(root, path, value, stat)` | Get value at path |
| `hsd_get_or(root, path, value, default, stat)` | Get with fallback default |
| `hsd_get_matrix(root, path, matrix, stat)` | Get 2D array |

### Query Operations

| Procedure | Description |
|-----------|-------------|
| `hsd_has_child(root, path)` | Check if path exists |
| `hsd_is_table(root, path)` | Check if node is a table |
| `hsd_is_value(root, path)` | Check if node is a leaf value |
| `hsd_child_count(root, path)` | Count children in table |
| `hsd_get_keys(root, path, keys)` | Get child key names |
| `hsd_get_attrib(root, path, attrib)` | Get attribute (e.g., unit) |

### Mutation and Tree Operations

| Procedure | Description |
|-----------|-------------|
| `hsd_set(root, path, value)` | Set value at path |
| `hsd_remove_child(root, path)` | Remove child node |
| `hsd_merge(target, source)` | Merge two trees |
| `hsd_clone(source, dest)` | Deep copy a tree |

### Validation

| Procedure | Description |
|-----------|-------------|
| `hsd_require(root, path, value, error)` | Get required value or error |
| `hsd_validate_range(value, min, max, error)` | Validate numeric range |
| `hsd_validate_one_of(value, options, error)` | Validate against allowed values |
| `schema_validate(schema, root, error)` | Validate against schema |

## HSD Format Reference

### Basic Syntax

```hsd
# Comments start with #
TagName {
  ChildTag = value
  WithUnit [eV/Angstrom] = 0.001
}

# Short form (assigns type to parent)
TagName = TypeName {
  ...
}
```

### Data Types

| Type | Examples |
|------|----------|
| Strings | `name = hello`, `name = "hello world"` |
| Integers | `count = 42` |
| Reals | `value = 3.14`, `value = 1.0e-5` |
| Booleans | `enabled = Yes`, `enabled = No`, `enabled = On/Off` |
| Arrays | `values = 1 2 3 4 5`, `values = 1, 2, 3` |

### Matrix Data (Multi-line)

```hsd
KPoints {
  4 0 0
  0 4 0
  0 0 4
}
```

### File Includes

```hsd
# Include and parse another HSD file
<<+ "other.hsd"

# Include raw text content
Data {
  <<< "datafile.dat"
}
```

Circular includes are automatically detected and reported as errors.

## Error Handling

All parsing and access operations can return detailed errors:

```fortran
type(hsd_error_t), allocatable :: error

call hsd_load("config.hsd", root, error)
if (allocated(error)) then
  call error%print()  ! Prints formatted error with location
  ! Access details: error%code, error%message, error%line_start, error%hint
end if
```

Error codes include: `HSD_STAT_OK`, `HSD_STAT_SYNTAX_ERROR`, `HSD_STAT_TYPE_ERROR`, `HSD_STAT_NOT_FOUND`, `HSD_STAT_FILE_NOT_FOUND`, and more.

## Documentation

- [API Reference](docs/api.rst) — Complete API documentation
- [User Guide](docs/user_guide.rst) — Tutorials and examples
- [HSD Format](docs/hsd_format.rst) — Format specification
- [Error Handling](docs/error_handling.rst) — Error types and handling
- [Thread Safety](docs/thread_safety.rst) — Concurrency guidelines
- [Fuzz Testing](utils/fuzz/README.rst) — AFL++ fuzzing guide

## License

BSD 2-Clause Plus Patent License. See [LICENSE](LICENSE) for details.

## Related Projects

- [hsd-python](https://github.com/dftbplus/hsd-python) — Python implementation
- [DFTB+](https://github.com/dftbplus/dftbplus) — Original user of HSD format
