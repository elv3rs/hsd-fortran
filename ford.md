---
project: HSD-Fortran
summary: Human-friendly Structured Data parser for Fortran
author: DFTB+ developers group
github: https://github.com/elv3rs/hsd-fortran
project_github: https://github.com/elv3rs/hsd-fortran
license: bsd
version: 0.1.0
year: 2026
src_dir: ./src
output_dir: ./ford_docs
exclude_dir: ./build
             ./external
             ./test
             ./.git
extensions: f90
            F90
docmark: !
predocmark: >
display: public
         protected
source: true
graph: true
search: true
macro: FORD_VERSION 0.1.0
extra_mods: iso_fortran_env:https://gcc.gnu.org/onlinedocs/gfortran/ISO_005fFORTRAN_005fENV.html
md_extensions: markdown.extensions.toc
               markdown.extensions.smarty

---

# HSD-Fortran Documentation

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

## Quick Start

```fortran
program example
  use hsd
  implicit none
  
  type(hsd_table) :: root
  type(hsd_error_t), allocatable :: error
  integer :: max_steps, stat
  
  ! Load HSD file
  call hsd_load("input.hsd", root, error)
  if (allocated(error)) then
    call error%print()
    stop 1
  end if
  
  ! Access values with path navigation
  call hsd_get(root, "Driver/MaxSteps", max_steps, stat)
  
  ! Use defaults for optional values
  call hsd_get_or(root, "Driver/Timeout", max_steps, default=3600, stat=stat)
end program example
```

## Architecture

### Module Organization

The library is organized into several layers:

- **Public API** (`hsd` module): Single entry point that re-exports all public interfaces
- **High-level API** (`src/api/`): User-facing operations (accessors, mutators, validation, schema)
- **Core infrastructure** (`src/core/`): Constants, error handling, hash tables
- **I/O layer** (`src/io/`): Lexer, parser, formatter
- **Data structures** (`hsd_types` module): Node types, tables, values, iterators

### Key Types

| Type | Purpose |
|------|---------|
| `hsd_node` | Abstract base for all nodes |
| `hsd_table` | Container node with children |
| `hsd_value` | Leaf node with scalar/array data |
| `hsd_error_t` | Error information with context |
| `hsd_iterator` | Stateful tree iteration |

## Building

### CMake

```bash
cmake -B build -DCMAKE_BUILD_TYPE=Release
cmake --build build
cmake --install build
```

### FPM

```bash
fpm build
fpm test
```

## Testing

The library uses [Fortuno](https://github.com/fortuno-repos/fortuno) for testing:

```bash
cmake -B build -DHSD_BUILD_TESTS=ON
cmake --build build
ctest --test-dir build --verbose
```

## License

Licensed under BSD-2-Clause-Patent. See [LICENSE](LICENSE) for details.
