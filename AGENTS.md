# AGENTS.md - HSD-Fortran Development Guide

## Main directive

The project has reached **v1.0.0 release** status.  All specification phases
(parser, accessors, mutators, unit-aware accessors, documentation) are complete.
Refactoring for simplicity is also complete (unified API, string-only storage,
shared accessor/mutator helper paths, unified child-pointer wrapper model).
The primary goal is now to serve as a drop-in replacement for DFTB+'s HSD stack,
with behavior aligned to DFTB+ ground truth (including lowercase canonical tag handling).

When making changes: ensure builds pass, `fortitude check` is clean, all tests
pass, and Sphinx docs build without warnings.  Commit atomically with clear
messages.

## Project Overview

HSD-Fortran is a Human-friendly Structured Data parser for Fortran, designed as a standalone library originating from DFTB+. Licensed under BSD-2-Clause-Patent. It is the core tree engine used by [hsd-data](../hsd-data/) and [DFTB+](../dftbplus/).

## Quick Reference

> **Intel ifx support:** The project is tested with both `gfortran` and Intel `ifx`.
> Use `FC=ifx cmake ...` to build with ifx. All 504 tests pass with both compilers.

```bash
# Build (gfortran, default)
cmake -B build -DCMAKE_BUILD_TYPE=Debug
cmake --build build 2>&1 | tail -5

# Build (Intel ifx)
FC=ifx cmake -B build -DCMAKE_BUILD_TYPE=Debug
cmake --build build 2>&1 | tail -5

# Lint (required before committing — must pass with zero warnings)
pip install fortitude-lint   # one-time setup
fortitude check

# Test
ctest --test-dir build 2>&1 | tail -5

# Run specific test
./build/test/hsd_testapp "lexer/simple_tokens"

# Documentation
uv venv .venv && source .venv/bin/activate
uv pip install -r docs/requirements.txt
sphinx-build -b html docs public 2>&1 | tail -5

# Coverage (requires GCC)
cmake -B build -DCMAKE_BUILD_TYPE=Debug -DHSD_COVERAGE=ON
cmake --build build && ctest --test-dir build 2>&1 | tail -5
lcov --capture --directory build/src --output-file build/coverage.info --ignore-errors inconsistent 2>&1 | tail -3
lcov --summary build/coverage.info 2>&1 | grep "lines"
```

> **Token-saving tip:** Pipe commands through `tail` to show only the summary.

## Code Quality

All source code **must** pass `fortitude check` with zero warnings before being committed (enforced by CI).
[Fortitude](https://github.com/PlasmaFAIR/fortitude) is a Fortran linter that enforces
consistent style and catches common mistakes.

```bash
# Check all source files and output errors, whilst fixing simple stuff like indentation
fortitude check --fix
```

Fortitude is configured via `fpm.toml`.


## Project Layout

```
hsd-fortran/
├── src/                        # Library source
│   ├── hsd.f90                 # Public API (re-exports all modules)
│   ├── hsd_types.f90           # Data structures (unified hsd_node, iterator)
│   ├── hsd_table_ops.f90      # Submodule: table & iterator operations
│   ├── hsd_value_ops.f90      # Submodule: value ops & parse helpers
│   ├── api/                    # High-level API modules
│   │   ├── hsd_api.f90         # Unified API (accessors, mutators, query)
│   │   └── hsd_validation.f90  # hsd_require, hsd_validate_range
│   ├── compat/                 # DFTB+ compatibility layer
│   │   └── hsd_compat.f90      # Legacy API wrappers
│   ├── core/                   # Core infrastructure
│   │   ├── hsd_constants.f90   # dp precision constant (iso_fortran_env)
│   │   ├── hsd_error.f90       # Error types, status codes
│   │   ├── hsd_utils.f90       # String utilities (to_lower, string_buffer_t)
│   │   └── hsd_build_env.f90   # CMake build environment (generated)
│   └── io/                     # Parsing and serialization
│       ├── hsd_lexer.f90       # Tokenizer
│       ├── hsd_token.f90       # Token types
│       ├── hsd_parser.f90      # Parser (includes file handling)
│       └── hsd_formatter.f90   # Output serialization
├── test/                       # Test suite
│   ├── testapp.f90             # Test driver (Fortuno)
│   ├── build_env.f90.in        # CMake-configured paths
│   ├── suites/                 # Test suites by category
│   │   ├── api/                # API tests
│   │   ├── core/               # Core module tests
│   │   ├── io/                 # Lexer, parser, formatter tests
│   │   ├── compat/             # Compatibility layer tests
│   │   └── coverage/           # Additional coverage tests
│   └── inputs/                 # Test data files
├── example/                    # Usage examples
│   ├── simple_read.f90         # Feature showcase
│   ├── matrix_demo.f90         # Array and Matrix operations
│   └── sample_input.hsd        # Example HSD file
├── docs/                       # Documentation
│   ├── conf.py                 # Sphinx configuration
│   ├── index.rst               # Main documentation page
│   └── *.rst, *.md             # Documentation files
└── cmake/                      # CMake config templates
```

## Testing Framework

Uses [Fortuno](https://github.com/fortuno-repos/fortuno) with automatic test discovery via `fortuno_discover_tests()`.

### Test Organization

Tests are organized into categories under `test/suites/`:

| Directory | Contents |
|-----------|----------|
| `api/` | High-level API tests (accessors, validation) |
| `core/` | Array parsing, error paths, edge cases |
| `io/` | Lexer, parser, formatter tests |
| `compat/` | DFTB+ compatibility layer tests |
| `coverage/` | Additional tests for code coverage |

### Writing Tests

```fortran
module test_example_suite
  use hsd
  use fortuno_serial, only : is_equal, test => serial_case_item, &
      & check => serial_check, suite => serial_suite_item, test_list
  implicit none
  private
  public :: tests

contains

  function tests()
    type(test_list) :: tests
    tests = test_list([&
        suite("example", test_list([&
            test("my_test", test_my_test)&
        ]))&
    ])
  end function tests

  subroutine test_my_test()
    call check(1 == 1, msg="One equals one")
  end subroutine test_my_test

end module test_example_suite
```

### File I/O in Tests

Use the `build_env` module for reliable absolute paths:

```fortran
use build_env, only : source_dir, build_dir

character(len=512) :: filepath
filepath = source_dir // "/test/inputs/mydata.hsd"
call hsd_load_file(trim(filepath), root, error)
```

This ensures tests work regardless of where CTest runs from.

### Test Caveats

1. **`is_equal()` limitation**: Does not support allocatable strings. Use `==` instead:
   ```fortran
   call check(token%value == "expected", msg="...")  ! Correct
   ```

2. **Check accumulation**: Multiple `check()` calls all execute even if earlier ones fail.

3. **Rebuild for discovery**: New tests require a rebuild for CTest to discover them.

## Adding New Tests

1. Add test subroutine to appropriate `test_*_suite.f90` in `test/suites/`
2. Register in the suite's `tests()` function
3. Rebuild: `cmake --build build`
4. Run: `ctest --test-dir build`

## Module Architecture

### API Layer (`src/api/`)

| Module | Purpose |
|--------|---------|
| `hsd_api` | Unified API (accessors, mutators, query) |
| `hsd_validation` | Value validation helpers |

### Compatibility Layer (`src/compat/`)

| Module | Purpose |
|--------|---------|
| `hsd_compat` | DFTB+ legacy API wrappers |

### Generic Tree Utilities (in `hsd_api`)

These functions were upstreamed from DFTB+ and are part of the public API:

| Function | Purpose |
|----------|---------|
| `hsd_get_name` | Returns the tag name of a node (table or value) |
| `hsd_has_value_children` | Checks whether a table contains any value children (`node_type == NODE_TYPE_VALUE`) (replaces DFTB+'s `hasInlineData`) |
| `hsd_get_inline_text` | Retrieves the concatenated text content from value children of a table (replaces DFTB+'s `getFirstTextChild`) |

### I/O Layer (`src/io/`)

| Module | Purpose |
|--------|---------|
| `hsd_lexer` | Tokenizes HSD source text |
| `hsd_parser` | Builds tree from tokens, handles includes |
| `hsd_formatter` | Serializes tree back to HSD format |

### Type System (`hsd_types` module)

| Type | Purpose |
|--------|---------|
| `hsd_node` | Unified concrete node type with a `node_type` discriminator (`NODE_TYPE_TABLE` or `NODE_TYPE_VALUE`). Tables hold children; values hold string data (parsed on demand). |
| `hsd_iterator` | Stateful tree iteration (references a `type(hsd_node)` table) |
| `hsd_node_ptr` | Wrapper holding `type(hsd_node), pointer` for child storage |
| `hsd_table_ptr` | Wrapper holding `type(hsd_node), pointer` (for `hsd_get_child_tables`) |
| `hsd_child_ptr` | Wrapper holding `type(hsd_node), pointer` (for `hsd_get_children`) |

## Design Notes

- **Booleans**: Reads `Yes/No`, `On/Off`, `1/0`, optionally `True/False`; writes `Yes/No`
- **Includes**: `<<< "file"` (text), `<<+ "file.hsd"` (parsed); cycle detection enabled
- **Formatting**: Dumps use consistent 2-space indent and `{}` block syntax
- **Child Lookup**: Linear search over children array (simple and sufficient for config files)
- **Thread Safety**: **Thread-safe for read-only access**. Value nodes no longer use internal caching, so getters do not mutate the tree. Modification requires external synchronization.
- **Duplicate Keys**: Are preserved in the tree; `hsd_get` returns the **last** occurrence (override behavior); iteration sees all
- **Status Parameters**: Optional `stat` parameters use `intent(out)` and must be set on ALL code paths (see `docs/error_handling.md`)

## CMake Options

| Option | Default | Description |
|--------|---------|-------------|
| `HSD_BUILD_TESTS` | `ON` | Build test suite |
| `HSD_BUILD_EXAMPLES` | `ON` | Build examples |
| `HSD_COVERAGE` | `OFF` | Enable code coverage (GCC only) |
| `HSD_SANITIZERS` | `OFF` | Enable sanitizers for leak/memory detection |

## Documentation

The project uses Sphinx for user-facing documentation and guides:

- Configuration: `docs/conf.py`
- Output: `public/index.html`

## Error Codes

| Constant | Value | Meaning |
|----------|-------|---------|
| `HSD_STAT_OK` | 0 | Success |
| `HSD_STAT_SYNTAX_ERROR` | 1 | Generic syntax error |
| `HSD_STAT_UNCLOSED_TAG` | 2 | Block not closed |
| `HSD_STAT_UNCLOSED_ATTRIB` | 3 | Attribute bracket not closed |
| `HSD_STAT_UNCLOSED_QUOTE` | 4 | String quote not closed |
| `HSD_STAT_ORPHAN_TEXT` | 5 | Text outside any block |
| `HSD_STAT_INCLUDE_CYCLE` | 6 | Circular include detected |
| `HSD_STAT_INCLUDE_DEPTH` | 7 | Too many nested includes |
| `HSD_STAT_FILE_NOT_FOUND` | 8 | File doesn't exist |
| `HSD_STAT_IO_ERROR` | 9 | I/O operation failed |
| `HSD_STAT_TYPE_ERROR` | 10 | Type conversion failed |
| `HSD_STAT_NOT_FOUND` | 11 | Key not found in tree |
