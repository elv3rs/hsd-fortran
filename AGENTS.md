# AGENTS.md - HSD-Fortran Development Guide

## Main directive
Work through the `TODO.md` step by step. Do not stop before finishing all tasks. Take your time in order to do things properly. Use git -- Whenever an atomic task is complete, remove it from the README and commit it. If a task changed anything important with regard to the project, update `AGENTS.md` correspondingly. 
The project is currently under development / undergoing major refactoring, thus breaking changes are allowed with no further consideration required, as long as the new behaviour is documented accordingly.
If during a task it becomes apparent that some hsd-fortran behaviour is suboptimal, instead of working around it prefer to either fix it on the spot or tack it onto the `TODO.md` to resolve it later on.

## Project Overview

HSD-Fortran is a Human-friendly Structured Data parser for Fortran, designed as a standalone library originating from DFTB+. Licensed under BSD-2-Clause-Patent.

## Quick Reference

```bash
# Build
cmake -B build -DCMAKE_BUILD_TYPE=Debug
cmake --build build

# Lint
pip install fortitude-lint
fortitude check

# Test
ctest --test-dir build --verbose

# Run specific test
./build/test/hsd_testapp "lexer/simple_tokens"

# Documentation
# Using uv for faster installation:
uv venv .venv && source .venv/bin/activate
uv pip install -r docs/requirements.txt

# Build Sphinx user docs
sphinx-build -b html docs public

# Coverage (requires GCC)
cmake -B build -DCMAKE_BUILD_TYPE=Debug -DHSD_COVERAGE=ON
cmake --build build && ctest --test-dir build
lcov --capture --directory build/src --output-file build/coverage.info --ignore-errors inconsistent
genhtml build/coverage.info --output-directory build/coverage_html
```

## Project Layout

```
hsd-fortran/
├── src/                        # Library source
│   ├── hsd.f90                 # Public API (re-exports all modules)
│   ├── hsd_types.f90           # Type aliases and re-exports
│   ├── api/                    # High-level API modules
│   │   ├── hsd_accessors.f90   # hsd_get, hsd_get_or, hsd_get_matrix
│   │   ├── hsd_mutators.f90    # hsd_set
│   │   ├── hsd_query.f90       # hsd_has_child, hsd_is_table, hsd_merge, etc.
│   │   ├── hsd_validation.f90  # hsd_require, hsd_validate_range
│   │   ├── hsd_schema.f90      # Declarative schema validation
│   │   └── hsd_visitor.f90     # Visitor pattern for tree traversal
│   ├── core/                   # Core infrastructure
│   │   ├── hsd_constants.f90   # dp, sp precision constants
│   │   ├── hsd_error.f90       # Error types and status codes
│   │   └── hsd_hash_table.f90  # O(1) child lookup
│   ├── io/                     # Parsing and serialization
│   │   ├── hsd_lexer.f90       # Tokenizer
│   │   ├── hsd_token.f90       # Token types
│   │   ├── hsd_parser.f90      # Parser (includes file handling)
│   │   └── hsd_formatter.f90   # Output serialization
│   └── hsd_types.f90           # Data structures (node, table, value, iterator)
├── test/                       # Test suite
│   ├── testapp.f90             # Test driver (Fortuno)
│   ├── build_env.f90.in        # CMake-configured paths
│   ├── suites/                 # Test suites by category
│   │   ├── api/                # API tests
│   │   ├── core/               # Core module tests
│   │   ├── io/                 # Lexer, parser, formatter tests
│   │   └── coverage/           # Additional coverage tests
│   └── inputs/                 # Test data files
├── example/                    # Usage examples
│   ├── simple_read.f90         # Feature showcase
│   ├── config_demo.f90         # Configuration & Schema validation
│   ├── matrix_demo.f90         # Array and Matrix operations
│   └── sample_input.hsd        # Example HSD file
├── docs/                       # Documentation
│   ├── conf.py                 # Sphinx configuration
│   ├── index.rst               # Main documentation page
│   └── *.rst, *.md             # Documentation files
├── external/fortuno/           # Test framework (git submodule)
└── cmake/                      # CMake config templates
```

## Testing Framework

Uses [Fortuno](https://github.com/fortuno-repos/fortuno) with automatic test discovery via `fortuno_discover_tests()`.

### Test Organization

Tests are organized into categories under `test/suites/`:

| Directory | Contents |
|-----------|----------|
| `api/` | High-level API tests (accessors, schema, validation) |
| `core/` | Array parsing, error paths, edge cases |
| `io/` | Lexer, parser, formatter tests |
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
call hsd_load(trim(filepath), root, error)
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
| `hsd_accessors` | Type-safe value retrieval with path navigation |
| `hsd_mutators` | Value modification (`hsd_set`) |
| `hsd_query` | Tree introspection (type checks, child enumeration) |
| `hsd_validation` | Value validation helpers |
| `hsd_schema` | Declarative schema-based validation |
| `hsd_visitor` | Visitor pattern for tree traversal |

### I/O Layer (`src/io/`)

| Module | Purpose |
|--------|---------|
| `hsd_lexer` | Tokenizes HSD source text |
| `hsd_parser` | Builds tree from tokens, handles includes |
| `hsd_formatter` | Serializes tree back to HSD format |

### Type System (`hsd_types` module)

| Type | Purpose |
|--------|---------|
| `hsd_node` | Abstract base for all nodes |
| `hsd_table` | Container node with children |
| `hsd_value` | Leaf node with scalar/array data |
| `hsd_iterator` | Stateful tree iteration |

## Design Notes

- **Booleans**: Reads `Yes/No`, `On/Off`, `1/0`, optionally `True/False`; writes `Yes/No`
- **Includes**: `<<< "file"` (text), `<<+ "file.hsd"` (parsed); cycle detection enabled
- **Formatting**: Dumps use consistent 2-space indent and `{}` block syntax
- **Hash Table**: O(1) child lookup for all tables using persistent hash indexing
- **Thread Safety**: Read-only access is thread-safe; modifications require external synchronization
- **Status Parameters**: Optional `stat` parameters use `intent(out)` and must be set on ALL code paths (see `docs/error_handling.md`)

## CMake Options

| Option | Default | Description |
|--------|---------|-------------|
| `HSD_ACCEPT_TRUE_FALSE` | `ON` | Accept `True`/`False` as boolean values |
| `HSD_BUILD_TESTS` | `ON` | Build test suite |
| `HSD_BUILD_EXAMPLES` | `ON` | Build examples |
| `HSD_COVERAGE` | `OFF` | Enable code coverage (GCC only) |

## Documentation

The project uses Sphinx for user-facing documentation and guides:

- Configuration: `docs/conf.py`
- Output: `public//index.html`

## Fuzz Testing

The `utils/fuzz/` directory contains AFL++ fuzzing infrastructure:

```bash
# Build fuzz target
cmake -B build-fuzz -S utils/fuzz
cmake --build build-fuzz

# Run AFL++ with QEMU mode (using relative path to corpus)
cd build-fuzz
afl-fuzz -Q -i ../utils/fuzz/corpus -o findings -x hsd.dict -- ./hsd_fuzz_stdin
```

See `utils/fuzz/README.md` for full documentation.

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
| `HSD_STAT_SCHEMA_ERROR` | 20 | Schema validation failed |
