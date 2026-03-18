# hsd-fortran

[![License](https://img.shields.io/badge/license-BSD--2--Clause--Patent-blue.svg)](LICENSE)
[![Fortitude](https://img.shields.io/endpoint?url=https://raw.githubusercontent.com/PlasmaFAIR/fortitude/main/docs/assets/badge/v0.json)](https://github.com/PlasmaFAIR/fortitude)
[![Tests](https://github.com/elv3rs/hsd-fortran/actions/workflows/tests.yml/badge.svg)](https://github.com/elv3rs/hsd-fortran/actions/workflows/tests.yml)
[![Linting](https://github.com/elv3rs/hsd-fortran/actions/workflows/lint.yml/badge.svg)](https://github.com/elv3rs/hsd-fortran/actions/workflows/lint.yml)
[![Docs & Coverage](https://github.com/elv3rs/hsd-fortran/actions/workflows/docs.yml/badge.svg)](https://github.com/elv3rs/hsd-fortran/actions/workflows/docs.yml)
[![Coverage](https://elv3rs.github.io/hsd-fortran/coverage/coverage.svg)](https://elv3rs.github.io/hsd-fortran/coverage/index.html)

> **⚠️ Agentic Coding Proof of Concept**
>
> This repository was developed as a proof-of-concept for AI-assisted
> (agentic) coding workflows. All code and documentation are **pending
> human review** and should not be used in production without thorough
> independent verification.

A lightweight, dependency-free HSD (Human-friendly Structured Data) parser for Fortran.

**📖 [Full Documentation](https://elv3rs.github.io/hsd-fortran/)** · **[Coverage Report](https://elv3rs.github.io/hsd-fortran/coverage/)**

## Overview

HSD is a human-friendly data format similar to JSON/YAML, originally developed as the input format for [DFTB+](https://github.com/dftbplus/dftbplus). This library parses HSD into a tree, provides path-based accessors, and serializes back to HSD.

## Quick Example

```fortran
use hsd
type(hsd_node) :: root
type(hsd_error_t), allocatable :: error
integer :: max_steps

call hsd_load_file("input.hsd", root, error)
if (allocated(error)) stop 1

call hsd_get(root, "Driver/MaxSteps", max_steps)
call hsd_set(root, "Driver/MaxSteps", 200)
call hsd_dump(root, "output.hsd")
```

```hsd
Driver = ConjugateGradient {
  MaxSteps = 100
  SCC = Yes
  Temperature [Kelvin] = 300.0
}
```

## Installation

Requires a Fortran 2008 compiler (gfortran ≥ 7, ifort ≥ 18, ifx) and CMake ≥ 3.14.

```bash
cmake -B build && cmake --build build
ctest --test-dir build
cmake --install build --prefix /path/to/install
```

Also available via [fpm](https://fpm.fortran-lang.org/):

```bash
fpm build && fpm test
```

## Documentation

Full docs at **[https://elv3rs.github.io/hsd-fortran](https://elv3rs.github.io/hsd-fortran)** — API reference, format spec, error handling, and examples.

```bash
pip install -r docs/requirements.txt
sphinx-build -b html docs public
```

## License

BSD 2-Clause Plus Patent License. See [LICENSE](LICENSE) for details.

## Related Projects

- [hsd-python](https://github.com/dftbplus/hsd-python) — Python implementation
- [DFTB+](https://github.com/dftbplus/dftbplus) — Original user of HSD format
