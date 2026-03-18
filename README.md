# hsd-fortran


> **⚠️ Agentic Coding Proof of Concept**
>
> This repository was developed as a proof-of-concept for AI-assisted
> (agentic) coding workflows. All code and documentation are **pending
> human review** and should not be used in production without thorough
> independent verification.

A lightweight, dependency-free HSD (Human-friendly Structured Data) parser for Fortran.

**[Documentation](https://elv3rs.github.io/hsd-fortran/)** · **[Coverage Report](https://elv3rs.github.io/hsd-fortran/coverage/)**

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

Requires a Fortran 2008 compiler.

```bash
cmake -B build && cmake --build build
ctest --test-dir build
```

Also available via [fpm](https://fpm.fortran-lang.org/):

```bash
fpm build && fpm test
```


## Related Projects

- [hsd-python](https://github.com/dftbplus/hsd-python) — Python implementation
- [DFTB+](https://github.com/dftbplus/dftbplus) — Original user of HSD format
