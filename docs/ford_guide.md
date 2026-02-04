# FORD Documentation Guide

This project uses [FORD (FORtran Documenter)](https://github.com/Fortran-FOSS-Programmers/ford) to automatically generate API documentation from source code comments.

## Building Documentation

### Prerequisites

Install FORD via pip:

```bash
pip install ford
```

### Building with CMake

```bash
cmake -B build -DHSD_BUILD_DOCS=ON
cmake --build build --target docs
```

The generated documentation will be in `ford_docs/`.

### Building Manually

You can also run FORD directly:

```bash
ford ford.md
```

## Documentation in Source Code

FORD uses special comment markers to extract documentation from Fortran source files.

### Module Documentation

```fortran
!> Brief description of the module
!!
!! Detailed description can span multiple lines.
!! It supports markdown formatting.
!!
!! @note Important notes can be added
!! @warning Warnings about usage
module my_module
```

### Procedure Documentation

```fortran
!> Brief description of the subroutine
!!
!! Detailed description with more information.
!!
!! @param[in] input_var Description of input parameter
!! @param[out] output_var Description of output parameter
!! @param[in,out] inout_var Description of in-out parameter
subroutine my_routine(input_var, output_var, inout_var)
```

### Type Documentation

```fortran
!> Brief description of the type
type :: my_type
  !> Description of this member
  integer :: member1
  
  !> Description of another member
  real(dp) :: member2
contains
  !> Method description
  procedure :: my_method
end type my_type
```

## Comment Markers

| Marker | Purpose |
|--------|---------|
| `!>` | Documentation comment (brief or detailed) |
| `!!` | Continuation of documentation |
| `!<` | Inline comment for previous line |

## Special Commands

| Command | Purpose |
|---------|---------|
| `@param[in]` | Input parameter |
| `@param[out]` | Output parameter |
| `@param[in,out]` | Input-output parameter |
| `@return` | Return value description |
| `@note` | Important note |
| `@warning` | Warning message |
| `@bug` | Known bug |
| `@todo` | To-do item |

## Markdown Support

FORD supports Markdown in documentation comments:

- **Bold**: `**text**`
- *Italic*: `*text*`
- `Code`: `` `code` ``
- Links: `[text](url)`
- Lists: Use `-` or `1.`
- Code blocks: Use triple backticks

## Configuration

The main FORD configuration is in `ford.md` in the project root. Key settings:

- `src_dir`: Source directory to scan (`./src`)
- `output_dir`: Output directory (`./ford_docs`)
- `exclude_dir`: Directories to exclude (build, test)
- `display`: What to show (`public`, `protected`, `private`)
- `source`: Include syntax-highlighted source (`true`)
- `graph`: Generate call graphs (`true`)

## Integration with Sphinx

The FORD-generated API documentation is automatically integrated with the main Sphinx documentation in CI. It's available at:

https://elv3rs.github.io/hsd-fortran/api/

## References

- [FORD Documentation](https://forddocs.readthedocs.io/)
- [FORD GitHub](https://github.com/Fortran-FOSS-Programmers/ford)
