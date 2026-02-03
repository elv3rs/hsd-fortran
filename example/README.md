# HSD Library Examples

This directory contains example programs demonstrating the HSD library features.

## simple_read.f90

A comprehensive feature showcase that demonstrates:

### 1. File I/O
- Loading HSD from file with `hsd_load()`
- Writing modified data with `hsd_dump()`
- Error handling

### 2. Navigation & Introspection
- Path-based access to nested values
- Checking node types with `hsd_is_table()`, `hsd_is_value()`
- Counting children with `hsd_child_count()`
- Getting all keys with `hsd_get_keys()`

### 3. Data Access
- Type-safe getters with `hsd_get()` for:
  - Integers, reals (single/double precision)
  - Logicals, complex numbers
  - Strings
  - Arrays of all types
  - 2D matrices
- Default values with `hsd_get_or()`
- Attribute extraction (e.g., unit labels)

### 4. Data Modification
- Setting values with `hsd_set()`
- Creating nested paths automatically

### 5. Tree Operations
- Deep cloning with `hsd_clone()`
- Merging configurations with `hsd_merge()`

### 6. Validation
- Required field checking with `hsd_require()`
- Range validation with `hsd_validate_range()`
- Enum validation with `hsd_validate_one_of()`

## Running the Example

```bash
# Build
cmake -B build
cmake --build build

# Run from example directory
cd example
../build/example/simple_read

# This reads sample_input.hsd and creates modified_output.hsd
```

## Input Files

- **sample_input.hsd**: Example DFTB+ style configuration demonstrating HSD syntax
- **modified_output.hsd**: Generated output showing modifications and additions

## Expected Output

The program produces detailed output showing:
- Successful parsing
- Structure exploration (6 sections)
- Value extraction with proper types
- Default value handling
- Unit attribute extraction
- Array/matrix handling
- Validation results
- Tree manipulation success

All operations complete successfully, demonstrating zero-error parsing and manipulation.
