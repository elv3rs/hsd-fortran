# HSD-Fortran TODO

All items from the previous TODO have been completed. Below are potential
future simplifications and improvements identified during the cleanup.

## Future Simplifications

### Split `hsd_types.f90` (~1744 lines → ~3 modules)

The largest file by far. It combines type definitions, table operations,
value getters/setters, and parsing helpers. A clean split:

| New module | Contents | Est. lines |
|------------|----------|-----------|
| `hsd_types.f90` | Type definitions, constructors, destructors | ~300 |
| `hsd_table_ops.f90` | `table_add_child`, `table_get_child*`, `table_remove_child*`, iterator | ~350 |
| `hsd_value_ops.f90` | `value_set_*`, `value_get_*`, `parse_*_array`, `parse_*_matrix`, tokenizers | ~1000 |

This would make each module focused and easier to maintain.

### Deduplicate array/matrix parse routines

`parse_int_array` and `parse_real_array` are near-identical (~30 lines each),
differing only in the `read` format. Same for `parse_int_matrix` /
`parse_real_matrix`. A generic approach or internal procedure could halve this.

### Implement `schema_validate_strict`

Currently documented as a stub that delegates to `schema_validate` without
checking for unknown fields. A proper implementation would iterate table
children and flag any not declared in the schema.

### Consider removing `hsd_value` caching

The `hsd_value` type caches parsed arrays/matrices (`int_array`, `real_array`,
`logical_array`, etc.) alongside the raw text. This doubles memory for every
value node and requires careful invalidation. An alternative: parse on every
access (fast for typical HSD file sizes) and remove the cache fields entirely.

### Make user guide examples compilable

Extract code blocks from `docs/user_guide.rst` into standalone `.f90` files
under `example/` and use Sphinx `literalinclude` directives. This prevents
doc/code drift.
