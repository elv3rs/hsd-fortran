hsd-fortran:

## Bugs
- Fix `remove_child_by_name` linear fallback when hash index is inactive (hsd_table_ops.f90)
- Fix `table_add_child` crash when called on uninitialized table (capacity=0)
- Fix `hsd_merge` not clearing stale base fields (string_value, raw_text, attrib) when overwriting
- Fix `scalars_equal` magic numbers — use VALUE_TYPE_* constants instead of 1-6
- Fix `collect_unknown_fields` hardcoded 256-error limit in hsd_schema.f90
- Fix `schema_add_field_enum` silent truncation at 64 chars — use allocatable strings
- Fix `hsd_build_env.f90` nonstandard `getcwd` — add portable fallback for Intel

## Missing features
- Add `hsd_set_string_array` to hsd_mutators.f90 for API symmetry
- Add `hsd_set_matrix` (integer and real) for round-trip support
- Add `hsd_set_attrib` for modifying node attributes via public API
- Add path normalization (trailing slashes, `//`) in `hsd_get_child`
- Rename `hsd_walk_mod` to `hsd_walk` for consistency with other modules

## Missing tests
- Test `remove_child_by_name` when hash index is not active
- Test `table_add_child` on uninitialized table
- Test `hsd_walk` with no callbacks provided
- Test `hsd_merge` attribute propagation and type-mismatch cases
- Test empty path, trailing slash, double slash in path-based APIs
- Test parsing empty HSD files and empty strings
- Test `hsd_load` when error argument is omitted and file doesn't exist
- Test unclosed quotes and malformed complex numbers
- Test `hsd_table_equal` with unnamed/anonymous children
- Test hash table rehash under high load (>100 entries)

## Documentation
- Add `hsd_walk` and `hsd_table_equal` to API docs
- Add `string_buffer_t` usage docs or remove from public exports
- Document `hsd_get_type` returning `VALUE_TYPE_NONE` for table nodes

## Deferred
- Head over to ../hsd-data and tackle ../hsd-data/TODO.md