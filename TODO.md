hsd-fortran:

## Bugs
- Fix `collect_unknown_fields` hardcoded 256-error limit in hsd_schema.f90
- Fix `schema_add_field_enum` silent truncation at 64 chars — use allocatable strings

## Missing features
- Add path normalization (trailing slashes, `//`) in `hsd_get_child`
- Add `hsd_get_array_with_unit` and `hsd_get_matrix_with_unit` (array/matrix overloads for unit conversion)
- Add `hsd_get_children` name-filtered iterator (returns array of children matching a name)

## Missing tests
- Test empty path, trailing slash, double slash in path-based APIs
- Test `hsd_load` when error argument is omitted and file doesn't exist
- Test unclosed quotes and malformed complex numbers
- Test hash table rehash under high load (>100 entries)

## Documentation
- Add `hsd_walk` and `hsd_table_equal` to API docs
- Add `string_buffer_t` usage docs or remove from public exports
- Document `hsd_get_type` returning `VALUE_TYPE_NONE` for table nodes
- Document new APIs: `hsd_get_or_set`, `hsd_set_attrib`, `hsd_rename_child`, `hsd_get_choice`

## Finally
- Remove this file and tackle ../TODO.md