hsd-fortran:

## Bugs ✅ (all fixed)
- ~~Fix `collect_unknown_fields` hardcoded 256-error limit in hsd_schema.f90~~ → dynamic growable array
- ~~Fix `schema_add_field_enum` silent truncation at 64 chars~~ → `alloc_string_t` wrapper

## Missing features ✅ (all implemented)
- ~~Add path normalization (trailing slashes, `//`) in `hsd_get_child`~~ → `normalize_path()` in hsd_query
- ~~Add `hsd_get_array_with_unit` and `hsd_get_matrix_with_unit`~~ → in hsd_validation
- ~~Add `hsd_get_children` name-filtered iterator~~ → returns `hsd_child_ptr` array

## Missing tests
- Test empty path, trailing slash, double slash in path-based APIs
- Test `hsd_load` when error argument is omitted and file doesn't exist
- Test unclosed quotes and malformed complex numbers
- Test hash table rehash under high load (>100 entries)
- Test `hsd_get_children` with multiple matching children
- Test `hsd_get_array_with_unit` and `hsd_get_matrix_with_unit`

## Documentation
- Add `hsd_walk` and `hsd_table_equal` to API docs
- Add `string_buffer_t` usage docs or remove from public exports
- Document `hsd_get_type` returning `VALUE_TYPE_NONE` for table nodes
- Document new APIs: `hsd_get_or_set`, `hsd_set_attrib`, `hsd_rename_child`, `hsd_get_choice`
- Document new APIs: `hsd_get_children`, `hsd_child_ptr`, `hsd_get_array_with_unit`, `hsd_get_matrix_with_unit`

## Finally
- Remove this file and tackle ../TODO.md
