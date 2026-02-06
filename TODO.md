# HSD-Fortran TODO

## Critical — Correctness Bugs

- [ ] **Fix `hash_string` integer overflow UB**: `abs(hash)` is undefined when
  `hash == -huge(1)-1`. Replace `hash = abs(hash)` with
  `hash = iand(hash, huge(hash))` in `src/core/hsd_hash_table.f90:70`.

- [ ] **Fix `tokenize_string` / `split_by_newlines` stack overflow on large inputs**:
  `character(len=len(text)) :: temp_tokens(len(text))` allocates O(n²) bytes on
  the stack. A 10 KB input uses ~100 MB; a 100 KB input would use ~10 GB.
  Rewrite with a two-pass approach or dynamic allocation in `src/hsd_types.f90`.

- [ ] **Fix `table_get_child_by_name` silent failure when index not built**:
  If `index_active` is false but children exist, the lookup silently returns
  `null()` instead of falling back to a linear scan. Add a linear fallback in
  `src/hsd_types.f90:403`.

- [ ] **Fix `hsd_merge` not copying all cached array/matrix fields**: Only
  `int_array` and `real_array` are copied. Missing: `logical_array`,
  `string_array`, `complex_array`, `int_matrix`, `real_matrix` in
  `src/api/hsd_query.f90:375-450`.

- [ ] **Fix `hsd_get_or` masking type errors as NOT_FOUND**: All `*_default`
  procedures set `stat = HSD_STAT_NOT_FOUND` regardless of whether the actual
  error was a type conversion failure. Should propagate `local_stat` instead in
  `src/api/hsd_accessors.f90`.

## High — API & Documentation Accuracy

- [ ] **Fix API docs: `hsd_dump` signature**: Docs show `stat` (integer) but
  actual takes `error` (hsd_error_t). Fix in `docs/api.rst` and the README
  overview table.

- [ ] **Fix API docs: `hsd_require` signature**: Docs show it retrieves a value,
  but actual only checks existence. Fix in `docs/api.rst` and
  `docs/user_guide.rst`.

- [ ] **Fix API docs: `hsd_validate_range` signature**: Docs show scalar value
  input, but actual takes `(table, path, min_val, max_val, error, context)`.
  Fix in `docs/api.rst` and `docs/user_guide.rst`.

- [ ] **Fix API docs: `hsd_validate_one_of` signature**: Docs show scalar input,
  but actual takes `(table, path, choices, error, context)`. Fix in
  `docs/api.rst` and `docs/user_guide.rst`.

- [ ] **Fix API docs: `hsd_get_with_unit` signature**: Docs omit the required
  `converter` procedure argument. Fix in `docs/api.rst`.

- [ ] **Fix API docs: `schema_add_field` signature**: Docs show wrong argument
  order and missing optional range/description parameters. Fix in
  `docs/api.rst`.

- [ ] **Fix API docs: `schema_validate` return type**: Docs show single `error`
  but actual returns `errors(:)` array. Fix in `docs/api.rst`.

- [ ] **Implement `schema_validate_strict` or document as stub**: Currently just
  calls `schema_validate` without checking for unknown fields. Either implement
  the advertised behavior or clearly mark as unimplemented in docs and code.

- [ ] **Fix version inconsistency**: `fpm.toml` says `1.0.0`, `CMakeLists.txt`
  says `0.1.0`. Decide on canonical version and synchronize.

- [ ] **Fix user guide code snippets**: Several examples in `docs/user_guide.rst`
  use wrong signatures that won't compile. Update to match actual API.

## Medium — Maintainability & Performance

- [ ] **Fix `name_index_lookup_ci` O(n) linear scan**: Case-insensitive lookup
  scans all buckets linearly, defeating the hash table's O(1) purpose. Should
  hash the lowered key and look up directly in `src/core/hsd_hash_table.f90`.

- [ ] **Fix `escape_quotes` O(n²) string concatenation**: Use `string_buffer_t`
  (already exists in the project) instead of repeated `escaped = escaped // ...`
  in `src/io/hsd_formatter.f90:336-351`.

- [ ] **Fix `write_table_to_string` O(n²) string concatenation**: Use
  `string_buffer_t` instead of `output = output // line` in
  `src/io/hsd_formatter.f90:354-403`.

- [ ] **Deduplicate path navigation**: `get_child_by_path` in
  `src/api/hsd_accessors.f90` and `get_first_child_table` in
  `src/api/hsd_query.f90` are near-identical implementations. Consolidate into
  a single shared helper.

- [ ] **Fix `set_raw` value type**: `value_set_raw` sets `VALUE_TYPE_STRING`
  instead of preserving or detecting the actual type (e.g., `VALUE_TYPE_ARRAY`
  for arrays set via `hsd_set`). This causes `hsd_is_array` to return false for
  programmatically set arrays. In `src/hsd_types.f90:682-688`.

- [ ] **Move `src/build_env.f90` to test directory**: This test support module
  lives in the library source and gets compiled into the library even for
  non-test builds. Should live in `test/`.

- [ ] **Remove unused `TOKEN_NAME` import from lexer**: `src/io/hsd_lexer.f90:11`
  imports `TOKEN_NAME` which resolves to the `token_name` function but is never
  used in the module.

- [ ] **Reduce `new_table` initial allocation**: Always allocates 8 child slots
  even for tiny tables. Consider starting at 2 or 4 for better memory usage in
  deep trees.

## Low — Polish & Documentation Completeness

- [ ] **Fix GitHub URL inconsistency**: README badges use `elv3rs/hsd-fortran`
  while docs reference `dftbplus/hsd-fortran`. Pick one and update everywhere.

- [ ] **Fix README fuzz link**: Points to `utils/fuzz/README.rst` but file is
  `utils/fuzz/README.md`.

- [ ] **Add `HSD_STAT_SCHEMA_ERROR` to error handling docs**: Missing from the
  error codes table in `docs/error_handling.rst`.

- [ ] **Remove or implement `setup_fpm_tests.sh`**: Referenced in
  `docs/build_systems.rst` but doesn't exist.

- [ ] **Document `HSD_BUILD_BENCHMARKS` CMake option**: Defined in CMakeLists.txt
  but not documented anywhere.

- [ ] **Remove unused Sphinx extensions**: `sphinx.ext.autodoc`,
  `sphinx.ext.viewcode`, and Python intersphinx are irrelevant for a Fortran
  project in `docs/conf.py`.

- [ ] **Clean up dead types**: `enum_token_kind` (hsd_token), `hsd_stat`
  (hsd_error) are defined and exported but barely/never used externally.

- [ ] **Add `format_value` support for `VALUE_TYPE_COMPLEX`**: Complex values
  fall through to the default case and may format as empty if set
  programmatically.

- [ ] **Add `config_demo` and `matrix_demo` to `fpm.toml` examples**: Only
  `simple_read` is registered; the other two cannot be run via `fpm run`.

- [ ] **Fix source comment**: `src/hsd_types.f90` references
  `docs/thread_safety.md` but the file is `docs/thread_safety.rst`.
