# HSD-Fortran TODO

Actionable task list from comprehensive code review. Ordered by priority.

---

## P0 — Bugs & correctness

### 1. `HSD_ACCEPT_TRUE_FALSE` compile flag has no effect
The CMake option `HSD_ACCEPT_TRUE_FALSE` is defined and documented
([CMakeLists.txt](CMakeLists.txt), [README.md](README.md),
[docs/installation.rst](docs/installation.rst)) and the preprocessor
definition is injected in [src/CMakeLists.txt](src/CMakeLists.txt) L60-61.
However, no source file contains an `#ifdef HSD_ACCEPT_TRUE_FALSE` guard;
`True`/`False` are always accepted. Either wire the flag into the boolean
parser in `hsd_types.f90` or drop the dead option and document that
`True`/`False` are always accepted.

### 2. `get_absolute_path` is a stub
[src/io/hsd_parser.f90](src/io/hsd_parser.f90) L729-753 — the function
doesn't actually resolve relative paths to absolute ones; it just prepends
`"./"`. This means include directives (`<<<`, `<<+`) in deeply-nested or
non-CWD parse contexts may fail silently. Replace with a `getcwd()`-based
resolution or use `realpath()` via C interop.

### 3. `schema_validate_strict` is a documented stub
[src/api/hsd_schema.f90](src/api/hsd_schema.f90) L320-329 — delegates to
`schema_validate` without checking for unknown fields. Implement the
documented behaviour: walk the tree's children and report any that are
**not** declared in the schema. The `allow_unknown` field on `hsd_schema_t`
already exists but is never read.

### 4. `hsd_max_line_length` is imported but never used
The constant is `use`-imported in `hsd_lexer.f90`, `hsd_parser.f90`, and
`hsd_error.f90` but no code references it. Either remove the imports or
enforce line-length limits in the lexer/parser.

---

## P1 — Code quality & maintainability

### 5. Remove 17 duplicate test files in `test/`
Every suite under `test/suites/{api,core,coverage,io}/` has an **identical**
copy directly in `test/` (e.g. `test/test_api_suite.f90` ≡
`test/suites/api/test_api_suite.f90`). CMake only compiles the `suites/`
versions. Delete all flat copies and `test/count_visitor_mod.f90`.

### 9. O(n²) string concatenation in array setters
`hsd_set_integer_array`, `hsd_set_real_dp_array`, `hsd_set_logical_array`,
and `hsd_set_complex_dp_array` in [src/api/hsd_mutators.f90](src/api/hsd_mutators.f90)
build output strings with `text = text // " " // …` in a loop. For large
arrays this is O(n²). Use `string_buffer_t` (already available from
`hsd_utils`) instead.

### 10. Formatter string-output path lacks `= ChildTag {` shorthand
`write_table_to_file` in [src/io/hsd_formatter.f90](src/io/hsd_formatter.f90)
detects single-child tables and emits the compact `Tag = ChildTag { … }`
syntax, but `write_table_to_string_buf` does not. The two paths should
produce identical output.

### 11. Create missing `utils/filter_coverage.py`
`AGENTS.md` documents a `utils/filter_coverage.py` script for stripping
`LCOV_EXCL_*` marked lines from coverage data. The script does not exist.
Either create it or remove the documentation reference.

---

## P2 — Robustness & safety

### 12. `hsd_set` silently ignores type mismatch in `select type`
All setters in `hsd_mutators.f90` call `get_or_create_child` then use
`select type (child) type is (hsd_value)`. If the existing child is an
`hsd_table` (type mismatch), the `select type` simply falls through and
reports `stat = HSD_STAT_OK` without modifying anything. Set
`stat = HSD_STAT_TYPE_ERROR` on the fall-through path.

### 13. Thread-safety documentation vs. reality
`AGENTS.md` claims "read-only access is thread-safe" but `hsd_value`'s
`get_*` methods mutate the cache fields (`int_value`, `real_value`,
`value_type`, etc.) on first read. Either document this accurately or
resolve by removing the cache (see P3 item below).

### 14. `hsd_load_string` doesn't initialize root on error
When `hsd_load_string` / `hsd_load` fail, the `root` table may be in an
indeterminate state. Callers that check `allocated(error)` and skip
`root%destroy()` may leak. Ensure `root` is always initialized via
`new_table(root)` at the top of the routine.

---

## P3 — Design improvements

### 15. Consider removing `hsd_value` caching
`hsd_value` caches parsed arrays/matrices (`int_array`, `real_array`,
`logical_array`, etc.) alongside `raw_text`. This doubles memory and
requires cache invalidation after `set_raw`. For typical HSD file sizes,
parsing on every `get_*` call would be fast and eliminate the entire
category of cache-coherence bugs.

### 16. Hash table case-insensitive lookup probes two buckets
`lookup_case_insensitive` in [src/core/hsd_hash_table.f90](src/core/hsd_hash_table.f90)
first probes the lowered-key hash bucket, then falls back to the
original-case hash bucket. Storing the lowered key at insertion time would
eliminate the double probe.

### 17. `MAX_ENUM_VALUES = 32` is hardcoded in schema
[src/api/hsd_schema.f90](src/api/hsd_schema.f90) — the `hsd_field_def_t`
type uses a fixed `character(len=64) :: enum_values(MAX_ENUM_VALUES)` array.
Replace with `allocatable` to remove the limit.

### 18. Make user-guide examples compilable
Extract code blocks from `docs/user_guide.rst` into standalone `.f90` files
under `example/` and use Sphinx `literalinclude` directives. This prevents
doc/code drift.

---

## P4 — Nice-to-haves

### 19. Token constants `TOKEN_WHITESPACE` and `TOKEN_INVALID` asymmetry
`TOKEN_WHITESPACE` is defined and checked in the parser, but the lexer
never emits it — it silently skips whitespace. `TOKEN_INVALID` is
defined but never emitted either. Document the intended usage or remove.

### 20. `hsd_require` optional `expected_type` overloads context string param
`hsd_require` has an overloaded positional interface where the 4th argument
is either `expected_type` (integer) or `context` (character). This is
error-prone. Consider keyword-only arguments or split into two routines.

### 21. `build_env.f90` exists both as `.in` template and flat file
[test/build_env.f90](test/build_env.f90) is a standalone file with runtime
`getcwd()` for fpm, while [test/build_env.f90.in](test/build_env.f90.in) is
the CMake template. Only the CMake path uses `.in`. This is correct but
confusing — add a comment to `build_env.f90` clarifying it's the fpm
fallback.

### 22. Improve error messages for matrix dimension mismatches
`hsd_get_matrix` in `hsd_accessors.f90` silently returns empty results when
child rows have inconsistent lengths. Emit a warning or error with row
details.

### 23. Add `hsd_set` for complex scalar
`hsd_set` supports complex arrays but not complex scalars. Add a
`hsd_set_complex_dp` overload for consistency with `hsd_get`.

### 24. Example `matrix_demo.f90` references missing input file
`matrix_demo.f90` loads `matrix_input.hsd` which isn't provided. Either
add the file under `example/` or have the demo create the data inline.

