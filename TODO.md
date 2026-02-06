hsd-fortran:
- remove fortran c interop.
- Add `leave_table` callback to `hsd_visitor_t`
- Add `hsd_table_equal(a, b)` comparison function
- Export `string_buffer_t` as public
- Add `hsd_walk` (non-visitor recursive iterator)
- fix fpm test not working. Symlinks not allowed.
 