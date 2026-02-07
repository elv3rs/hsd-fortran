hsd-fortran:
- ensure project compiles and test pass
- Add `hsd_table_equal(a, b)` comparison function
- Export `string_buffer_t` as public
- Add `hsd_walk` (non-visitor recursive iterator)
- fix fpm test not working. Symlinks not allowed. (Might require falling back to relative paths / providing a build_env in src that gets overriden by cmake.) Probably best: mimic cmakes templating using a build script in fpm, e.g. build-script = "Makefile" is supported. Put the script in utils.
- fix all errors shown by fortitude check
- survey project to determine production readiness, add all discovered items to TODO
- Implement new discovered items
- Head over to ../hsd-data and tackle ../hsd-data/TODO.md