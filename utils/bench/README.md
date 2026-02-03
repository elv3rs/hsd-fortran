# HSD-Fortran Performance Benchmarks

This directory contains automated benchmarks for evaluating the performance characteristics of the HSD-Fortran library.

## Benchmarks Overview

| Benchmark | Purpose | Key Findings |
|-----------|---------|--------------|
| `bench_lookup` | Hash table performance vs number of children | $O(1)$ lookup time regardless of child count. |
| `bench_copy` | Tree cloning (Deep Copy) overhead | Significant overhead for deep trees; justifies Move/Borrow semantics. |
| `bench_parser` | Parsing and Serialization speed | Measured throughput for `hsd_load_string` and `hsd_dump_to_string`. |
| `bench_access` | Path-based access vs Shallow access | Path navigation (`a/b/c`) has measurable but low linear overhead. |
| `bench_array` | Impact of "Cache-on-Read" for arrays | Subsequent access is ~1000x faster than the first read. |

## Running Benchmarks

### Using CMake
```bash
cmake -B build -DCMAKE_BUILD_TYPE=Release
cmake --build build --target benchmarks
# Run individually
./build/utils/bench/bench_lookup
./build/utils/bench/bench_copy
./build/utils/bench/bench_parser
./build/utils/bench/bench_access
./build/utils/bench/bench_array
```

### Using FPM
```bash
fpm test --profile release --target bench_lookup
fpm test --profile release --target bench_copy
fpm test --profile release --target bench_parser
fpm test --profile release --target bench_access
fpm test --profile release --target bench_array
```

## Performance Observations (Typical Results)

### Lookup Performance
Always-on hash tables provide $O(1)$ lookup (~50-100ns) even for small tables, eliminating the need for linear search.

### Path Access
Shallow access (`a`) is ~70ns. A deep nested path (`a/b/c/d/e`) is ~1000ns. This represents approximately 200ns of overhead per path segment.

### Array Caching
One of the most significant optimizations in HSD-Fortran is the "Cache-on-Read" for large arrays.
- **First Access**: ~115us (includes parsing text to `real` array)
- **Subsequent Access**: ~0.08us (retrieves cached array)
- **Benefit**: >1000x speedup for re-reading data.

### Cloning Overhead
Since HSD-Fortran uses polymorphic pointers for child nodes, a "clone" operation performs a deep copy of the tree structure.
- Depth 2: ~30us
- Depth 4: ~3500us
- **Recommendation**: Avoid frequent tree cloning; prefer passing pointers or adopting Copy-on-Write (CoW) if tree modification is common.
