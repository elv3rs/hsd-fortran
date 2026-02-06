# HSD-Fortran Performance Benchmarks

This directory contains benchmarks for evaluating the performance characteristics
of the HSD-Fortran library.

## Benchmarks Overview

| Benchmark | Purpose |
|-----------|---------|
| `bench_lookup` | Hash table lookup time vs number of children |
| `bench_copy` | Tree cloning (deep copy) overhead |
| `bench_parser` | Parsing and serialization speed |
| `bench_access` | Path-based access vs shallow access |
| `bench_array` | Cache-on-read impact for arrays |

## Running Benchmarks

```bash
cmake -B build -DCMAKE_BUILD_TYPE=Release
cmake --build build
./build/utils/bench/bench_lookup
./build/utils/bench/bench_copy
./build/utils/bench/bench_parser
./build/utils/bench/bench_access
./build/utils/bench/bench_array
```

## Typical Results

Results from GCC 14 / Debug build on a typical x86-64 machine:

### Lookup — O(1) via Hash Table

| Children | Time/Lookup (ns) |
|---------:|------------------:|
| 2 | 75 |
| 64 | 70 |
| 1024 | 91 |

Hash tables are always active, providing constant-time lookup regardless of child count.

### Path Access

| Operation | Time (ns) |
|-----------|----------:|
| Shallow (`a`) | ~125 |
| Deep (`a/b/c/d/e`) | ~1130 |

Approximately 200 ns overhead per path segment.

### Parsing and Serialization

| Operation | Time (µs) |
|-----------|----------:|
| Parse string (medium HSD) | ~50 |
| Dump to string | ~7 |

### Array Caching (1000 reals)

| Operation | Time (µs) |
|-----------|----------:|
| Parse + first access | ~257 |
| Cached access | ~0.15 |

**>1000× speedup** on subsequent accesses.

### Cloning Overhead

| Tree Depth | Nodes | Clone + Destroy (µs) |
|-----------:|------:|---------------------:|
| 1 | 9 | 6 |
| 2 | 73 | 67 |
| 3 | 585 | 786 |
| 4 | 4681 | 9745 |

Deep cloning scales super-linearly with tree size. Prefer passing pointers
over cloning when possible.
