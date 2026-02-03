# HSD-Fortran Fuzz Testing with AFL++

This directory contains infrastructure for coverage-guided fuzz testing of the HSD parser using AFL++.

**Note**: The fuzzer is kept separate from `test/` because it's not a unit test that runs automatically.
Fuzzing is a continuous, long-running process that discovers edge cases over hours/days.

## Prerequisites

Install AFL++ on Ubuntu/Debian:
```bash
sudo apt-get install afl++
```

## Quick Start

### 1. Build the fuzz target

```bash
cd /path/to/hsd-fortran

# Build with standard gfortran (QEMU mode handles instrumentation)
cmake -B build-fuzz -S utils/fuzz
cmake --build build-fuzz
```

### 2. Run the fuzzer

```bash
cd build-fuzz

# Create output directory
mkdir -p findings

# Run AFL++ with QEMU mode (no need to copy corpus)
afl-fuzz -Q -i ../utils/fuzz/corpus -o findings -x hsd.dict -- ./hsd_fuzz_stdin
```

### 3. Monitor progress

In another terminal:
```bash
afl-whatsup build-fuzz/findings
```

## Fuzzer Options

```bash
# Basic fuzzing with QEMU mode (recommended)
afl-fuzz -Q -i ../utils/fuzz/corpus -o findings -- ./hsd_fuzz_stdin

# With dictionary (recommended)
afl-fuzz -Q -i ../utils/fuzz/corpus -o findings -x hsd.dict -- ./hsd_fuzz_stdin

# Parallel fuzzing (multiple cores)
# Terminal 1 - Main fuzzer
afl-fuzz -Q -i ../utils/fuzz/corpus -o findings -M main -x hsd.dict -- ./hsd_fuzz_stdin
# Terminal 2+ - Secondary fuzzers
afl-fuzz -Q -i ../utils/fuzz/corpus -o findings -S fuzzer2 -x hsd.dict -- ./hsd_fuzz_stdin
afl-fuzz -Q -i ../utils/fuzz/corpus -o findings -S fuzzer3 -x hsd.dict -- ./hsd_fuzz_stdin

# Quick mode (faster but less thorough)
afl-fuzz -Q -i ../utils/fuzz/corpus -o findings -d -x hsd.dict -- ./hsd_fuzz_stdin

# With memory limit (default 50MB)
afl-fuzz -Q -i ../utils/fuzz/corpus -o findings -m 100 -x hsd.dict -- ./hsd_fuzz_stdin
```

## Output

AFL++ creates these directories in `findings/`:

| Directory | Contents |
|-----------|----------|
| `queue/` | Inputs that discovered new code paths |
| `crashes/` | Inputs that caused crashes |
| `hangs/` | Inputs that caused timeouts |

### Reproducing issues

```bash
# Reproduce a crash
./hsd_fuzz_stdin < findings/crashes/id:000000*

# Reproduce with gdb
gdb -ex run -ex bt --args ./hsd_fuzz_stdin < findings/crashes/id:000000*
```

## Dictionary

The `hsd.dict` file contains HSD-specific tokens that help AFL++ generate syntactically interesting inputs:
- Structural tokens: `{`, `}`, `[`, `]`, `=`
- Include directives: `<<<`, `<<+`
- Boolean values: `Yes`, `No`, `On`, `Off`
- And more...

## Alternative: Source-level instrumentation

For better performance, you can use source-level instrumentation instead of QEMU mode:

```bash
# Build with AFL++ instrumented compiler (Linux only, requires afl++)
AFL_USE_QEMU=0 cmake -B build-fuzz -S utils/fuzz -DCMAKE_Fortran_COMPILER=afl-gfortran
cmake --build build-fuzz

# Run without -Q flag (faster but requires special build)
afl-fuzz -i ../utils/fuzz/corpus -o findings -x hsd.dict -- ./build-fuzz/hsd_fuzz_stdin
```

Note: QEMU mode (-Q flag) is easier to set up and works with any compiler, but is 2-5x slower than source instrumentation.

## Corpus Management

### Minimize corpus
```bash
afl-cmin -Q -i ../utils/fuzz/corpus -o corpus_min -- ./hsd_fuzz_stdin
```

### Minimize a crashing input
```bash
afl-tmin -Q -i findings/crashes/id:000000* -o crash_min -- ./hsd_fuzz_stdin
```

## Files

| File | Purpose |
|------|---------|
| `CMakeLists.txt` | Build configuration |
| `fuzz_stdin_driver.f90` | Main fuzz target (reads from stdin) |
| `hsd.dict` | AFL++ dictionary with HSD tokens |
| `corpus/` | Seed inputs |

## Troubleshooting

### "Hmm, your system is configured to send core dump notifications..."
```bash
sudo sh -c 'echo core > /proc/sys/kernel/core_pattern'
```

### "The target binary is pretty slow"
- HSD parsing is inherently quick; slow runs usually indicate I/O issues
- Use a RAM disk for corpus/findings: `mount -t tmpfs -o size=1G tmpfs /tmp/fuzz`

### AFL++ UI shows 0 paths
- Ensure you're using the `-Q` flag for QEMU mode (required for non-instrumented binaries)
- Verify the corpus directory path is correct (`-i ../utils/fuzz/corpus`)
- Check that the binary runs successfully: `./hsd_fuzz_stdin < ../utils/fuzz/corpus/minimal.hsd`
