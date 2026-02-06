#!/usr/bin/env python3
"""Post-process lcov .info file to apply LCOV_EXCL markers.

lcov 2.0 does not properly apply LCOV_EXCL_LINE and LCOV_EXCL_START/STOP
markers in Fortran source files. This script manually filters them out.

Usage:
    python3 utils/filter_coverage.py build/coverage.info build/coverage_filtered.info
"""
import os
import sys


def get_exclusions(source_file):
    """Return set of line numbers to exclude based on LCOV_EXCL markers."""
    if not os.path.exists(source_file):
        return set()
    excluded = set()
    in_region = False
    with open(source_file) as f:
        for i, line in enumerate(f, 1):
            if "LCOV_EXCL_START" in line:
                in_region = True
                excluded.add(i)
            elif "LCOV_EXCL_STOP" in line:
                in_region = False
            elif in_region:
                excluded.add(i)
            elif "LCOV_EXCL_LINE" in line:
                excluded.add(i)
    return excluded


def filter_info(info_in, info_out):
    """Filter .info file, removing lines with LCOV_EXCL markers."""
    excl_cache = {}
    total_removed = 0

    with open(info_in) as fin, open(info_out, "w") as fout:
        current_file = None
        for line in fin:
            if line.startswith("SF:"):
                current_file = line.strip()[3:]
                if current_file not in excl_cache:
                    excl_cache[current_file] = get_exclusions(current_file)
                fout.write(line)
            elif line.startswith("DA:"):
                parts = line.strip()[3:].split(",")
                lineno = int(parts[0])
                if current_file and lineno in excl_cache.get(current_file, set()):
                    total_removed += 1
                    continue
                fout.write(line)
            elif line.startswith("BRDA:"):
                parts = line.strip()[5:].split(",")
                lineno = int(parts[0])
                if current_file and lineno in excl_cache.get(current_file, set()):
                    continue
                fout.write(line)
            else:
                fout.write(line)

    # Recalculate LF/LH counts
    with open(info_out) as f:
        content = f.read()

    records = content.split("end_of_record\n")
    new_records = []
    for record in records:
        if not record.strip():
            continue
        lines = record.strip().split("\n")
        da_lines = [ll for ll in lines if ll.startswith("DA:")]
        lf = len(da_lines)
        lh = sum(1 for ll in da_lines if int(ll.split(",")[1]) > 0)
        new_lines = []
        for ll in lines:
            if ll.startswith("LF:"):
                new_lines.append(f"LF:{lf}")
            elif ll.startswith("LH:"):
                new_lines.append(f"LH:{lh}")
            else:
                new_lines.append(ll)
        new_records.append("\n".join(new_lines))

    with open(info_out, "w") as f:
        for r in new_records:
            f.write(r + "\nend_of_record\n")

    print(f"Filtered {total_removed} excluded coverage lines")
    return total_removed


if __name__ == "__main__":
    if len(sys.argv) != 3:
        print(f"Usage: {sys.argv[0]} input.info output.info")
        sys.exit(1)
    filter_info(sys.argv[1], sys.argv[2])
