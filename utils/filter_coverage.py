#!/usr/bin/env python3
"""Filter LCOV coverage data to honour LCOV_EXCL_LINE/START/STOP markers.

lcov 2.0 does not apply LCOV_EXCL_LINE / LCOV_EXCL_START / LCOV_EXCL_STOP
markers for Fortran files.  This script post-processes the .info file to
strip excluded lines so that coverage reports are accurate.

Usage:
    python3 utils/filter_coverage.py <input.info> <output.info>
"""

from __future__ import annotations

import re
import sys
from pathlib import Path


def load_exclusion_ranges(source_path: str) -> set[int]:
    """Return the set of 1-based line numbers excluded by markers in *source_path*."""
    excluded: set[int] = set()
    try:
        lines = Path(source_path).read_text(errors="replace").splitlines()
    except FileNotFoundError:
        return excluded

    in_block = False
    for lineno, line in enumerate(lines, start=1):
        stripped = line.strip()
        # Check for single-line exclusion
        if "LCOV_EXCL_LINE" in stripped:
            excluded.add(lineno)
            continue
        # Check for block start
        if "LCOV_EXCL_START" in stripped:
            in_block = True
            excluded.add(lineno)
            continue
        # Check for block end
        if "LCOV_EXCL_STOP" in stripped:
            in_block = False
            excluded.add(lineno)
            continue
        if in_block:
            excluded.add(lineno)

    return excluded


def filter_info(input_path: str, output_path: str) -> None:
    """Read *input_path* (.info), drop excluded lines, write *output_path*."""
    current_source: str | None = None
    excluded: set[int] = set()

    with open(input_path) as fin, open(output_path, "w") as fout:
        for raw_line in fin:
            line = raw_line.rstrip("\n")

            # Track the current source file
            if line.startswith("SF:"):
                current_source = line[3:]
                excluded = load_exclusion_ranges(current_source)
                fout.write(raw_line)
                continue

            # Filter DA: (line execution data) records
            if line.startswith("DA:"):
                match = re.match(r"DA:(\d+),", line)
                if match:
                    lineno = int(match.group(1))
                    if lineno in excluded:
                        continue  # drop this line from the output

            # Pass everything else through
            fout.write(raw_line)


def main() -> None:
    if len(sys.argv) != 3:
        print(__doc__.strip(), file=sys.stderr)
        sys.exit(1)

    input_path, output_path = sys.argv[1], sys.argv[2]
    filter_info(input_path, output_path)
    print(f"Filtered {input_path} -> {output_path}")


if __name__ == "__main__":
    main()
