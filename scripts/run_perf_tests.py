#!/usr/bin/env python3
"""Run SelfPlay perf tests in parallel across branches and aggregate results."""

from __future__ import annotations

import argparse
import concurrent.futures as cf
import re
import statistics
import subprocess
import sys
from dataclasses import dataclass
from pathlib import Path


NODES_RE = re.compile(r"Total nodes:\s*(\d+)")
TIME_MS_RE = re.compile(r"Total time \(ms\):\s*(\d+)")
NPS_RE = re.compile(r"Nodes/second:\s*(\d+)")

@dataclass(frozen=True)
class RoundStats:
    nodes: int
    time_ms: int
    nps: int


def normalize_path(path_text: str) -> Path:
    """Accept both Windows paths and /c/... style paths."""
    s = path_text.strip()
    if re.match(r"^/[a-zA-Z]/", s):
        drive = s[1].upper()
        rest = s[3:]
        return Path(f"{drive}:/{rest}")
    return Path(s)


def run_one(exe: Path, input_file: Path, depth: int, branch: str, round_no: int) -> RoundStats:
    cmd = [str(exe), "-P", "-i", str(input_file), "-d", str(depth)]
    cp = subprocess.run(cmd, text=True, capture_output=True, check=False)
    if cp.returncode != 0:
        err = cp.stderr.strip() or cp.stdout.strip() or "unknown error"
        raise RuntimeError(
            f"[{branch}] run {round_no} failed with code {cp.returncode}: {err}"
        )
    nodes_m = NODES_RE.search(cp.stdout)
    time_m = TIME_MS_RE.search(cp.stdout)
    nps_m = NPS_RE.search(cp.stdout)
    if not (nodes_m and time_m and nps_m):
        raise RuntimeError(
            f"[{branch}] run {round_no} did not report nodes/time/nps.\n"
            f"Output:\n{cp.stdout}"
        )
    return RoundStats(
        nodes=int(nodes_m.group(1)),
        time_ms=int(time_m.group(1)),
        nps=int(nps_m.group(1)),
    )


def filtered_average(values: list[int]) -> float:
    vals = sorted(values)
    return statistics.fmean(vals[1:-1])


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Run multiple SelfPlay perf runs in parallel for several branches."
    )
    parser.add_argument("-i", "--input", required=True, help="Input FEN file")
    parser.add_argument("-d", "--depth", required=True, type=int, help="Search depth")
    parser.add_argument(
        "-r",
        "--runs",
        required=True,
        type=int,
        help="Number of runs per branch (must be >= 3)",
    )
    parser.add_argument(
        "--exe-dir",
        default="/c/astra/SelfPlay",
        help="Folder containing SelfPlay-<branch>.exe (default: /c/astra/SelfPlay)",
    )
    parser.add_argument("branches", nargs="+", help="Branch suffixes (e.g. cxi cxis)")
    args = parser.parse_args()

    if args.runs < 3:
        print("ERROR: --runs must be at least 3 (best and worst are filtered out).", file=sys.stderr)
        return 2

    input_file = normalize_path(args.input)
    if not input_file.exists():
        print(f"ERROR: input file not found: {input_file}", file=sys.stderr)
        return 2

    exe_dir = normalize_path(args.exe_dir)
    exes: dict[str, Path] = {}
    for branch in args.branches:
        exe = exe_dir / f"SelfPlay-{branch}.exe"
        if not exe.exists():
            print(f"ERROR: executable not found for branch '{branch}': {exe}", file=sys.stderr)
            return 2
        exes[branch] = exe

    results: dict[str, list[RoundStats]] = {branch: [] for branch in args.branches}

    for round_no in range(1, args.runs + 1):
        print(f"Round {round_no}/{args.runs}")
        with cf.ThreadPoolExecutor(max_workers=len(args.branches)) as pool:
            future_map = {
                pool.submit(run_one, exes[branch], input_file, args.depth, branch, round_no): branch
                for branch in args.branches
            }
            round_stats: dict[str, RoundStats] = {}
            for fut in cf.as_completed(future_map):
                branch = future_map[fut]
                try:
                    stats = fut.result()
                except Exception as exc:
                    print(f"ERROR: {exc}", file=sys.stderr)
                    return 1
                results[branch].append(stats)
                round_stats[branch] = stats
            for branch in args.branches:
                stats = round_stats[branch]
                print(
                    f"  {branch}: nodes={stats.nodes} time={stats.time_ms}ms nps={stats.nps}"
                )

    print("\nFiltered averages (drop one best + one worst):")
    for branch in args.branches:
        nps_values = [s.nps for s in results[branch]]
        vals_sorted = sorted(nps_values)
        avg = filtered_average(nps_values)
        print(
            f"  {branch}: avg={avg:.2f} nps  "
            f"(best={vals_sorted[-1]}, worst={vals_sorted[0]}, kept={vals_sorted[1:-1]})"
        )

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
