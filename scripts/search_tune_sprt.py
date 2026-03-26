#!/usr/bin/env python3
"""Search TuneSGD parameters and validate candidates with SelfPlay SPRT."""

from __future__ import annotations

import argparse
import json
import os
import re
import shutil
import subprocess
import sys
from dataclasses import dataclass
from datetime import datetime, timezone
from decimal import Decimal, InvalidOperation
from pathlib import Path
from typing import Any


STATE_FILE = "state.json"
BEST_DIR = "best"
BEST_CONFIG_NAME = "best.cfg"
BEST_METADATA_NAME = "metadata.json"

TUNE_NEW_BEST_RE = re.compile(r"\*\*\* New best:\s*([0-9eE+.\-]+)\s*<")
TUNE_HISTORY_RE = re.compile(r"^Loss\s+([0-9eE+.\-]+):", re.MULTILINE)
SPRT_STDOUT_RE = re.compile(r"(?:SPRT:|Termination:\s+SPRT)\s+accepted\s+(H[01])")
SPRT_SAVE_RE = re.compile(r"^verdict\s*=\s*Sprt(H[01]|Continue)\s*$", re.MULTILINE)


@dataclass(frozen=True)
class SearchParams:
    k: int
    q_ticks: int

    @property
    def q_text(self) -> str:
        return format_q_ticks(self.q_ticks)

    @property
    def key(self) -> str:
        return make_key(self.k, self.q_ticks)


def normalize_path(path_text: str) -> Path:
    """Accept both Windows paths and /c/... style paths."""
    s = path_text.strip()
    if re.match(r"^/[a-zA-Z]/", s):
        drive = s[1].upper()
        rest = s[3:]
        return Path(f"{drive}:/{rest}")
    return Path(s)


def fail(message: str) -> RuntimeError:
    return RuntimeError(message)


def utc_now() -> str:
    return datetime.now(timezone.utc).isoformat()


def make_key(k: int, q_ticks: int) -> str:
    return f"{k}:{q_ticks}"


def parse_decimal(text: str) -> Decimal:
    try:
        return Decimal(text)
    except InvalidOperation as exc:
        raise fail(f"Invalid decimal value: {text}") from exc


def parse_q_value(text: str) -> int:
    dec = parse_decimal(text)
    scaled = dec * Decimal(20)
    if scaled != scaled.to_integral_value():
        raise fail(f"q must be aligned to 0.05 steps: {text}")
    ticks = int(scaled)
    if ticks < 0 or ticks >= 20:
        raise fail(f"q must satisfy 0 <= q < 1.0: {text}")
    return ticks


def format_q_ticks(q_ticks: int) -> str:
    return f"{q_ticks / 20:.2f}".rstrip("0").rstrip(".")


def parse_k_spec(values: list[str]) -> tuple[int, int]:
    if len(values) not in (1, 2):
        raise fail("--k expects one integer or two integers")
    try:
        nums = [int(v) for v in values]
    except ValueError as exc:
        raise fail("--k values must be integers") from exc
    if len(nums) == 1:
        return nums[0], nums[0]
    lo, hi = nums
    if lo > hi:
        lo, hi = hi, lo
    return lo, hi


def parse_q_spec(values: list[str]) -> tuple[int, int]:
    if len(values) not in (1, 2):
        raise fail("--q expects one value or two values")
    nums = [parse_q_value(v) for v in values]
    if len(nums) == 1:
        return nums[0], nums[0]
    lo, hi = nums
    if lo > hi:
        lo, hi = hi, lo
    return lo, hi


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Alternate TuneSGD and SelfPlay SPRT runs while searching k/q."
    )
    parser.add_argument("--work-dir", required=True, help="Root directory for the search")
    parser.add_argument("--tune-input", required=True, help="TuneSGD input directory or file")
    parser.add_argument("--selfplay-input", required=True, help="SelfPlay input FEN file")
    parser.add_argument("--k", required=True, nargs="+", help="One integer or two integers")
    parser.add_argument("--q", required=True, nargs="+", help="One value or two values in 0.05 steps")
    parser.add_argument("--tune-optim", required=True, type=int, help="TuneSGD -O value")
    parser.add_argument("--sprt-alpha", type=float, default=0.05, help="SelfPlay SPRT alpha")
    parser.add_argument("--sprt-beta", type=float, default=0.05, help="SelfPlay SPRT beta")
    parser.add_argument("--sprt-elo0", type=float, default=0.0, help="SelfPlay SPRT elo0")
    parser.add_argument("--sprt-elo1", type=float, default=5.0, help="SelfPlay SPRT elo1")
    return parser.parse_args()


def resolve_existing_path(path_text: str, label: str) -> Path:
    path = normalize_path(path_text).expanduser().resolve()
    if not path.exists():
        raise fail(f"{label} not found: {path}")
    return path


def resolve_executable_from_env(var_name: str) -> Path:
    value = os.environ.get(var_name, "").strip()
    if not value:
        raise fail(f"Environment variable {var_name} is not set")
    path = resolve_existing_path(value, f"{var_name} executable")
    if path.is_dir():
        raise fail(f"{var_name} points to a directory, expected an executable: {path}")
    return path


def load_json(path: Path) -> dict[str, Any]:
    with path.open("r", encoding="utf-8") as fh:
        data = json.load(fh)
    if not isinstance(data, dict):
        raise fail(f"Expected a JSON object in {path}")
    return data


def write_json(path: Path, data: dict[str, Any]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    tmp = path.with_suffix(path.suffix + ".tmp")
    with tmp.open("w", encoding="utf-8", newline="\n") as fh:
        json.dump(data, fh, indent=2, sort_keys=True)
        fh.write("\n")
    tmp.replace(path)


def midpoint(lo: int, hi: int) -> int:
    return (lo + hi) // 2


def default_fixed_value(lo: int, hi: int) -> int:
    return midpoint(lo, hi)


def state_matches_args(state: dict[str, Any], args: argparse.Namespace, tune_input: Path, selfplay_input: Path) -> bool:
    checks = {
        "tune_input": str(tune_input),
        "selfplay_input": str(selfplay_input),
        "tune_optim": args.tune_optim,
        "k_spec": [*parse_k_spec(args.k)],
        "q_spec": [*parse_q_spec(args.q)],
        "sprt_alpha": args.sprt_alpha,
        "sprt_beta": args.sprt_beta,
        "sprt_elo0": args.sprt_elo0,
        "sprt_elo1": args.sprt_elo1,
    }
    for key, value in checks.items():
        if state.get(key) != value:
            return False
    return True


def build_initial_state(args: argparse.Namespace, tune_input: Path, selfplay_input: Path) -> dict[str, Any]:
    k_lo, k_hi = parse_k_spec(args.k)
    q_lo, q_hi = parse_q_spec(args.q)
    return {
        "schema": 1,
        "created_at": utc_now(),
        "updated_at": utc_now(),
        "tune_input": str(tune_input),
        "selfplay_input": str(selfplay_input),
        "tune_optim": args.tune_optim,
        "sprt_alpha": args.sprt_alpha,
        "sprt_beta": args.sprt_beta,
        "sprt_elo0": args.sprt_elo0,
        "sprt_elo1": args.sprt_elo1,
        "k_spec": [k_lo, k_hi],
        "q_spec": [q_lo, q_hi],
        "k_interval": [k_lo, k_hi],
        "q_interval": [q_lo, q_hi],
        "axis_turn": "k",
        "best_loss": None,
        "best_loss_params": None,
        "incumbent": {"mode": "base-current", "config_path": None, "source_run": None},
        "records": {},
        "next_run_id": 1,
    }


def load_or_init_state(work_dir: Path, args: argparse.Namespace, tune_input: Path, selfplay_input: Path) -> dict[str, Any]:
    state_path = work_dir / STATE_FILE
    if state_path.exists():
        state = load_json(state_path)
        if not state_matches_args(state, args, tune_input, selfplay_input):
            raise fail(f"Existing state in {state_path} does not match the current CLI arguments")
        return state
    return build_initial_state(args, tune_input, selfplay_input)


def save_state(work_dir: Path, state: dict[str, Any]) -> None:
    state["updated_at"] = utc_now()
    write_json(work_dir / STATE_FILE, state)


def get_record(state: dict[str, Any], params: SearchParams) -> dict[str, Any] | None:
    record = state["records"].get(params.key)
    return record if isinstance(record, dict) else None


def set_record(state: dict[str, Any], params: SearchParams, record: dict[str, Any]) -> None:
    state["records"][params.key] = record
    current = state.get("best_loss")
    tune = record.get("tune")
    if isinstance(tune, dict) and isinstance(tune.get("loss"), (int, float)):
        loss = float(tune["loss"])
        if current is None or loss < current:
            state["best_loss"] = loss
            state["best_loss_params"] = {"k": params.k, "q_ticks": params.q_ticks}


def get_best_loss_params(state: dict[str, Any]) -> SearchParams | None:
    raw = state.get("best_loss_params")
    if not isinstance(raw, dict):
        return None
    return SearchParams(k=int(raw["k"]), q_ticks=int(raw["q_ticks"]))


def choose_fixed_other(state: dict[str, Any], axis: str) -> int:
    best = get_best_loss_params(state)
    if axis == "k":
        if best is not None:
            return best.q_ticks
        q_lo, q_hi = state["q_spec"]
        return default_fixed_value(q_lo, q_hi)
    if best is not None:
        return best.k
    k_lo, k_hi = state["k_spec"]
    return default_fixed_value(k_lo, k_hi)


def choose_axis(state: dict[str, Any]) -> str | None:
    k_lo, k_hi = state["k_interval"]
    q_lo, q_hi = state["q_interval"]
    k_open = k_lo < k_hi
    q_open = q_lo < q_hi
    if not k_open and not q_open:
        return None
    turn = state["axis_turn"]
    if turn == "k":
        if k_open:
            return "k"
        if q_open:
            return "q"
    else:
        if q_open:
            return "q"
        if k_open:
            return "k"
    return None


def build_params(axis: str, axis_value: int, other_value: int) -> SearchParams:
    if axis == "k":
        return SearchParams(k=axis_value, q_ticks=other_value)
    return SearchParams(k=other_value, q_ticks=axis_value)


def line_probe_values(lo: int, hi: int) -> list[int]:
    if lo == hi:
        return [lo]
    if hi - lo == 1:
        return [lo, hi]
    mid = midpoint(lo, hi)
    return [mid, lo, hi]


def extract_line_losses(state: dict[str, Any], axis: str, other_value: int, values: list[int]) -> dict[int, float]:
    found: dict[int, float] = {}
    for value in values:
        params = build_params(axis, value, other_value)
        record = get_record(state, params)
        if record is None:
            continue
        loss = record.get("tune_loss")
        if isinstance(loss, (int, float)):
            found[value] = float(loss)
    return found


def tighten_interval(lo: int, hi: int, losses: dict[int, float]) -> tuple[int, int]:
    if lo == hi:
        return lo, hi
    if hi - lo == 1:
        best = lo if losses[lo] <= losses[hi] else hi
        return best, best
    mid = midpoint(lo, hi)
    loss_lo = losses[lo]
    loss_mid = losses[mid]
    loss_hi = losses[hi]
    if loss_lo < loss_mid:
        return lo, mid - 1
    if loss_hi < loss_mid:
        return mid + 1, hi
    new_lo = mid if mid - lo <= 1 else (lo + mid) // 2
    new_hi = mid if hi - mid <= 1 else (mid + hi + 1) // 2
    return new_lo, new_hi


def advance_search_frontier(state: dict[str, Any]) -> SearchParams | None:
    guard = 0
    while True:
        guard += 1
        if guard > 10000:
            raise fail("Search frontier did not converge")
        axis = choose_axis(state)
        if axis is None:
            return None
        lo, hi = state[f"{axis}_interval"]
        other_value = choose_fixed_other(state, axis)
        probes = line_probe_values(lo, hi)
        missing = [
            build_params(axis, value, other_value)
            for value in probes
            if get_record(state, build_params(axis, value, other_value)) is None
        ]
        if missing:
            return missing[0]
        losses = extract_line_losses(state, axis, other_value, probes)
        if len(losses) != len(probes):
            raise fail(f"Missing losses for axis {axis} within [{lo}, {hi}] at fixed value {other_value}")
        new_lo, new_hi = tighten_interval(lo, hi, losses)
        state[f"{axis}_interval"] = [new_lo, new_hi]
        state["axis_turn"] = "q" if axis == "k" else "k"


def run_command(cmd: list[str], cwd: Path, stdout_path: Path, stderr_path: Path) -> subprocess.CompletedProcess[str]:
    cp = subprocess.run(cmd, cwd=cwd, text=True, capture_output=True, check=False)
    stdout_path.write_text(cp.stdout, encoding="utf-8", newline="\n")
    stderr_path.write_text(cp.stderr, encoding="utf-8", newline="\n")
    return cp


def parse_tune_loss(stdout_text: str) -> float:
    values = [float(x) for x in TUNE_NEW_BEST_RE.findall(stdout_text)]
    values.extend(float(x) for x in TUNE_HISTORY_RE.findall(stdout_text))
    if not values:
        raise fail("Could not parse a TuneSGD loss from stdout")
    return min(values)


def parse_sprt_verdict(stdout_text: str, save_path: Path) -> str | None:
    stdout_hits = SPRT_STDOUT_RE.findall(stdout_text)
    if stdout_hits:
        return stdout_hits[-1]
    if save_path.exists():
        text = save_path.read_text(encoding="utf-8")
        save_hits = SPRT_SAVE_RE.findall(text)
        if save_hits:
            raw = save_hits[-1]
            if raw == "Continue":
                return None
            return raw
    return None


def ensure_candidate_exists(candidate_path: Path) -> None:
    if not candidate_path.exists():
        raise fail(
            f"TuneSGD completed without producing {candidate_path}. "
            "Try increasing --tune-optim if this run found no improvement."
        )


def make_run_dir(work_dir: Path, state: dict[str, Any]) -> Path:
    run_id = int(state["next_run_id"])
    while True:
        run_dir = work_dir / f"run-{run_id:04d}"
        if not run_dir.exists():
            state["next_run_id"] = run_id + 1
            run_dir.mkdir(parents=True, exist_ok=False)
            return run_dir
        run_id += 1


def tune_command(
    exe: Path,
    params: SearchParams,
    tune_input: Path,
    output_path: Path,
    tune_optim: int,
) -> list[str]:
    return [
        str(exe),
        "-i",
        str(tune_input),
        "-o",
        str(output_path),
        "-O",
        str(tune_optim),
        "-F",
        "-1",
        "-L",
        "5",
        "-k",
        str(params.k),
        "-q",
        params.q_text,
    ]


def selfplay_command(
    exe: Path,
    candidate_path: Path,
    incumbent: dict[str, Any],
    selfplay_input: Path,
    stage_dir: Path,
    pairs: int,
    args: argparse.Namespace,
) -> list[str]:
    cmd = [
        str(exe),
        "--sprt",
        str(stage_dir),
        "-a",
        str(candidate_path),
        "-i",
        str(selfplay_input),
        "-n",
        str(pairs),
        "--sprt-alpha",
        str(args.sprt_alpha),
        "--sprt-beta",
        str(args.sprt_beta),
        "--sprt-elo0",
        str(args.sprt_elo0),
        "--sprt-elo1",
        str(args.sprt_elo1),
    ]
    if incumbent["mode"] == "base-current":
        cmd.append("--base-current")
    else:
        cmd.extend(["-b", str(incumbent["config_path"])])
    return cmd


def update_best_incumbent(work_dir: Path, state: dict[str, Any], candidate_path: Path, source_run: str) -> dict[str, Any]:
    best_dir = work_dir / BEST_DIR
    best_dir.mkdir(parents=True, exist_ok=True)
    best_cfg = best_dir / BEST_CONFIG_NAME
    shutil.copy2(candidate_path, best_cfg)
    best_meta = {
        "updated_at": utc_now(),
        "config_path": str(best_cfg),
        "source_run": source_run,
    }
    write_json(best_dir / BEST_METADATA_NAME, best_meta)
    incumbent = {
        "mode": "config",
        "config_path": str(best_cfg),
        "source_run": source_run,
    }
    state["incumbent"] = incumbent
    return incumbent


def execute_run(
    work_dir: Path,
    state: dict[str, Any],
    args: argparse.Namespace,
    tune_exe: Path,
    selfplay_exe: Path,
    tune_input: Path,
    selfplay_input: Path,
    params: SearchParams,
) -> dict[str, Any]:
    run_dir = make_run_dir(work_dir, state)
    candidate_path = run_dir / "candidate.cfg"
    tune_stdout = run_dir / "tune.stdout.txt"
    tune_stderr = run_dir / "tune.stderr.txt"
    incumbent_before = dict(state["incumbent"])

    tune_cmd = tune_command(tune_exe, params, tune_input, candidate_path, args.tune_optim)
    tune_cp = run_command(tune_cmd, run_dir, tune_stdout, tune_stderr)
    if tune_cp.returncode != 0:
        raise fail(f"TuneSGD failed for k={params.k}, q={params.q_text} with code {tune_cp.returncode}")
    ensure_candidate_exists(candidate_path)
    tune_loss = parse_tune_loss(tune_cp.stdout)

    record: dict[str, Any] = {
        "run_dir": str(run_dir),
        "run_name": run_dir.name,
        "params": {"k": params.k, "q_ticks": params.q_ticks, "q": params.q_text},
        "started_at": utc_now(),
        "completed_at": None,
        "tune": {
            "command": tune_cmd,
            "returncode": tune_cp.returncode,
            "stdout_path": str(tune_stdout),
            "stderr_path": str(tune_stderr),
            "candidate_path": str(candidate_path),
            "loss": tune_loss,
        },
        "sprt": [],
        "sprt_passed_100k": False,
        "incumbent_before": incumbent_before,
        "incumbent_after": incumbent_before,
    }

    for pairs, label in ((1000, "1k"), (10000, "10k"), (100000, "100k")):
        stage_dir = run_dir / f"sprt-{label}"
        stage_dir.mkdir(parents=True, exist_ok=False)
        stdout_path = run_dir / f"sprt-{label}.stdout.txt"
        stderr_path = run_dir / f"sprt-{label}.stderr.txt"
        cmd = selfplay_command(
            selfplay_exe,
            candidate_path,
            state["incumbent"],
            selfplay_input,
            stage_dir,
            pairs,
            args,
        )
        cp = run_command(cmd, run_dir, stdout_path, stderr_path)
        if cp.returncode != 0:
            raise fail(f"SelfPlay SPRT {label} failed for run {run_dir.name} with code {cp.returncode}")
        verdict = parse_sprt_verdict(cp.stdout, stage_dir / "selfplay.sav")
        stage_record = {
            "pairs": pairs,
            "label": label,
            "command": cmd,
            "returncode": cp.returncode,
            "stdout_path": str(stdout_path),
            "stderr_path": str(stderr_path),
            "stage_dir": str(stage_dir),
            "verdict": verdict,
        }
        record["sprt"].append(stage_record)
        if verdict == "H1":
            continue
        if verdict == "H0":
            break
        raise fail(f"SelfPlay SPRT {label} finished without accepted H0/H1 for run {run_dir.name}")

    if record["sprt"] and record["sprt"][-1]["pairs"] == 100000 and record["sprt"][-1]["verdict"] == "H1":
        record["sprt_passed_100k"] = True
        record["incumbent_after"] = update_best_incumbent(work_dir, state, candidate_path, run_dir.name)
    record["completed_at"] = utc_now()
    write_json(run_dir / "metadata.json", record)
    return record


def summarize_result(record: dict[str, Any]) -> str:
    last_stage = record["sprt"][-1] if record["sprt"] else None
    last_text = "no SPRT"
    if isinstance(last_stage, dict):
        last_text = f"{last_stage['label']}={last_stage['verdict']}"
    return (
        f"run={record['run_name']} "
        f"k={record['params']['k']} "
        f"q={record['params']['q']} "
        f"loss={record['tune']['loss']} "
        f"{last_text}"
    )


def main() -> int:
    args = parse_args()

    if args.tune_optim <= 0:
        print("ERROR: --tune-optim must be > 0", file=sys.stderr)
        return 2

    tune_input = resolve_existing_path(args.tune_input, "TuneSGD input")
    selfplay_input = resolve_existing_path(args.selfplay_input, "SelfPlay input")
    work_dir = normalize_path(args.work_dir).expanduser().resolve()
    work_dir.mkdir(parents=True, exist_ok=True)

    tune_exe = resolve_executable_from_env("TUNESGD_PATH")
    selfplay_exe = resolve_executable_from_env("SELFPLAY_PATH")

    state = load_or_init_state(work_dir, args, tune_input, selfplay_input)
    save_state(work_dir, state)

    while True:
        params = advance_search_frontier(state)
        if params is None:
            break
        record = get_record(state, params)
        if record is None:
            print(f"Running k={params.k} q={params.q_text}")
            record = execute_run(
                work_dir,
                state,
                args,
                tune_exe,
                selfplay_exe,
                tune_input,
                selfplay_input,
                params,
            )
            set_record(state, params, record)
            save_state(work_dir, state)
            print(summarize_result(record))
        else:
            set_record(state, params, record)
            save_state(work_dir, state)

    best = get_best_loss_params(state)
    if best is None:
        print("Search finished without any recorded TuneSGD loss.")
    else:
        print(
            "Search finished. "
            f"Best loss point: k={best.k} q={best.q_text} loss={state['best_loss']}"
        )
    incumbent = state["incumbent"]
    if incumbent["mode"] == "base-current":
        print("Best SPRT incumbent: compiled current weights")
    else:
        print(f"Best SPRT incumbent: {incumbent['config_path']}")
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except Exception as exc:  # pragma: no cover
        print(f"ERROR: {exc}", file=sys.stderr)
        raise SystemExit(1)
