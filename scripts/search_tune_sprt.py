#!/usr/bin/env python3
"""Search TuneSGD k values for a fixed q and validate candidates with SelfPlay SPRT."""

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
        return str(self.k)


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


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Alternate TuneSGD and SelfPlay SPRT runs while searching k for a fixed q."
    )
    parser.add_argument("--work-dir", required=True, help="Root directory for the search")
    parser.add_argument("--tune-input", required=True, help="TuneSGD input directory or file")
    parser.add_argument("--selfplay-input", required=True, help="SelfPlay input FEN file")
    parser.add_argument("--k", required=True, nargs="+", help="One integer or two integers")
    parser.add_argument("--q", required=True, help="One fixed value in 0.05 steps")
    parser.add_argument("--tune-optim", required=True, type=int, help="TuneSGD -O value")
    parser.add_argument(
        "--selfplay-max-pairs",
        type=int,
        default=30000,
        help="SelfPlay -f max pairs (default: 30000)",
    )
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


def build_session(
    args: argparse.Namespace,
    tune_input: Path,
    selfplay_input: Path,
    q_ticks: int,
    k_lo: int,
    k_hi: int,
) -> dict[str, Any]:
    return {
        "created_at": utc_now(),
        "updated_at": utc_now(),
        "finished": False,
        "tune_input": str(tune_input),
        "selfplay_input": str(selfplay_input),
        "tune_optim": args.tune_optim,
        "selfplay_max_pairs": args.selfplay_max_pairs,
        "sprt_alpha": args.sprt_alpha,
        "sprt_beta": args.sprt_beta,
        "sprt_elo0": args.sprt_elo0,
        "sprt_elo1": args.sprt_elo1,
        "q_ticks": q_ticks,
        "k_spec": [k_lo, k_hi],
        "k_interval": [k_lo, k_hi],
        "best_loss": None,
        "best_k": None,
        "records": {},
    }


def migrate_legacy_state(old: dict[str, Any]) -> dict[str, Any]:
    incumbent = old.get("incumbent")
    if not isinstance(incumbent, dict) or incumbent.get("mode") not in {"base-current", "config"}:
        incumbent = {"mode": "base-current", "config_path": None, "source_run": None}
    next_run_id = old.get("next_run_id")
    if not isinstance(next_run_id, int) or next_run_id < 1:
        next_run_id = 1
    return {
        "schema": 2,
        "created_at": old.get("created_at", utc_now()),
        "updated_at": utc_now(),
        "next_run_id": next_run_id,
        "incumbent": incumbent,
        "session": None,
    }


def load_or_init_state(
    work_dir: Path,
    args: argparse.Namespace,
    tune_input: Path,
    selfplay_input: Path,
    q_ticks: int,
    k_lo: int,
    k_hi: int,
) -> dict[str, Any]:
    state_path = work_dir / STATE_FILE
    if state_path.exists():
        loaded = load_json(state_path)
        schema = loaded.get("schema")
        state = loaded if schema == 2 else migrate_legacy_state(loaded)
    else:
        state = {
            "schema": 2,
            "created_at": utc_now(),
            "updated_at": utc_now(),
            "next_run_id": 1,
            "incumbent": {"mode": "base-current", "config_path": None, "source_run": None},
            "session": None,
        }

    session = state.get("session")
    desired = {
        "tune_input": str(tune_input),
        "selfplay_input": str(selfplay_input),
        "tune_optim": args.tune_optim,
        "selfplay_max_pairs": args.selfplay_max_pairs,
        "sprt_alpha": args.sprt_alpha,
        "sprt_beta": args.sprt_beta,
        "sprt_elo0": args.sprt_elo0,
        "sprt_elo1": args.sprt_elo1,
        "q_ticks": q_ticks,
        "k_spec": [k_lo, k_hi],
    }
    if not isinstance(session, dict):
        state["session"] = build_session(args, tune_input, selfplay_input, q_ticks, k_lo, k_hi)
    else:
        matches = all(session.get(key) == value for key, value in desired.items())
        if not matches:
            state["session"] = build_session(args, tune_input, selfplay_input, q_ticks, k_lo, k_hi)
    return state


def save_state(work_dir: Path, state: dict[str, Any]) -> None:
    state["updated_at"] = utc_now()
    session = state.get("session")
    if isinstance(session, dict):
        session["updated_at"] = utc_now()
    write_json(work_dir / STATE_FILE, state)


def get_session(state: dict[str, Any]) -> dict[str, Any]:
    session = state.get("session")
    if not isinstance(session, dict):
        raise fail("Missing active session in state")
    return session


def get_record(session: dict[str, Any], k: int) -> dict[str, Any] | None:
    record = session["records"].get(str(k))
    return record if isinstance(record, dict) else None


def set_record(session: dict[str, Any], k: int, record: dict[str, Any]) -> None:
    session["records"][str(k)] = record
    tune = record.get("tune")
    current = session.get("best_loss")
    if isinstance(tune, dict) and isinstance(tune.get("loss"), (int, float)):
        loss = float(tune["loss"])
        if current is None or loss < current:
            session["best_loss"] = loss
            session["best_k"] = k


def line_probe_values(lo: int, hi: int) -> list[int]:
    if lo == hi:
        return [lo]
    if hi - lo == 1:
        return [lo, hi]
    mid = midpoint(lo, hi)
    return [mid, lo, hi]


def extract_line_losses(session: dict[str, Any], values: list[int]) -> dict[int, float]:
    found: dict[int, float] = {}
    for value in values:
        record = get_record(session, value)
        if record is None:
            continue
        tune = record.get("tune")
        if isinstance(tune, dict) and isinstance(tune.get("loss"), (int, float)):
            found[value] = float(tune["loss"])
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


def advance_search_frontier(session: dict[str, Any], q_ticks: int) -> SearchParams | None:
    guard = 0
    while True:
        guard += 1
        if guard > 10000:
            raise fail("Search frontier did not converge")
        lo, hi = session["k_interval"]
        probes = line_probe_values(lo, hi)
        missing = [SearchParams(k=value, q_ticks=q_ticks) for value in probes if get_record(session, value) is None]
        if missing:
            return missing[0]
        losses = extract_line_losses(session, probes)
        if len(losses) != len(probes):
            raise fail(f"Missing losses for k interval [{lo}, {hi}]")
        new_lo, new_hi = tighten_interval(lo, hi, losses)
        session["k_interval"] = [new_lo, new_hi]
        if new_lo == new_hi and get_record(session, new_lo) is not None:
            session["finished"] = True
            return None


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
        "-f",
        str(args.selfplay_max_pairs),
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
    session: dict[str, Any],
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
        "search": {
            "k_spec": list(session["k_spec"]),
            "k_interval_at_start": list(session["k_interval"]),
            "tune_input": str(tune_input),
            "selfplay_input": str(selfplay_input),
            "tune_optim": args.tune_optim,
            "selfplay_max_pairs": args.selfplay_max_pairs,
        },
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
        f"maxpairs={record['search']['selfplay_max_pairs']} "
        f"{last_text}"
    )


def main() -> int:
    args = parse_args()

    if args.tune_optim <= 0:
        print("ERROR: --tune-optim must be > 0", file=sys.stderr)
        return 2
    if args.selfplay_max_pairs <= 0:
        print("ERROR: --selfplay-max-pairs must be > 0", file=sys.stderr)
        return 2

    k_lo, k_hi = parse_k_spec(args.k)
    q_ticks = parse_q_value(args.q)

    tune_input = resolve_existing_path(args.tune_input, "TuneSGD input")
    selfplay_input = resolve_existing_path(args.selfplay_input, "SelfPlay input")
    work_dir = normalize_path(args.work_dir).expanduser().resolve()
    work_dir.mkdir(parents=True, exist_ok=True)

    tune_exe = resolve_executable_from_env("TUNESGD_PATH")
    selfplay_exe = resolve_executable_from_env("SELFPLAY_PATH")

    state = load_or_init_state(work_dir, args, tune_input, selfplay_input, q_ticks, k_lo, k_hi)
    session = get_session(state)
    save_state(work_dir, state)

    if not session.get("finished", False):
        while True:
            params = advance_search_frontier(session, q_ticks)
            if params is None:
                break
            record = get_record(session, params.k)
            if record is None:
                print(f"Running k={params.k} q={params.q_text}")
                record = execute_run(
                    work_dir,
                    state,
                    session,
                    args,
                    tune_exe,
                    selfplay_exe,
                    tune_input,
                    selfplay_input,
                    params,
                )
                set_record(session, params.k, record)
                save_state(work_dir, state)
                print(summarize_result(record))
            else:
                set_record(session, params.k, record)
                save_state(work_dir, state)

    session["finished"] = True
    save_state(work_dir, state)

    best_k = session.get("best_k")
    best_loss = session.get("best_loss")
    q_text = format_q_ticks(q_ticks)
    if best_k is None or best_loss is None:
        print(f"Search finished without any recorded TuneSGD loss for q={q_text}.")
    else:
        print(f"Search finished. Best loss point for q={q_text}: k={best_k} loss={best_loss}")
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
