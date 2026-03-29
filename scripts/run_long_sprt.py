#!/usr/bin/env python3
"""Run one long SelfPlay SPRT candidate autonomously from cron."""

from __future__ import annotations

import argparse
import configparser
import os
import re
import shlex
import shutil
import subprocess
import sys
from contextlib import contextmanager
from dataclasses import dataclass
from datetime import datetime
from pathlib import Path
from typing import Iterator, TextIO

if os.name == "nt":  # pragma: no cover
    import msvcrt
else:  # pragma: no cover
    import fcntl


SCRIPT_NAME = "run_long_sprt"
LOCK_FILE_NAME = f".{SCRIPT_NAME}.lock"
RUN_LOG_NAME = "supervisor.log"
STDOUT_LOG_NAME = "selfplay.stdout.log"
STDERR_LOG_NAME = "selfplay.stderr.log"
SAVE_FILE_NAME = "selfplay.sav"
VERDICT_RE = re.compile(r"(?:SPRT:|Termination:\s+SPRT)\s+accepted\s+(H[01])")
SAVE_VERDICT_RE = re.compile(r"^verdict\s*=\s*Sprt(H[01]|Continue)\s*$", re.MULTILINE)
RUN_BUCKETS = ("candidates", "running", "accepted", "rejected", "fatal", "undecided")
INI_SECTION = "selfplay"
FLAG_MAP = {
    "config": "-c",
    "input": "-i",
    "depth": "-d",
    "nodes": "-n",
    "node_margin": "-M",
    "skip": "-s",
    "fens": "-f",
    "log_level": "-l",
    "sprt_alpha": "--sprt-alpha",
    "sprt_beta": "--sprt-beta",
    "sprt_elo0": "--sprt-elo0",
    "sprt_elo1": "--sprt-elo1",
    "sprt_save_minutes": "--sprt-save-minutes",
    "stats_every": "--stats-every",
}


class SprtScriptError(RuntimeError):
    """Raised for operator-facing script failures."""


@dataclass(frozen=True)
class RunSelection:
    run_dir: Path
    candidate_path: Path
    resumed: bool


def timestamp() -> str:
    return datetime.now().astimezone().strftime("%Y-%m-%d %H:%M:%S%z")


def log(message: str, *, stream: TextIO = sys.stdout, run_log: Path | None = None) -> None:
    line = f"{timestamp()} {message}"
    print(line, file=stream, flush=True)
    if run_log is not None:
        with run_log.open("a", encoding="utf-8", newline="\n") as handle:
            handle.write(line)
            handle.write("\n")


def parse_args(argv: list[str] | None = None) -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Run one SelfPlay SPRT candidate from a cron job.")
    parser.add_argument("selfplay_path", help="Path to the SelfPlay executable")
    parser.add_argument("base_dir", help="Base directory containing config.ini and run buckets")
    return parser.parse_args(argv)


def resolve_existing_file(path_text: str, label: str) -> Path:
    path = Path(path_text).expanduser().resolve()
    if not path.exists():
        raise SprtScriptError(f"{label} not found: {path}")
    if not path.is_file():
        raise SprtScriptError(f"{label} is not a file: {path}")
    return path


def resolve_base_dir(path_text: str) -> Path:
    path = Path(path_text).expanduser().resolve()
    if not path.exists():
        raise SprtScriptError(f"Base directory not found: {path}")
    if not path.is_dir():
        raise SprtScriptError(f"Base directory is not a directory: {path}")
    return path


@contextmanager
def file_lock(lock_path: Path) -> Iterator[bool]:
    lock_path.parent.mkdir(parents=True, exist_ok=True)
    with lock_path.open("a+", encoding="utf-8", newline="\n") as handle:
        acquired = try_lock(handle)
        try:
            yield acquired
        finally:
            if acquired:
                unlock(handle)


def try_lock(handle: TextIO) -> bool:
    if os.name == "nt":  # pragma: no cover
        try:
            handle.seek(0)
            msvcrt.locking(handle.fileno(), msvcrt.LK_NBLCK, 1)
            handle.seek(0)
            handle.write(str(os.getpid()))
            handle.truncate()
            handle.flush()
            return True
        except OSError:
            return False

    try:  # pragma: no cover
        fcntl.flock(handle.fileno(), fcntl.LOCK_EX | fcntl.LOCK_NB)
        handle.seek(0)
        handle.write(str(os.getpid()))
        handle.truncate()
        handle.flush()
        return True
    except BlockingIOError:  # pragma: no cover
        return False


def unlock(handle: TextIO) -> None:
    if os.name == "nt":  # pragma: no cover
        handle.seek(0)
        try:
            msvcrt.locking(handle.fileno(), msvcrt.LK_UNLCK, 1)
        except OSError:
            pass
        return

    fcntl.flock(handle.fileno(), fcntl.LOCK_UN)  # pragma: no cover


def ensure_layout(base_dir: Path) -> dict[str, Path]:
    paths = {name: base_dir / name for name in RUN_BUCKETS}
    for path in paths.values():
        path.mkdir(parents=True, exist_ok=True)
    return paths


def load_selfplay_args(config_path: Path) -> list[str]:
    text = config_path.read_text(encoding="utf-8")
    if not text.endswith("\n"):
        text += "\n"

    stripped_lines = [line.strip() for line in text.splitlines() if line.strip()]
    has_header = any(line.startswith("[") for line in stripped_lines[:1])
    parser = configparser.ConfigParser(interpolation=None, strict=False)
    parser.optionxform = str.lower
    if has_header:
        parser.read_string(text)
    else:
        parser.read_string(f"[{INI_SECTION}]\n{text}")

    if not parser.sections():
        section = parser.defaults()
    elif len(parser.sections()) == 1:
        section = parser[parser.sections()[0]]
    else:
        raise SprtScriptError(f"config.ini must contain at most one section: {config_path}")

    entries = {key.lower(): value.strip() for key, value in section.items()}
    if "input" not in entries:
        raise SprtScriptError(f"config.ini is missing required key 'input': {config_path}")

    unknown = sorted(key for key in entries if key not in FLAG_MAP and key != "param")
    if unknown:
        raise SprtScriptError(f"Unsupported config.ini key(s): {', '.join(unknown)}")

    args: list[str] = []
    for key, flag in FLAG_MAP.items():
        value = entries.get(key)
        if value:
            args.extend([flag, value])

    for param in parse_param_entries(entries.get("param", "")):
        args.extend(["-p", param])

    return args


def parse_param_entries(raw: str) -> list[str]:
    return [line.strip() for line in raw.splitlines() if line.strip()]


def detect_running_candidate(run_dir: Path) -> Path:
    candidates = sorted(path for path in run_dir.iterdir() if path.is_file() and path.suffix == ".txt")
    if len(candidates) != 1:
        raise SprtScriptError(
            f"Expected exactly one candidate .txt in running directory {run_dir}, found {len(candidates)}"
        )
    return candidates[0]


def discover_resumable_run(running_dir: Path) -> RunSelection | None:
    run_dirs = sorted(path for path in running_dir.iterdir() if path.is_dir())
    if not run_dirs:
        return None
    if len(run_dirs) > 1:
        raise SprtScriptError(f"More than one running candidate directory found in {running_dir}")
    run_dir = run_dirs[0]
    candidate_path = detect_running_candidate(run_dir)
    return RunSelection(run_dir=run_dir, candidate_path=candidate_path, resumed=True)


def oldest_candidate(candidates_dir: Path) -> Path | None:
    candidates = [path for path in candidates_dir.glob("*.txt") if path.is_file()]
    if not candidates:
        return None
    return min(candidates, key=lambda path: (path.stat().st_mtime, path.name))


def prepare_fresh_run(candidates_dir: Path, running_dir: Path) -> RunSelection | None:
    candidate = oldest_candidate(candidates_dir)
    if candidate is None:
        return None

    run_dir = running_dir / candidate.stem
    if run_dir.exists():
        raise SprtScriptError(f"Running directory already exists for candidate {candidate.name}: {run_dir}")
    run_dir.mkdir(parents=True, exist_ok=False)
    destination = run_dir / candidate.name
    shutil.move(str(candidate), str(destination))
    return RunSelection(run_dir=run_dir, candidate_path=destination, resumed=False)


def build_command(selfplay_path: Path, candidate_path: Path, run_dir: Path, config_args: list[str]) -> list[str]:
    return [
        str(selfplay_path),
        "--sprt",
        str(run_dir),
        "-a",
        str(candidate_path),
        "--base-current",
        *config_args,
    ]


def append_command_line(run_log: Path, command: list[str]) -> None:
    with run_log.open("a", encoding="utf-8", newline="\n") as handle:
        handle.write(f"command: {shlex.join(command)}\n")


def parse_verdict(stdout_path: Path, save_path: Path) -> str | None:
    if stdout_path.exists():
        text = stdout_path.read_text(encoding="utf-8")
        hits = VERDICT_RE.findall(text)
        if hits:
            return hits[-1]

    if save_path.exists():
        text = save_path.read_text(encoding="utf-8")
        hits = SAVE_VERDICT_RE.findall(text)
        if hits:
            result = hits[-1]
            if result == "Continue":
                return None
            return result

    return None


def move_run_directory(run_dir: Path, destination_root: Path) -> Path:
    target = unique_destination(destination_root / run_dir.name)
    shutil.move(str(run_dir), str(target))
    return target


def unique_destination(path: Path) -> Path:
    if not path.exists():
        return path
    stem = path.name
    index = 2
    while True:
        candidate = path.parent / f"{stem}-{index}"
        if not candidate.exists():
            return candidate
        index += 1


def execute_selfplay(command: list[str], run_dir: Path) -> subprocess.CompletedProcess[int]:
    stdout_path = run_dir / STDOUT_LOG_NAME
    stderr_path = run_dir / STDERR_LOG_NAME
    with stdout_path.open("w", encoding="utf-8", newline="\n") as stdout_handle:
        with stderr_path.open("w", encoding="utf-8", newline="\n") as stderr_handle:
            return subprocess.run(
                command,
                cwd=run_dir,
                check=False,
                stdout=stdout_handle,
                stderr=stderr_handle,
                text=True,
            )


def run_once(selfplay_path: Path, base_dir: Path) -> int:
    config_path = base_dir / "config.ini"
    if not config_path.exists():
        raise SprtScriptError(f"Missing config.ini in {base_dir}")

    buckets = ensure_layout(base_dir)
    with file_lock(base_dir / LOCK_FILE_NAME) as acquired:
        if not acquired:
            log("another instance is already running; exiting")
            return 0

        selection = discover_resumable_run(buckets["running"])
        if selection is None:
            selection = prepare_fresh_run(buckets["candidates"], buckets["running"])
            if selection is None:
                log("no pending candidate found; exiting")
                return 0

        run_log = selection.run_dir / RUN_LOG_NAME
        config_args = load_selfplay_args(config_path)
        command = build_command(selfplay_path, selection.candidate_path, selection.run_dir, config_args)
        append_command_line(run_log, command)
        mode = "resuming" if selection.resumed else "starting"
        log(
            f"{mode} candidate {selection.candidate_path.name} in {selection.run_dir} "
            f"with {shlex.join(command)}",
            run_log=run_log,
        )

        try:
            cp = execute_selfplay(command, selection.run_dir)
        except OSError as exc:
            log(f"failed to start SelfPlay for {selection.candidate_path.name}: {exc}", stream=sys.stderr, run_log=run_log)
            target = move_run_directory(selection.run_dir, buckets["fatal"])
            log(f"moved run directory to {target}", run_log=target / RUN_LOG_NAME)
            return 1

        stdout_path = selection.run_dir / STDOUT_LOG_NAME
        save_path = selection.run_dir / SAVE_FILE_NAME
        verdict = parse_verdict(stdout_path, save_path)
        log(
            f"SelfPlay finished for {selection.candidate_path.name} with exit status {cp.returncode} "
            f"and verdict {verdict or 'undecided'}",
            run_log=run_log,
        )

        if cp.returncode != 0:
            target_root = buckets["fatal"]
            outcome = "fatal"
        elif verdict == "H0":
            target_root = buckets["rejected"]
            outcome = "rejected"
        elif verdict == "H1":
            target_root = buckets["accepted"]
            outcome = "accepted"
        else:
            target_root = buckets["undecided"]
            outcome = "undecided"

        target = move_run_directory(selection.run_dir, target_root)
        log(f"moved run directory to {target} ({outcome})", run_log=target / RUN_LOG_NAME)
        return 0 if cp.returncode == 0 else 1


def main(argv: list[str] | None = None) -> int:
    args = parse_args(argv)
    try:
        selfplay_path = resolve_existing_file(args.selfplay_path, "SelfPlay executable")
        base_dir = resolve_base_dir(args.base_dir)
        return run_once(selfplay_path, base_dir)
    except SprtScriptError as exc:
        log(f"ERROR: {exc}", stream=sys.stderr)
        return 1


if __name__ == "__main__":
    raise SystemExit(main())
