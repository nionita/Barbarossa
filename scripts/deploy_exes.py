#!/usr/bin/env python3
"""Deploy Stack-built executables with branch/version naming conventions.

Edit the destination directories below for your machine.
"""

from __future__ import annotations

import argparse
import os
import re
import shutil
import subprocess
import sys
from pathlib import Path


# Machine-specific destination folders.
BARBAROSSA_DEST_DIR = Path("C:/Engines/Barbarossa")
SELFPLAY_DEST_DIR = Path("C:/astra/SelfPlay")
TUNESGD_DEST_DIR = Path("C:/astra/TuneSGD")


def run_cmd(args: list[str], cwd: Path) -> str:
    cp = subprocess.run(args, cwd=cwd, text=True, capture_output=True, check=False)
    if cp.returncode != 0:
        err = cp.stderr.strip() or cp.stdout.strip() or "unknown error"
        raise RuntimeError(f"Command failed ({' '.join(args)}): {err}")
    return cp.stdout.strip()


def read_version(cabal_path: Path) -> str:
    text = cabal_path.read_text(encoding="utf-8")
    m = re.search(r"^Version:\s*([^\s]+)\s*$", text, flags=re.MULTILINE)
    if not m:
        raise RuntimeError(f"Could not parse Version from {cabal_path}")
    return m.group(1)


def sanitize_branch(branch: str) -> str:
    branch = branch.strip()
    if not branch:
        raise RuntimeError("Empty git branch name")
    return re.sub(r"[^A-Za-z0-9._-]+", "-", branch)


def should_copy(src: Path, dst: Path) -> bool:
    if not dst.exists():
        return True
    # Copy only if source executable is newer than deployed file.
    return os.path.getmtime(src) > os.path.getmtime(dst)


def deploy_file(src: Path, dst: Path, dry_run: bool, verbose: bool) -> bool:
    if not src.exists():
        if verbose or dry_run:
            print(f"Skip (missing source): {src}")
        return False

    if not should_copy(src, dst):
        if verbose or dry_run:
            print(f"Up-to-date: {dst}")
        return False

    if verbose or dry_run:
        print(f"{src} -> {dst}")
    if dry_run:
        return True
    dst.parent.mkdir(parents=True, exist_ok=True)
    shutil.copy2(src, dst)
    return True


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Copy and rename Barbarossa executables after stack build."
    )
    parser.add_argument("--dry-run", action="store_true", help="Print operations only")
    parser.add_argument("--verbose", action="store_true", help="Print copy operations")
    args = parser.parse_args()

    repo_root = Path(__file__).resolve().parents[1]
    install_root = Path(run_cmd(["stack", "path", "--local-install-root"], repo_root))
    bin_dir = install_root / "bin"

    version = read_version(repo_root / "Barbarossa.cabal")
    branch = sanitize_branch(run_cmd(["git", "branch", "--show-current"], repo_root))

    src_barbarossa = bin_dir / "Barbarossa.exe"
    src_selfplay = bin_dir / "SelfPlay.exe"
    src_tunesgd = bin_dir / "TuneSGD.exe"

    dst_barbarossa = BARBAROSSA_DEST_DIR / f"Barbarossa-{version}-{branch}.exe"
    dst_selfplay = SELFPLAY_DEST_DIR / f"SelfPlay-{branch}.exe"
    dst_tunesgd = TUNESGD_DEST_DIR / f"TuneSGD-{branch}.exe"

    copied_barbarossa = deploy_file(src_barbarossa, dst_barbarossa, args.dry_run, args.verbose)
    copied_selfplay = deploy_file(src_selfplay, dst_selfplay, args.dry_run, args.verbose)
    copied_tunesgd = deploy_file(src_tunesgd, dst_tunesgd, args.dry_run, args.verbose)

    if not args.dry_run:
        if copied_barbarossa:
            print(f"Deployed: {dst_barbarossa}")
        else:
            print(f"Skipped:  {dst_barbarossa}")
        if copied_selfplay:
            print(f"Deployed: {dst_selfplay}")
        else:
            print(f"Skipped:  {dst_selfplay}")
        if copied_tunesgd:
            print(f"Deployed: {dst_tunesgd}")
        else:
            print(f"Skipped:  {dst_tunesgd}")
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except Exception as exc:  # pragma: no cover
        print(f"ERROR: {exc}", file=sys.stderr)
        raise SystemExit(1)
