# AGENTS.md

## Repository expectations

- Module exports must be always explicit

## Editing notes

- In this repo, `apply_patch` can intermittently fail with a Windows sandbox refresh/setup error even when the patch itself is correct.
- If that happens, retry with a smaller patch first.
- If it still fails, use a non-destructive shell-based file edit as fallback, then re-check formatting and line endings.

## Build Identity

- Before any build, check the chess engine identity is correctnes, which means: top-level variable progVerSuff in Main/Barbarossa.hs is set to the branch name

## Post-Build Deployment

- After a successful build, deploy executables with:
  - `python scripts/deploy_exes.py`
- The script renames and copies:
  - `Barbarossa.exe` to `Barbarossa-<version>-<branch>.exe`
  - `SelfPlay.exe` to `SelfPlay-<branch>.exe`
  - `TuneSGD.exe` to `TuneSGD-<branch>.exe`
- Destination folders are configured in `scripts/deploy_exes.py` and must match the local machine setup.

## Line Endings

Before any build, check these line ending rules for all changes source files:
- no unnecessary empty spaces before an end of line
- restore Unix file format (LF only)

## Empty lines
Before any build, check these empty lines rules:
- no empty lines at the end of a source file
- no more than 1 consecutive empty lines in the source files
