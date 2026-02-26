# AGENTS.md

## Repository expectations

- Module exports must be always explicit

## Build Identity

- Before any build, check thet the chess engine identity is correct, which means: top-level variable progVerSuff in Main/Barbarossa.hs is set to the branch name

## Post-Build Deployment

- After a successful build, deploy executables with:
  - `python scripts/deploy_exes.py`
- The script renames and copies:
  - `Barbarossa.exe` to `Barbarossa-<version>-<branch>.exe`
  - `SelfPlay.exe` to `SelfPlay-<branch>.exe`
  - `TuneSGD.exe` to `TuneSGD-<branch>.exe`
- Destination folders are configured in `scripts/deploy_exes.py` and must match the local machine setup.

## Line Endings

- After every source change, restore Unix file format (LF only) for all source files
