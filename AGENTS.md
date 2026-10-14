# AGENTS.md

## Repository expectations

- Module exports must be always explicit

## Editing notes

- In this repo, `apply_patch` can intermittently fail with a Windows sandbox refresh/setup error even when the patch itself is correct.
- If that happens, retry with a smaller patch first.
- If it still fails, use a non-destructive shell-based file edit as fallback, then re-check formatting and line endings.

## Build Identity

- Before any build, check the chess engine identity is correctnes, which means: top-level variable progVerSuff in Main/Barbarossa.hs is set to the branch name
- After merging another branch, check progVerSuff again: the merged branch brings its own identity

## Post-Build Deployment

- After a successful build, deploy executables with:
  - `python scripts/deploy_exes.py`
- The script renames and copies:
  - `Barbarossa.exe` to `Barbarossa-<version>-<branch>.exe`
  - `SelfPlay.exe` to `SelfPlay-<branch>.exe`
  - `TuneSGD.exe` to `TuneSGD-<branch>.exe`
- Destination folders are configured in `scripts/deploy_exes.py` and must match the local machine setup.
- The script copies all three executables from the stack install directory, also those which were not
  rebuilt (e.g. after `stack build ... Barbarossa:exe:SelfPlay`): they may be older builds, now with the new
  branch name.
- Copying fails while a deployed executable is running (locked); builds for measurements next to running
  jobs are copied under their own names (e.g. `SelfPlay-<branch>m.exe`).

## Build Flavours

- Normal build: `stack build`.
- Deterministic perf build of SelfPlay (history without random values, no QS collect payload):
  `stack build --flag Barbarossa:-qscollect --flag Barbarossa:reproselfplay Barbarossa:exe:SelfPlay`
  (`scripts/build-perf.sh`); `SelfPlay -B` shows the flavour.
- Switching between the flavours rebuilds everything.

## Verification of Changes

Refactorings of the eval, the search or the position state must not change the engine; check:
- Eval: `TuneSGD -X -i <dir with *-filt.csv> -o <out>` before and after, the `eval` column must be identical
  (the export also reports whether the eval is linear in the weights and its reconstruction error).
- Search: deterministic perf build before and after, `SelfPlay -P -i tests/data/tests.fen -d 10` (a local file:
  `tests/data/` is git-ignored) and a set of endgame positions: identical `Total nodes`. Normal builds are not
  deterministic (random history init).
- Move execution and incremental position state: `SelfPlay --perft --fen <fen> -d <n>` on the 5 standard perft
  positions (counts must match the known values; perft also checks the incremental PST sums at every node).
- A config with eval parameters (`-c`) changes the compiled engine only by its values: a "neutral" config
  (e.g. for self-play data, candidate = compiled engine) must set a weight to its current compiled value,
  which changes with every compiled-in tuning.

## SelfPlay and UCI Notes

- SelfPlay names its log file `selfplay-<start second>-<pid>.log`, in the current directory (only with `-l`;
  default is no log), so parallel processes in the same directory do not share it. If the log file cannot be
  opened or written, the error goes to `Barbarossa_collected_errors.txt` and SelfPlay continues without logging.
- Branch builds log at Info level by default (Origin lines included): for matches with fastchess or a GUI use
  `-l 5`.

## Line Endings

Before any build, check these line ending rules for all changes source files:
- no unnecessary empty spaces before an end of line
- restore Unix file format (LF only)

## Empty lines
Before any build, check these empty lines rules:
- no empty lines at the end of a source file
- no more than 1 consecutive empty lines in the source files
