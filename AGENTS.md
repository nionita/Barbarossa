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
- Barbarossa (UCI) names its log `barbarossa-<version>-<branch>-<start second>-<pid>.log`, in the current
  directory: engines started in the same second (fastchess concurrency) do not share it (before, they wrote
  interleaved lines into one file). Log errors are collected as in SelfPlay, the engine goes on without log.
- Training data from games against other engines: `-d` (works with `-l 5`) writes per search one line
  `S|ply|start|root fen|root score|leaf score|leaf fen|best move` (leaf as SelfPlay `--data` takes it) and `N` at
  every ucinewgame into `barbarossa-<version>-<branch>-<start second>-<pid>.data` in the working directory;
  BarbarossaTuning `uci_data.py` joins it with the fastchess PGN.

## Cutting a Release

A release is made on `master`; the development branch keeps its identity and logging.

1. On the development branch: everything committed and pushed, no compiler warnings, the checks of
   "Verification of Changes" passed, the final strength measurement done.
2. Fast forward `master` to the development branch: `git checkout master`, `git merge --ff-only <branch>`.
3. On `master`, the release identity: `progVerSuff = ""` in `Main/Barbarossa.hs` (the version alone, e.g.
   `0.8.0`, no suffix). With an empty suffix the default logging is `LogNever` (`logOptDefault`): no log file
   without `-l`.
4. `CHANGELOG.md`: a section for the new version with the changes since the last release only: hints at
   what changed and its benefit (if not self-explaining), no technical details, nothing that was tried and
   taken back.
5. `README.md` and `BUILDING.md`: the last released version, strength statements, build hints still true.
6. Release build (speed flavour, without the QS collect payload), on Windows and on Linux:
   `bash scripts/build-prod.sh` (`stack build --flag Barbarossa:-qscollect Barbarossa:exe:Barbarossa`). The
   identity check accepts the empty suffix only on `master`; the release engine is then copied to
   `dist/Barbarossa-<version>.exe` (Windows) or `dist/Barbarossa-<version>` (Linux): these are the files for
   the GitHub release. `--deploy` additionally runs `scripts/deploy_exes.py` (needs `python` on the PATH, local
   Windows folders only). Check the UCI `id name` (`Barbarossa <version>`) and that no log file is written.
7. Commit on `master`, tag with the version (e.g. `0.8.0`, like `0.7.0`), push `master` and the tag.
8. The next development starts on a new branch from `master`: raise `progVersion` (and the version in
   `Barbarossa.cabal`) and set a suffix: all unreleased versions have one (e.g. `boas`).

## Line Endings

Before any build, check these line ending rules for all changes source files:
- no unnecessary empty spaces before an end of line
- restore Unix file format (LF only)

## Empty lines
Before any build, check these empty lines rules:
- no empty lines at the end of a source file
- no more than 1 consecutive empty lines in the source files
