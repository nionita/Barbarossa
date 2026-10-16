---
name: on-policy-tuning
description: One round of on-policy tuning of Barbarossa's HCE (weights + piece square table) - self-play data with real results, fit on top of the compiled eval, yardstick, compile-in with identity checks, matches. Use when the user wants a new training round, new self-play data, a refit of the eval weights or PST, or to compile a fitted candidate into the engine.
---

# On-policy tuning round (Barbarossa HCE)

Background and results: `TUNING.md` (state, lessons, ideas) and `TUNING-HISTORY.md` findings 24-36. The method
works only with positions from the engine's own games labelled with the real game results (on-policy); data of
other engines or adjudicated results (LogScan) made the engine weaker. Tools: BarbarossaTuning (`hv_*.py`,
`run_*.ps1`), next to this repository. Python: see the user's machine notes (on the mini PC
`C:\Users\Nicu\miniconda3a\envs\jacob\python.exe`).

## 1. Data: the compiled engine plays itself

- Neutral config (candidate = compiled engine): one weight set to its CURRENT compiled value in
  `Struct/Weights.hs` (e.g. `mid.ewKingSafe = <value>`). Check it: `TuneSGD -X` with and without `-c` on a small
  position set gives identical `eval` columns.
- Long runs go to the second machine (MEMO): pack `SelfPlay-<branch>.exe` and the config as a zip with the
  folder structure below `C:\` into `Z:\nicu\transf\<name>.zip`, give the `Expand-Archive` command and plain
  `.\run_sharded.ps1` commands (no ExecutionPolicy wrapper there).
- `run_sharded.ps1 -Name <run> -Config <neutral.cfg> -Shards 4 -Pairs <p> -Nodes <n> -DataDir <dir>` with
  `-Offset` / `-Fens` so that runs use disjoint openings (`open-moves-uniq.fen`, `open-chilo-eq.fen`).
  Speed on MEMO: about 1 game/s per shard at 10k nodes, about 100 positions per game.
- More nodes per move = better data but proportionally slower; the gains grew with depth.
- Back: `Compress-Archive` of the data dir to Z:, unpack, check the game count in the last line of each `.meta`.
- Keep the raw data in `C:\Learn\Data\raw\selfplay\<set>` (engine games: `raw\games\<set>`) and add the set to
  `C:\Learn\Data\DATA.md` (engine, branch and commit, config, nodes or time control, openings and range, games,
  positions); that folder is the backup set, derived data stays in the work folder.

## 2. Prep, export, fit

- `hv_prep_sp.py --src <data> --val <shard,...> --out <dir> --files 16`: validation = whole shards (game
  disjoint), split by game into va-01 (selection) and va-02 (test).
- Export with the CURRENT build: `TuneSGD-<branch>.exe -X -i <dir>\data-train -o <dir>\exp-train` (and data-val).
  The export puts the compiled PST into `const`; the fit learns corrections on top of the compiled eval, so the
  ridge prior (centered on 0) is centered on the current values.
- `hv_fit.py --train ... --val ... --out <fit> --models hce,hce-tuned,hce-tuned+pst --tau 1.25,2.5,5,10,20`.
  With free HCE weights k stays at the baseline value (scale and k are degenerate). If the best tau is at the
  edge of the grid, extend the grid.

## 3. Candidates and yardstick

- `hv_cfg.py --hce <fit>\hce-tuned+pst.cfg --pst <fit>\pst-hce-tuned+pst.csv --out cand\<name>.cfg`.
- Yardstick before any match: `TuneSGD -X -c cand\<name>.cfg` on the validation data (`cval-<name>`), then
  `hv_cand_eval.py --yard <dir> --cands <names>` (95% interval by game blocks). It ranks candidates well but its
  Elo scale has an offset: about -0.8 .. -1% of loss is needed before a candidate gains in play.

## 4. Matches

- `run_sharded.ps1 -Name <name> -Config cand\<name>.cfg -Shards 4 -Pairs 2500 -Exe SelfPlay-<branch>.exe`
  (base = compiled engine); 1000 nodes and 10k nodes (gains show more at depth). Results:
  `groups\<name>\result.txt`; a failed shard leaves `failed.txt`.
- Timed games: fastchess with both engines as `Barbarossa-<ver>-<branch>.exe -l 5`, openings with full move
  number 1 (`open-moves-uniq-fm1.fen`), a fresh `-pgnout` file per match.

## 5. Compile in (new branch or the current one, identity = branch name)

- Weights: config without `pst.` lines, `python scripts/config_to_weights.py <cfg> Struct/Weights.hs`.
- PST: the candidate holds corrections, so add them:
  `python scripts/config_to_pst.py --base Struct/Pst.hs <cfg> Struct/Pst.hs`.
- Check: the new build without config gives the same `eval` column as the old build with `-c <cfg>` on the
  validation positions (0 differences). Then build, deploy, update the neutral config for the next round.
- Record the round as a new finding in `TUNING-HISTORY.md` (data, fit numbers, yardstick, matches; a row in its
  summary table), update `TUNING.md` (state, lessons, ideas) and commit.
