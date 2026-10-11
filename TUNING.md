# Tuning the HCE evaluation with TuneSGD - state of 2026-10-11

Branch `tsub` (from `cxig`). Experiment folder on the tuning machine: `C:\Learn\Tune\cxig`.
Strength is always judged by node-limited SelfPlay matches (1000 nodes per move) against the compiled weights.

## Setup

- Data: chilo-g4 (198M lines, leaf QS positions with depth 6 scores of the gen3 net and the game result).
  Preprocessed by `prep_data.py`: hash sample 6%, game phase >= 120, dedup on the position key, result + 1
  (0/1/2 = loss/draw/win, side to move), 2M records in 10 files `data\cxig-NN-filt.csv`.
  Openings for the matches: `open\open-moves-uniq.fen` (23,535 unique, shuffled).
- Loss (`-L 5`): `(q * sigmoid(score / k) + (1 - q) * result / 2 - sigmoid(eval / k))^2`.
  `-q` is the weight of the SCORE (q = 0: game result only = Texel), `-k` is the sigmoid scale in cp.
  With `-q 0` this is the Texel error. Differences: targets with |score| > 2000 are dropped, `posExactEval`
  has no special evaluations. The best k for the results is 200-225 (k scan, result-only loss); the earlier
  400 and the default 595 are wrong for this data.
- TuneSGD `-O` is a cyclic coordinate search with steps +-1, 2, 4, 8, 16 per dimension (not random).
  New on this branch: `-p` parameter subset (bare name = mid and end), `-c` start values from a config,
  interleaved mid/end order, `old -> new` report after each improvement.
- Tools in the experiment folder: `run_tune.ps1` (one tuning), `run_group.ps1` (one candidate vs compiled,
  `-Scale`, `-Skip`), `run_sharded.ps1` (3 parallel shards + merge), `merge_groups.ps1`,
  `run_sweep.ps1` (weight sweep, see the plan below).

## Findings

1. Tuning all 92 values with the loss gave worse engines: run1 (k=400, q=0.65) -49 Elo, run2 (k=220, q=0.65)
   -20, run3 (k=220, q=0) -25 (SPRT, +-12..17). The loss improved in every run (-0.7% result-only,
   -5% at q=0.65, up to -16% score-only). k explained about 29 Elo, q nothing (run2 = run3).
2. The three runs move the weights almost identically (correlation 0.8-0.9); about 65 of 90 weights end exactly
   at the step cap of 16. The signal is consistent but does not translate to strength.
3. Group tests (6000 games, +-8 Elo; run3 values vs compiled): king +3.9, rook -11.9, material (x1.25) -3.7,
   mobility -15.9, center -4.7, pawns -4.0, misc -5.9. Half-step mirror tests were neutral (rook -1.7,
   mobility +1.1 / +2.1); queen mobility alone +5.5. Extreme groups regress to the mean; do not over-read them.
4. Large tests (42k games, +-3.1): whole run3 at half step -2.4; the 12 untuned values
   (see 6) at the run3 step +4.2; the same 12 converged by tuning +2.0.
5. The eval is a stack of terms with fixed shapes: material is a global multiplier (`ewMaterialDiff`, 8 = x1)
   over constants in `Moves/Core.hs` (`matPiece1`), the passed pawn curve comes from `ep*` params that
   TuneSGD cannot reach, several weights are small integers (step +-1 is a large relative change).
6. Untuned terms (commit df90eec 2026-02-12, values unchanged since): `ewKingProtB/N`, `ewRookHOpen` (average
   of old open / half-open), `ewRookKingFile` ("SF inspired"), `ewRookKingRank` ("guessed"),
   `ewCenterBAtts`. Converged: `ewRookKingRank` goes to 0 / 0 at no cost; the rest moved far without
   measurable strength change. The old `ewRook7th` was replaced by `ewRookKingRank` in that commit.
7. One-dimensional recovery: `mid.ewRookHOpen` from 247 and from 119 both converge to 75 (compiled 183) with
   a loss gain of only 0.058%; on HCE self-play data it goes even lower. A match at 75 gives +3.8 +- 8.1.
   The data source (gen3 net vs HCE) does not explain it. The loss is very flat and its minimum is far from the
   compiled value without strength changing.
8. Mid and end values of one weight trade off (U-shaped paths in the logs). Reason: the data has game phase
   >= 120 only, so end values are poorly identified, and every converged value is conditional on the other
   weights being at their compiled values.
9. Many compiled values carry a "texel2" / CLOP / DSPSA heritage and are probably close to the strength optimum
   (some are old: the code changed since).

10. Screening sweep (8 weights at x0.5 and x1.5 of the compiled value, 6000 games each, +-8 Elo; loss at
   k=220, q=0). Only `ewMaterialDiff` (x0.5: -95, x1.5: -36 Elo) and `ewPassPawnLev` (-24 / -12) are
   sensitive; the others stay within +-9 even at +-50%. Over the 14 non-material points the strength
   follows the loss: about -44 Elo per +1% loss (r = -0.82; with material -33, r = -0.93). The loss minimum
   and the strength peak coincide where strength is sensitive: material 1.23 against 1.11, passed pawn
   1.05 against 1.09 (parabola fits, +-0.1).
11. The relation is one-sided. A loss increase costs strength, but a loss decrease does not buy it: the
   slope predicts +23 (all-h), +37 (run3) and +9 (untuned-conv), observed -2, -22 and +2. Model that fits
   everything: the strength optimum is at about the compiled values (CLOP/DSPSA tuned), the loss optimum
   is displaced in the weakly determined directions, and the Elo cost of moving there is about 25-45 Elo
   per 1% of loss gained. The loss is a good local proxy for the cost of leaving the compiled values, but
   its minimum is not the strength optimum.

## Problems

- The displacement of the loss minimum against the strength optimum (finding 11), in directions where the data
  carries little information, is what makes the tuned configs lose. Its source is not known (data, loss
  form or search/eval coupling).
- Around the compiled values the strength is flat: differences of a few Elo cannot be seen with +-3, so no
  proxy can be shown to correlate there. The loss gains we see (0.2-0.8%) are in the same range.
- Loss and strength can disagree because the loss is a static predictor while the engine uses the eval as a
  leaf function of a search with fixed margins (futility `futMinVal`, null move, LMR constants).
- 6000 games (+-8 Elo) are too few for anything but large effects; 42k games (+-3) take about 50 minutes on
  this machine (3 shards, about 4.6 games/s each).

## Ways we see

1. Measure loss and strength where strength is sensitive: sweep a few weights far from the compiled value and
   compare the loss curve with the strength curve (plan below).
2. Use that to compare loss variants (k, q, `-L` form, cutoff) by how well they follow strength.
3. Decouple search from eval (variant build with LMR / futility off), to test the mismatch hypothesis.
4. Test candidates at 10k nodes per move (about 10x slower per game; only for final candidates).
5. New eval terms (knight outposts, supported pawns) and piece value constants: only place with larger
   potential, tuned with the method and measured by match.
6. Method fixes if needed: reparametrise mid/end pairs (sum, difference), include positions of all phases.

## Plan: weight sweep (`run_sweep.ps1`)

- Screening: 8 candidate weights (`ewMaterialDiff`, `ewPassPawnLev`, `ewKingThreat`, `ewCenterPAtts`,
  `ewSpace`, `ewBishopPair`, `ewIsolPawns`, `ewMobilityKnight`) at factors 0.5 and 1.5 of the compiled values
  (mid and end together), 6000 games each. A weight with |Elo| >= 20 is sensitive.
- Sweep: for the 2-3 most sensitive weights, factors 0.25 ... 1.75, 12000 games per point, and the loss at every
  point for several (k, q, form) variants.
- Read: argmin of the loss against argmax of the strength. If they coincide, the loss is fine and the
  landscape is flat at the compiled values. If they differ, choose the loss variant whose curve follows the
  strength best, and re-tune the sensitive weights with it.
