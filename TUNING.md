# Tuning the HCE evaluation with TuneSGD - state of 2026-10-12

Branch `tsub` (from `cxig`). Experiment folder on the tuning machine: `C:\Learn\Tune\cxig`, scripts also in
`C:\Users\Nicu\Documents\GitHub\BarbarossaTuning`.
Strength is judged by node-limited SelfPlay matches (1000 nodes per move unless noted) against the compiled
weights; 42k games give +-3.1 Elo (95%), 6000 games +-8.

## Where we stand

- Tuning with the Texel-like loss on mid-game data (phase >= 120, k = 220-400) lost 20-50 Elo (runs 1-3).
- With data of all phases and the matching k (about 190) the same method gives a **candidate that gains about
  +5 to +6 Elo at half step**, validated on two opening sets and at 1000 and 2000 nodes (findings 13, 14, 16).
  The full step overshoots (-6). The candidate is `groups\val-open-s1\candidate.cfg` (92 integer values, the
  half step of `run4-all\tuned.cfg`). It is not in the engine yet.
- Where the strength is sensitive (material, passed pawn level) the loss and the strength agree; around the
  compiled values the strength is flat and the loss gains are small (findings 10, 11, 17).

## Setup

- Data: chilo-g4 (198M lines): PV leaf positions with the game result and the static score of the playing
  engine at the leaf (gen3 net). Preprocessed by `prep_data.py`: hash sample, dedup on the position key,
  result + 1 (0/1/2 = loss/draw/win, side to move), 2M records in 10 files.
  `data\` has game phase >= 120 (mid game only), `data-all\` has all phases except pure pawn endings
  (`--phase-min 1`); `data-all-600k\` is the first 3 files of it.
  Openings for the matches: `open\open-moves-uniq.fen` (23,535 unique, shuffled) and
  `open\open-chilo-eq.fen` (23,000 others, for validation).
- Loss (`-L 5`): `(q * sigmoid(score / k) + (1 - q) * result / 2 - sigmoid(eval / k))^2`.
  `-q` is the weight of the SCORE (q = 0: game result only = Texel), `-k` is the sigmoid scale in cp.
  With `-q 0` this is the Texel error. Differences: targets with |score| > 2000 are dropped, `posExactEval`
  has no special evaluations. Best k (compiled weights, parabola): all-phase data q = 0: 190, q = 0.2: 205,
  q = 0.4: 235; mid-game data q = 0: 200-225. The earlier 400 and the default 595 are wrong for this data.
- TuneSGD `-O` is a cyclic coordinate search with steps +-1, 2, 4, 8, 16 per dimension (not random).
  New on this branch: `-p` parameter subset (bare name = mid and end), `-c` start values from a config,
  interleaved mid/end order, `old -> new` report after each improvement.
- Tools: `run_tune.ps1` (one tuning, defaults `-K 220 -Q 0`; use `-K 190 -Data ...\data-all` for the best
  setting), `run_group.ps1` (one candidate vs compiled, `-Scale`, `-Skip`), `run_sharded.ps1` (3 parallel
  shards + merge), `merge_groups.ps1`, `run_sweep.ps1` (weight sweep with loss variants),
  `loss_vs_strength.py` (loss variants against the measured candidates), `prep_data.py`
  (`--phase-min`, `--prefix`).

## Findings

1. Tuning all 92 values with the loss gave worse engines: run1 (k=400, q=0.65) -49 Elo, run2 (k=220, q=0.65)
   -20, run3 (k=220, q=0) -25 (SPRT, +-12..17). The loss improved in every run (-0.7% result-only,
   -5% at q=0.65, up to -16% score-only). k explained about 29 Elo, q nothing (run2 = run3).
2. The three runs move the weights almost identically (correlation 0.8-0.9); about 65 of 90 weights end exactly
   at the step cap of 16. The signal is consistent but did not translate to strength on the mid-game data.
3. Group tests (6000 games, +-8 Elo; run3 values vs compiled): king +3.9, rook -11.9, material (x1.25) -3.7,
   mobility -15.9, center -4.7, pawns -4.0, misc -5.9. Half-step mirror tests were neutral (rook -1.7,
   mobility +1.1 / +2.1); queen mobility alone +5.5. Extreme groups regress to the mean; do not over-read them.
4. Large tests on the mid-game data (42k games, +-3.1): whole run3 at half step -2.4; the 12 untuned values
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
   The loss is very flat there and its minimum is far from the compiled value without strength changing.
8. Mid and end values of one weight trade off (U-shaped paths in the logs). The phase filter was only part
   of the reason (finding 12): the end material weight is still pulled about 50% above the compiled value
   with all phases. Every converged value is conditional on the other weights being at their compiled
   values.
9. Many compiled values carry a "texel2" / CLOP / DSPSA heritage and are probably close to the strength
   optimum (some are old: the code changed since).
10. Screening sweep (8 weights at x0.5 and x1.5 of the compiled value, 6000 games each, +-8 Elo; loss at
    k=220, q=0). Only `ewMaterialDiff` (x0.5: -95, x1.5: -36 Elo) and `ewPassPawnLev` (-24 / -12) are
    sensitive; the others stay within +-9 even at +-50%. Over the 14 non-material points the strength
    follows the loss: about -44 Elo per +1% loss (r = -0.82; with material -33, r = -0.93). The loss minimum
    and the strength peak coincide where strength is sensitive: material 1.23 against 1.11, passed pawn
    1.05 against 1.09 (parabola fits, +-0.1).
11. On the mid-game data the relation is one-sided. A loss increase costs strength, but a loss decrease does
    not buy it: the slope predicts +23 (all-h), +37 (run3) and +9 (untuned-conv), observed -2, -22 and +2.
    Model that fits: the strength optimum is at about the compiled values, the loss optimum is displaced in
    the weakly determined directions, and the Elo cost of moving there is about 25-45 Elo per 1% of loss
    gained.
12. Zero-start experiment: ewMaterialDiff and ewPassPawnLev (mid and end) started at 0, the other weights
    at their compiled values, 4 dimensions, `-Q 0 -K 220`. On the mid-game data it lands at mid/end material
    8 / 14 and passed pawn 3 / 6; match -6.2 +- 3.1 (42k games). On data of all phases it lands at 9 / 12
    and 1 / 9; match +0.7 +- 3.1. Both landings have the same loss gain on the mid-game data (-0.28%, which
    predicted -6 to -12 Elo), but the all-phase loss ranks them correctly (-0.82% against -1.15%). So the
    phase filter was part of the problem and the all-phase data is the better yardstick.
13. Full tune of all 92 values on the all-phase data (`run4-all`: `-Q 0 -K 190 -Fix -1 -Optim 184`, 2 rounds,
    80 minutes; loss -1.36% on the all-phase data, -1.04% on the mid-game data). Match at the full step
    -6.1 +- 3.1 (shards -4.4 / -6.8 / -7.2), at half step (`-Scale 0.5`) **+5.7 +- 3.1** (shards +7.1 / +4.6 /
    +5.4). The damage of the full step dropped from about -22 (run3) to -6, and a half step gains. Fit
    `a*s - b*s^2` through both points: a = 29 +- 7, b = 35 +- 7, peak at s = 0.4 with about +6 Elo (+- 4).
    The price of a loss gain is now about 5 Elo per 1% loss, against 25-44 on the mid-game data.
14. Validation of the half step on 23,000 openings that are not in the first set (balanced positions at
    move 6-14 from the chilo data), 1000 nodes, 42k games: **+6.2 +- 3.1** (shards +4.3 / +6.6 / +7.7).
15. The scores of the data are the static evals of the playing engine at the PV leaf (`Origin` log lines,
    `Tune/LogScan.hs`): gen3 net for chilo, older HCE versions for trainself. So q > 0 means imitating
    that engine's static eval, not matching a search score. Both data sets are leaf positions.
16. 2000-node validation of the half step on the original openings (42k games): **+5.2 +- 3.1** (shards
    +0.0 / +5.9 / +9.6). With finding 14: about +5.7, +6.2, +5.2 in three conditions.
17. Offline check of loss variants against the 35 measured candidates (`loss_vs_strength.py`, 600k all-phase
    positions): the loss change predicts Elo only through the few large deviations (material x0.5, passed
    pawn x0.5/1.5); r = -0.74 for L5 q=0 k=190, -0.67 q=0.2, -0.58 q=0.4, -0.63 q=1, -0.55 ... -0.66 for
    L1-L3, -0.11 for L4 (k=190). Without the 4 material / passed pawn sweep points all variants fall to
    |r| < 0.2, and on the 7 big tests (42k games) none correlates. Loss decreases do not predict gains for
    any variant. No variant is better than q=0; the test has little power for small differences.
18. Re-scoring with Stockfish 18 (`rescore_sf.py`, `analyze_labels.py`; 20k all-phase positions, Texel error of
    the label against the result, best k): gen3 static 0.0836 (k 200), SF depth 1 0.0986 (210), depth 6 0.0910
    (210), depth 10 0.0915 (220). The playing engine's own eval predicts its games' results best (on-policy);
    SF d6 is far better than d1, d10 adds nothing. A mix 0.7 gen3 + 0.3 SF6 gives 0.0816 (-2.4%), so SF
    carries independent information. Cost: depth 6 about 770 pos/s on 4 workers (2M = 45 minutes).
19. Tuning on 600k all-phase positions (`-Optim 184`): A SF6 labels q=0.3 k=240 (loss -2.4%), B gen3 q=0.3
    k=220 (-3.4%), C gen3 q=0 k=190 (-1.3%, same as run4-all on 2M), D SF6 score only (q=1, k=190, -9.3%).
    Weight deltas correlate 0.85-0.98 among A, B, C and run4-all, but D differs (0.68-0.77): mainly
    ewEnpAttacked (+30 mid, others -3..-8), ewEnpHanging, ewWepAttacked, ewPawnBlockO, ewBackPawns, ewKingPawn.
    Match of only D's en-prise group (ewEnpHanging/EnPrise/Attacked, ewWepTotal/Attacked) against compiled,
    6000 games: **-22.1 +- 8.2 Elo** (three shards -23.0, -20.9, -22.4). The largest loss gain was a strength loss:
    labels that already contain the capture outcomes of a search double-count with our own search.
20. k for large q: the loss has a minimum in k only through the result term. In the linear regime
    L(u) = (1-q)^2 E[r'^2] + 2(1-q) B u + A u^2 with u = 1/(4k), so k* grows like 1/(1-q). Measured on SF6
    labels (600k): q=0 about 190, q=0.3 245, q=0.5 about 400, q=0.7 none up to 700, q=0.9 none up to 4000,
    q=0.95 about 10000-20000. For q >= 0.7 k is a weighting choice (small k = win probability scale), not
    something the loss decides.
21. Distribution of the data: of the 198M lines about 133M positions are distinct (mean multiplicity 1.49);
    44.6% of the records are in positions that occur more than once, 12.3% in positions that occur more than
    10 times. The phase mix of records and of distinct positions differs by less than 1.5 points (opening
    phases 11.0% against 9.8%). `prep_data.py --natural` makes a uniform sample of the lines without dedup
    (`data-nat`, 2M, all phases except pawn endings; opening share +2 points against `data-all`). Runs on the
    second machine (Ryzen 7 8700F): `run7-nat` (data-nat, q=0, k=200), `run7-natq2` (data-nat, q=0.2, k=205),
    `run7-allq2` (data-all, q=0.2, k=205), compared with `run4-all`. Result: loss gains -1.32% (nat q=0),
    -2.19% (nat q=0.2), -2.38% (dedup q=0.2), -1.36% (run4-all); weight deltas correlate 0.94-0.98 with
    run4-all, mean difference to run4-all 3-4 units. Cross evaluation (q=0, k=200): every tuned config
    gains -1.42 .. -1.47% on data-all and -1.20 .. -1.32% on data-nat, differences <= 0.1%. The dedup /
    natural distribution does not change the optimum at this level.
22. Matches of the run7 candidates and of random controls (second machine, 5 shards x 2000 pairs = 20k games,
    +-4.5 Elo, 1000 nodes, against the compiled weights; groups `r7-*-h`, `rnd-*`):
    half step of run7-nat +5.2, of run7-natq2 +6.3, of run7-allq2 +3.9 (run4-all half step +5.7 at 42k);
    random +-1 on all 92 values (`rand-pm1.cfg`, mean change 1.0) **-10.1**; 10% of the value with random
    sign, weights <= 5 untouched (`rand-rel10.cfg`, 67 values, mean change 5.9) **-15.8**; the magnitudes of
    run7-nat's change with random signs at half step (`rand-sign-nat.cfg`, scale 0.5, mean change about 9)
    **-7.4**. Consequences: (a) the compiled weights are NOT in a flat region: small random changes cost 7-16
    Elo (sensitivity is per weight, not per size: +-1 on the tiny weights and 10% on the big ones cost more
    than the half-step sign-shuffled move); (b) the loss direction carries real information: at the same step
    size the tuned direction is 11-14 Elo better than a random one; (c) with the random half step at -7.4 the
    strength curvature b of a random direction is about 30 Elo per (full step)^2, the same as the tuned
    direction (a = 29, b = 35, finding 13): the overshoot at the full step is the generic cost of moving
    18 units per weight. The half step is not a step length of an optimisation (we jump to the loss
    minimum directly and a fraction of it has a HIGHER loss); it is a shrinkage towards the compiled point,
    which was established by other methods (CLOP, SPSA, SPRT-tested changes) and is good in the directions
    the loss cannot see. Iterating "tune again from the half step" therefore only walks back to the loss
    minimum and is not a valid plan. Each random control is a single draw.
    The three run7 variants give the same gain within noise, so data distribution and q do not matter.
    Full steps (same 20k games protocol): run7-nat -3.9, run7-natq2 0.0 (run4-all -6.1 at 42k). Through
    half and full step: a = 25, b = 29 (peak at s = 0.43, +5.3) for nat, a = 25, b = 25 (peak at 0.5,
    +6.3) for natq2; run4-all a = 29, b = 35. All within the noise of each other.
    Loss change of the same configs (data-all, L5 q=0 k=200 / q=0.2 k=205) against the Elo: rand-pm1
    +1.39% / +1.85% (-10.1), rand-rel10 +1.02% / +1.60% (-15.8), rand-sign half step **-0.21% / -0.47%
    (-7.4)**, run7-nat half step -1.00% / -1.59% (+5.2), run7-natq2 half step -1.13% / -1.85% (+6.3),
    run7-nat full step -1.46% / -2.35% (-3.9 +- 4.5); run7-natq2 full step (loss -2.35% at q=0.2) 0.0 +- 4.5. The loss sees the damage of the ±1 and 10% moves, but is
    blind to the random-sign half-step move: the loss improves while the strength drops 7 Elo. This
    config is a test case any candidate loss must rank correctly.

## Problems

- The full tuning step overshoots (finding 13): the loss minimum is not the strength optimum in the weakly
  determined directions. Its remaining source is not known (data, loss form, search/eval coupling).
- Around the compiled values the strength is flat: differences of a few Elo cannot be seen with +-3, so no
  proxy can be shown to correlate there. The loss gains we see (0.2-1.4%) are in the same range.
- The loss is a static predictor while the engine uses the eval as a leaf function of a search with fixed
  margins (futility `futMinVal`, null move, LMR constants), and also at interior nodes (pruning), which are
  not quiet and are not in the data.
- The eval does not see forks or one-move tactics (pawn push forks, check and capture). The en-prise terms
  are mover-only and linear in piece values.
- 42k games take about 50 minutes on this machine (3 shards, about 4.6 games/s each at 1000 nodes).

## Ways we see

Done: all-phase data and k fit (12, 13), validation (14, 16), loss variants offline (17).

1. (Withdrawn, see finding 22: iterating from the half step only walks back to the loss minimum.)
   Instead: find a loss that ranks the measured configs correctly (the random controls of finding 22 and
   the earlier sweeps are the validation set), then tune with it.
2. q = 0.2 with k = 205 on the all-phase data (full tune and half-step match). Needs 80 + 55 minutes.
3. (Done, negative: findings 18-20.) Re-scoring with a strong engine at depth 6: its labels predict the
   results worse than the playing engine's own eval; a score-only fit moves the en-prise terms and loses
   about 22 Elo on that group. If used at all, tune with the en-prise and Wep terms fixed.
3b. Natural data distribution (finding 21) and on-policy data: log the positions the HCE engine really
   evaluates in its games from the match openings, with game order for TDLeaf-style targets.
4. New eval terms: simplified en-prise (one weight on the number of own attacked pieces, rising),
   mover-only fork patterns (pawn push forking two pieces, knight fork squares, check and capture), knight
   outposts, supported pawns. Tune the new weights with the method, measure by match.
5. Decouple search from eval (variant build with LMR / futility off) to test the mismatch hypothesis; test
   the final candidate at 10k nodes per move and with a normal SPRT at a longer time control.
6. Reparametrise mid/end pairs (sum, difference) if the valleys remain a problem.
7. Piece value constants (`matPiece1`) with variant builds.
8. To adopt the candidate: write the 92 values into `Struct/Weights.hs` on a branch (the half step values are
   already integers), check by SPRT against the previous engine.
