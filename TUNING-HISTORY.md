# Tuning history - what was tried and what it gave

The record of the tuning experiments on Barbarossa's HCE (2026-02 .. 2026-10). The current state, the general
lessons and the open ideas are in `TUNING.md`; findings are cited there by their number here.
Strength is judged by node-limited SelfPlay matches (1000 nodes per move unless noted) against the compiled
weights; 42k games give +-3.1 Elo (95%), 20k +-4.5, 6000 +-8.
Data paths in the findings are the original ones; the raw data, openings, configs and results are now in
`C:\Learn\Data` (catalog: `C:\Learn\Data\DATA.md`).

## Summary

| Findings | Tried | Result | Verdict |
|---|---|---|---|
| 1-4 | TuneSGD full tune (92 values) on chilo mid-game data, k 220-400, q 0 / 0.65 | loss -0.7 .. -16%, Elo -20 .. -49 | failed |
| 5-9 | Structure of the eval (fixed shapes, untuned terms, mid/end trade-off) | | knowledge |
| 10-11 | Sweep x0.5 / x1.5 of 8 weights | only material and passed pawn level sensitive; loss decreases do not buy strength | knowledge |
| 12-13 | All-phase data, matching k (190) | full tune -6 instead of -22 | better, still negative |
| 14 | Scores of the chilo data | static evals of another engine at its PV leaves | knowledge |
| 15 | Loss variants (q, L1-L4) offline against 35 measured candidates | none predicts gains | failed |
| 16-18 | Stockfish relabelling, score targets (q > 0) | en-prise group -22 Elo | failed |
| 19 | Dedup against natural distribution | same optimum | neutral |
| 20 | Random controls (+-1, 10%) | -10 / -16 Elo: the compiled point is not flat | knowledge |
| 21 | Search without eval margins (`nopr`) | full step still -7.9 | margins not the cause |
| 22-23 | Synthetic data without hidden variables | the method recovers a correct model; 2 rounds do not converge | knowledge |
| 24 | Export, linear fit, PST headroom on chilo data | -3.7% loss (62% of the HCE -> gen3 gap) | knowledge |
| 25 | chilo-fitted PST in play | -2.2 / -11.7 Elo | failed (off-policy) |
| 26 | Own games with LogScan-adjudicated results | -6 / -46 Elo | failed |
| 27-29 | On-policy data (`SelfPlay --data`, real results), joint HCE + PST fit, yardstick | `sp-both` +27.9 (1000 nodes), +86 (10k) | worked |
| 30-32 | `sp-both` with the play search, compiled incremental PST, timed games | +47.8 (1000 nodes), +90 timed vs old, +78.6 vs `boas` | worked, compiled |
| 33 | Pure piece-square terms folded into the PST | identical eval and search, 3 weights less | done |
| 34 | Round 2 of the on-policy iteration (10k nodes) | -0.39% loss, +0.3 (1000 nodes) / +6.8 (10k) | compiled (small) |
| 35 | Futility margins: harmful cut rates, 5 direction tests at 50k nodes | all directions -14 .. -45 | defaults are a local optimum |
| 36 | Round 3 (50k nodes data), joint fit with round 2 data | -0.2% loss | fixed point, not compiled |
| 37 | Hidden variables: drawishness scale (bias tables, fitted scale factors, 50 moves scale) | -2.0% loss; +10.6 at 10k nodes, timed +3.7 +- 5.7 (8000 games); strong 50 moves scale -4 | worked, small (about +4 Elo) |

Old plan items (2026-10-13) and what became of them: a loss that ranks the measured configs (replaced by the
on-policy yardstick, 29); q = 0.2 on the all-phase data (obsolete: on-policy data is fitted on the result only);
relabelling with a strong engine (negative, 16-18); natural / on-policy data (done, 19, 27); decoupling search
and eval (done, 21: not the cause); reparametrising mid/end pairs (obsolete: the joint L-BFGS fit has no
valleys problem); piece value constants with variant builds (done differently: material folded into the PST,
33); confirm `sp-both`, incremental PST, timed tests, redundant terms, iteration (done, 30-34, 36).

## Setup of the TuneSGD phase (findings 1-23)

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
  setting), `run_group.ps1` (one candidate vs compiled, `-Skip`), `run_sharded.ps1` (3 parallel
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
   mobility -15.9, center -4.7, pawns -4.0, misc -5.9. Queen mobility alone +5.5. Extreme groups regress to
   the mean; do not over-read them.
4. Large tests on the mid-game data (42k games, +-3.1): the 12 untuned values (see 6) at the run3 step +4.2;
   the same 12 converged by tuning +2.0.
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
    80 minutes; loss -1.36% on the all-phase data, -1.04% on the mid-game data). Match -6.1 +- 3.1
    (shards -4.4 / -6.8 / -7.2). The damage dropped from about -22 (run3) to -6. The price of a loss gain is
    now about 5 Elo per 1% loss, against 25-44 on the mid-game data.
14. The scores of the data are the static evals of the playing engine at the PV leaf (`Origin` log lines,
    `Tune/LogScan.hs`): gen3 net for chilo, older HCE versions for trainself. So q > 0 means imitating
    that engine's static eval, not matching a search score. Both data sets are leaf positions.
15. Offline check of loss variants against the 35 measured candidates (`loss_vs_strength.py`, 600k all-phase
    positions): the loss change predicts Elo only through the few large deviations (material x0.5, passed
    pawn x0.5/1.5); r = -0.74 for L5 q=0 k=190, -0.67 q=0.2, -0.58 q=0.4, -0.63 q=1, -0.55 ... -0.66 for
    L1-L3, -0.11 for L4 (k=190). Without the 4 material / passed pawn sweep points all variants fall to
    |r| < 0.2, and on the 7 big tests (42k games) none correlates. Loss decreases do not predict gains for
    any variant. No variant is better than q=0; the test has little power for small differences.
16. Re-scoring with Stockfish 18 (`rescore_sf.py`, `analyze_labels.py`; 20k all-phase positions, Texel error of
    the label against the result, best k): gen3 static 0.0836 (k 200), SF depth 1 0.0986 (210), depth 6 0.0910
    (210), depth 10 0.0915 (220). The playing engine's own eval predicts its games' results best (on-policy);
    SF d6 is far better than d1, d10 adds nothing. A mix 0.7 gen3 + 0.3 SF6 gives 0.0816 (-2.4%), so SF
    carries independent information. Cost: depth 6 about 770 pos/s on 4 workers (2M = 45 minutes).
17. Tuning on 600k all-phase positions (`-Optim 184`): A SF6 labels q=0.3 k=240 (loss -2.4%), B gen3 q=0.3
    k=220 (-3.4%), C gen3 q=0 k=190 (-1.3%, same as run4-all on 2M), D SF6 score only (q=1, k=190, -9.3%).
    Weight deltas correlate 0.85-0.98 among A, B, C and run4-all, but D differs (0.68-0.77): mainly
    ewEnpAttacked (+30 mid, others -3..-8), ewEnpHanging, ewWepAttacked, ewPawnBlockO, ewBackPawns, ewKingPawn.
    Match of only D's en-prise group (ewEnpHanging/EnPrise/Attacked, ewWepTotal/Attacked) against compiled,
    6000 games: **-22.1 +- 8.2 Elo** (three shards -23.0, -20.9, -22.4). The largest loss gain was a strength loss:
    labels that already contain the capture outcomes of a search double-count with our own search.
18. k for large q: the loss has a minimum in k only through the result term. In the linear regime
    L(u) = (1-q)^2 E[r'^2] + 2(1-q) B u + A u^2 with u = 1/(4k), so k* grows like 1/(1-q). Measured on SF6
    labels (600k): q=0 about 190, q=0.3 245, q=0.5 about 400, q=0.7 none up to 700, q=0.9 none up to 4000,
    q=0.95 about 10000-20000. For q >= 0.7 k is a weighting choice (small k = win probability scale), not
    something the loss decides.
19. Distribution of the data: of the 198M lines about 133M positions are distinct (mean multiplicity 1.49);
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
20. Matches of the run7 candidates at the full step and of random controls (second machine, 5 shards x 2000
    pairs = 20k games, +-4.5 Elo, 1000 nodes, against the compiled weights; groups `r7-*`, `rnd-*`):
    run7-nat -3.9, run7-natq2 0.0 (run4-all -6.1 at 42k); random +-1 on all 92 values (`rand-pm1.cfg`,
    mean change 1.0) **-10.1**; 10% of the value with random sign, weights <= 5 untouched (`rand-rel10.cfg`,
    67 values, mean change 5.9) **-15.8**. Consequence: the compiled weights are NOT in a flat region: small
    random changes cost 10-16 Elo (sensitivity is per weight, not per size: +-1 on the tiny weights and 10%
    on the big ones cost a lot). Each random control is a single draw.
    Loss change of the same configs (data-all, L5 q=0 k=200 / q=0.2 k=205) against the Elo: rand-pm1
    +1.39% / +1.85% (-10.1), rand-rel10 +1.02% / +1.60% (-15.8), run7-nat full step -1.46% / -2.35%
    (-3.9 +- 4.5); run7-natq2 full step (loss -2.35% at q=0.2) 0.0 +- 4.5. The loss sees the damage of the
    +-1 and 10% moves.
21. The search without eval margins (branch `nopr`: futility, null move and QS delta cut off, LMR on; in the
    old SelfPlay futility and LMR were already off through the tuning flag, null move and delta cut were on).
    run7-natq2 against the compiled weights, 12k games (+-5.8, second machine, 4 shards x 1500 pairs): full
    step **-7.9** (shards -1/-5/-15/-10). Old search at 20k games: 0.0. The full step is not better without
    the margins (within noise), so the search margins are not what limits it (plan item 5 rejected). Open:
    hidden variables of the feature set (proxy terms), data type (PV leaves of another engine), compiled
    point already close to the strength optimum of this feature set. Proposed test: relabel the positions
    with the compiled eval (no hidden variables by construction), start from the measured random configs
    (rand-rel10 -15.8, rand-pm1 -10.1), tune, match; strength back to 0 means the method works for a correct
    model and the problem is the feature set.
22. Synthetic control without hidden variables (`TuneSGD -E SEED`: results drawn from the static eval of the
    compiled weights, win p^2, draw 2p(1-p), loss (1-p)^2 with p = sigmoid(eval / 200), same 600k positions,
    `data-syn-600k`; no search involved). The compiled weights are the loss minimum (k = 200: 52106, every
    other config higher: rand-pm1 +2.07%, run4-all +1.04%, run7-natq2 +1.35%, rand-rel10 +0.32%). Tunings
    (q = 0, k = 200, -Optim 184, run on the second machine) and matches in `nopr`, 12k games (+-5.8) against
    the compiled weights:
    A (start compiled): loss -0.008%, mean change per weight 2.2, strength +1.4 at full step (old search
    20k games: +0.9): the noise floor of the method is 0; the real data moves 17.3 per weight (48 values at
    the cap) and gives -7.9 at full step, so the real movement is signal about a displaced optimum, not
    noise.
    B (start rand-rel10, -11.9): loss back to the compiled level (+0.0006%), mean distance 5.6, strength -3.5.
    C (start rand-pm1, -7.1): loss +0.12% above compiled, mean distance 6.9 (tiny weights moved by 5.4,
    large ones by 11), strength -5.2. Partial recovery at best, not distinguishable from full recovery (B)
    at +-8 for the differences.
23. Convergence: 184 steps (2 rounds) do not settle. In round 2 of the real runs 77-82 of 92 steps still
    improve and round 2 gives 23-30% of the loss gain; the largest weight change is 32 (2 x the cap of 16).
    The synthetic runs also still improve (syn-b 50 steps, 17% of the gain in round 2). So the "full step"
    measured so far is the point after two sweeps, not the loss minimum; B and C are intermediate states.
    Continuations with 4 more rounds (-Optim 368, `-Config` from the previous tuned.cfg): `run6-gq0-x`
    (real, 600k), `syn-a2`, `syn-b2`, `syn-c2`.
24. Export and headroom (branch `hvar`, folder `C:\Learn\Tune\hvar`). `TuneSGD -X` writes for every position
    the HCE eval, the 46 feature values and pseudo derivatives: the eval is exactly linear in the weights
    before phase, scale factor and 50 moves (reconstruction <= 1.1 cp). `hv_fit.py` (BarbarossaTuning) fits on
    top of it, k and offset refitted per model, the ridge prior of the PST chosen on held-out data. Chilo
    all-phase data, 4M training, 2 x 500k validation from 9 held-out source files (game and position
    disjoint), test loss against the HCE: PST (antisymmetric, own piece +1 / opponent -1, 2 x 384) -2.74%
    (46% of the HCE -> gen3 gap), full refit of the 92 weights -2.01%, both -3.69% (62%). Of the PST gain the
    placement is -1.80%, the material part (per piece means) -0.69%, only jointly, as a rescaling (k 191 ->
    219); pawns and rooks carry most of the placement (7th rank and kingside pawns, rooks on the opponent's
    kingside).
25. The chilo PST in play (1000 nodes, 20k games, search of `nopr`): `pst40` (full PST) -2.2 +- 4.5,
    `pst40-pos` (placement only) -11.7 +- 4.5. On HCE self-play positions (`data-self`) the same PST makes the
    loss worse (+1.34% / +2.21%), in the order of the matches: fitted on gen3 games it describes gen3's play
    (a proxy), not what the squares are worth to this engine.
26. The `data-self` labels are not game results: LogScan adjudicates from Barbarossa's own last 5 search scores
    (all |s| <= 100 draw, all >= 450 win or loss, otherwise the game is dropped). Fits on it improved its own
    validation loss (-0.60% / -1.43%) and lost in play: `self-pst` -6.4 +- 4.5, `self-both` -46.2 +- 4.6.
27. On-policy data with real results: `SelfPlay --data` writes the evaluated leaf of every move (as in the
    Origin log lines) with the real game result, plus a .meta file (game, ply, root score). The compiled
    engine against itself, 10k nodes, 40k games, 4.2M positions (two runs on `open-moves-uniq.fen`).
    Yardstick: loss of a candidate's eval (exported with `-c`) on held-out games of this kind, c and k fitted
    on one half, loss on the other, 95% interval by a bootstrap over games (positions of a game share the
    result). It ranked the four candidates of 25 and 26 like the matches (r about -0.8) and predicted 26
    correctly, where the LogScan labels predicted the opposite.
28. Fit on these positions (`sp10k`: 3.5M training, validation shard `gen10k-b-s4`) and matches (1000 nodes,
    20k games each against the compiled weights): `sp-tuned` (92 weights refitted) **+6.7 +- 4.5** (all 4
    shards positive), `sp-pst` (compiled HCE + PST) +1.7 +- 4.5, `sp-both` (HCE and PST fitted jointly, tau 5)
    **+27.9 +- 4.5** (shards +25 .. +33). The same kind of full refit that lost on chilo data (13: -6.1) gains
    on on-policy data with real results, so the earlier failures came from the data (another engine's
    games, adjudicated labels), not from the loss or the search method. The PST helps only when fitted
    jointly with the HCE weights: alone on the fixed HCE it takes over material values (end game queen +208,
    rook +92 cp) and is neutral; jointly the per piece means stay within +-9 cp and the HCE adapts (end game
    material x1.19, bishop pair 386 -> 275 mid).
29. Independent yardstick: 40k on-policy games on `open-chilo-eq.fen`, never used in a fit (test half 20k
    games, +-0.2%). Loss against the compiled weights: `pst40` -1.00, `pst40-pos` +0.01, `self-pst` +0.71,
    `self-both` +1.47, `sp-pst` -1.55, `sp-tuned` -1.44, `sp-both` -1.85, `hce-tuned` (chilo fit) -0.24 (%).
    It ranks well but has an offset (`pst40` at -1.0% played -2.2): the positions are those of the compiled
    engine, so a candidate that changes the play is measured partly off-policy; the Elo scale is rough (the
    line through the played points predicted about +15 for `sp-both`). Confirmation of `sp-both`: on
    `open-chilo-eq.fen` (20k games, 1000 nodes) **+19.1 +- 4.5** (shards +12 .. +25), at 10k nodes per move
    (the regime of the data, 4k games) **+86.2 +- 10.0** (shards +75 .. +100). All matches of 24-29 ran with
    the search of `nopr` (no futility, null move, QS delta cut) on both sides.

30. `sp-both` with the play search (branch `pstf`: futility, null move, QS delta cut on again; candidate with
    the config PST against the compiled weights): 1000 nodes **+47.8 +- 4.5** (20k games, shards +44 .. +51),
    10k nodes **+74.9 +- 9.6** (4k games). The eval-margin cuts, set for the old eval, do not eat the gain.
    Speed (`pstf`, depth 10 on `tests/data/tests.fen`, deterministic perf build): old engine 636k nodes/s,
    `sp-both` with the config PST (all pieces at every eval) 566k (-10.9%), with the compiled incremental
    PST (Struct.Pst, sums in MyPos like mater) 620k (-2.5%); the rest is the bookkeeping in the move
    execution (the old eval on the new build: 614k). `sp-both` is compiled into `pstf` (Struct.Weights,
    Struct.Pst); the old engine is the same build with `results/hvar/old-compiled.cfg` (BarbarossaTuning:
    old weights and `pst.off = 1`). Not yet tested: timed games.
31. Timed games (fastchess on MEMO, 10+0.1 s, 2000 games, `open-moves-uniq.fen` with full move 1, both
    engines the same `pstf` binary, `-l 5`): `sp-both` compiled (new) against the old weights with
    `pst.off = 1` (old): **+90.0 +- 12.5 Elo** (62.7%, W/L/D 960 / 453 / 587, pentanomial 49 / 109 / 338 /
    294 / 210). Same binary: this is the eval at equal speed, the cost of the PST bookkeeping (2-3% nodes/s
    against the hvar binary) is not in it. A first attempt was invalid: the UCI parser read the FEN full
    move number as one character, fastchess sent 32768 for the openings with full move 0, the moves after
    the FEN were dropped and the engines played illegal moves (fixed in `pstf`, b6e9621; openings now with
    full move 1).
32. Against the best previous version `boas` (own binary, everything included: eval, PST cost, search
    changes since then), same conditions (10+0.1, 2000 games): **+78.6 +- 12.7 Elo** (61.1%, W/L/D 946 / 501 /
    553, pentanomial 56 / 129 / 337 / 270 / 208). With 31: `old` (pstf code, old eval) and `boas` are about
    equal (difference about 11 +- 18), the gain is the new eval.
33. Branch `fold`: the pure piece-square terms are folded into the compiled PST: material (`ewMaterialDiff`
    times `matPiece1` on every square of each piece type), `ewRookPawn` (pawns on the a and h files),
    `ewCenterNOcc` (knights on c3-f6); the terms and their 3 weights are removed (43 weights), and the `mater`
    field of the position (its last users, two special evaluations, compute it from the bitboards). The
    eval is identical (505k positions) and the deterministic search too (same node counts on tests.fen and
    10 special endgame positions). Not pure PST and kept: `ewAdvPawn5/6` (non passed pawns only) and
    `ewKingPlaceCent` (opponent material, queens, pawn shelter).
34. Round 2 of the on-policy iteration: data of the new engine (`pstf`, `sp-both` compiled), 80k games at 10k
    nodes (8.1M positions), validation 2 shards (10k games, both opening files), export of `fold`. The fit
    gains much less: `hce-tuned` -0.20 +- 0.12%, `hce-tuned+pst` (`r2-both`) -0.39 +- 0.19%, the ridge prior
    hardly matters (tau 1.25 .. 20 equal on the selection half): near the fixed point of the iteration.
    `r2-both` against the compiled `fold` (SelfPlay): 1000 nodes +0.3 +- 4.4 (20k games), 10k nodes +6.8 +- 6.5
    (8k games): a small gain that shows only at depth. Learning curve of round 1 (joint fit on 25 / 50 / 100% of
    its data, independent yardstick): -1.16 / -1.53 / -1.85%, about 0.33% per doubling, no saturation then.
35. Futility margins (`fold`, `24 << (d-1)`: 24 .. 768 cp for d = 1 .. 6), measured on 5k quiet positions of the
    round 2 games with `SelfPlay -P --perf-quiet` (per depth the best value of the prunable quiet moves and of
    the other moves, each child searched to d - 1, QS for d = 1) and `hv_margins.py` (BarbarossaTuning). A cut
    is harmful when the best skipped quiet move beats both static + margin (the smallest alpha that prunes,
    worst case) and the best searched move. Harmful at the current margins (old eval / new eval): d1 12.3 /
    14.4%, d2 8.1 / 8.9%, d3 4.7 / 5.0%, d4 2.4 / 2.4%, d5 1.0 / 1.1%, d6 0.20 / 0.25%. Margins for 2% harmful
    cuts (new): 72, 144, 192, 220, 244, 292; for 1%: 84, 248, 332, 356, 404, 424. So the doubling schedule takes
    very different risks per depth (aggressive at d1-2, nearly no pruning at d5-6), and the new eval needs
    slightly larger margins than the old one (5-10% more cp for the same risk; the broad measure with all
    moves agrees), not smaller. Equal risk is not necessarily best (shallow errors are cheap, shallow nodes
    many); candidates for node-limited matches at 50k nodes: A 72/144/192/224/248/296, B 24/48/96/192/256/320,
    C 48/96/144/192/384/768.
    The margins are now eval parameters `epFutil1 .. epFutil6` (config); futility prunes in PV nodes up to depth
    5, in non-PV nodes up to 6 (guard `d >= maxFutilDepth && pv`). Direction tests at 50k nodes against the
    defaults, 100 games each (+-55): A -45, B -17, C -38 (all negative, none significant alone); D
    16/32/96/192/384/768 (more pruning at d1-2) 400 games: -23.5 +- 26.7 (W/D/L 109/155/136); E 24/48/96/192/
    30000/30000 (no futility at d5-6: symmetric up to 4 in PV and non-PV nodes) 400 games: -13.9 +- 26.6
    (114/156/130). All five directions negative (weighted mean about -18 Elo over 1400 games): the default margins are
    a local optimum of this search, although their measured risks per depth differ strongly; equal risk per depth
    does not predict strength (as in the chilo BO experiments). Futility at d5-6 probably helps a little (E, not significant).
    Per game phase (5% harmful cuts, old / new, d1 .. d6): middle game (phase > 150) 56/80/100/112/136/140 and
    60/72/92/104/124/132; transition (80 .. 150) 52/92/124/152/176/188 and 56/104/124/152/164/204; end game (< 80)
    40/64/80/108/124/132 and 48/72/96/116/128/140. The transition needs the largest margins (+30-40% from
    depth 3), the end game not more than the middle game; the new eval moved the end game up (its scale).
    Scale: the margins are in cp of the eval; when tuning changes the scale (new eval: pawn 89 cp in the
    middle game, 130 in the end game, against 100 / 100), the risk of fixed cp margins changes. Ideas for later
    (after the next training round, user decision: no futility tuning now): margins in units of the eval
    (e.g. pawn value of the phase), phase dependent margins, a fixed k for all fits. Data, binaries and commands
    to repeat: C:\Learn\Tune\hvar\margins (README.txt), `hv_margins.py --quiet-rest [--by-phase]`.
36. Round 3 of the on-policy iteration: data of `fold` with `r2-both` compiled, 25k games at 50k nodes
    (gen3 a/b on MEMO, c here; 2.6M positions), validation 2 shards (5k games, both opening files), training
    2.0M positions. Fit (tau 0.6 .. 10, best 0.6): `hce-tuned` -0.22%, `hce-tuned+pst` -0.25% (test); the PST
    adds almost nothing, the selection half prefers the HCE refit alone. Yardstick (va-02, k and c refitted):
    `r3-tuned` -0.17 +- 0.20%, `r3-both` -0.22 +- 0.19%, half step -0.11 +- 0.08%. Joint fit with the round 2
    data (8.9M positions, re-exported with the current build, best tau 2.5): `r3j-tuned` -0.07 +- 0.04%,
    `r3j-both` -0.17 +- 0.15%, not better. All far below the yardstick offset (-0.8 .. -1%; `r2-both` at -0.39%
    was +0.3 Elo at 1000 nodes, +6.8 +- 6.5 at 10k): the HCE with PST is at the fixed point of the iteration
    for this engine, more data or deeper data of its own games does not move it. The deeper data is more
    predictable: k 148 (50k nodes) against 163 (10k nodes) for the same eval. Not matched, not compiled in.
37. Hidden variables at the fixed point (branch `scale`). Bias tables (`hv_bias.py`) on the round 3 positions: per
    group of a descriptor an own offset and k, fitted on training and measured on held-out games. The joint model
    of all descriptors gains 3.0% of the loss, almost all as scale (k), not offset; forward selection: pawn count
    2.0%, material signature +0.5, halfmove clock +0.3, opposite bishops +0.13, pawn wings +0.07, the rest nothing.
    The missing variable is drawishness: with few pawns, opposite bishops, pawns on one wing, rook or queen
    endings and a high 50 moves counter the eval is far too confident. The linear fit cannot express it (a
    factor on the eval), the hand made `scaleFactor` rules were never tuned.
    Export: `TuneSGD -X` writes eflat / eend (eval before the scale factor), special, sknow (knowledge part of
    the scale factor). `hv_scale.py` fits eval' = (eflat + eend * s0) * s(position) * g(m50), s = product of table
    factors (pawns of the stronger side 0 .. 8+, opposite bishops only / with pieces, stronger pawns on one wing,
    R v R, Q v Q), g = (1 - m50 / 100) ^ p (p = 2.2 fits as well as a free quadratic or a table; the old linear
    (128 - m50) / 128 gives 0.4% less). Engine: eval parameters epScaleNew (switch, default 0 = old), epScP0 ..
    epScP8, epScOcbOnly, epScOcbPcs, epScOneWing, epScRookEnd, epScQueenEnd, epSc50Pow (< 0: old linear);
    with the switch off the eval is identical (504k positions), with it on it agrees with the Python model
    (mean 0.5 cp).
    Matches against the compiled engine (same binary), 1000 nodes 10k games / 10k nodes 8k games:
    `scale-pow` (free fit, normalized to the old k: middle game eval x 1.27 by mistake) -12.8 / -;
    `scale-cap-lin` (pawn factors capped at 1, linear 50 moves) -3.2 / -; `scale-c-lin` (constrained fit:
    factors <= 1, pawn factors monotone, no normalization, linear 50 moves: 22/36/52/59/63 for 0 .. 4 pawns, 64
    from 5, opposite bishops only 23, with pieces 53, one wing 55, R v R 59, Q v Q 50) -1.1 / **+10.6 +- 6.3**;
    `scale-c-pow` (the same with (1 - m50 / 100) ^ 2.2) -3.9 / -4.0. Yardsticks (self-play / engine games):
    scale-c-lin -1.98 / -1.44%, scale-c-pow -2.43 / -1.92%.
    Lessons: (1) an eval change must keep the cp scale of the middle game (margins and other cp constants), so
    factors are 1 in normal positions and no normalization to the old k; (2) the constraint (<= 1, monotone)
    costs no loss: the free fit's factors above 1 came from a small reference group (8+ pawns: 1.2%);
    (3) the strong 50 moves scale predicts the result (high counter = game already drawn) but as an eval term it
    costs about 15 Elo against the linear one: a predictor is not necessarily a good lever for move choice;
    (4) the gain shows only at depth (as all eval gains before).
    Further tests: 50 moves scale with two segments (old slope to 20 / 40 halfmoves, then to 0 at 100): the loss
    prefers a steep start again (best knee 30 at 0.37, like the power form), K20 +4.2, K40 0.0 at 10k nodes, so
    the old linear scale stays; the three variants with the same tables give about +5 +- 4 together. Timed
    (10+0.1, fastchess, same binary, config against old scaling): 2000 games -1.6 +- 11.3 with the first,
    allocating `drawScale` (about 3% slower), 8000 games **+3.7 +- 5.7** (LOS 90%) with the optimized one (as
    fast as `fold`). Gauntlet against 6 engines: ug-b (new) 54.8% against ug-a (old) 54.5%, +2 +- 13. Refit of
    weights and PST under the new scaling with own and engine data (fit4, 14 sources, `scale-r4`): yardsticks
    -2.20% (self-play), -2.12 / -1.95% (engine games) against -1.98 / -1.44 / -1.16% for scale-c-lin, but at
    10k nodes in self-play +10.6 +- 6.4, the same as scale-c-lin: its gain, if any, is against other engines
    (two-version gauntlet ug-c: both versions against 6 engines with the same openings, 2400 games each: r4
    48.4%, clin 48.3%, paired r4 - clin +0.6 +- 16.5 Elo, head-to-head 50.4% for r4 in 400 games: no difference,
    r4 not compiled in). Verdict: the drawishness scale is worth about +4 Elo; compiled in (scale-c-lin, 52f0194).

How to repeat: the commands are in `README.md` of BarbarossaTuning, section "On-policy tuning of the HCE".
Engine changes on `hvar`: `TuneSGD -X` (export), optional PST read from the config (`pst.mid.Ne4 = 37`, 1/8 cp,
computed from all pieces at every eval: slow), `SelfPlay --data`.

## Problems seen up to round 1

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
- The PST of `hvar` is computed from all pieces at every eval; node-limited matches do not see its cost. Before
  timed tests it must become incremental (with the moves, replacing the material term).
