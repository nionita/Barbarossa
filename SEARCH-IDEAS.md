# Search ideas - the register (0.9.0 development)

One block per idea, always current: what was tried in which form, what came out, how it is read today. The numbers and how they
were obtained are in the log `SEARCH-TUNING.md` (S1, S2, ...; speed: `PROFILING.md`); this file holds the interpretation, which may
change with more evidence. Both live on `rel090` (before 2026-11-13 on `dev090`). How to test and how to keep this file: skill `search-idea-test`.

Status: open (not tried), running, promising, adopted, parked, rejected in this form. "Parked" and "rejected" name the form tested, not
the idea. Elo from the candidate's side with 95% error bars. "Adopted" means in the best branch, nothing is merged into `rel090` yet.

Older branches (before 0.8.0, e.g. `lmrfix`, `lmrfix14`, `hlmr`, `slmr`, `ttlmr`, `hiss`, `mpru`) hold earlier tries of some of these ideas; they
are not reviewed here. The user's experience: negative history and squared bonuses lost Elo each time.

## Now (2026-11-17, last added 2026-11-19)

- Base: `chcm` (S26) = `fbas` + counter moves + continuation history, +22.5 +- 8.0 against `expo` (S27, 4000 games). `fbas` (S24) = `expo`
  (`nmlm` with cleanups, +13.0 vs `hprf`) + filter + T100. At 60+1 `chcm` against `dev090` +82.5 +- 16.0, 1000 games (S46): the short
  screening gains arrive at the long time control.
- LMR, the main line (blocks "Retune of the adaptive LMR" and "LMR cap depending on the size of the search"): at 192+3.2 (boxes, = 60+1)
  against cap 15: cap 13 +40.7 +- 10.9, cap 17 -23.5 +- 11.0 (2000 games each), cap 19 about -74 (283 games, stopped) (S40). Cap 13 at
  10+0.1: -9.1, H0 [-5, 0]. Depth floor f = 4 at cap 15 (`lmfl`): +16.7 +- 15.5, LOS 98.3% at 60+1 (S48); at 10+0.1 +1.2 +- 3.5
  after 20000 games (S52): it costs nothing in short games. Adopted: `lmfl` merged into `rel090`, `epLmrFlr = 4` compiled in
  (S53; new perf reference 11367370 nodes on tests.fen depth 10, 12086912 with `epLmrFlr=0`). `r90a` (frozen `rel090` cf00634)
  against `chcm` at 60+1: +11.8 +- 22.8, 500 games; pooled with S48 +15.1 +- 12.8, LOS 98.9% (S56).
  Box runs on the old TT (2026-11-19): floor on cap 13 against cap 13 +5.2 +- 10.3 (S65), cap 12 against 13 +6.4 +- 10.3 (S66),
  neither shown. Running: cap 13 against 15 on the fixed TT, `r90b`, 64 MB, batches of 400 games on the boxes.
  Then the decision: keep cap 15 with the floor (now compiled), or cap 13 with or without it, by the time control rule (Methods).
- Mate bounds, stalemate after pruning, eval clamp (`mbnd`): -5.5 +- 4.4 at 10+0.1, H0 of [-5, 0] (S54); not merged. Next: split
  (stalemate test, clamp and finding 7 without the mate guards).
- Rejected in this form at 10+0.1: late move pruning D3 M1 C1 -7.6 (S28); root overflow fixes (S29); history in the LMR (S32); LMR level
  carried over -25.0 (S33); less LMR in three forms near -7 (S34); table growth 1.1 -0.35 (S35). Lim 6000 +1.86 +- 3.50 after 20000
  games, no decision (S36).
- Hash: every match so far played with 4 MB, not 16; the engine fix of its hash option is in `rel090` (merged 2026-11-17,
  S50); `-Hash` only for builds that contain it, older branches (e.g. `lmfl`) leak (block "Hash option of the engine").
- TT replacement bug (block "TT replacement"): `ttrp` +12.0 +- 11.0 at 10+0.1 (2000 games, S59), +121.1 +- 15.6 at 60+1
  (1000 games, S63), 1.65 ply deeper at the same time: merged into `rel090` (S64, new perf reference 11671539).
  Plan (user, 2026-11-18), after the 10+0.1 run decides: every earlier match is judged on whether the crippled table could have
  favoured or hurt one setup (e.g. the LMR caps, the floor, the long time control results): a replay or just a sanity check on
  the fixed base. Matches stay at 4 MB with the right rule; larger hashes as a further check.
  2026-11-19 (user): new runs use a hash by the time control (16 MB at 10+0.1, 64 MB at 60+1, 256 MB at 5m+1s, about the
  table use of real games); 4 MB only for replays of 4 MB results.
- PV node type after a fail low (S60, 2026-11-18): found while measuring aspiration (`aspw`); `epPvsNt=1` -12% to -16% nodes
  at depth 10; `pvnt` at 10+0.1 +2.8 +- 11.1 (S62), at 60+1 +56.8 +- 13.7 (S67): adopted, merged into `rel090` 2026-11-20
  (55c9bac, user: before the box runs ended, which play the frozen `r90b`); new perf reference 10251160 (11671539 with
  `epPvsNt=0`); frozen base `r90c`.
- Mate bound fixes without the guards (`mbsp`, findings 7 and 4 with the `evalMax` clamp): +4.5 +- 14.0 at 60+1 (S70), merged
  into `rel090` 2026-11-20 (user); perf reference unchanged (10251160). Not in `r90c`; the next base has it.
  Aspiration itself saves no nodes (parked).
- Designs for later (2026-11-17, nothing built): `SPEED-IDEAS.md` (speed and simplification), `Search/TB-SYZYGY.md` (Syzygy probes),
  `Eval/KPK-BITBASE.md` (KPK bitbase); blocks below.
- Eval (2026-11-18): `pegk` (`pawnEndGame` fix, KPK bitbase, lone king endings below the eval clamp, KBNK corner) adopted:
  sanity run at 60+1 clean (+3.5 +- 45.9, 100 games, no time losses or crashes; S57), merged into `rel090` (S57).
- Plan for the LMR after the box runs: the branches A to D of 2026-11-15 in "LMR cap depending on the size of the search" (outcome C).
- Road to 0.9.0 (user, 2026-11-20), in this order:
  1. Settle the running questions: LMR cap 13 against 15 on `r90c` (MEMO + box batches, pooled) and the check extension limit
     (`chxd-k3`: SPRT [-5, 0] at 10+0.1, a long time control check). Their adopted settings go into a frozen base (with `mbsp`).
  2. The best one or two versions played by the user in the GUI (compiled settings, `-fast` builds: a GUI passes no options).
  3. Gauntlet at 60+1: release 0.8.0 and the best version against a pool of other engines (number of games open), to see
     where 0.9.0 stands and how much it gained over 0.8.0.
  4. Only then the release plan: a few features or fixes that need little testing, maybe ponder (today `go ponder` is
     ignored and `ponderhit` not implemented, `Main/Barbarossa.hs`). The size of the gain over 0.8.0 decides the scope.
     Ponder built (2026-11-20, feature session, user): branch `pond` 9c05ae2 (identity 789fcfa) from `rel090` 0637ff7, not
     merged. Option `Ponder` (bestmove then carries the ponder move); `go ponder` searches without limit; at `ponderhit` the
     move's time starts, answer at once if the pondering already took the normal time of the move; a watchdog stops the
     running iteration at the hard limit; a finished ponder search waits for `ponderhit` or `stop`. Perf unchanged (10251160).
     Local test (cutechess-cli 1.3.1, fastchess has no ponder; 10+0.1): 40 games ponder against no ponder, 8 ponder against
     ponder: no sequence errors, time losses or hangs; hit rate 63-70%. Strength not measured (needs cutechess on MEMO).
     Depth check (2026-11-20): pondering buys depth, +1.29 +- 0.08 ply per game for the ponder side (40 games, 10+0.1, side
     taken from the board: the EPD openings start with Black to move; a first count by ply parity had the sides swapped).
     Fix `pond` ca8824a: an answer at once after `ponderhit` (or a watchdog stop) now repeats the last info line before
     `bestmove`, so GUIs and PGNs get score and depth for it (527 moves had none). Perf unchanged; rerun clean, 21-5-14.
     60+1 (8 games, 2026-11-20): clean (no sequence errors, time losses, disconnects), hit rate 65%, ponder side +1.39 +- 0.20
     ply per game (19.4 against 18.0); 2-3-3. Found there: the depth cap of 25 plies (`md` in `doGo`) is reached often at
     60+1 (1858 info lines at depth 25; 86 ponder searches ended at the cap): a candidate fix for the release list (a
     higher cap), to check that a search at the cap does not just stop early in endgames where more depth would help.
     User (2026-11-20): 25 may be too low now that the search is faster; an independent feature before the release.
     Places that know 25 (read, not changed): `md` in `doGo` (`Main/Barbarossa.hs`), `drawPath` / `matedPath` with
     `pathDepth = 25` (`Search/Core.hs`: their depth then means "exact at any draft", it must follow the cap), the null
     move depth table index clamp `nmDMaxIx = 25` (clamped, safe), `maxDepthAtThisRate`; depth indexed tables (LMR) to check.
     History (checked 2026-11-20 after the user's concern about 2^d and Int32): the bonus is `histw ply = 2^(31 - ply)`
     (`Moves/History.hs`), by the ply from the root, not by the draft, clamped to 1 from ply 31; every caller passes `absdp`.
     A higher cap gives no ply a larger bonus and no new overflow (the only overflow is ply 0, bug 1, independent of the cap);
     it only adds more updates at deep plies with tiny weights. No damped update needed for the cap; games at 60+1 decide.
  Larger changes (time management change A, `improving`, singular extensions) wait for that decision.

## Methods

Readings rest on the method of their time; when a method falls, the readings resting on it are marked below.

Method review (2026-11-15, `Search/METHOD-REVIEW.md`, read from the records with a simulation of the tests, nothing played): an H0 stop
of [0, 5] fixes the estimate by the number of games (about 2.5 - 38000 / games), so "H0 near -7" says "not +5", not "-7"; [-3, 2]
passes a true 0 in only 64% of the runs (for "harmless" put 0 at the upper bound: [-5, 0] or [-8, 0]); the most likely value of the
predictions was above the result in all 11 cases S28-S40 (a tuned search starts near -5); 2000 long games decide only effects from
about 15 Elo; early looks: loser stops at fixed looks are safe, winner stops only by a rule fixed before the start; the long runs
use the engine's start hash of 4 MB (until 2026-11-17 written as 16: the engine announces 16 and starts with 4, S49), unlike real
use; fixed in lab 49a0f82 (`run_timed -Hash <MB>`), usable only with engines that free the old table ("Hash option of the engine"); node scans: per position ratios with a bootstrap
interval, one process per position, a placebo, positions with 5 men or fewer apart. Adjudication measured on Grw 110: about 9% of the
time at 10+0.1 for about 1% changed results. Not yet adopted: each point is decided when it is used.

Time controls (decision, 2026-11-16, user): the target time controls are 60+1 to 4m+1 and a bit longer; they cannot be tested
at scale. Eval changes: 10+0.1 is a good test (fast, still some search in the games); for very fast eval checks fixed nodes per
move would do, but they hide speed costs, so an eval change that costs speed still needs a timed check. Search changes: 10+0.1
screens, fixed matches at 60+1 decide. It held in sum: `chcm` against `dev090` +82.5 at 60+1 for about +80 of summed short SPRTs
(S46). Exceptions are parameters that act with the size of the search (LMR cap and ramp, S37, S40), and 10+0.1 has its own
distortions (0.2 to 0.3 s per move: fixed overheads and the time rules act differently; 4 MB hash fills differently). Rule: a
search setting that wins clearly at 60+1 and loses a little at 10+0.1 is kept, if no better one is found with reasonable
effort. Clearly: LOS >= 95% at the planned number of games, read once at the end (1000 games show about 15 Elo, 2000 about 10).
A little: a loss of up to about 10 at 10+0.1; a larger one points more to an instability than to a time control dependence.
A middle step when needed: 30+0.3 on MEMO, for parameters that depend on the size of the search, once a trace shows they act
there. Online tournaments give diagnostics (time use, real opponents, as S41), not tuning evidence.

| Method | For | Status | Why | Ref |
|---|---|---|---|---|
| Move 4+ cuts at fixed LMR level 11, two position sets, against a placebo floor | ordering | current | a proven change shows clearly; floor +-0.03 openings, +-0.065 real | S20, S21 |
| Cuts under the adaptive LMR | ordering | blurred | removing the threat sort hardly shows (+0.012, +0.056); on the older base it was -0.174 (S5), the difference is not explained | S20 |
| Late cuts | ordering | weak | placebo up to +-0.18, one at 2 sigma | S20 |
| Node totals at fixed depth | search quality | identity check only | one perf run is a single sample; placebo spread 3-5%; 50 positions: +29% was noise | S2, S18, S20 |
| Node minimum at fixed depth | pruning parameters | refuted | the M38 was a blow-up effect, no minimum for RFP or LMR | S10, S13 |
| Worst 1% of the roots | reductions | no selector | right on 14, wrong on 11 against adaptive | S13 |
| Overlap of the top 1% roots | anomalies | useful | 79% with the guard, 9-12% without | S13 |
| 10k-node games | large effects, pruning | limited | depth 4-5 only; threat sort +3.5 against +10.8; agreed on RFP | S3, S5, S8 |
| 1000 games, one batch of openings | anything | too weak | +18.8 then -6.3 | S18 |
| Timed SPRT on MEMO, 10+0.1 | strength | screens | search changes: decided at 60+1 since 2026-11-16 (time control decision above); eval changes: decides | |
| Fixed matches at 60+1 (MEMO) or 192+3.2 (boxes), read once at the end | strength, search | decides | LOS >= 95% at the planned games; 1000 games show about 15 Elo, 2000 about 10 | S40, S46, S48 |
| GHC profiler | time shares | counts only | 3x slower, sort 30% against 7% | S16, `PROFILING.md` |
| Direct timers in the optimized build | time, allocation | current | | `PROFILING.md` |

To review under the current method: the cut numbers of S4 to S7, measured under the adaptive LMR.

Check of the screening at the long time control (2026-11-15, step A2 of the LMR plan, method review point 3): `chcm` against
`dev090` at 60+1 on MEMO, 1000 games (500 rounds), fixed, read once at the end, run `chcm-dev090-tc60`. The short SPRTs since
`dev090` add up to about +80 at 10+0.1 (qthr +10.8, hpqt +5.8, rfut +17.6, nmlm +13.0, chcm +22.5 against `expo`, the filter step),
a sum of steps selected for passing, so inflated; parts of it (LMR, pruning margins) were tuned at 10+0.1 only. Question: does the
gain arrive at 60+1? Prediction (2026-11-15, before the result): +10 to +55, most likely +30; below +15 the short screening misleads
for the long time control and the cap and ramp results at 60+1 weigh more than the 10+0.1 ones. Started on MEMO 2026-11-15.
Result (2026-11-16): +82.5 +- 16.0, LOS 100%, 1000 games (S46). Prediction wrong, too low (above its range): the gain of the
short SPRTs arrives in full at 60+1, about the sum of the steps. The screening at 10+0.1 does not mislead for the long time
control in sum; single parameters that depend on the size of the search (the LMR cap, S40) remain the exception to check there.

## Match builds and players

Rule (user, 2026-11-19; lab `MATCHES.md` "Players"): matches never use a moving branch. A player is a frozen build plus `-p`
settings as differences from its compiled values, named `<build>[-<short>]`; when a setting is adopted, the next frozen build has
it compiled in and the table records the equality by the perf node count (tests.fen depth 10, deterministic perf build).
Names `r<version><letters>` (`r90a` ... `r90z`, `r90aa`, ...; `r91a` after a release); the newest base is reused while only
documents changed on `rel090` since its commit (skill `match-runs`, "Frozen base").

Frozen builds:
| Build | From | Identity commit | Contains | Compiled (LMR cap, floor, TT rule) | Perf nodes |
|---|---|---|---|---|---|
| `r90a` | `rel090` cf00634 | 771f656 | `chcm` + floor f = 4 + `spdg` speed | `epLmrMax` 15, `epLmrFlr` 4, old TT rule (smallest move code; no `epTTRep`) | 11367370 |
| `r90b` | `rel090` b00f14a | ace64a7 | `r90a` + TT replacement (`ttrp`, S64) | `epLmrMax` 15, `epLmrFlr` 4, `epTTRep` 2 | 11671539 (S64; 11367370 with `epTTRep=0` = `r90a`) |
| `r90c` | `rel090` 55c9bac | b3a63d6 | `r90b` + PV node type after a fail low (`pvnt`, S67) | `epLmrMax` 15, `epLmrFlr` 4, `epTTRep` 2, `epPvsNt` 1 | 10251160 (11671539 with `epPvsNt=0` = `r90b`) |

Player comparisons from 2026-11-19 (earlier ones: the log, by their S numbers):
| Player A | Player B | Conditions | Result (A's side) | S |
|---|---|---|---|---|
| `r90b-m13` | `r90b` | m3a, m3b, 192+3.2 (= 60+1), 64 MB, 1400 games in 4 pooled batches | +8.2 +- 12.6, LOS 89.9% | S71 |

Obsolete (2026-11-20: done, recorded in S71 as `r90b-m13` against `r90b`, pooled). Exception (2026-11-19, user; to correct, then mark obsolete): batch 1 of cap 13 against 15 on m3a (`rel090-m13-m15-b1`) was
started before this rule with the moving branch: `rel090` a96b702 (code = b00f14a = `r90b` without its identity commit), names in
the games `m13` and `rel090`. It is not restarted. When it is done: it counts as `r90b-m13` against `r90b` (the code is the same) and is pooled
with the other batches (no reading of its own); the pooled S entry carries this note; then mark this exception obsolete
here and in `RUNS.md`.

## Move ordering

### Threatened pieces first (status: adopted)
Now: the best supported ordering change, by games. 2026-10-31.
Forms:
| Form | Branch, commit | Parameters | Differs by |
|---|---|---|---|
| Escapes first | `qthr` f5324a5 | `epQThr=2` | fast path when nothing is threatened, speed -2..3% |
| Softer variants, moves to attacked squares last | working tree | | none better (S6) |
Evidence:
| Date | Form | Test | Result | Ref | Reading then |
|---|---|---|---|---|---|
| 2026-10 | Escapes first | 10k nodes, 32k games | +3.5 +- 3.2 | S5 | small |
| 2026-10 | Escapes first | SPRT [0, 5], 10+0.1 | +10.8 +- 7.3 | S5 | worked |
| 10-31 | Removed | move 4+, level 11 | +0.115, +0.121 | S20 | good control |
Use: the control for measuring ordering changes.

### History prior from piece order and PST (status: adopted)
Now: gain in games together with the threat sort; its own effect is small and not visible in the current cut measure. Not sure on its own. 2026-10-31.
Forms: `hpst` 5919678, `epHistPst=3`, `epHistPstSh=-4`. Untested: a to-from key (S4).
Evidence:
| Date | Form | Test | Result | Ref | Reading then |
|---|---|---|---|---|---|
| 2026-10 | PST alone | 16k games: 1000 nodes, 10k nodes | +6.3 +- 4.9, +2.0 +- 4.5 | S4 | small gain |
| 2026-10 | With threat sort, against `qthr` | SPRT [0, 5], 10+0.1 | +5.8 +- 4.6 | S7 | worked |
| 10-31 | Removed | move 4+, level 11 | -0.009, +0.027 | S20 | below or near the floor |

### Counter moves (status: adopted)
Now: real ordering gain, the same in all four measures. Elo unknown: 10k-node games cannot show it. 2026-10-31.
Forms:
| Form | Branch, commit | Parameters | Differs by |
|---|---|---|---|
| After the killers | `cmov` adbce56 | `epCMov=1` | also stored on TT cuts; none after a null move; table cleared per search |
Evidence:
| Date | Form | Test | Result | Ref | Reading then |
|---|---|---|---|---|---|
| 10-31 | After killers | move 4+, adaptive | -0.247, -0.241 | S19, S20 | real |
| 10-31 | After killers | move 4+, level 11 | -0.234, -0.217 | S20 | real, far above the floor |
| 10-31 | After killers | 10k nodes, 2000 games, 2 batches | about -1 +- 12 | S19 | not informative |
| 11-10 | After killers | SPRT [0, 5], 10+0.1, stopped at 5398 games | +1.54 +- 6.80 | S26 | small at most |
Interpretation history: 10-31: "no Elo" withdrawn, the test cannot see ordering gains (S3, S5).
Review when: LMR retune.
2026-11-10: the large cut gain did not carry Elo alone; kept in `chcm` because the continuation history was measured on top of them (S25).
Next (optional): continuation history with and without counter moves, to see whether they add anything.
Prediction (2026-11-10, before the result): +3 to +10, most likely about +5; H1 more likely than H0. Basis: twice the cut gain of the threat sort (S20: -0.234 / -0.217
against +0.115 / +0.121), which gave +10.8 in its SPRT; gains in cuts turn into less Elo than that; almost no speed cost. Strong engines report a few Elo for counter moves.

### History malus (status: parked)
Now: no gain in any size under the current method; the earlier late-cut gain did not repeat. Fits the user's earlier experience. 2026-10-31.
Forms: `hmal` f10edb1, `epHMal=n`: quiet moves failing low before a cut get the bonus shifted right by n - 1.
Evidence:
| Date | Form | Test | Result | Ref | Reading then |
|---|---|---|---|---|---|
| 10-31 | 1/2, 1/4, 1/8, no root update | late cuts, adaptive, openings | -0.31, -0.35, -0.42, about +-0.2 | S17 | direction good |
| 10-31 | 1/8, no root update | 16 games | noise | | no crash |
| 10-31 | 1/2, 1/4, 1/8, no root update | move 4+, level 11: openings; real | -0.005, -0.049, +0.036; -0.059, +0.016, -0.054 | S21 | at the floor |
Interpretation history: 10-31: the 2 sigma above is within what the placebo showed (S20); then not repeated (S21).
Review when: another bonus scale; continuation history, where a malus may act differently. No games so far.

### History bonus by remaining depth (status: parked)
Now: no gain in cuts under the current method, worse on real positions; no effect in games at 10k and 50k nodes. Deep play not tested. 2026-10-31.
Forms:
| Form | Branch, commit | Parameters | Differs by |
|---|---|---|---|
| `2^(s+d)`, cap 2^30 | `hrem` f28fa1b | `epHRem=s` | s = 16, 20, 24 |
| Half slope, per own ply | `hrem` f802556 | `epHRem=-14` | no cap (user) |
Evidence:
| Date | Form | Test | Result | Ref | Reading then |
|---|---|---|---|---|---|
| 10-31 | s = 20 | 10k nodes, batch 1 | +18.8 +- 17.4 | S18 | "promising", wrong |
| 10-31 | s = 20 | 10k nodes, batch 2 | -6.3 +- 17.7 | S18 | noise |
| 10-31 | s = 24, s = 16, half | 10k nodes, 1000 each | +4.9, -14.3, -3.1, each +- 18 | S18 | no effect |
| 10-31 | s = 20 | 50k nodes, 1000 games | +3.5 +- 17.1 | S18 | no effect |
| 10-31 | s = 20 | move 4+, level 11 | +0.034, +0.059 | S20 | no gain |
| 10-31 | half slope | move 4+, level 11 | +0.096, +0.083 | S20 | slightly worse |
Interpretation history: 10-31: the gain on openings under the adaptive LMR (S18) came with the LMR (S20).
Review when: timed games at depth are cheap; a better history; separating aging from the weight of reduced lines (bonus `2^(r + ply - draft)`, not built).

### Overflow of the history bonus at the root (status: parked)
Now: a real bug (at absolute depth 0 the Int32 overflows and makes the move the worst), but both fixes play about -4 on `chcm` and pass no non-regression reading:
keep it, it is harmless or even useful. Do not "clean it up" without a test. 2026-11-12.
Forms: `hmal` f10edb1: `epHGrav=0` as before, `2` root as depth 1 (still 2^30), `3` no root update. On `chcm`: `hroot` 1f0bec8, `epHRoot` 0 as before, 1 root as
depth 1, 2 no root update.
Evidence: no root update, move 4+ -0.043 under the adaptive LMR on openings (S17); at level 11 -0.046, +0.031, at the floor (S21). No games.
Review when: the bonus is changed for another reason.
Evidence (games):
| Date | Form | Test | Result | Ref | Reading then |
|---|---|---|---|---|---|
| 11-12 | `epHRoot=1` on `chcm` | SPRT [0, 5], 10+0.1, 5824 games | -4.12 +- 6.52, H0; LLR under [-3, 2] -1.64 | S29 | not neutral; prediction 0 to +3: H0 right |
| 11-12 | `epHRoot=2` on `chcm` | SPRT [0, 5], 10+0.1, stopped at 3400 games | -3.37 +- 8.67 | S29 | not better than `epHRoot=1` |

### Root best move deprioritized deeper (status: open)
Idea (discussion of S29 with the user, 2026-11-12): the root overflow is not a lasting "worst" mark. The history is new per search and not cleared
between iterations; a stable root best move B gets the root update in every iteration, so its entry (side, piece type, to square) flips: from
oh < 0 to about +2^31 (worst), at the next update back to oh; from oh >= 0 to about -2^31 (best), then the whole table is halved and B lands at +2^30.
Under B the entry hardly acts (the to square holds the own piece); it acts in the subtrees of the other root moves X (zero window, expected fail
low), where it mostly means "B one move later", a near-transposition of the main line. Hypothesis: B last there (reduced most) lets X fail low
more easily: fewer best move changes and re-searches, a stability bias. The S29 order fits: B high (`epHRoot=1`, 2^30) -4.1, no update
(`epHRoot=2`) -3.4, the bug (worst every other iteration) 0; together about -3.8 +- 5.2, about 1.5 sigma: weak, may be noise.
Forms: `epHRoot=3`: B's entry fixed to the worst value at every root update (no flipping, no halving of the table); general form: reduce the
root best move more in the subtrees of the other root moves.
Measure: first a perf build diagnostic (how often the entry is read below the root, by node type, cut rate of the delayed B), then an SPRT
[0, 5] against the current base. Even: the overflow is noise, fix it when convenient (S29). Not built, after the `lmrh` runs.

### History gravity (status: rejected in this form)
Now: clearly worse alone, no gain with a malus. It pulls good values back without a malus to balance them. 2026-10-31.
Forms: `hmal` f10edb1, `epHGrav=1`.
Evidence: move 4+ at level 11: alone +0.570, +0.733; with malus 1/8 -0.054, +0.138 (S21). No games.

### Continuation history (status: adopted)
Now: on top of counter moves a smaller ordering gain, in both sets, about a third to a half of theirs, for about 2% speed; best setting not resolved. Elo unknown. 2026-11-01.
Forms:
| Form | Branch, commit | Parameters | Differs by |
|---|---|---|---|
| Two tables, added to the history in the sort | `chis` e2ad098 | `epCHist` (1 opponent move, 2 own move, 3 both), `epCHistW` (quarters) | bonus only, as the history; cleared per search |
Evidence:
| Date | Form | Test | Result | Ref | Reading then |
|---|---|---|---|---|---|
| 11-01 | 15 settings | move 4+, level 11, 1 run | best -0.115, -0.205 | S23 | base pattern on the real set |
| 11-01 | Best 3 | move 4+, level 11, 4 seeds | -0.05 to -0.107, -0.052 to -0.095 | S23 | small, real |
| 11-01 | `epCHist=3, W=2` | speed, 6 pairs, disturbed PC | -1.9% +- 0.7 (SE) | S23 | rough |
| 11-01 | - | timed games, script error | none valid | S23 | no reading |
| 11-10 | `epCHist=3, W=2` on counter moves | SPRT [0, 5], 10+0.1, 5010 games | +10.54 +- 7.22 | S25 | gain, larger than predicted |
Interpretation history: 2026-11-10: the prediction 0 to +4 was too low; small cut gains at level 11 can carry a real gain in games. Compiled in `chcm` (`epCHist=3`, `epCHistW=2`).
Review when: LMR retune.
Next: timed SPRT [0, 5] on `fbas`, `epCMov=1,epCHist=3,epCHistW=2` against `epCMov=1` (`chis-cmov-tc10`); watch the time losses (S24).
Prediction (2026-11-10, before the result): 0 to +4, likely no quick decision. Basis: a third to a half of the counter move cut gain (S23), minus about 2% speed (about
-1.5 Elo at 10+0.1). If after about 8000 games the estimate is under +2, stop it and park the form; a weight or table form that costs less speed would be the next try.

## Pruning

### Reverse futility pruning (status: adopted)
Now: a clear gain; D6 / M50 compiled. The margin comes from games; the node method behind M38 is refuted. 2026-10-31.
Forms: `rfut` 9b7a4a1, `epRFutD=6`, `epRFutM=50`.
Evidence:
| Date | Form | Test | Result | Ref | Reading then |
|---|---|---|---|---|---|
| 2026-10 | M100, M50, M25 | 10k nodes, 8000 games each | +11.1, +25.2, +21.7, each +- 6.4 | S8 | peak about M50 |
| 2026-10 | D6 / M50 | SPRT [0, 5], 10+0.1 | +17.6 +- 9.9 | S8 | worked |
| 2026-10 | Sublinear margins | node scan | no gain against a linear one | S13 | old method |
Interpretation history: S10's method looked good, then S13 showed the blow-ups behind it.
Review when: the margin measured without the LMR guard (S8 ran with it); timed games for M25 and M38.

### RFP: more margin when own pieces hang (status: adopted)
Now: a margin of 100 is probably a small gain: +4.8 +- 4.6, SPRT [0, 5] undecided at 12000 games (LLR 2.06). Skipping RFP in these nodes lost. 2026-11-01.
Forms: `rfth` 7693aa9, `lmfx` 32c4bb8, `epRFutT`.
Evidence:
| Date | Form | Test | Result | Ref | Reading then |
|---|---|---|---|---|---|
| 2026-10 | T10000 (no RFP) | 10k nodes, 8000 games | -12.5 +- 6.3 | S12 | skipping is wrong |
| 2026-10 | T25 .. T200 | node scan | noise | S12 | old method |
| 2026-11 | T100 | SPRT [0, 5], 10+0.1, 12000 games | +4.75 +- 4.58 | S22 | likely small gain, no decision |
2026-11-01: compiled in `fbas` on this evidence (user), without a passed SPRT.

### RFP: no cut with an opponent passer on rank 6/7 (status: parked)
Now: P100 played (S69): -0.4 +- 7.9 in 4000 games at 10+0.1, no effect shown; the strong diagnostic signal does not turn
into Elo at this frequency (2.7% of the cuts). Parked in this form (P100; user, 2026-11-20).
Before (2026-10-31): the sharpest signal of the shadow diagnostic, never played; wrong cuts can be cheap.
Evidence: wrong rate 5.3x, 10.4x in endgames; gives up 10% of the decisions for 38% of the wrong cuts (S13).
Built (2026-11-19, feature session): `rfpp` a3bc06b (identity 0a00424) from `r90b` ace64a7, parameter `epRFutP` (cp, compiled
0 = off; 10000 = no RFP cut there): when the base margin would cut and the opponent has a passed pawn on its 6th or 7th rank
(`yoAdvPassed`: the passed bitboard, ranks 2/3 for a black opponent), the margin grows by `epRFutP`; added to `epRFutT`.
Deterministic perf, tests.fen depth 10: off 11671539 (= `r90b`), P50 10906095, P100 10582632, P10000 11237441 (less pruning,
fewer nodes in all three: the chaotic tree, node totals do not select). Census (build flag `lmrcensus`) at off: 2.7% of the
RFP cuts have such a passer (28526 of 1055253), mostly at depth 1-2 (19646, 5874, 2105, 582, 230, 89 for d1..d6). Perft
right, no warnings. The S13 rate on today's engine would need `rfpdiag` ported (not done).
Forms to play: P100 (the analogue of `epRFutT`, S22) and P10000 (no cut, the S13 form; for hanging pieces this lost, S12).
Next (user, 2026-11-19): no 10k-node games; MEMO directly. Run `rfpp-p100-r90b-tc10`: `rfpp` a3bc06b with `epRFutP=100`
(`rfpp-p100`) against `r90b` ace64a7, 10+0.1, 16 MB, SPRT [0, 5] with a cap of 4000 games (2000 rounds, openings from 8301;
user: a tiny effect is not worth the usual 20000, a clear one decides early anyway). At the cap without a decision: read
the estimate (+- about 7.5) and decide with the user. The no-cut form (P10000) only if P100 looks positive.
Prediction (2026-11-19, before any game): -4 to +6, most likely +1. Reasons: the condition is in 2.7% of the RFP cuts
(census), far rarer than hanging pieces (41% of the correct cuts in S11), whose margin gave +4.8 +- 4.6 (S22); the S13 wrong
rate was measured on an older engine; the extra nodes are few. Most likely outcome: no SPRT decision at 4000 games.
Result (2026-11-20, S69): -0.43 +- 7.87, LOS 45.7%, LLR -0.91, cap of 4000 games without a decision; prediction right.
P10000 not played (only after a positive P100). Review when: RFP margins are retuned, or deeper games show endgame losses
to advanced passers.

### RFP: cut more when an opponent piece hangs (status: open)
Now: wrong rate 0.3x (S13). User: only after the RFP SPRTs. 2026-10-31.

### Null move and QS delta cut at a mate bound (status: rejected in this form)
Now: bugs found by reading the code (review 2026-11-15, `Search/SEARCH-REVIEW-BUGS.md` findings 2 and 9), not measured. Mate scores
carry no distance, so windows at the mate bounds are frequent: a node whose moves so far are mated searches the next ones with
(-20000, -19996), its grandchildren have b = -19996. (1) `nullMoveFailsHigh` has no `nearmate b` guard (RFP and futility have one):
at b = -19996 the null move window lies beyond the mate score and always "fails high", so a mate found by a reduced search is taken
back in its re-search (node types swapped, now a CutNode with null move): forced mates by quiet moves are seen only through PV
re-searches, later or at higher cost. (2) The QS delta cut (`qsNormal`, `qsInCheck`) fires before captures or checks are generated
when alpha = 19996: the QS cannot find a mate by its own capture there ("can you mate?" answered no). Related: "LMR: mate escape".
Forms: `| nearmate b = return NoNullMove` in `nullMoveFailsHigh`; no delta cut when `nearmate (qsScore qsa)`. Two lines.
Measure: node counts and the time to the mate on a set of mate positions (mates by quiet moves, found and denied), then an SPRT with
no-loss bounds [-3, 2] (rare positions, little Elo expected). Not built.
Built (2026-11-15): `mbnd` b7fb86e from `rel090`, both guards. Local checks (S43): perft unchanged; roots-500 geomean nodes 1.017,
mates found earlier or at all in 4 of 6 mate positions; the level 19 explosions of S42 shrink 10 and 12 times in two KQ/KR positions,
less than half in the third (362, check extension still the candidate). Next: SPRT [-5, 0] against `chcm` at 10+0.1 when a slot is
free (a no-loss test: a bug fix kept unless it loses).
The "-19744" of S43 is the clamp of the static eval (`matesc = 20000 - 255` in `Eval/Core.hs`, rounded to the grain), one grain
outside `nearmate` (>= 19745): the guards of `mbnd`, RFP and futility do not hold there. Frequent in lost games (151 draft scores,
5 of the 8 lost games of S41 reach it before -20000). Open: align the two definitions (decision: it is an eval score, not a mate).
Done (2026-11-16, `mbnd` 4708209, S47): the clamp is `evalMax` (19744) in `Eval/Core.hs`, `nearmate` and `isMating` are defined
as beyond it (scores unchanged: the rounding brought the old clamp to the same value); with it review finding 4 (stalemate after
pruning, cheap form). Screen (2026-11-16): `mbnd` against `chcm` on MEMO, 10+0.1, SPRT [-5, 0], run `mbnd-chcm-tc10b`, started 2026-11-17 (a first start
without the bounds ran [0, 5] and was stopped at once). Prediction (before the result): -4 to +3, most likely -1; H1 much more likely (bug fixes in rare positions, small node
changes; the mate guards cost about 1.7% nodes on roots-500, S43).
Result (2026-11-18, S54): -5.47 +- 4.40 after 12822 games, LOS 0.74%, H0 accepted; 1 time loss, no crash. Prediction wrong (too
optimistic). Reading: the 1.7% nodes explain at most 1 to 2 Elo; the mate guards are the part that changes the search outside mate
windows (S43: geomean +1.7%, single positions 7 and 27 times the nodes with no mate score), the stalemate test changes tests.fen by
-0.09% (S47), finding 7 not at all. Most likely the guards cost, not shown by this run. Rejected: the three commits of `mbnd` as one
package; nothing merged. The bugs stay (findings 2, 4, 7, 9 open in `rel090`).
2026-11-18 (S55, user's question): "1.7%" was the geomean; the total on roots-500 was +11% (S43), and the clock pays the total. The
games confirm it: same time per move, `mbnd` 0.087 ply shallower on average (0.05 in balanced positions, 0.37 with a mate score on
the board), of the order of the -5.5. The loss is the search cost of the guards (by elimination). Cause: mate scores carry no
distance, so a single mated move puts the whole subtree of each alternative into mate bound windows, where the guards switch off
the null move and the delta cut. The narrower form to try: mate distance scores (with mate distance pruning), then the guards; or
the guards only in PV nodes.
Review when: a form of the guards that does not change the search outside true mate windows (e.g. only when the node has a mate
score of its own, or only in the QS delta cut); or after the check extension limit, which acts on the same KQ/KR explosions.
Next: split. Form 1: c2378e8 (finding 7) + 4708209 (stalemate test, clamp `evalMax`) without b7fb86e, from the current `rel090`,
[-5, 0] at 10+0.1 against the current base (prediction before the start). The guards alone only in a narrower form. The plan of
`TIME-MATE-EXTENSION-REVIEW.md` (limit on top of `mbnd`) needs a new base: the current `rel090` or form 1.
Form 1 built (2026-11-18): `mbsp` 01a4667 from `rel090` 7dfca9b (code = `r90a`), findings 7 (64a1b49) and 4 with the clamp
`evalMax` (c584205), no guards; comments at `nullMoveFailsHigh` and `qsDeltaCut` (the inexactness, S54/S55, mate distance as the
possible rescue). Deterministic perf, depth 10: tests.fen 11367370 -> 11367334 (-36 nodes); 206 mate roots one process each: 205
identical, one -920 nodes (sum -0.0006%; batched +0.98%, the shared TT and history spread the one change); perft unchanged.
The search is the same as `r90a` except in the rare nodes where every move was pruned and in the order of mated root moves.
Prediction (before any game): -1 to +1, most likely 0; a [-5, 0] SPRT passes with high probability.
Plan (user, 2026-11-18): no SPRT (nothing left to measure at these node counts); a sanity run `mbsp-r90a-tc60s` against `r90a`,
60+1, 100 games (openings from 3101), on MEMO after the `pegk` one; then merge `mbsp` into `rel090`.
Sanity result (2026-11-18, S58): -20.9 +- 44.2, nothing odd in the games (depth equal, no false stalemate zeros, gradual losses).
Parked (user, 2026-11-18): not merged; when MEMO has nothing else waiting, a few more games at 60+1 against `r90a` (openings
from 3151) to see the trend, then the decision.
Trend (2026-11-18, S61): 200 more games -8.7 +- 32.6 (`r90a` lost one on time); pooled with S58 300 games -12.8 +- 26.2,
LOS 16.9%: the prediction is inside the interval, read as noise by the node counts. Decision (merge or not) is the user's.
Depth at the same time over the 300 games: -0.016 +- 0.073 ply (S61): no large slowdown; nothing odd in the games.
Parked (user, 2026-11-18): until a MEMO slot is free for a further match; then the merge decision.
On the fixed base (2026-11-19): `mbsp` 2b28e1a = merge of `rel090` b00f14a (code of `r90b`) into 01a4667, made by a
feature session (no conflicts: the KPK changes in `Eval/Core.hs` leave the clamp alone, and special evals, KPK included,
pass the `evalMax` clamp in `posEval`). Deterministic perf, tests.fen depth 10: 11671538 (`r90b` 11671539, -1 node);
perft unchanged, no warnings. Ready for the further match against `r90b` (players `mbsp` against `r90b`, 60+1, 64 MB;
size and slot by the user). Prediction unchanged: -1 to +1, most likely 0.
Result (2026-11-20, S70): +4.5 +- 14.0, 1000 games at 60+1 against `r90b`, LOS 73.6%; prediction right (0 inside). No sign of a
loss on the fixed base; the earlier -12.8 +- 26.2 (300 games, `r90a`) reads as noise. Merge decision with the user.
Merged into `rel090` (user, 2026-11-20, after S70): no conflicts; identity reset to `rel090`. Perf tests.fen depth 10:
10251160 (= `r90c`, the fixes change no node there), 11671538 with `epPvsNt=0` (= `mbsp`); perft right. The next frozen base
contains it; `r90c` does not (the runs on `r90c` are not affected).

### Late move pruning (status: rejected in this form)
Now: loses, more with depth, also on the new ordering and with the count for the LMR level. Without `improving` late moves are not reliably bad. 2026-11-11.
Forms:
| Form | Branch, commit | Parameters | Differs by |
|---|---|---|---|
| Limit A + M d d / 2 from the last capture | `lmpr` 484b2e6 | `epLmpD`, `epLmpA=3`, `epLmpM` | zero window, not in check |
| A pruned move counts for the adaptive LMR | `lmpc` cae8bcd | `epLmpC` | keeps the LMR level |
| As `lmpc`, on the new ordering | `lmpn` 256dbad | as `lmpc` | base `chcm`: filter, T100, counter moves, continuation history |
Evidence:
| Date | Form | Test | Result | Ref | Reading then |
|---|---|---|---|---|---|
| 2026-10 | D3 M1 | SPRT [0, 5], 10+0.1 | -9.4 +- 8.8 | S15 | rejected |
| 10-31 | D5 M1 | SPRT [0, 5], 10+0.1 | -25.4 +- 13.4 | S15 | rejected, deeper costs more |
| 10-31 | With the count | LMR level, nodes | C1 restores most of the level | S15 | not played |
| 11-11 | D3 M1 C1 on `chcm` | SPRT [0, 5], 10+0.1 | -7.6 +- 8.1, 3888 games | S28 | rejected; prediction -5 to +5 (about 0): direction right |
| 11-11 | Shadow, LMR level | lost cuts, nodes by level | about 1/3 of the move 4+ cuts lost, 78% at depth 1; C0 lowers, C1 raises the level | S30 | the pruning itself loses |
| 11-11 | Shadow by history | AUC, cuts lost by ranking | k and history about equal and overlapping; ranking by both loses almost as many cuts | S31 | a history limit does not make LMP safe |
Review when: `improving` and history in the LMR are in; first form then: no LMP at depth 1 (S30); a limit by history adds little (S31).

## Reductions

### LMR without the near-mate guard (status: adopted)
Now: a bug fix with a clear gain. Rare missed mates not measured. 2026-10-31.
Forms: `nmlm` 5795f3f. Evidence: SPRT [0, 5] +13.0 +- 8.2; the 20 worst roots cost 344x less (S13).

### Fixed LMR level (status: rejected in this form)
Now: adaptive is stronger than 11 and 14. Useful for measuring ordering. 2026-10-31.
Forms: `lmfx` 32c4bb8, `epLmrFix`; with ordering changes `lfix` fbfb381.
Evidence: SPRT [0, 5] level 14 -36.4 +- 15.7, level 11 -8.6 +- 8.5 (S13). Levels 12, 13 not played.
Review when: ordering has changed much.

### LMR level carried over between iterations (status: rejected in this form)
Now: the adaptive LMR restarts at level 8 in every iteration and climbs; 29% of the nodes are searched at level 8, mean level 10.8 (S30). Carried
over: -30% nodes on tests.fen, -24% on roots-500 at depth 10 (S33). 2026-11-13.
Forms: `lmrc` 6894348: `epLmrCarry` 1 the level, 2 also the node limit (practically the same).
Prediction (2026-11-13, before the result): -10 to +8, most likely -1, H0 more likely than H1.
Evidence:
| Date | Form | Test | Result | Ref | Reading then |
|---|---|---|---|---|---|
| 11-13 | Carry 1 on `chcm` | SPRT [0, 5], 10+0.1, 1366 games | -24.97 +- 13.28, H0 | S33 | the restart at level 8 is worth about 25 Elo; prediction (-1) right in direction only |

### Lower LMR start level, less LMR in PV nodes (status: parked)
Now: from S33 (carrying the level over -25), S13 (fixed 11 -8.6, 14 -36.4) and S32 (G24 -9.9): every change that raises the reduction where an
iteration starts loses. The other direction was never tried: each iteration starts lower than 8 (`lmrInitLv` 6, 7) or the level climbs slower
(`lmrInitLim` higher); or one ply less reduction in PV nodes (now the same table for PV and zero window). 2026-11-13.
Forms: `lmrb` 5092839: `epLmrIni` (8), `epLmrLim` (8500), `epLmrPV` (0).
Next: SPRT [0, 5], 10+0.1, Ini 6 and PV 1 against `chcm` on MEMO, Lim 17000 on m3a (S34); Ini 7 built, not played.
Prediction Lim 17000 (2026-11-13, before the result): -5 to +6, most likely +1, H0 more likely.
Prediction (2026-11-13, before the results): Ini 6 -4 to +8, most likely +2; PV 1 -5 to +6, most likely +1; H0 more likely for both.
Evidence:
| Date | Form | Test | Result | Ref | Reading then |
|---|---|---|---|---|---|
| 11-13 | Ini 6 on `chcm` | SPRT [0, 5], 10+0.1, MEMO, 3506 games | -8.13 +- 8.31, H0 | S34 | a lower start loses too; prediction (+2) wrong |
| 11-13 | PV 1 on `chcm` | SPRT [0, 5], 10+0.1, MEMO, 4286 games | -6.65 +- 7.72, H0 | S34 | less reduction in PV nodes loses too; prediction (+1) wrong |
| 11-14 | Lim 17000 on `chcm` | SPRT [0, 5], 32+0.32 (= 10+0.1), m3a, 4084 games | -6.72 +- 7.75, H0 | S34 | a slower climb loses too; prediction (+1) wrong |
Interpretation history: 2026-11-13: the start level 8 looks like a local optimum (higher -25, lower -8); a slower climb (Lim 17000) is now expected
to lose as well; Ini 7 would at best be neutral, not worth an SPRT.
2026-11-13 (PV 1): every change of the amount of reduction loses, in both directions and also only in PV nodes: the amount is near its optimum
in this form. Open: the shape (S35) and a depth term.
2026-11-15 (user): PV 1 deserves a second chance once the other LMR experiments have settled. Reason: the adaptive level has
counterbalanced almost every LMR change so far, and it may have done so here too: the late moves of PV nodes go through
`zeroWithLMR` like all others, so with less reduction there fewer of them fail high, the level is penalized less, climbs higher
and reduces the zero window nodes more (inference, not measured). Measured only at 10+0.1, where PV nodes are a small share, and
only as one ply less for every late move. Later forms: PV 1 at the long time control; PV reductions kept out of the level credit
(`moreLMR` only from zero window nodes) with and without PV 1; gentler forms (only at larger remaining depth). After the floor
and the cap are decided.
2026-11-17: all three runs ended H0 (table); parked, the PV 1 second chance after the floor and the cap.

### LMR: mate escape (status: open)
Idea (user, 2026-11-11): less or no LMR in nodes where alpha is a "being mated" score for the side to move (alpha <= -mate threshold): all
moves so far get mated, and a reduced late move may hide the escape. Background (S13): the beta guard was removed (`nmlm` 5795f3f, SPRT +13.0);
`nearmate a` was only node-scanned (50 roots, depth 10): 48 equal to no guard, 2 still blew up (real near-mate windows, probably defender PV
nodes), so a full LMR stop there is costly. Missed mate escapes were never measured.
Forms: only the losing side (not +-mate); one ply less reduction instead of none; only from some depth; only for checks and evasions.
Measure: not by node totals. A mate-escape test set, compare the time or depth to the correct escape; then an SPRT against the current base
with no-loss bounds [-3, 2] (a gain from rare escapes will hardly show). Not built, for later.

### Retune of the adaptive LMR after ordering changes (status: parked)
2026-11-17: Grw 110 H0, Pen 15 stopped, Lim 6000 no decision at 10+0.1; the cap moved on to the block "LMR cap depending on the size
of the search" (results there); the speed of the ramp at the long time control (Lim) is step A3 of that plan, after the cap.
Now: user's point: the adaptive parameters (`lmrInitLim`, weight 2^d of a re-search) were set for the old ordering. No data. 2026-10-31.
2026-11-13: the start level is a local optimum (S33 higher -25, S34 lower -8.1); the other constants are now parameters, tuned one at a time.
Forms: `lmrb` 5092839: `epLmrIni` (8), `epLmrLim` (8500), `epLmrPV` (0); `lmrt` dea4035: `epLmrGrw` (120), `epLmrPen` (20), `epLmrNoD` (1).
Next: Grw 110 on MEMO (S35), then Pen 15 or 30 on m3a after Lim 17000; SPRTs [0, 5] against `lmrt` with its defaults (same binary).
Prediction Grw 110 (2026-11-13, before the result): -4 to +4, most likely 0, H0 much more likely.
Prediction Pen 15 on MEMO (2026-11-13, before the result): -12 to +4, most likely -4, H0 much more likely.
Interpretation history: 2026-11-13 (S36): the level is a ramp, not a regulator: it starts at 8 in every iteration and rises one step per about
8500 nodes to the cap 15, drops are rare (38 in 105M nodes). The knobs are the start, the ramp speed (Lim) and the cap; the penalty hardly acts
(Pen 15 now expected about 0). The ramp is an implicit depth term: large iterations reach high levels.
Next (2026-11-13): Pen 15 stopped; Lim 6000 on MEMO (faster ramp) against Lim 17000 on m3a (slower); then the cap (13 instead of 15).
Prediction Lim 6000 (2026-11-13, before the result): -10 to +4, most likely -3, H0 much more likely.
Evidence:
| Date | Form | Test | Result | Ref | Reading then |
|---|---|---|---|---|---|
| 11-15 | Grw 110 on `lmrt` | SPRT [0, 5], 10+0.1, MEMO, 13964 games (resumed twice) | -0.35 +- 4.30, H0 | S35 | the table growth hardly matters; prediction (0) right |
| 11-15 | Lim 6000 on `lmrt` | SPRT [0, 5], 10+0.1, MEMO, 20000 games (limit) | +1.86 +- 3.50, no decision, LLR -1.00 | S36 | the faster ramp is neutral to slightly positive at 10+0.1; first result above the most likely predicted value (-3) |
Form `lmrm` 790aca7: `epLmrMax` (15), the cap. S37: at a MEMO-like 0.5 s per move the cap hardly binds (2.7% of the nodes), at 1.5 s a third of
the nodes run at it and lower caps cost depth (13: -0.76 ply). Cap 13 at 10+0.1 not worth an SPRT; above 15 needs a table extension and a
longer time control.
S38 (`lmrm` e908bd8, table to level 19): at 3 s per move cap 19 +0.41 +- 0.13 ply against 15, at 1.5 s no difference; the cap that costs no
depth grows with the time. Possible link to the weaker play at long time controls. Next: an SPRT of a higher cap at a long time control when a
MEMO slot is free; a ramp by depth instead of nodes would not depend on the time control at all.
Cap 19 at a long time control (2026-11-13): `lmrm` e908bd8 `epLmrMax=19` against `lmrm` (cap 15, same binary) on m3b at
192+3.2 (60+1 on MEMO, factor 3.2), a fixed 2000 games read once at the end (LOS >= 95% to adopt), run `lmrm-m19-lmrm-tc60`.
Prediction (before the result): -6 to +12, most likely +3; LOS >= 95% unlikely at 2000 games (+-12).
Result cap 19 (2026-11-14): stopped by hand after 283 games as a clear loss: +50 -109 =124, about -74 Elo (at 200 games -75.9 +- 35.9,
LOS 0.00%), no time losses (S40). Prediction (+3) badly wrong: the extra depth of S38 does not buy strength, the extra reduction loses moves.
Next (S40): the cap on both sides of 15 at the same long time control, fixed 2000 games each: cap 13 on m3b (`lmrm-m13-lmrm-tc60`),
cap 17 on m3a (`lmrm-m17-lmrm-tc60`), against `lmrm` (cap 15, same binary).
Prediction (2026-11-14, before the results): cap 17 -45 to -5, most likely -25; cap 13 -15 to +15, most likely 0 (S37: 13 costs 0.76 ply at
1.5 s, but if more reduction loses this steeply above 15, less may not cost much); LOS >= 95% for cap 13 unlikely.
S39: in games the endgame runs at lower levels at equal draft (10+0.1, d13+: 10.0 against 12-13), not in searches from scratch: the phase
reaches the level through the iteration size (TT, time), not by itself. A depth ramp would need the phase or the move count to keep it.

### LMR cap depending on the size of the search (status: running)
Idea (user, 2026-11-14): the mechanism of the adaptive LMR: every iteration starts at level 8 and collects the important moves
(history), then the level rises and the less promising moves are searched less hard (corrected only by re-searches). Two
questions: how high (the cap) and how fast (Lim). The time control acts together with the LMR, so the cap may need a measure of
the time control, or of the time for the current move.
Now: evidence so far: the start and the ramp at 10+0.1 are near their optimum in both directions (S33, S34: start 6 -8.1, carry
-25, Lim 17000 -6.7; Lim 6000 running). The cap binds only in large iterations (S37: 2.7% of the nodes at 0.5 s per move), so
it is measured at long time controls: cap 19 about -74 (S40); caps 13 and 17 running (2000 games each). A lower cap was never
played at 10+0.1.
2026-11-17 (supersedes the line above): cap 13 +40.7, cap 17 -23.5 at 192+3.2; cap 13 -9.1 at 10+0.1; floor f = 4 +16.7 at 60+1
(S40, S48); floor f = 4 +1.2 +- 3.5 at 10+0.1 (S52), compiled into `rel090` (S53): outcome C of the plan below. Running: floor on cap 13 against cap 13, cap 12 against cap 13 (boxes, predictions at the
end of the plan).
First, before any encoding: if cap 13 holds at the long time control and is neutral at 10+0.1, a fixed cap serves both (it hardly
binds in short games). An encoding is needed only if the best cap at short time controls is higher than at long ones.
Measure: cap 13 at 10+0.1 (SPRT with no-loss bounds, e.g. [-3, 2]); cap 11 or 12 at 192+3.2 to find the bottom; the speed of
the ramp at the long time control too (a faster ramp reaches the cap sooner: Lim 6000 may gain at 10+0.1 and lose at long time
controls if the cap is too high there).
Forms, if an encoding is needed (preferred first):
1. The draft of the iteration (or the remaining depth): a cap falling with the draft. Plausible mechanism: in deep iterations
   large reductions pile up along a path (cap 19 searched deeper and lost). Independent of time and hardware, measurable with
   the deterministic perf build, node counts and node limited games. S39: the node ramp follows the game phase through the TT,
   so the draft should only bound the level, the ramp stays by nodes.
2. The node budget of the move (allotted time times measured nodes per second): the size of the search on any hardware;
   node limited matches emulate it (budget = node limit), the deterministic checks do not.
3. The time allotted to the root move: simple, but hardware dependent (the same time control on a faster machine reduces
   differently).
4. The remaining time: not recommended, it mixes the time control with the time already spent (S41: the clock is gone by move
   20 at 4+1), so the time management problem would enter the LMR.
Choice between 1 and 2 (depth or size of the search): the cap 11/12 run and a trace like S37 of the levels by draft in long games.
Review (2026-11-15, read from the code, nothing measured: `Search/LMR-CAP-REVIEW.md`): levels above 15 do not widen the set of
reduced moves (exhausted at 15: one unreduced quiet move), they add k plies to every reduced move (smallest reduction 1 ply at 15,
5 at 19); the reduced depth is `max 1 (d1 - r)` without relation to the remaining depth d1, so near the horizon (d1 <= 6) level 19
sends every reduced move to depth 1, and a depth 1 search rarely fails high (re-searches cannot catch it). Cap 13 against 15 changes
the width and the size together, cap 17/19 only the size: different dimensions. Preferred form, new here (form 0, before the four
above): a bound of the reduction by the remaining depth at the node (large reductions only where there is room); it scales with the
time control by itself and keeps the node ramp (S39). Under this mechanism the bound rises with the depth (form 1 with the opposite
sign). Recommendation: do not encode the time control now; if cap 13 holds, the cap first, the depth bound second.
Measure, in this order: M3, a census in `zeroWithLMR` (search unchanged, node totals as check): reduced searches by level, d1, d'
and w, their fail highs and confirmations, full searches by level and d1; fixed levels 13, 15, 17, 19 on roots-500 at one draft, then
the adaptive caps at 3 s as in S38. M1 later, as the check of a candidate table: root move agreement by draft and by nodes against one
deep level 8 search per root (needs a root move line per position in `SelfPlay -P`).
Next: M3 build (trace build on `lmrm`); the cap runs end about 2026-11-17.
Cap 13 at the short time control (2026-11-15): `lmrm` `epLmrMax=13` against `lmrm` (cap 15, same binary) on MEMO, 10+0.1, SPRT
no-loss [-3, 2], run `lmrm-m13-lmrm-tc10` (`lm13`). Question: is a fixed cap 13 harmless in short games (where the cap binds in about
3% of the nodes, S37), so that one cap could serve both time controls? Smoke on mini: 16 games, clean.
Prediction (2026-11-15, before the result): -3 to +3, most likely 0; H1 (not worse) more likely than H0.
Restarted (2026-11-15, after the method review: [-3, 2] passes a true 0 in only 64% of the runs): the [-3, 2] run stopped after
4000 games without a decision (0.00 +- 7.90, LLR 0.15; seen only after the restart was decided, not used for it); new run `lmrm-m13-lmrm-tc10b`, same engines and parameter, SPRT [-5, 0] (a loss of up to 5 at 10+0.1 is
accepted if the long time control gains). Prediction (before the result): -3 to +2, most likely -1; H1 (not worse than -5) much more
likely.
Result (2026-11-15): H0, -9.08 +- 6.53, 5664 games (S40). Cap 13 costs at 10+0.1, prediction wrong. A fixed cap 13 cannot serve both
time controls; if it gains at 60+1 (box run, about 2026-11-17) that is branch C below, and the floor (S45, keeps level 15) is the
first candidate before a cap by the size of the move.
M3 census (S42, 2026-11-15, fixed levels on roots-500, draft 12): reductions end at depth 1 in 90% (level 15) and 99% (19) of the
reduced searches; the confirmation rate at equal d1 and w hardly falls from 15 to 19 near the horizon (no large blind spot shown),
somewhat for d1 7-10. New: fixed level 19 needs 619M nodes against 327M (15) and 270M (17), the extra nodes outside the LMR loop
(PV or aspiration re-searches?): a possible instability at the top level. Next: nodes per position (15, 17, 19) and re-search counts.
Plan after the running cap and ramp runs (2026-11-15, strategy; the predictions of each run are written at its start, about 5
lower than the first guess, see Methods). Working picture: the cap matters only where it binds, so the long time control decides it,
and lower looks better there. Running: Lim 6000 [0, 5] and cap 13 [-5, 0] on MEMO (10+0.1), cap 17 (m3a) and cap 13 (m3b) at
192+3.2, 2000 games each, to about 2026-11-17. Interims at 798 games, for planning only (fixed matches are read at the end): cap 17
-24.4 +- 17.5, cap 13 +36.7 +- 17.1; they point to A. Branches, by the outcome:
- A (most expected): cap 13 gains at 60+1, cap 17 loses, cap 13 passes [-5, 0] at 10+0.1. Lower is better where the cap binds and
  harmless where it does not: no time control encoding yet. 1. The bottom of the curve: cap 11 (m3a) and cap 12 (m3b) against cap
  15 at 192+3.2, 2000 games each, still with 4 MB hash so that the four points compare. 2. MEMO, after Lim 6000: `chcm` against
  `dev090` at 60+1, 1000 games (does the gain of the short SPRTs arrive at the long time control). 3. The best cap against 15 with
  a realistic hash (size to settle, 64 to 128 MB), then the speed of the ramp (Lim) at the long time control with the new cap.
- B: cap 13 neutral at 60+1 (the interim a lucky start), cap 17 loses. Flat optimum between 13 and 15, only higher hurts. 1. Cap 13
  as the default (safer for longer time controls, free at 10+0.1). 2. The other axis of the review: a bound of the reduction by the
  remaining depth (e.g. d' >= d1 / 3), against the collapse to depth 1 (S42), tested first at the long time control. 3. MEMO short
  slots: new mechanisms with [0, 5] (`improving`, singular extensions, aspiration).
- C: cap 13 gains at 60+1 but fails [-5, 0] at 10+0.1. A real time control dependence (the user's encoding idea). 1. Cap by the
  planned size of the move (form 2 above: node budget; 15 below a threshold, 13 above). 2. Screen at 30+0.3 on MEMO (40% of 60+1)
  after eight trace games show that the cap binds there. 3. Confirm at 10+0.1 and 60+1.
- D (unexpected): cap 17 neutral or better, or both caps far from their interims. The long results are not trusted as they stand:
  1. validate a box by a replay of a known result (`lmrc`, -25, 600 games); 2. one cap again with a large hash (effect of the 4 MB
  load).
  Outcome (2026-11-17, S40): C. Cap 17 -23.5 +- 11.0 (prediction -25, right), cap 13 +40.7 +- 10.9 (prediction 0, wrong), at
  10+0.1 cap 13 -9.1. Cap 13 qualifies under the time control decision as it is; still open before adopting: the floor on top
  of cap 13, and the bottom of the cap curve (12, 11) at the long time control.
  Next box runs (2026-11-17, user's go): `lmfl` da23a27, one binary per run, 192+3.2, 4 MB hash, concurrency 8 (calibrated:
  factor 3.2 stays), a fixed 2000 games each, read once at the end (LOS >= 95% to adopt).
  1. m3a, `lmfl-m13f4-m13`: floor f = 4 on cap 13 (`epLmrMax=13,epLmrFlr=4`, `l13f`) against cap 13 (`epLmrMax=13`). Question:
  does the floor add to cap 13? Prediction (before the result): -12 to +18, most likely +3 (both act on the same deep
  reductions; cap 13 already reduces less, so less is left for the floor than its +16.7 at cap 15).
  Result (S65, 2026-11-19): +5.2 +- 10.3, LOS 84%, 2000 games; prediction right (in the range, near +3). Not shown; old
  TT rule, so limited to the old engine. The fixed-TT question is now cap 13 against 15 with the floor on both sides.
  2. m3b, `lmfl-m12-m13`: cap 12 (`epLmrMax=12`, `lm12`) against cap 13. Question: is the bottom of the curve below 13?
  Prediction (before the result): -15 to +20, most likely +3 (the steps were 23 and 41 Elo per 2 levels above 13, but a
  curve that steep does not go on forever, and 12 already costs more at short time controls).
  Result (S66, 2026-11-19): +6.4 +- 10.3, LOS 89%, 2000 games; prediction right (in the range, near +3). Not shown; old TT
  rule. With S65: on the old engine neither the floor on cap 13 nor cap 12 adds clearly to cap 13. On the fixed TT the
  question is first cap 13 against 15 (both with the floor, batches running); cap 12 only if cap 13 wins there.
- Depth bound design (Fable, 2026-11-15): `Search/LMR-DEPTH-BOUND.md`; for branch B and as the alternative to a lower cap in A. Form: a
  floor of the reduced depth, `d' = max dmin (d1 - r)`, `dmin = max 1 (d1 * f / 8)`, parameter `epLmrFlr` (0 = off), start f = 4 (largest
  reduction 2-3 at d1 4-6, 4-5 at d1 7-10: about level 13's mean reduction with level 15's width). Reading of S42 corrected: of the 90%
  at d' = 1 more than half are d1 2-3 (the horizon, not a collapse); the collapse and the lost confirmations are at d1 >= 4, the tail
  the floor lifts. Floor first, the check extension limit after (its k depends on the floor).
- Plan change for the boxes (user, 2026-11-15), replacing step A1 when the cap runs end: one box cap 12 against cap 15, the other the
  floor (f = 4) at cap 15 against cap 15, both 2000 games at 192+3.2, 4 MB hash; the floor is read beside the cap 13 result (same
  opponent, time control, hash). The rest of the cap curve (cap 11) after the floor is decided. Optional, a few hundred games: cap 19
  with the floor against cap 15 (near 0 instead of about -74: the cap is no longer the sensitive parameter). Before the floor run:
  build with `epLmrFlr`, f = 0 identical (12086912 nodes), census at f = 3, 4, 5 (S42 set), positions 182, 362, 485.
  Done (S45, branch `lmfl` 5463b64): f = 4 lifts `conf / fh` at d1 >= 4 most of the way to level 13 at no node cost (-8% at
  fixed level 15, -6% adaptive); f = 3 hardly acts, f = 5 costs 24 to 43% more nodes. The explosions move, they are not removed.
  f = 4 for the box run.
  Screen at the short time control (2026-11-15): `lmfl` da23a27 `epLmrFlr=4` (`lf4`) against `lmfl` (floor off, same binary;
  off identical to `lmrm`, 12086912 nodes) on MEMO, 10+0.1, SPRT [-5, 0], run `lmfl-f4-lmfl-tc10`. Question: does the floor, which
  keeps level 15, avoid the cost of cap 13 in short games (-9.1, S40)? Prediction (before the result): -5 to +4, most likely -1;
  H1 more likely than H0, but not by much (cap 13 lost where the cap rarely binds; the floor acts wherever a reduction exceeds
  half the remaining depth, at any level, and the level may counterbalance it, as with most LMR changes).
  At the long time control (2026-11-16, user's choice for the free MEMO slot, ahead of the box plan): the same engines and
  parameter at 60+1 on MEMO, a fixed 1000 games (500 rounds), read once at the end, run `lmfl-f4-lmfl-tc60`. Question: does the
  floor bring the long time control gain that cap 13 showed in its interim (+37 at 798 games, 192+3.2) while keeping level 15?
  1000 games show effects from about 15 Elo; smaller ones need the box's 2000. Prediction (before the result): -10 to +25, most
  likely +8 (about 5 below the first guess, by the method review).
  Result (S48): +16.69 +- 15.54, LOS 98.3%, 1000 games: a clear win at 60+1 by the decision rule (LOS >= 95%); prediction right
  (in the range, above the most likely value). Adopted if the 10+0.1 screen loses no more than about 10 (time control decision,
  Methods); a box run of the floor is no longer needed.
  Result of the screen (S52): +1.22 +- 3.51, LOS 75%, 20000 games; it ran SPRT [0, 5] instead of the planned [-5, 0] (started
  without bounds) and ended undecided at the game cap (LLR -2.01). Prediction right (in the range, 2 above the most likely -1).
  The 95% interval (-2.3, +4.7) excludes a loss of 5: the planned [-5, 0] would have passed. The floor at cap 15 qualifies under
  the time control decision (+16.7 at 60+1, about 0 at 10+0.1), unlike cap 13 (-9.1 at 10+0.1). Not merged yet: the box runs
  (floor on cap 13, cap 12) decide between the floor at cap 15 and cap 13 with or without it.
  2026-11-17 (user): merged now, `epLmrFlr = 4` compiled in at cap 15 (S53); the box runs can still change the cap.
  Confirmation at the long time control (2026-11-17, user): `r90a` (frozen `rel090` cf00634, identity commit 771f656: `chcm` +
  floor f = 4 + the `spdg` speed, +4.5% nodes/s) against `chcm` on MEMO, 60+1, a fixed 500 games (250 rounds), openings from
  start 1001 (disjoint from the 500 openings of `lmfl-f4-lmfl-tc60`: `order=random` uses the same fastchess seed every run),
  read once at the end, run `r90a-chcm-tc60`. Questions: is `rel090` better than `chcm` at 60+1, and by how much? Pooled
  with the 1000 games of S48 (floor alone) the 1500 games estimate the gain conservatively for `r90a` (S48 lacks the speed),
  but S48 is the batch that led to the adoption (selection bias upwards). Prediction (before the result): -10 to +35, most
  likely +12 (floor about +8 to +10 after the regression of S48, speed +2 to +4); 500 games alone +- about 22.
  Result (2026-11-18, S56): +11.8 +- 22.8, LOS 84.6%, 500 games; prediction right (at the most likely value). Pooled with S48
  (pentanomial, 1500 games): +15.1 +- 12.8, LOS 98.9%, an upper estimate (S48 chose the adoption). `rel090` as of cf00634 is
  better than `chcm` at 60+1 by about +12 (this batch alone, unbiased) to +15 (pooled).
- Floor replay on the fixed TT (2026-11-19, review after the TT replacement bug): S48 (+16.7 at 60+1, the batch that led to
  the adoption) was played with the old TT rule, under which the 4 MB table is almost useless at 60+1. Run `ttrp-f4-f0-tc60`:
  the same `ttrp` binary on both sides with `epTTRep=2`, floor 4 (compiled) against `epLmrFlr=0`, MEMO, 60+1, 1000 games fixed
  (500 rounds, openings from 5001), 4 MB, read once at the end. Smoke on mini (10+0.1, 16 games, the exact parameters): normal
  endings, no time losses, both parameter sets passed.
  Prediction (before any game): -10 to +20, most likely +6. Reasons: S48 is selected (regression towards about +10); with a
  working table the search is about 1.65 ply deeper at 60+1 (S59), and part of what the floor gave may have been a lower cost of
  re-searches that the old table did not keep; the floor still limits the deepest reductions, which should matter more, not
  less, in deeper searches. LOS >= 95% needs about +14 in 1000 games: confirmation alone about one in four; a clear loss (below
  about -10) would reopen the floor.
  Result (2026-11-19, S68): +16.3 +- 14.2, LOS 98.8%, 1000 games; prediction right (in the range, above the most likely +6).
  The floor holds on the fixed TT at the size of S48 and passes the decision rule again; it stays compiled in.
- Cap 13 against 15 on the fixed TT (2026-11-19, user's go, boxes): S40 (+40.7 for cap 13, no floor) and the box runs of
  2026-11-17 (floor on cap 13, cap 12) were played with the old TT rule. Run: `rel090` b00f14a (floor 4 and `epTTRep = 2`
  compiled; = `r90b`, see "Match builds and players") on both sides, `epLmrMax=13` (`r90b-m13`) against the compiled cap 15
  (`r90b`), 192+3.2 (= 60+1), 64 MB (see Hash below); batch 1 (m3a) under the exception noted there,
  concurrency 8. The boxes are near their shutdown (user): b1, b2 400 games, b3, b4 300 games (chained, to end before the
  shutdown; 1400 in all); planned as batches of a fixed 400 games (200 rounds) each (user, 2026-11-19;
  first plan 200), m3a and m3b in parallel on disjoint openings (`-Start` 7001 m3a, 7201 m3b, then 7401, 7601), as many
  batches as end before the shutdown, pooled and read once after the last one. The MEMO run `r90b-m13-r90b-tc60` (queued; first planned as `ttrp-m13-m15-tc60`) asks the same question and can be pooled.
  Prediction (before any game): -15 to +35, most likely +10. Reasons: with the old table both sides together give about +29 for
  floor on 13 against floor on 15 (+40.7 cap alone, +5 interim of the floor on cap 13, +16.7 floor on 15); regression of the
  selected +40.7, and the floor already takes part of the deep reductions cap 13 removes; with a working table re-searches are
  cheaper, so part of the cost of cap 15's deep reductions may be gone. 400 games +- about 27, 800 +- 19: a pass (LOS >= 95%)
  needs about +30 at 400, +20 at 800.
  Hash (2026-11-19, before any game; user's rule: hash by time control, 64 MB at 60+1): the batches and the MEMO run play
  with 64 MB, not 4 MB. Prediction kept (+10): with the fixed rule the 4 MB table at 60+1 loses less than before, so
  the larger table changes both sides little.
  MEMO on the new base (2026-11-20, user's go for the merge): `r90c-m13-r90c-tc60`, `r90c` with `epLmrMax=13` (`r90c-m13`)
  against `r90c` (cap 15), 60+1, 1000 games fixed (500 rounds, openings from 10301), 64 MB, read once at the end. It replaces
  the queued `r90b-m13-r90b-tc60` and is not pooled with the box batches (other base: `pvnt` searches 0.9 ply deeper).
  Prediction (2026-11-20, before any game and before the box batches are read): -15 to +30, most likely +7. Reasons: the +10
  above; `pvnt` deepens the search (+0.9 ply at 60+1), so the 0.76 ply that cap 13 costs (S37) weighs a little less, and
  part of the cost of deep reductions (fail low cascades after a re-search) is gone with it: a little smaller than on `r90b`.
  1000 games +- about 15: a pass needs about +15.
  Result on `r90b` (2026-11-20, S71): +8.2 +- 12.6, LOS 89.9%, 1400 games pooled; prediction +10 right; not decided.
  Box batches on `r90c` (2026-11-20; boxes on until 2026-11-21 09:00, user: small batches so that other small experiments
  can get a box): `r90c-m13-r90c-b<k>`, the same players, 192+3.2 (= 60+1), 64 MB, 300 games each (150 rounds), disjoint
  openings (b1 m3a from 20801, b2 m3b from 20951, chained after the `r90b` batches), pooled with the MEMO run and read once
  when the last one is done (same build and question, same prediction +7).
- Lim 6000 apart: H0 (expected) ends the single ramp steps at 10+0.1; a pass keeps 6000 and checks it at the long time control with
  the cap. Result: neither, +1.86 +- 3.50 after 20000 games (S36): no more ramp steps at 10+0.1; Lim 6000 stays a candidate for step
  A3 (the ramp speed at the long time control with the new cap).
- Lower priority, independent of A to D, in this order: TT replacement and mate bound fixes, a limit of the check extension (clean
  node statistics, KQKR still solved), time management change A, `pawnEndGame` in the eval phase.

### History and depth in the LMR (status: rejected in this form)
Now: the reduction depends only on the level and the move number. Within the same move number the best fifth by sort value cuts 3 to 5 times as often
as the worst fifth (S31, late moves at depth 1 to 3): history holds information the LMR does not use. Use the rank in the node or size classes, not
fixed values. 2026-11-11.
Forms: `lmrh` 4019537: `epLmrHG` g (one less from s <= -2^g), `epLmrHB` (one more without a bonus); only moves the table reduces.
Prediction (2026-11-11, before the results): G24 B1 0 to +8, most likely +3, H1 about as likely as H0; B1 alone about 0, H0 more likely.
Evidence:
| Date | Form | Test | Result | Ref | Reading then |
|---|---|---|---|---|---|
| 11-13 | G24 B1 on `chcm` | SPRT [0, 5], 10+0.1, 6078 games | -3.60 +- 6.30, H0 | S32 | both directions together do not pay; prediction (+3) wrong |
| 11-13 | G24 alone on `chcm` | SPRT [0, 5], 10+0.1, 3114 games | -9.93 +- 8.95, H0 | S32 | less reduction for good history costs; prediction (-2) right in direction |
| 11-13 | B1 alone on `chcm` | SPRT [0, 5], 10+0.1, 7584 games | -2.57 +- 5.73, H0 | S32 | even at best; prediction (about 0) right |
Prediction G24 alone (2026-11-13, before the result): -6 to +2, most likely -2, H0 much more likely.
Reading: the move number already holds most of the history (sorted by it), the adaptive level answers changes in the mean reduction.
Review when: the LMR itself changes (start level, PV nodes); the rank in the node and depth as a factor were not tried.

## Other search

### Small findings of the search review (status: partly done)
`Search/SEARCH-REVIEW-BUGS.md` findings 4 to 8 (2026-11-15). Decided with the user, 2026-11-15:
- 5 (TT node count past 32 bits into depth and type) and 8 (LMR parameters without range check): fixed without games, the engine
  is unchanged (12086912 nodes on tests.fen at depth 10, perft right, out of range values run). On `rel090` the node count and
  `epLmrFix`; on `lmfl` also `epLmrIni`, `epLmrNoD = 0` and the penalty table (the parameters exist only on the LMR branches).
- 6 (quiet moves after the 128th of a sort group dropped): a known limit, kept by choice (more than 128 quiet moves need about
  three queens; lifting it costs allocation or a length pass at every sort). No fix.
- 7 (`insertToPvs`: a mated root move kept in front like a mate): a bug, the rule means mates for the side to move. Costs a full
  window search of the losing move in the next iteration, in lost positions only. Fix: `>= mateScore - 255` instead of
  `pnearmate`. Changes the search there: with the mate guards (`mbnd`) in their [-5, 0] screen.
- 4 (stalemate not seen in a futility pruned zero window node): a bug, one way only (a real stalemate missed, never a false
  one): the stalemated side gets an upper bound below 0 instead of 0 at remaining depth 1 to 6, the PV search is right. Needs a
  king without legal moves and only illegal (pinned) other moves. History: the PV loop has "one legal move before pruning" since
  2018 (1d5b245, "otherwise mates or stalemates missed"); the zero window loop never had it (6244700 unified them with
  `zw ||`). The user remembers that the condition in zero window nodes cost strength (one quiet move searched in every
  node); not found in the history, plausible. So not the form of the review. Cheap form: no extra search; only when a node ends
  with no legal move made and some pruned, test the pruned moves for legality (make and undo, no search) and return 0 if none
  is legal and the side is not in check. Costs only in nodes where everything was pruned. With `mbnd` in its screen, as the eval
  clamp aligned with `nearmate`. Done in `mbnd` 4708209 (S47).

### TT replacement (status: adopted)
Now: a bug found by reading the code (review 2026-11-15, `Search/SEARCH-REVIEW-BUGS.md` finding 3), present since the TT
reorganisation years ago: `scoreReplaceLow` (`Hash/TransTab.hs:242`) masks with `lowc .&. 0xFFFF`, which keeps the move instead of
removing it: in a full cell the entry with the smallest move code is replaced (white moves before black ones), generation, type,
depth and nodes play no role (except the wrap test of the 8 bit generation). With 4 MB in the matches the table is full, more so at
long time controls: a candidate for part of the long time control weakness. The minimal fix (`complement 0xFFFF`) would order by
generation > type > depth > nodes: every entry of the previous move goes before a current depth 1 entry, every upper bound before any
lower bound whatever the depth (doubtful). Proposed rule (review): replace the entry with the smallest (depth - 4 * age, type, nodes),
age = generations since the store, modulo 256; 8 per generation as the second value. Related finding 5: the node count is not
limited to its 32 bits and can run into depth and type at the root (`quintToCacheEn`: `min 0xFFFFFFFF`).
Measure: only with fastchess (UCI): SelfPlay never calls `posNewSearch`, the generation stays 0 there (the age part is invisible in
node limited games and perf node counts); a hash size that fills at the time control (4 MB does); the proposed rule directly against
the current engine, at 10+0.1 and at a long time control. Not built.
Forms (2026-11-18, branch `ttrp` c64ad37 from `r90a`, parameter `epTTRep`, compiled 0; the rule is a field of the table, set
by `posToState`, kept by `resizeCache`; `writeCache` chooses the score function once per store):
| Form | Branch, commit | Parameters | Differs by |
|---|---|---|---|
| 0 | `ttrp` c64ad37 | `epTTRep=0` (compiled) | none: the old rule (smallest move code replaced) |
| 1 | `ttrp` c64ad37 | `epTTRep=1` | minimal fix: move masked away, generation > type > depth > nodes |
| 2 | `ttrp` c64ad37 | `epTTRep=2` | review rule: smallest (depth - 4 * age, type, nodes), age = generations since the store mod 256 |
Checks (2026-11-18): the three rules on hand built entries of one cell through `writeCache` (throwaway test, not committed):
replaced as intended (0: the smallest move code; 1: an older generation first, then type 0; 2: depth - 4 * age, an entry of the
previous search with depth 10 survives a current depth 1 one). Deterministic perf build, `SelfPlay -P -d 10` (generation always 0
there, so only depth, type, nodes act; anomaly detector, not a selector): tests.fen r90a 11367370, form 0 11367370, form 1 11050559
(-2.8%), form 2 11671539 (+2.7%); roots-500 r90a 87385187, form 0 87385187, form 1 81932525 (-6.2%), form 2 79218664 (-9.3%).
Perft of the 5 standard positions unchanged. Speed of form 0 against `r90a` (alternating perf builds, identical nodes, 9 pairs):
median nodes/s ratio 1.008 (pairs 0.86 to 1.75, this PC throttled; the middle pairs 0.98 to 1.04): no measurable cost. fastchess
smoke `ttrp` `epTTRep=2` against `r90a`, 4 MB, 10+0.1, 16 games: all normal endings, no time losses (posNewSearch at every go:
the generation advances in UCI games).
Chosen for the games: form 2 (the review's rule; form 1 carries the type over depth dominance and would say little).
Run plan (MEMO, prepared 2026-11-18, both default hash 4 MB, no `-Hash`; the boxes are busy until about 2026-11-19):
a) `ttrp-f2-r90a-tc10`: 10+0.1, SPRT [0, 5] (explicit bounds).
b) `ttrp-f2-r90a-tc60`: 60+1, 1000 games fixed (500 rounds, openings 2001 to 2500; earlier 60+1 runs: 1 to 500, 1001 to 1250),
read once at the end, LOS >= 95% to adopt.
Plan changed (user, 2026-11-18): b first (it decides); then a as a fixed 2000 games (1000 rounds, about 4.5 hours on MEMO), read
once at the end: under the time control decision it only has to show no loss beyond about 10 (+- about 12), an SPRT [0, 5] could
take a day or two. The predictions stand.
Prediction (2026-11-18, before any result): the old rule is a replacement by move code, close to random with respect to depth
and blind to age, so deep and current entries are lost in full cells; a depth and age rule should gain, more at the long time
control where the table is fuller. a) 10+0.1: -3 to +12, most likely +5; H1 slightly more likely than H0 (about 55%).
b) 60+1: -5 to +25, most likely +10 (LOS >= 95% needs about +14 in 1000 games: adoption by b alone about one in three).
Interim of b (2026-11-18, trend only, the run is read at its end): +145.3 +- 35.8 after 220 games, far outside the prediction.
Mechanism checked in the partial PGN (226 games, `C:\Learn\Data\results\memo\ttrp-f2-r90a-tc60`, `depths-part.txt`): at the same
time per move (1.68 s against 1.72 s) `ttrp` reaches 1.65 +- 0.09 ply deeper, growing with the game (0.76 below ply 20, 2.1 from
ply 120: the table fills) and with the score (3.7 at 10 pawns or more); no time losses, all terminations normal. So the old rule
makes the 4 MB table almost useless at 60+1. If it holds: every 60+1 result so far (LMR caps S40, floor S48, `r90a` S56) was
measured with a crippled table; the important ones need a check on the fixed base, and the gain with a bigger hash (GUI) is
smaller.
Result of a (2026-11-18, S59): +11.99 +- 10.99, LOS 98.4%, 2000 games: a gain also at 10+0.1 (prediction +5, at the upper
edge). Interim of b at 500 games (trend only): +128.6 +- 22.3. Fill of the 4 MB table (estimate, S59): writes per move about
0.2 to 0.3 of the entries at 10+0.1, 1.3 to 2 at 60+1, which explains the factor of about ten. GUI games at 5m+1s with 256 or
512 MB: about 0.03 to 0.1, the bug probably cost little there (unless no Hash was set: 4 MB).
Result of b (2026-11-19, S63): +121.1 +- 15.6, LOS 100%, 1000 games (prediction +10: wrong by an order of magnitude).
Adopted by the time control rule (60+1 decides, 10+0.1 no loss: +12.0). Merge of `ttrp` with `epTTRep = 2` compiled in on the
user's request (new perf reference then 11671539 on tests.fen depth 10).
Merged into `rel090` 2026-11-19 (dc3fb20, S64): `epTTRep = 2` compiled in, perf reference 11671539 (tests.fen depth 10).

### Hash option of the engine (status: done)
Now: both issues fixed on branch `hfix`, merged into `rel090` 2026-11-17 (S50); engine play unchanged (same nodes, perft).
(1) Start size: the engine starts with `newCache 1`, which the floor of 2^18 entries x 16 bytes makes 4 MB, but its UCI option
list announced `Hash` default 16, minimum 16 (`guiUciOptions`, `Main/Barbarossa.hs`). All matches so far played with 4 MB (the
records said 16). Fix on `hfix`: it announces what it does, `option name Hash type spin default 4 min 4 max 1024`; the start size
is unchanged (16 would change every run without `-Hash` and break the comparison with the earlier runs).
(2) Leak: `setOptionHash` allocated a new table and never freed the old one. fastchess sends `setoption name Hash` after
`ucinewgame` before every game and the engine applies it every time (refused only during a search, which is right): in a piped
test with 256 MB the process grew 261, 488, 725, 957 MB over 4 games. With `-Hash 64` a 2000 game run at concurrency 8 (about
250 games per engine process) would need about 16 GB per process. Fix on `hfix`: `resizeCache` (`Hash/TransTab.hs`) keeps the
table when the new size gives the same number of entries, otherwise frees the old table and allocates the new one. Safe: setoption
is applied only when not `working`; then the search thread has ended (`finishSearch` comes after its last table access, the
bestmove report reads no table) or was killed (`doStop`: `stopSearch`, then the synchronous `killThread`); the snapshots
(`forGui`, the status in `finishSearch`/`stopSearch`) are used only for the bestmove report; table reads are strict IO (`peek`).
A kept table keeps its content into the next game, as without `-Hash` (`ucinewgame` does not clear it).
(3) New tables are zeroed (`callocArray` instead of `mallocArray`): the content no longer depends on memory reuse after a free.
Every build before the fix leaks: `-Hash` only when both engines of a run have the fix.
Checks (2026-11-17): deterministic perf build, `SelfPlay -P -i tests/data/tests.fen -d 10`: 12086912 total nodes on `rel090` and
on `hfix`; perft of the 5 standard positions unchanged (4865609 d5, 4085603 d4, 11030083 d6, 15833292 d5, 2103487 d4). Piped UCI
test, 6 games of 2 searches at 300 ms, setoption before each game (working set after each game): 256 MB each time: 273, 287,
289, 289, 289, 289 MB (`sfen` build as the leaking reference: 268, 481, 677, 862, 1056, 1267 MB); sizes 64/256 alternating: 97,
246, 92, 242, 92, 246 MB; 12 bestmoves in each run. fastchess smoke, `hfix` against `hfix`, 2+0.02, `option.Hash=64`, `-l 5`,
20 games, concurrency 2: 4 engine processes, each flat at 95 to 101 MB over the run (sampled every 3 s); 19 games ended normally,
1 time forfeit (move 24, the engine spent 0.3 to 0.5 s on early moves of a 2 s game: time management at this time control,
no setoption during a game).
Interpretation history: 2026-11-17: the lab session read "the engine applies the first setoption and ignores the later ones (still
working), no leak"; its test harness sent `go` without a time limit and never `stop`, so the engine was still searching when the
next game began. Corrected the same day (S49).

### Aspiration windows (status: parked)
Now: built as parameters on `aspw` 0e52350 (from `ttrp`), measured by node counts only (S60, `Search/ASPIRATION.md`): with the
PV node fix (`epPvsNt=1`) no window saves nodes (36 to 75 within about +-2% at depth 10 and 12, narrower ones cost 7 to 19%);
without it every window cost much (the defect, block "PV node type after a fail low"). Moderately sure for the nodes;
games are not measured. 2026-11-18.
Forms:
| Form | Branch, commit | Parameters | Differs by |
|---|---|---|---|
| old | `useAspirWin` (off since 2013) | window 24 (`Uci/UciGlue.hs`; 36 in the dead `Search/Thread.hs`) | one try, then full window |
| A | `aspw` 0e52350 | `epAspW=w`, `epAspGrow=0` | as old, window as parameter, no aspiration at mate or clamp scores |
| B | `aspw` 0e52350 | `epAspW=w`, `epAspGrow=4` | the failing side widened by 4, up to 2 more tries, then full window |
Evidence:
| Date | Form | Test (method, conditions) | Result | S<n> | Reading then |
|---|---|---|---|---|---|
| 2026-11-18 | A, B, w 15 to 75 | node counts, perf build, epTTRep=2, roots-500 and tests.fen depth 10, 100 roots depth 12 | with epPvsNt=1: w 36-75 about 0 (+-2%), 25 +7%, 15 +19%; with epPvsNt=0: +4% to +340% | S60 | no node gain; the defect hid it before |
Review when: `epPvsNt` adopted; then possibly a game test of w 50 or 75 with growth 4 (TT bounds, order after a fail and the
fallback move on timeout are not node questions). Low priority.
Next: nothing before the PV node type is decided.

### PV node type after a fail low (status: adopted)
Now: found while measuring aspiration (S60, 2026-11-18): a PV node whose first move fails low searched all its moves as PV
nodes (full window, no LMR, no null move), recursively; `epPvsNt=1` moves to zero window after the first move, as in PVS.
-12% nodes on tests.fen, -16% on roots-500 (depth 10, epTTRep=2). Fewer nodes are not Elo: the extra effort sat in the
unstable nodes. Not tested in games.
Forms:
| Form | Branch, commit | Parameters | Differs by |
|---|---|---|---|
| 1 | `aspw` b54d34d | `epPvsNt=1` (compiled 0) | `checkFailOrPVLoop`, `checkFailOrPVRoot`: `nxtnt` moves on also after a fail low |
Evidence:
| Date | Form | Test (method, conditions) | Result | S<n> | Reading then |
|---|---|---|---|---|---|
| 2026-11-18 | 1 | node counts, deterministic perf build, depth 10, epTTRep=2 | tests.fen 11671539 -> 10251160, roots-500 83.2M -> 69.7M | S60 | an efficiency defect; games decide |
Branch `pvnt` (2026-11-18, from `ttrp` c64ad37): 8ad4922 identity, 8c61267 `epPvsNt = 1` compiled in (the parameter stays,
`epPvsNt=0` is `ttrp`). Checks (feature session): `epPvsNt=0` identical to `ttrp` (11367370; 11671539 with epTTRep=2); new perf
reference tests.fen depth 10: 10606600 (-6.7% with the compiled epTTRep=0; -12.2% with epTTRep=2); roots-500 depth 10 with
epTTRep=0: 81.4M -> 71.0M (-12.7%). Data `C:\Learn\Data\measurements\search\pvnt`. Fast build (integration session) `id name
Barbarossa 0.9.0 pvnt`; smoke on mini against `ttrp`, both `-p epTTRep=2`, 10+0.1, 16 games: normal endings, no time losses.
Run plan (2026-11-18): MEMO `pvnt-t2-ttrp-tc10`, `pvnt` against `ttrp`, both with `epTTRep=2` (the fixed TT rule, on which
the saving is about twice as large; the only difference is the fix), 10+0.1, 2000 games fixed (1000 rounds), 4 MB, read
once at the end; then 60+1 by the result.
Prediction (2026-11-18, before any game): about 12% fewer nodes at the same depth act like a speedup of 0.18 doublings, an
upper estimate of 8 to 14 Elo if the search quality stays; the saved nodes are the unstable ones (a zero window fail high not
confirmed), which the old code searched more thoroughly, so the quality may go either way. 10+0.1: -5 to +15, most likely +6.
60+1: -5 to +15, most likely +5.
Result at 10+0.1 (2026-11-19, S62): +2.8 +- 11.1, 2000 games, LOS 68.8% (prediction +6): no loss, no clear gain; the node
saving does not arrive as a speedup. 60+1 decides: `pvnt-t2-ttrp-tc60` on MEMO, 1000 games, openings from 4001, read once
at the end (prediction above: +5).
Result at 60+1 (2026-11-19, S67): +56.8 +- 13.7, 1000 games, LOS 100%; prediction wrong, far too low. The gain appears in long
searches only (S62 at 10+0.1: +2.8), like the TT fix: the cost of the PV cascade after a fail low grows with the depth.
Adopted by the time control decision; merge into `rel090` on the user's request (then a new frozen base, `r90c` = `r90b` +
`pvnt`). Depth at the same time (S67): +0.93 +- 0.05 ply, growing with the game (+0.33 to +1.19) and the |score| (up to +2.1):
the mechanism confirmed. Merge (user, 2026-11-19): after the box runs (cap 13 against 15 on `r90b`), for the same comparisons.
Merged 2026-11-20 (user, earlier: the box runs play the frozen `r90b`, a moving `rel090` does not touch them): `rel090` 55c9bac,
conflicts only in the identity and the parameter list; perf tests.fen depth 10 10251160 (11671539 with `epPvsNt=0` = `r90b`).

### `improving` (status: open)
Now: not built; `staticScore` two plies back is on `stack`, so cheap. 2026-10-31.

### Check extension without a limit (status: running)
Now: every check is extended by one ply (`exten`), with no limit by ply or count; repetition is the only stop. In queen endings this
makes the explosions of the high LMR levels (S42): with the mate guards (S43) and no extension beyond ply 2 * draft position 362
needs 8.9M nodes at level 19 instead of 252M; without the check extension 474k (S44). 2026-11-15.
Forms: B: no extension beyond ply 2 * draft (diagnostic build, not committed); a limit on the number of extensions per line; no check
extension in reduced lines; an extension only while the line is not too far below the draft.
Measure: roots-500 node scan by stratum (queen endings apart), a set of KQKR and mate by check positions (solved, time to mate), then
an SPRT with no-loss bounds; on top of `mbnd` (the guards), against `chcm` or `mbnd`.
Review (2026-11-15, `TIME-MATE-EXTENSION-REVIEW.md`, read only): a one-sided check chain costs a ply every two plies and ends near
ply 2 * draft by itself; checks of both sides in turn cost nothing, and a reduced line at depth 1 keeps extending (checking moves
are not exempt from LMR, `noLMR` is passed pawns only). Recommended form D: extend only while `ply + d >= draft - k` (k 2 or 3, no
new state; the full depth re-search is back above the threshold), with the ply limit 2 * draft as backstop, both as parameters
compiled off first; the QS still sees a mate in one by check (all evasions in `qsInCheck`); the last queen / last rook extension
stays. In the 4+1 log the S44 mechanism shows in a won queen ending (game 7: drafts 7 to 16 in 0.86 s, 144 moves to the end), not
in the losses. Test plan in the review: S43/S44 positions, mates by check, game 7 roots, 100 KQKR positions played out at fixed
nodes, then SPRT [-5, 0] against `mbnd` and a fixed match at 60+1; cap 19 against 15 with the fixes at 60+1 (the -74 of S40 may
contain the explosions).
Test sets (2026-11-15): the 652 roots of the 4+1 log of S41 (BarbarossaTuning `log_roots.py`): all, the 206 with a final mate
score (+-20000), the 184 of game 7 (won queen ending); `C:\Learn\Data\measurements\search\mbnd\roots-ics`.
Next: after the LMR results; it also cleans the node statistics of the LMR scans.
Built (2026-11-20, feature session): `chxd` 0250bfb (identity 675aa85) from `rel090` 55c9bac (= `r90c` code), two parameters
compiled 0 = off: `epChkExK` = k (form D: a check is extended only while ply + d >= draft - k) and `epChkExP` = p (no
extension into a child beyond ply p * draft; the review's backstop is p = 2). A check that also takes the last queen (or the
last rook without queens) keeps its extension. Deterministic perf, tests.fen depth 10: off 10251160 (= `r90c`), K=2 10236014,
K=3 10442532, P=2 10251160 (never reached; P=1 10685292), K=3 with P=2 10442532. Perft right.
Mates (206 mate roots of the 4+1 log, depth 10, first depth with a mate score): off 182 found, K2 183, K3 184, K3+P2 183;
none lost; total nodes within 2% (108.8M off, 110.7M K3).
Finding (2026-11-20): on `r90c` the S44 explosion is gone (362 at level 19: 112k nodes off, S44 252M); the cause is `pvnt`:
with `epPvsNt=0` 362 needs 225M (floor 0), 468M with `epTTRep=0` as well, and K=3 cuts the latter to 53.5k. Not the floor or
the TT rule (50k each with only that one reverted). The mechanism stays, `pvnt` hides it in these positions; the limit
removes it. Explosion positions on `r90c` (182/362/485, depth 12, levels 15 and 19): all below 200k, off or on.
Run (2026-11-20, user's go): `chxd-k3-r90c-tc10`, `chxd` with `epChkExK=3` (`chxd-k3`) against `r90c`, MEMO, 10+0.1, 16 MB,
SPRT [-5, 0] (non-regression: the aim is safety against the explosions, not Elo), cap 20000 games (10000 rounds, openings
from 10801). Then, if it passes, a fixed match at 60+1. P stays off (it never acts at the drafts of these tests).
Prediction (2026-11-20, before any game): -3 to +3, most likely 0; H1 (pass) more likely than H0. Reasons: on `r90c` the
explosions do not occur in the test positions, so the limit removes little that costs time; the mates are not lost; the
node totals move by 2%, within the chaos of the tree.

### Singular extensions (status: open)
Now: not built, needs good ordering and TT first. 2026-10-31.

### Time management at long time controls (status: open)
Now: S41 (2026-11-14): in an online tournament at 4+1 (`0.8.0 w5`, 12 games against other engines) half of the clock is
gone by about move 20 and most of the endgame is played on the increment (games with 10 to 37 moves under 10 s). Two parts of
`compTime` (`Main/Barbarossa.hs`, the same in `chcm`): a single move may take up to about 15% of the remaining time (often
reached: 25 to 47 s against a target of about 6 s), and the moves to go come from the score (`estimateMovesToGo`: 50 below
1 pawn, 10 at 4 pawns, 2 at 8 pawns), so a large score spends the clock as if the game ended soon. Self-play matches cannot
show it (both sides waste alike). A candidate explanation of the weaker play at long time controls against other engines.
Measure: a change of the time management against the same engine with the old one (asymmetric, so self-play works), at a
long time control; the clock at moves 20 and 40 and the moves under 10 s from the logs (Info level). Not built.
Background (user, 2026-11-14): many experiments with the time management in earlier versions; none was at least as good at
60+1 and better at 300+1, all failed badly: the time management matters much for the strength. No sensible formula was found
to keep it for short games (60+1) and change it for longer ones. 0.8.0 still had the LMR explosion fixed since; with `chcm`
the time use is expected better, not much.
`chcm` at 4+1 (tournament 2026-11-14, S41): the same as 0.8.0 (clock 29% at move 20, 3% at move 40, 53% of the moves under
10%, used 1.6 times the target), with deeper searches; at 60+1 in the GUI it keeps 58% / 26%. The cause is the time control
(the share of the increment), not the engine version. In the 8 lost games the clock went under 10% at moves 21 to 51 with
scores of -136 to -504, before the game was decided; playing on the increment after a mate is found costs nothing (user: mate
scores without the distance, on purpose, longer games but no lost mate). The user's reading: right for 60+1, not for longer.
Review (2026-11-15, read from the code, nothing measured: `TIME-MANAGEMENT-REVIEW.md`): the target is not the cause (at the target
every time control spends about 2% of the clock per move), the maximum is: the extra above the target is 15 to 18% of the clock,
9.4 increments at the start of 60+1, 37 at 4+1; the score-drop paths (`exte`, `timeProlongation`) send moves to the maximum move after
move in sinking games. Change A: compute the extra from the clock capped at 60 increments (identical at 60+1 for every score and
repetition; 4+1 maximum at the start 16.0 s instead of 43.0 s, 5+3 37.8 s instead of 55.8 s); a first form (flat bound of 9.44
increments) was not identical at 60+1 for scores of a pawn or more (user's check, corrected). Change B, only if needed after A: the
losing side divides only the first 60 increments by the short moves to go. Test: asymmetric self-play at 60+1 (constraint: capped
moves about zero), 4+1 (main, fixed games), 5+3; then a gauntlet at 4+1 against other engines.
Next: after the search experiments (user).
Interpretation history: 2026-11-14: first seen in the tournament log of 0.8.0; the same evening confirmed for `chcm` at 4+1,
not seen at 60+1: a long time control effect of `compTime`, not of the LMR explosion.
2026-11-15 (review `TIME-MATE-EXTENSION-REVIEW.md`, the 4+1 log of S41 read again, 652 searches): the mate bound bugs and the
check extension do not explain the clock: the 8 lost games reach -500 with 2 to 9% of the clock left, the decided phase is played
on the increment (2% of the time over target). The time goes in moves 1 to 10 and to the score drop rule (`exte`, no stop
before the maximum, and `reds`, a longer target). (Corrected by the next review: the 109 moves / 36% were selected by the final
score; by the mechanism the drop gives 124 moves 609 s of extra time, 20%.) So change A and the score drop rule come first. Second lever, for the decided phase: the search goes on after a mate score (mate
scores carry no distance, the move cannot change): 174 s, 6% of the time; stopping at a stable mate score is a candidate.
2026-11-15 (review `TIME-SCORE-DROP-REVIEW.md`, replay of the logged drafts without the drop handling; 4+1 log and the 60+1 GUI
logs): the drop gives extra time to 124 of 652 moves at 4+1 (609 s, 20%, 4.9 increments per move) and 196 of 1753 at 60+1 (315 s,
9.5%, 1.6). The best move changes in one of nine at both; the changes come within 3 to 5 increments, the long extensions (10 or
more increments: 21 moves, 368 s at 4+1) changed one move. The larger part comes from `reds` (`timeProlongation`), not from `exte`.
Change A alone saves the same at 4+1 as removing the whole drop handling (604 s) and touches 6 moves at 60+1: A is the first
score-drop rule. Second rule D, after A in its own run: on a clock above 60 increments the drop gives at most 5 increments over the
target (`epTmDropIncs`; 4+1: 132 s more, 1 other move; 60+1: 3 moves, 14 s). Last-draft-only and two-draft confirmation act on
`exte` only: little at 4+1 and they change 60+1, not recommended. Order: A, then D against A, then B.

### Standard FEN from `posToFen` (status: done)
Was: `posToFen` (`Moves/Notation.hs`) ended the board with a '/' (`extline` appended one after every rank); python-chess and
other tools reject that (BarbarossaTuning `log_roots.py` stripped it). Found 2026-11-15 in the 4+1 log.
Done 2026-11-17 on branch `sfen` (from `rel090`): the ranks are joined with '/' (`intercalate`); the move number stays 1 (`MyPos`
does not keep it, 1 is valid), the halfmove clock was already written. Old data and logs will not be reused (user decision), so
the written files need not match old ones. Readers: the parser (`fenToAssocs`, `Moves/Core.hs`) stops at the end of the string or
below a1 and takes both forms, no change; `Tune/LogScan.hs` `sameReplayFen` compares log FENs with fresh `posToFen` output and
now drops a trailing '/' of the board (old logs still match); the other comparisons there are between log FENs of one engine.
BarbarossaTuning: `log_roots.py`, `uci_data.py`, `sample_roots.py` strip the slash (now a no-op), the `hv_*` parsers walk the
board char by char: no code change needed (only the comments about the trailing '/' updated).
Checks: deterministic perf build, `SelfPlay -P -i tests/data/tests.fen -d 10`: 12086912 nodes before (rel090 + identity) and
after; perft of the 5 standard positions: known counts; round trip parse -> `posToFen` -> parse on 1528 positions (tests.fen and
the Origin root/leaf FENs of 2 old logs, 1479 with the old trailing '/'): same position (bitboards, ep/castle/50-move, zobrist,
PST sums), no slash written, python-chess accepts all; a SelfPlay `--data` file (437 FENs) and a UCI `-d` file (239 FENs, 2 games)
read by python-chess without a reject. Not a search change. Merged into `rel090` 2026-11-17.

### Mistakes in lost games: tracing the refutation through the tree (status: open)
Now: an idea and a method, nothing built. 2026-11-16 (user's idea, method discussed in the session).
Idea: learn from games lost against (much) stronger engines. Two extreme trajectories, mixed in real games: an equal game that
turns at one move (a clear critical position), and a slow loss with ever harder positions (no single move). The first is the one
to analyse per game; the second only statistically (positions where our score stays too optimistic for many moves against a
reference: biased eval terms, closer to eval tuning). Games against stronger engines: online tournaments (S41: 8 of 12 games end
with a mate score against us) or a few dedicated games; our matches are self-play. Time problems excluded.
Method:
1. Locate: a reference engine over the game marks our first move after which its score drops sharply (e.g. more than 1 pawn)
   while ours stays level; the `-d` data or the log gives what we believed there (root and leaf score). The reference only marks
   the position, it is no label (the SF labels were negative for tuning).
2. Classify: search the critical position again with one pruning off at a time (LMR, futility, null move, RFP, LMP) and with
   10 times the nodes: the first that finds the refutation names the culprit (search). If deep search and every switch keep our
   move, compare our eval with the reference along its line: an eval misjudgement, not search.
3. Trace (the core, user: "extremely useful"): a debugger-like walk of a given line (our move, their refutation, ...) through
   our real search, with all reductions and pruning in place. At each ply of the line: was the node reached, at which remaining
   depth and window, the move's index in the order (and why: TT, killer, counter, history value), its LMR reduction and the level,
   whether futility, LMP or a null move or RFP cut ended the node before it, and the score it returned against alpha and beta;
   step by step down, as far as the search went. That turns "why was the counter move not seen" into numbers (e.g. quiet move at
   index 23, reduced by 4 at depth 7, the re-search never came) and shows the misses the census cannot count (S42). Form to work
   out: a probe in SelfPlay (FEN and a line in UCI moves; a hook in the search that recognizes the path by its move stack and
   logs at the nodes on it, behind a build flag like `lmrcensus` so the engine stays unchanged); the deterministic perf build
   makes the walk repeatable, and the probe can then rerun with a parameter changed to see where the line's fate changes.
4. Collect: the critical positions with the explanation found by the trace (culprit, ply, numbers, the reference line) into a
   lasting database of positions where the engine errs, to guide further development: which mechanism fails how often, and as a
   test set (solve rate at fixed nodes, like mate206). A diagnostic, not adoption evidence: a fix from it still passes the SPRT
   and the long match, and a solve rate is not Elo.
Next: after the LMR decisions. First the probe (3); then 3 or 4 lost games through 1 to 3, to see what kind of positions come out.

### Syzygy tablebase probes (status: open)
Now: design only (2026-11-17, `Search/TB-SYZYGY.md`): WDL in the tree, DTZ at the root; Fathom (C, MIT) by FFI recommended over a Haskell
port (2-4 days against 3-5 weeks); off by default, so the engine stays unchanged without `SyzygyPath`. Not built. For after the search
work; a match needs the tables on every match machine.

### Threaded search (status: open)
Now: after the lower fruits (user). Per-thread state arrays would fit. 2026-10-31.

## Speed

Search unchanged, so no games are needed where nodes stay identical. Shares and method: `PROFILING.md`.

### Fewer modules, smaller interfaces (status: rejected in this form)
Now: no effect on speed; more unfoldings neither. Explicit exports stay for cleanliness (`expo` 3227638). Merging modules not built. 2026-10-31.

### Statistics counters off in the engine (status: adopted)
Now: -2.2% time, -4.6% allocation measured in SelfPlay, same nodes; on `expo` 1e7130b, not re-measured in the engine. 2026-10-31.

### Filter before the quiet sort (status: adopted)
Now: +3.0% nodes/s against `expo`, 8% less allocation per node, but 2.5% more nodes (LMR move numbers), time to depth -0.5%. Games decide. 2026-10-31.
Forms: `fsrt` 696882e, `epQFilt=1`. 2026-11-01: non-regression SPRT [-5, 0] against `expo` passed: +6.68 +- 7.72, 4422 games (S22).
2026-11-01: always on in `fbas`, parameter removed.

### Older speed and simplification ideas (status: open)
Now: checked against the code of `lmfl` (2026-11-17, `SPEED-IDEAS.md`): open are per-colour constant records, move generation directly
into vectors, the move coding and generation pipeline (one-step move building first, special moves by a table of 208 entries) and
information from earlier plies; three older ideas dropped with reasons. Speed work comes before a release (search unchanged, node
counts identical); nothing measured. 2026-11-17: the one-step move building done on `spdg`, merged (S51); +5.5% nodes/s here,
+4.5% on MEMO (`PROFILING.md`, "Move generation without lists of pairs"); the others still open.

### Other speed candidates (status: open)
Now: each small, `PROFILING.md` lists them: search state in mutable arrays by depth (user; half of the allocation is in the search itself), quiet generation
without pairs, insertion sort for captures, pawn check in `genMovePromo`, lazy selection in `dirSort`. A realistic total of maybe 10-20% speed. 2026-10-31.
2026-11-17: quiet generation without pairs, insertion sort for captures and the `genMovePromo` test done on `spdg`, merged (S51).
Engine unchanged (12086912 nodes, perft), allocation -6.2%, +5.5% nodes/s on this PC (median of 7 pairs, +4.4% to
+7.9%); MEMO +4.5% (7 pairs, +2.9% to +6.0%). Open: search state in mutable arrays, `dirSort`.

## Eval

Reviews of the eval and of the tuning method from reading the code (2026-11-14, `lmrm`, nothing measured), for the eval phase
after the search work: `Eval/EVAL-REVIEW-EXISTING.md` (king attack, passed pawns), `Eval/EVAL-REVIEW-MISSING.md` (safe checks,
pawn shelter and storm), `Eval/FORK-TERMS.md`, `Eval/TUNING-REVIEW.md`, `Eval/PAWN-ENDGAME-BUG.md`.

### `pawnEndGame`: square used as the distance to promotion (status: adopted)
Now: a bug, in the engine since before 0.8.0 and in `chcm`: in `pawnEndGame` (`Eval/Core.hs`, the special eval for kings and pawns
only) `mim`/`miy` take the square of the escaped pawn nearest to promotion (`fst`) instead of its distance (`snd`). Read from the
code (`Eval/PAWN-ENDGAME-BUG.md`): with one escaped pawn the score falls 64 cp per step for White (rises 64 for Black, 8 meant);
with escaped pawns on both sides the race compares square numbers, so a won race can be scored about -750 (example
`7k/1K6/P7/8/7p/8/8/8 w`: as coded -752, meant +984), and a position and its colour flip get different scores. The tuning does not
see it (`posExactEval` skips the special evals); the search repairs it once the promotions are inside the horizon, the damage is at
leaves where going into a pawn race is decided. Fix: `snd` in the two lines (or `minimum $ map snd`). Not an identity change
(perf node counts change where pawn endings are reached). Measure: the example and its flip, the table positions (+8 cp per step
for both colours), a set of pawn endings with known results; a match hardly shows it (rare). Prediction: 0 to +3 Elo, not
measurable by a match of normal size. Not built.
Next: fix it in the eval phase, after the search work (user, 2026-11-14: to be corrected, but little strength expected).
Interpretation history: 2026-11-14: found in the eval review, confirmed by reading `Eval/Core.hs` lines 1186-1195.
Built (2026-11-18): `pegk` bf8d52b from `r90a` (the frozen base: `chcm` + floor f = 4 + spdg speed), identity `pegk`: `mim`/`miy`
= `minimum $ map snd` (the distance), imports `minimumBy`, `comparing` removed; the smaller points of the doc unchanged. Checks
(perf build, `TuneSGD -X` special score): the example and its colour flip before -752 / +872, now +984 for both; the table positions
(a- and h-pawn, ranks 2-7, both colours) now +8 cp per step for both colours and equal for the flip (before White -64, Black +64 per
step); perft unchanged; perf nodes tests.fen d10 11367370 (unchanged), the 14 endgame positions of spdg (local
`endgames.fen`) d14 6327943 -> 6327940, 40 pawn endings from self-play (r2-gen2) d12 2588328 -> 2747418.
Run: together with the KPK bitbase below (one branch, one run), see there.

### KPK bitbase (status: adopted)
Now: design only (2026-11-17, `Eval/KPK-BITBASE.md`): an exact KPK table of 24 KB, generated by retrograde passes (lazily at run
time or as a generated module), used in `specialsOf` before `pawnEndGame` with exactly one pawn; checked against Syzygy. About one
day. Today the engine cannot tell a won from a drawn KPK. Eval phase, with the `pawnEndGame` fix above.
Built (2026-11-18): `pegk` e04a604 (on the `pawnEndGame` fix bf8d52b), variant 1 of the design: `Eval/KPK.hs`, a top level table
(196608 normalised positions, 24 KB) computed by retrograde passes at the first use (68 ms), forced at `isready` (first `isready`
117 ms against 50 ms for `r90a`, the second 0.1 ms; SelfPlay pays it once per process). `kpkEval` in `specialsOf` before
`pawnEndGame` with exactly one pawn: win = 1000 + 10 * relative rank of the pawn (side with the pawn), everything else 0.
Checks: the table against Syzygy KPvK (python-chess), all 196608 indices, both sides to move: 0 mismatches (165676 valid, 111282
wins); through the eval (`TuneSGD -X` special score) all 662704 legal KPK positions (both pawn colours, all files, both sides to
move): win / draw as Syzygy, values as designed, colour flip and file mirror twins equal (scores from the side to move); 15
classical positions (opposition, king on the 6th, pawn on the 7th with stalemate, rook pawn with the defender in the corner or in
front, key squares) right. Perf nodes: tests.fen d10 11367370 (unchanged), endgames d14 6327940 -> 2844303 (per position small
changes, the total also by the table and history carried between positions), pawn endings d12 2747418 -> 2762456; perft unchanged.
Smoke on mini (16 games 10+0.1, default hash, against `r90a`): no time losses, no crashes, 1-7-8 (no strength); the one game with
KPK (pegk White) was won by the bitbase as Syzygy says, then drawn in KQK by the 50 move rule: block "Lone king endings above the eval clamp".
Run plan (prepared, not started; MEMO by the user): `pegk-r90a-tc10`, `pegk` against `r90a`, 10+0.1, non-regression SPRT [-5, 0]
(a correctness fix of rare positions; a gain is not measurable at a normal size), default openings, 4 MB. Optional second run,
more sensitive: the 2000 endgame starts `endg-rm-2000.epd` (data catalog: K, pawns and one piece each, R-R or minor-minor, balanced,
from self-play leaves), 4000 games fixed (each start with both colours), read once at the end; needs an openings parameter in
`run_timed.ps1` (lab), not built.
Prediction (2026-11-18, before any result): default openings -1 to +3, most likely +1, H1 (no regression) very likely; endgame
starts +2 to +12, most likely +5 (pawn endings and the trades into them are frequent there; KQK unchanged in both engines).
2026-11-18, with the third commit (lone king endings, 3cdb421; block "Lone king endings above the eval clamp"): the run is the
same, `pegk` 3cdb421 against `r90a`. Prediction again, before any result: default openings unchanged, -1 to +3, most likely +1,
H1 very likely (lone king endings are rare from the openings, and `r90a` mates most of them with time). Endgame starts
(`endg-rm-2000.epd`, 4000 games fixed): +3 to +15, most likely +6: the pawn endings as before, and the wins after a promotion
(KQK, KQRK, KRBK) that `r90a` scores flat, KBNK to the right corner (rare there).
Prediction revised (2026-11-18, before any result, with the frequency of the lone king draws above): default openings -1 to +3,
most likely +1 = about +0.3 from the lone king endings (measured frequency) and the rest from the pawn endings (KPK, race; not
measured, smaller than +1 by the rarity); H1 very likely, a gain not measurable at this size. Endgame starts: +3 to +15, most
likely +6 = the lone king share some times larger than with the openings (more promotions from these starts: estimate +1 to +2)
and the pawn endings, which arise there often (the larger part).
Runs packed for MEMO (2026-11-18): `pegk-r90a-tc10` (SPRT [-5, 0]) and `pegk-r90a-endg-tc10` (the endgame starts, sequential, 2000
rounds = 4000 games fixed; `run_timed.ps1` got `-Openings` and `-Order` in lab); smokes again: 16 + 8 games, no time losses or crashes.
Plan changed (user, 2026-11-18): both runs dropped. The fixes are proved by their checks (Syzygy, play-outs, symmetry); the gain is
too small for a match of normal size. Instead a sanity run `pegk-r90a-tc60s`, 60+1, 100 games (50 rounds, openings from 3001),
next on MEMO: only crashes, time losses or anything odd count. After it: merge `pegk` into `rel090`.
Result (2026-11-18, S57): +3.5 +- 45.9, 100 games, 27-26-47, no time losses, no crashes: clean. Merged into `rel090`
(305043d, identity 550e38a); the code is `pegk` 3cdb421 apart from the identity, so its checks stand (tests.fen d10 11367370).

### Lone king endings above the eval clamp (KQK and the family; status: adopted)
Now: found 2026-11-18 in the `pegk` smoke (in `r90a` too, so in `chcm` and `rel090`): `scoreToMate` (`Eval/Core.hs`) gives KQK
21440 to 22370 (`TuneSGD -X` special score, e.g. `8/7K/8/3k4/8/8/1Q6/8 w` 21440), above the clamp of the static eval (19744,
`matesc` / `evalMax`): `posEval` clamps every KQK position to 19744, the drive to the edge and the king distance are lost. In
the smoke game the winning side shuffled the queen 100 plies at +197.44 and the 50 move rule drew it (search depth 10-16 did not
reach the mate); `r90a` shows the same flat 19744 on that position (UCI, depth 15). KRK (16760 to 17690) is below the clamp and
keeps its gradient. Fix: keep `scoreToMate` below the clamp for the queen (e.g. without the material term or a smaller scale), a
separate branch from `r90a`; check: KQK and KRK mates from a set of positions within the 50 moves, special scores below 19744 with
the gradient. Not built.
2026-11-18, review of the main session: the whole family goes through `scoreToMate` (`mateKMajxK`, KBBK, KBNK). Measured
(`TuneSGD -X` special score from the strong side, 6000 random legal positions each, both sides to move): KRK 16760-17690, KBNK
17750-18690, KBBK 18080-19010 below the clamp; above it, so flat: KRBK and KRNK 19880-20810, KQK 21440-22370, KRRK 21680-22610,
KQBK 24560-25490, KQRK 26360-27290, KQQK 31040-31970. A plain cap is no fix (KQRK and KQK both at the cap: losing the rook free).
Found with it: `lightSquares` and `darkSquares` (`Moves/Internal/Pattern.hs`) are swapped (bit 0 = a1 is dark), so `mateKBNK`
drove the weak king to the corner of the wrong colour (dark bishop on c1: highest score with the king on a8); the other uses
(bishop pair, opposite bishops, bishop and pawns of its colour) pair the masks with each other and do not change.
Built (2026-11-18): `pegk` 3cdb421 (third commit of the package, same run against `r90a`): `scoreToMate` = 10000 + 9250 * m /
(m + 2000) + 5 * (edge^2 - kingdist^2), m the material of the strong side in 2^shift2Cp units: strictly increasing in the material,
at most 19650 for any material, the progress span 5 * 126 (was 10 * 126); the clamp and its name unchanged (`mbnd` renames it:
the merge touches other lines). The masks swapped back to the right colours. Checks: the special scores now KRK 16496-16961, KBNK
16759-17229, KBBK 16924-17389, KRBK = KRNK 17327-17792, KQK 17575-18040, KRRK 17607-18072, KQBK 17913-18378, KQRK 18050-18515,
KQQK 18297-18762 (all below 19744, ordered by the material; KRBK = KRNK as bishop = knight in the material); in each: edge above
centre, kings near above far; KBNK highest in the corner of the bishop's colour (dark bishop h8, light a8, before the reverse);
the `TuneSGD -X` export of 100000 data positions identical after the mask swap. Play-outs (one engine on both sides, 100 ms per
move, both colours): 20 fixed starts (the smoke position, centre starts of the 10 endings): `pegk` 20 mates of 20, `r90a` 14
(KRBK and both KBNK drawn by the 50 move rule); 60 random centre starts: `pegk` 60 of 60. Perf nodes: tests.fen d10 11367370
(unchanged), endgames d14 2844303 -> 1952007 (1925265 with the compression alone), pawn endings d12 2762456 -> 2789257; perft
unchanged.
Seen in the play-outs (search, not changed here): with mate scores without the distance (block "Time management", the user's
choice) the engine reports a mate in 2 or 3 at every move and plays a check that keeps it, missing mates in 1 (`Qee3#` in
`8/8/8/8/3k4/1Q6/8/K3Q3 w`, also in `r90a` and `mbnd`); the mate comes when the 50 move rule makes the delaying lines draws.
KQQK mated on the 50th move in 4 of 6 random starts, KQK up to 45 moves, KQRK 30: no mate lost in 80 play-outs, but none with a
margin when the clock of the 50 move rule does not start at 0. Review when: the mate distance or the time after a mate score
is looked at again.
Frequency (2026-11-18, main session, MEMO run `lmfl-f4-lmfl-tc10`, 10+0.1, 20000 games, both engines with the bug): 69 games
drawn by the 50 move rule with the eval at the clamp (197.44 in the comments), 0.35% of the games, 0.6% of the decisive ones; the
reading of that run unchanged (dilution about 0.6%). Expected gain of the fix with the default openings at 10+0.1 about +0.3 Elo
(half of these games won, half a point each); more with the endgame starts.

### Threat features in the HCE (status: open)
Now: after the search work (user). S14.

### NNUE (status: open)
Now: not on the list (user).
