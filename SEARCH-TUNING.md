# Search tuning - state and findings (0.9.0 development)

The record of the search experiments since 0.8.0: move ordering, pruning, and how to measure them. The HCE
tuning has its own record (`TUNING.md`, `TUNING-HISTORY.md`, cited there as plain numbers); the findings here
are cited as S1, S2, ... This file lives on the release collector (`rel090`, since 2026-11-13; before on the
development base branch `dev090`, which now stays as the identity of its data), as commits that touch nothing
else: the experiment branches carry code, so a merged or dropped branch never takes its findings with it. Every
finding names the branch and commit it came from.

Data of the measurements: `C:\Learn\Data\measurements\search` (see `C:\Learn\Data\DATA.md`); tools:
BarbarossaTuning (`sample_roots.py`, `node_scan.ps1`, `node_scan_report.py`, `rfp_shadow_report.py`).
Elo values are from the candidate's side, with 95% error bars.

This file is the log: facts, later corrections as notes. The current reading of every idea is in `SEARCH-IDEAS.md`.

## State (2026-10-30)

| Branch | From | What | Status |
|---|---|---|---|
| `dev090` | master (0.8.0) | 0.9.0 base: one piece value table (S1) | base |
| `hpst` | dev090 | history init per search from piece order + PST (S4) | in hpqt |
| `qthr` | dev090 | quiet moves of threatened pieces first (S5) | in hpqt; SPRT +10.8 vs dev090 |
| `hpqt` | qthr + hpst | both orderings (S7) | SPRT +5.8 vs qthr; pushed |
| `rfut` | dev090 | reverse futility pruning, D6 / M50 (S8) | SPRT +17.6 vs dev090 |
| `hprf` | hpqt + rfut | the best version so far (S9) | not merged; no SPRT vs hpqt (user: no interference expected) |
| `rfpdiag` | hprf | RFP shadow diagnostic (S11) | measurement only, not for merging |
| `rfth` | hprf | RFP margin parameters: threat margin, constant part (S12) | experiment, compiled = hprf |
| `nmlm` | rfth | LMR without the near-mate guard (S13) | SPRT +13.0 vs hprf, the best version so far |
| `lmfx` | nmlm | LMR level fixed by parameter `epLmrFix` (S13) | closed, compiled = nmlm; SPRT vs adaptive: level 14 -36.4, level 11 -8.6 |
| `hmal` | expo | history malus, gravity, root bug by parameters (S17) | compiled off; no games yet |
| `hrem` | expo | history bonus by remaining depth, `epHRem` (S18) | parked, compiled off; no effect at 10k and 50k nodes |
| `cmov` | expo | counter move after the killers, `epCMov` (S19) | compiled off; no Elo at 10k nodes |
| `lfmal` | hmal | experiment: fixed LMR level + history malus (S21) | for measuring only |
| `lfix` | cmov | experiment: fixed LMR level + counter moves + remaining depth bonus (S20) | for measuring only |
| `lmpr` | nmlm | late move pruning by parameters (S15) | compiled off; SPRT vs off: D3 M1 -9.4, D5 M1 -25.4 |
| `lmpc` | lmpr | a pruned move counts for the adaptive LMR, `epLmpC` (S15) | compiled off; no games yet |

All branches of this table are pushed (2026-10-31), not merged; the commits cited here exist only while the branches
are kept. The whole repository: `C:\Learn\Data\history\barbarossa-all.bundle`.

Open:
- LMR near-mate guard (S13): removal accepted by SPRT; mate finding not measured.
- RFP: SPRT of the threat margin T100 running on MEMO; it comes first.
- RFP: no cut with an opponent passer on rank 6/7 (S13): game test. Cutting more when an opponent piece hangs: only
  after these (user).
- Search ideas next, before going back to the eval (user): not in Search/Core.hs are `improving`,
  history in the LMR, singular extensions; aspiration windows are there but off.
- Merge decisions for dev090 (hprf); the UCI engine ignores `go movetime` and `go nodes` (to implement in 0.9.0).
- HCE: threat features (S14), after the search work (user).

## Summary

| Findings | Tried | Result | Verdict |
|---|---|---|---|
| S1 | One piece value table (PST means) for SEE, QS delta pruning, special evals | | base of 0.9.0 |
| S2-S3 | How to measure search changes; what 10k-node games reach | | knowledge |
| S4 | History init per search from piece order + PST | +6.3 (1000 nodes), +2.0 (10k), late cuts -0.17 | small gain |
| S5-S6 | Quiet moves of threatened pieces first; softer variants | SPRT +10.8 +- 7.3 at 10+0.1 | worked (mode 2) |
| S7 | Both orderings combined (hpqt) | additive; SPRT +5.8 +- 4.6 vs qthr | worked |
| S8 | Reverse futility pruning, linear margin | M50 +25.2 at 10k nodes; SPRT +17.6 +- 9.9 | worked |
| S9 | hpqt + RFP (hprf) | | best so far |
| S10 | Pruning parameter by the minimum of the total nodes at fixed depth | M ~38 on 400 openings, agrees with the Elo | method plausible |
| S11 | RFP shadow diagnostic: wrong cuts by margin, depth, node features | own hanging pieces 5x, linear margin over-protects deep nodes | knowledge |
| S12 | RFP threat margin | node counts: noise; no RFP with own pieces hanging -12.5 at 10k nodes | skipping failed; T100 open |
| S13 | Node scan and shadow on real-game roots | shadow: endgame wrong cuts come from opponent passers on 6/7 (10x), own hanging 2.7x; node totals are ruled by the LMR near-mate guard, without it the 20 worst roots cost 344x less; without it no node minimum down to M10; removal SPRT +13.0; fixed LMR level against adaptive: 14 -36.4, 11 -8.6 | guard removal accepted; adaptive LMR stays; passer filter open |
| S14 | HCE: the en-prise term and what RFP shows about it | | idea |
| S15 | Late move pruning | off = nmlm; typical saving at most -19% on 50 roots; SPRT D3 M1 -9.4, D5 M1 -25.4; it lowers the adaptive LMR level | rejected in this form |
| S16 | Speed: module interfaces | no speed from more unfoldings or an explicit Moves.Core export list ; direct timers: eval 29%, quiet sort 7%, update of the position 4%, no large hot spot | structure does not limit the speed |
| S17 | History: overflow at the root, malus | late cuts -0.42 +- 0.25 with malus 1/8, nodes +5% | no games yet |
| S18 | History bonus by remaining depth | late cuts -0.48 +- 0.27, nodes +9%; 10k nodes: 5000 games over 4 settings 50.0%; 50k nodes: s = 20 +3.5 +- 17.1 | parked |
| S19 | Counter moves | late cuts -0.48 +- 0.14, move 4+ -0.25 +- 0.03, nodes +4%; 10k nodes: about -1 +- 12, 2000 games | ordering better, no Elo seen |
| S20 | Why ordering changes seemed to raise the nodes | the one perf base run is a single sample, placebo spread; move 4+ at fixed level 11 on two sets is the reliable measure | method; counter moves real |
| S21 | Malus, gravity under the current method | malus, no root update: at the floor; gravity alone clearly worse (+0.57, +0.73); floor at level 11: +-0.03 openings, +-0.065 real | malus parked |

## Findings

**S1.** One piece value table (dev090, 42358d7): `matPiece1` = PST means over squares and phases (P 125, N = B 390,
R 615, Q 1200), also used as `seeValue`, for QS delta pruning and the special evals. Knight must equal
bishop: the material classes of the special evals (`scaleKnowledge`) depend on it.

**S2.** Measuring search changes.

- Total perf node counts (`SelfPlay -P`) of variants are chaotic: chunk sd about 13% per 25 positions at
  depth 10, SE about 3% on 400 positions; they say little about search quality. Ordering changes are
  measured by the beta-cut statistics (perf prints the cuts at move 1, 2, 3, 4+; b555078 / 41b9a5d),
  paired by chunks of positions; the late-cut rate and the cuts at move 4+ are the metrics.
- Pruning changes cannot be compared by cut rates: pruning removes the easy cut nodes (selection).
- Node counts remain the identity check of refactorings (deterministic perf build, `reproselfplay`): the
  UCI engine is not deterministic (random history init).
- Since hpst the perf test resets the history per position as games do (tests.fen -d 10: 18,154,085
  nodes, before 19,527,603); compare only builds with the same behaviour.
- Strength needs games; node-limited games do not count speed costs.

**S3.** 10k-node SelfPlay games (node margin 50%: search budget 5000) reach depth 4 completely and depth 5 partly
(cumulative nodes per position: d4 ~4000, d5 ~8800, d6 ~19k). Ordering effects which grow with depth
hardly show there; timed games are needed.

**S4.** History init per search (hpst, b826452, 5919678; `epHistPst` bits 1 = PST of the to square, 2 = piece
order of the generation, default 3, `epHistPstSh` -4). 400 positions (open-chilo-eq): nodes noise, late
cuts 6.54% -> -0.17 +- 0.09 points with order + PST. MEMO, PST alone against zero init, 16k games each:
+6.3 +- 4.9 at 1000 nodes, +2.0 +- 4.5 at 10k. The to square ranks quiet moves only moderately like
to - from (Spearman median 0.47): a to-from key is an untested alternative.

**S5.** Quiet moves of threatened pieces first (qthr, c287654, f5324a5; `epQThr` = 2): moves of pieces attacked by
a cheaper enemy piece (N, B by pawns; R also by N, B; Q also by R) are sorted before the others, both
groups by history. Cuts at move 4+ -0.174 +- 0.020 points (of 1.50%); mode 1 (moves to attacked squares
last) nothing. Speed: -11% NPS, with the fast path (plain sort when no piece is threatened, f5324a5)
-2..3%. Matches: 10k nodes +3.5 +- 3.2 (32k games), 10+0.1 +4.5 +- 8.1 (4000), SPRT [0, 5] at 10+0.1
H1 after 4622 games: +10.8 +- 7.3.

**S6.** Softer escape priorities (qthr working tree, never committed), cuts at 4+ against history only:
escapes first -0.174; best history move first, then escapes -0.099; history bonus as min(own, best * f)
about 0 (all escapes tie and lose their history order: that order matters); additive bonus
own + best * f/8: 8/8 -0.150, 6/8 -0.119, 4/8 -0.084, 2/8 -0.101; constant bonus of one cut at ply E:
E = 4 -0.080, E = 6..10 about -0.03; malus for moves to attacked squares (M = 2..14 with E = 1) -0.168 ..
-0.131. Nothing beats plain escapes first; moves to attacked squares give no useful signal in any form.

**S7.** hpqt (a833787 = qthr + hpst): on 400 positions, against zero init + history sort, cuts at 4+ escapes
-0.151 +- 0.018, prior -0.037, both -0.168 +- 0.030 (interaction +0.020 +- 0.026); late cuts prior
-0.174, escapes -0.054. The two act on different move numbers and add up. SPRT hpqt against qthr
(10+0.1, [0, 5]): H1 after 11,844 games, +5.8 +- 4.6.

**S8.** Reverse futility pruning (rfut, 8247d5d, 9b7a4a1): in zero window nodes (`pvZeroW`) after the TT probe and
before the null move, a node of remaining depth d <= `epRFutD` fails high when static - `epRFutM` * d >=
beta (not in check, not near mate, not with only king and pawns). Nodes at depth 10 (400 positions):
D6/M50 -24%, D6/M100 -13%, D3/M100 -13% (the cuts are at small depths). 10k-node matches against off
(8000 games, +-6.4): M100 +11.1, M50 +25.2, M25 +21.7; the parabola peaks at about 50. Compiled D6 / M50;
SPRT against dev090 (10+0.1, [0, 5]): H1 after 2610 games, +17.6 +- 9.9.

**S9.** hprf (5a22269 = hpqt + rfut): the best version; no SPRT against hpqt (the user expects no interference;
a few hours on MEMO if doubts come up).

**S10.** Pruning parameters by the minimum of the total nodes at fixed depth (the user's method): searching many
positions to a fixed depth, a more aggressive setting saves nodes until wrong cuts, discovered by later
iterations, make the total rise again. Parameter: the inverse margin k = 100 / M (k = 0 = off), doubling
to a bracket, then bisection and a parabola in log k. On 400 openings (open-chilo-eq, every 57th: early
middle games, all with queens), depth 12, nodes per position (one process per position):

- Totals against off: M100 +7.6%, M50 -22.1%, M35 -25.3%, M25 -18.3%; minimum M ~38, bootstrap over the
  positions median M39, 80% M34 .. M49, off never lowest. This agrees with the 10k-node matches (S8).
- The typical position (geometric mean of the ratios to off) saves more and more with smaller margins
  (-12, -27, -31, -36%); the total turns up only through blow-ups: about 22 positions per margin with
  more than twice the nodes of off hold 33-43% of all nodes. They are mostly different positions per
  margin (chaos of the tree); root features explain little (best move changes at depths 6-12 the
  strongest, Spearman +0.2 .. +0.3).
- Caveat (user): 400 openings are the wrong distribution; repeated on real-game roots (S13).

**S11.** RFP shadow diagnostic (rfpdiag, ce29057, de52c8a): with `epRFutD` < 0 every node is searched as without
RFP, the RFP decision is counted by depth and excess per depth (static - beta) / d, the wrong ones (the
search fails low) are kept with the position, and 1 of 256 positions (by hash key) of the correct ones as
a control sample. The search does not change, so one run with M10 gives the statistics of every M >= 10.
400 openings, depth 12, 54M decisions:

- Wrong rate M25 0.23%, M50 0.11%, M100 0.05%. By depth at M50: d1 0.10, d2 0.14, d3 0.11, d4 0.10,
  d5 0.07, d6 0.06%: the linear margin over-protects the deeper nodes; equal risk needs a margin growing
  slower than linear (a + b * d).
- Wrong rate with / without a node feature at M50: own piece hanging (attacked and undefended, or by a
  cheaper piece) 5.0x (in 78% of the wrong cuts, 41% of the correct ones); 2+ hanging 4.3x; an opponent
  piece forking two of ours 5.4x; opponent passer on rank 6/7 3.3x; 3+ attacks on the own king ring
  2.6x; an opponent piece hanging 0.2x; an own fork 0.1x. The static score does not see pending
  captures, in either direction.

**S12.** RFP threat margin (rfth, 7693aa9; `epRFutT` cp added to the margin when own pieces hang, compiled 0):
node scan as S10, against M50 (T0): T25 +14%, T50 +6%, T100 -3%, T200 +6%, T10000 (no RFP then) +14%,
bootstrap 90% ranges about +-17%: noise. The typical saving against off drops from -26.6% (T0) to about
-23% (T25 .. T100), -16% (T200), -13% (T10000); the blow-ups' share falls from 32% to about 24%.
Fewer cuts against fewer expensive mistakes: match T10000 (no RFP when own pieces hang) against T0 at 10k
nodes, 8000 games: -12.5 +- 6.3 (shards 1304/1242/1454, 1291/1280/1429). Giving up the cuts costs more
than the avoided wrong cuts gain: skipping is wrong; a moderate threat margin (T100) is untested in games.
977360a adds `epRFutA`, the constant part of the margin (compiled 0).

**S13.** Real-game roots (running, 2026-10-29): 2000 root positions sampled uniformly from 1.32M searches of
Barbarossa's engine games (data `-d` of the sets ug-a .. ug-d, mate scores excluded: decided, and their
depth-12 searches can be very expensive), `sample_roots.py`. Non pawn material quartiles 12 / 23 / 44
(start 62), 31% endgames: very different from the 400 openings. Node scan on MEMO (10 settings: off,
M25/38/50/100, M50 with T100 and T10000, 20 + 30 d, 40 + 30 d, 60 + 20 d) and the shadow diagnostic
(by game phase) on the same roots.

Shadow diagnostic on the first 1000 roots (rfpdiag, one process at depth 12, 31.0G nodes, 15.4 h; the second
1000 were stopped, see below). Wrong rate at M50 0.18% (openings 0.11%), by depth d2 0.29% .. d6 0.19%; by
phase npm >= 30 0.13%, 14-29 0.24%, <= 13 0.39%.

| Feature at M50 (wrong rate with / without) | all | npm >= 30 | 14-29 | <= 13 |
|---|---|---|---|---|
| own hanging >= 1 | 2.7 | 3.5 | 2.7 | 2.4 |
| opponent fork | 3.3 | 4.4 | 3.1 | 1.9 |
| own pinned piece | 2.3 | 2.3 | 3.0 | 4.1 |
| opponent passer on rank 6/7 | 5.3 | 3.0 | 4.4 | 10.4 |
| opponent hanging >= 1 | 0.3 | 0.3 | 0.4 | 0.5 |

Against the openings (S11): own hanging falls from 5.0x to 2.7x, still in 62% of the wrong cuts; the opponent
passer on 6/7 rises from 3.3x to 5.3x. In endgames it is in 75% of the wrong cuts and 22% of the decisions
(wrong rate 1.28% with it, 0.12% without): the static score overrates the side to move there, and QS does not
see quiet pushes. As a filter it is sharper than own hanging: no RFP with such a passer gives up 10% of the
decisions and removes 38% of the wrong ones (endgames only: 2% and 13.5%); own hanging gives up 37% for 62%,
and that lost 12.5 Elo (S12). Not tested in games; wrong cuts can be cheap.

Node scan on MEMO (build rfth, near-mate guard in LMR, depth 12, stopped early): 1500 roots done by all 10
settings (shards 2 and 6 left out: positions 250-499 and 1250-1499, hanging on a blow-up, e.g. root 1310
`6Q1/8/3k3K/8/1p5P/8/1PB2P2/8 w - - 1 1`, depth 9 in 0.75M nodes, depth 10 not finished in a day).

| Setting | Total vs off, all | Geomean vs off, all | Total vs off, without 48 blow-ups (max over settings >= 50M) |
|---|---|---|---|
| M25 | -9.4% | -26.8% | -22.7% |
| M38 | +7.1% | -23.3% | -19.2% |
| 20 + 30 d | -0.8% | -19.9% | -8.9% |
| M50 | -8.7% | -17.7% | -4.7% |
| 40 + 30 d | -4.2% | -16.6% | -10.6% |
| M50 T100 | -13.4% | -15.4% | -7.5% |
| 60 + 20 d | -7.0% | -15.1% | -4.3% |
| M50 T10000 | -6.2% | -9.8% | +2.0% |
| M100 | -4.9% | -9.1% | +3.5% |

The 48 blow-ups (3.2% of the roots) hold 93.5% of the nodes of off. Two kinds: stable (all settings similar,
decided endgames: |score| >= 2000 has median 44M, all roots 0.4M) and chaotic (max / min over the settings up
to 4000, also at normal scores: -416 at npm 48, +208 at npm 46). So a rule by root score does not separate
them: |score| < 500 still gives M38 +300%. Without the blow-ups no minimum over the margin shows: the fewer
nodes the lower the margin, down to M25, the end of the range (the M38 of S10 was set by blow-ups). The
sublinear margins and the threat margin show no gain against the linear margin of the same typical saving.

Cause: the near-mate guard of LMR (`reduceLmr`, `nearmate b`), nmlm 5795f3f removes it. The 20
worst roots at depth 12 with M50: 15.29G nodes with the guard, 44.4M without (factor 344); the highest
17.7M, one root up by 9x (0.76M to 7.1M). 50 evenly spaced roots at depth 10: total -79%, geomean -17%. Neither of the
two checked blow-up roots has a mate score up to depth 13-15, so a stop on a mate at the root does not help.
Two effects: at PV nodes the guard tests beta, near mate in every full-window search (aspiration windows are
off, `useAspirWin`) without any mate; with `nearmate a` there 48 of the 50 roots have exactly the nodes of no guard (geomean -8%
vs guard). The other 2 still blow up: real near-mate windows, probably from a PV node of the defender whose
first move gets mated (not confirmed). Node scans before this change measure the guard as much as the margin.
LMR does not create false mates: a mate score still comes from a real mate line. Games of nmlm against hprf
(this PC, 10+0.1, 400 games): -13.9 +- 26, no crash, illegal move or log warning. In all 242 decided games the
winner had reported a mate score, and no engine that reported one failed to win (nmlm 113 of 113, hprf 129 of
129); median depth per move 9 for both. So no weird effect, but too few games for the strength; rare missed
mates are not measured.

SPRT nmlm vs hprf (MEMO, 10+0.1, 16 MB, [0, 5]): H1 accepted after 3738 games, +13.0 +- 8.2, nElo +17.6 +- 11.1, LOS
99.9%, draws 36.65%, pentanomial [112, 442, 685, 454, 176]. The 400 games above were within the noise.

Origin of the guard: already in the first commit that compiles (2890d3e, 2013), no reason recorded; chessprogramming.org
has no such LMR condition. The same code guards futility, where the condition is usual, so it may have been carried
over from there (not provable). In 2013 PV nodes tested alpha, but had no LMR anyway. b478f31 (2018, LMR at PV
nodes) reused the zero-window form with beta, probably by mistake: since then a full-window search has no LMR.

Node scan without the guard (nmlm, this PC, depth 12, 500 roots = every 4th, 18 min on 4 shards, no search hit
the 300 s limit). On the same roots the guard build needed 26 to 40x the nodes (largest root 3.2G, now at most
26M; the guard scan lacks its worst roots, so the factor is too low).

| Margin | Total vs off | Geomean vs off |
|---|---|---|
| M100 | -15.5% | -9.3% |
| M50 | -4.1% | -12.4% |
| M38 | -15.9% | -16.5% |
| M25 | -3.9% | -22.7% |
| M15 | -4.9% | -26.3% |
| M10 | -18.2% | -29.2% |

The geomean falls steadily down to M10, no minimum; the totals do not resolve the margins (bootstrap about +-25%,
no order). Chaotic roots remain, but much smaller: 0.1M to 25M at one margin and 0.7M at the next, and at no
fixed margin. RFP has no re-search to bring the nodes back, so a node minimum need not exist for it: nodes
give its cost, not what the cuts lose (M25 did not beat M50 at 10k nodes). The M38 of S10 was a blow-up effect.

LMR with a fixed level instead of the adaptive one (lmfx 32c4bb8, `epLmrFix`, compiled off; this PC, depth 12, 2000
roots, 4 shards, 120 s limit, a root over it is skipped for later configs; 1957 roots done by all):

| Level | Total vs adaptive | 90% range | Geomean vs adaptive | Median | p90 | Max | New timeouts |
|---|---|---|---|---|---|---|---|
| adaptive | 0 | | 0 | 0.259M | 0.84M | 19.9M | 1 |
| 2 | +261.8% | +222..+306 | +147.1% | 0.625M | 4.64M | 53.6M | 27 |
| 5 | +166.2% | +139..+197 | +106.1% | 0.533M | 3.51M | 30.6M | 10 |
| 8 | +87.8% | +68..+109 | +59.9% | 0.416M | 2.40M | 30.2M | 0 |
| 11 | +17.7% | +6..+30 | +13.1% | 0.297M | 1.54M | 23.8M | 1 |
| 14 | -6.0% | -18..+6 | -35.0% | 0.155M | 0.89M | 38.0M | 1 |
| 15 | -24.0% | -34..-13 | -51.5% | 0.114M | 0.71M | 26.8M | 3 |

No minimum: the nodes fall up to level 15, the end of the table, in spite of the re-searches. So the node count does
not give the level either, it needs games, and no finer scan was run. The adaptive level works: its median is
that of a level near 11, its p90 that of level 14, with the lowest maximum, so it reduces most where the tree is large.
From level 14 the top 20 roots hold 30% of the nodes (15% at 5 to 11). The adaptive scheme does have parameters:
the node count `lmrInitLim` for raising the level and the weight 2^d of a re-search.

Tail measures instead of the total (`roots-2000\tail_metrics.py`, same data). The maximum is useless: one root, a
different one at nearly every setting, split halves of the roots agree on the winner in 0-2% of the splits. The
"lowest maximum" of adaptive above is no finding (bootstrap: adaptive 38%, level 11 36%). The mean of the worst 1% of the
roots is more stable for LMR, with a minimum inside the range: level 2 34.8M, 5 22.5M, 8 16.7M, 11 9.7M, adaptive
13.0M, 14 16.1M, 15 13.2M; level 11 wins 89% of the bootstraps, split halves agree in 83%. p99 points the same way
(level 11 6.3M, adaptive 7.1M, 14 8.9M; 67%, 46%). For RFP (500 roots, top 1% = 5 roots) it does not resolve the
margins; it prefers M100 where the 10k-node matches had M50 (S8; those ran with the guard). So it is a hypothesis only.
Prediction, recorded before the end of the SPRTs: the worst 1% says level 11 > adaptive > 14; total and geomean say 14
> adaptive > 11. The top 1% roots of two settings overlap by 9-12% without the guard, by 79% with it: a high overlap
marks a systematic anomaly, as the guard was.

SPRT level 14 vs adaptive (MEMO, same binary, 10+0.1, [0, 5]): H0 after 1024 games, -36.4 +- 15.7. So 35% fewer nodes
per position cost clearly: total and geomean are refuted as a guide.

SPRT level 11 vs adaptive (same conditions): H0 after 3556 games, -8.6 +- 8.5 (LOS 2.3%). The adaptive scheme stays and
lmfx is closed; levels 12 and 13 were not played. The prediction above: the worst 1% had adaptive > 14 and 11 > 14
right (11 and 14 only through their results against adaptive) and 11 > adaptive wrong, so it does not find the best
setting. Total and geomean had only adaptive > 11 right. Three settings are too few to call any of them predictive; none
is a selection criterion for reductions, games decide.

**S14.** HCE and hanging pieces. The en-prise term (`enPrise`, Eval/Core.hs) counts only the side to move's pieces,
linear in the hanging material; its tuned middle game weight is about 0 (`ewEnpHanging` -3 / -59): with
the move, one hanging piece is usually saved, so the linear sum predicts little. The real effect is not
linear: one threatened piece costs little, with two the second most valuable is lost, and the opponent's
capturable pieces are a gain for the side to move (not in the term). Idea: features nonlinear in the
position, linear in the weights (second most valuable own threatened piece, most valuable capturable
opponent piece); cheap, the bitboards are computed already. Problems: the training data are QS leaves,
where capturable opponent pieces hardly occur (the feature cannot be fitted from them, user: but those
evals then stay unchanged, only interior nodes change); the eval is also the QS stand pat, which would
cut before the capture is searched.

**S15.** Late move pruning (lmpr 484b2e6, from nmlm, compiled off): in zero window nodes of depth up to `epLmpD`, not in
check and not near mate, the moves `canPruneMove` accepts (no capture, check or passer move) are not searched from
`epLmpA` + `epLmpM` * d * d / 2 moves after the last capture (the count of the LMR). Off is identical to nmlm: 9919183
nodes on tests.fen at depth 10 with both perf builds. Nodes on 50 roots at depth 12 against off, A = 3:

| Setting | Total | Geomean |
|---|---|---|
| D3 M1 | -29.3% | -15.4% |
| D5 M1 | -20.0% | -19.2% |
| D7 M1 | +55.8% | -11.3% |
| D3 M2 | +14.6% | -5.1% |
| D5 M2 | +53.3% | -1.3% |
| D7 M2 | +3.4% | -3.3% |

50 roots only (total 90% ranges about +-40% and more), and nodes give the cost, not the loss (S13). The saving is small
at M2: the LMR already reduces these moves. For games: D3 M1 and D5 M1 against off.

SPRT D3 M1 vs off (MEMO, same binary, 10+0.1, [0, 5]): H0 after 3340 games, -9.4 +- 8.8 (LOS 1.8%). One timeout of d3m1, in
3340 games no effect on the verdict; not looked into.

Side effect on the adaptive LMR: a pruned move no longer adds its +1 to the counter (`moreLMR`), the re-searches of the
others still subtract. Level after draft 11 on tests.fen (50 positions): off mean 14.28, D3 M1 13.80 (49 read), D5 M1
13.52, lower than off in 22 positions and higher in 5; nodes -6.1%, -16.6%. So the LMP also reduces the other moves less.
The share of this in the -9.4 is not measured. D5 M1 has the same limits up to depth 3.

Variant (lmpc cae8bcd, compiled off, off identical: 9919183 nodes): a move pruned only by the LMP adds `epLmpC` to the
counter. Same measurement, depth 11, off 14.28 and 26486056 nodes (`roots-2000\lmp-credit-tests-d11`):

| Setting | Mean level | Nodes vs off |
|---|---|---|
| D3 C0 | 13.82 | -6.1% |
| D3 C1 | 14.12 | -17.6% |
| D3 C2 | 14.64 | -50.7% |
| D3 C4 | 14.84 | -28.3% |
| D3 C8 | 14.98 | -44.3% |
| D5 C1 | 14.12 | -46.0% |
| D5 C4 | 14.92 | -41.8% |

So the missing count does lower the level: C1 brings most of it back, more goes above off. The nodes do not follow the
credit in order: the total of 50 positions is ruled by a few of them, and each change moves the tree. C1 is the setting
that counts a pruned move as a move that failed low; for games D3 C1 against off, parked (user: too long for a small effect).

SPRT D5 M1 vs off (same conditions): H0 after 1414 games, -25.4 +- 13.4. D5 has the limits of D3 up to depth 3 and prunes
also at depths 4 and 5: the deeper part costs clearly more. This form of the LMP (no `improving`, limit from the last
capture) loses; D3 C1 has little chance to change that much, so it stays parked.

**S16.** Speed and the module structure (perf build of lmpc, tests.fen depth 10, 9919183 nodes in all builds, alternating
runs on this PC, spread about +-3%). `-fexpose-all-unfoldings -fspecialise-aggressively`: no gain (about 590k nodes/s
both). Explicit export list of Moves.Core (27 of its 94 functions): no gain (567k against 559k, noise). In the optimized
Core of Search.Core no monad dictionaries are left and few calls into other modules (betaCut, doNullMove, qsDelta,
legalMove, moveChecks, retrieveEntry, writeCache). Merging modules was not built; these results give it little chance.
Allocation: 70 GB for 9.9M nodes, about 7 KB per node, GC only 2% of the time: a start for the profile.

Cleanup (expo 3227638, from nmlm): explicit export list of Moves.Core, unused exports and 3 dead functions removed. Same
engine (9919183 nodes, perft of the 5 standard positions as known), same speed (399k against 396k nodes/s, 5 alternating
runs; this PC was throttling, about 590k before: compare speeds only in alternating runs).

Profile (expo). GHC profile with late cost centres (`--library-profiling --executable-profiling --ghc-options "-fno-prof-auto
-fprof-late"`, needs `Other-extensions: TemplateHaskell` in the stanza, otherwise the TH step finds no objects): 3 times
slower (155k nodes/s) also with `--profiling-detail=none`, the vector operations remain as cost centres with hundreds of
millions of entries, so it overstates the sort (about 30%). Its call counts are right; its times not. Direct timers
(QueryPerformanceCounter, around forced calls, optimized build, runs agree within 1%), each call corrected by half of
the measured timer cost (93 ns per timed call), on 16.65 s without timers:

| Part | Calls | Per call | Share |
|---|---|---|---|
| posEval | 9571623 | 0.50 us | 29% |
| dirSort (depth up to 2) | 588112 | 1.38 us | 5% |
| makeMTS, selection steps (depth over 2) | 3315191 | 0.11 us | 2% |
| updatePos | 9986434 | 0.07 us | 4% |
| not timed: move generation, SEE, TT, search loop | | | 60% |

Eval is already lazy (`staticScore`), yet 96% of the nodes need it, mostly as the QS stand pat. Allocation is spread: the
largest cost centre is pvZeroW with 12%. Files in `C:\Learn\Data\measurements\search\speed`.

The further measurements (move generation, SEE, TT, allocation) and the methods are in `PROFILING.md`.

**S17.** History malus (hmal, from expo, compiled off, off identical: 9919183 nodes). Found on the way: `histw` is
`1 << (31 - absolute depth)` and the root has absolute depth 0 (the comment there says 1), so at the root the Int32 overflows
to minBound and the subtraction makes the value of the move huge and bad; repeated it wraps back. Root moves are sorted
by nodes, so the damage would be in the deeper nodes through the shared table. Parameters: `epHGrav` 0 = as before, 2 = root
with the bonus of depth 1, 3 = no update at the root, 1 = as 2 with gravity; `epHMal` n > 0: at a cut by a quiet move the quiet
moves which failed low before get the bonus shifted right by n - 1.

Paired late cuts (cuts not at move 1) against off, 400 openings (pos400.fen) in 16 chunks of 25, one process each, depth 10
(`hist_scan.py`, `C:\Learn\Data\measurements\search\hmal`):

| Setting | Late cuts | Move 4+ | Nodes (geomean) |
|---|---|---|---|
| Root as depth 1 | -0.234 +- 0.149 | -0.084 +- 0.034 | +8.4% +- 2.3 |
| No root update | -0.117 +- 0.115 | -0.043 +- 0.029 | +3.4% +- 2.7 |
| No root update, malus 1/2 | -0.312 +- 0.159 | -0.067 +- 0.043 | +6.7% +- 2.7 |
| No root update, malus 1/4 | -0.349 +- 0.174 | -0.075 +- 0.043 | +4.5% +- 2.9 |
| No root update, malus 1/8 | -0.416 +- 0.249 | -0.098 +- 0.062 | +5.2% +- 3.8 |

For scale: the threat sort of S6 had -0.151 +- 0.018 and SPRT +10.8. Here all under about 2 sigma; only 16 chunks. The
direction is better ordering, with more nodes at fixed depth (S2: node totals say little). A full run of the same 400 in one
process (the TT carries over, without error bars) gave other signs for some settings, e.g. root as depth 1 worse:
fresh TT per chunk and shared TT are not the same measurement. Malus 1/8 was also the best in that run. `SelfPlay -P`
ignores `-s` and `-f`: chunks need their own files. On 50 positions root as depth 1 had +29% nodes: noise of few positions.

**S18.** History bonus by the remaining depth (hrem, from expo, compiled off, off identical: 9919183 nodes): `epHRem` = s > 0:
bonus `1 << (s + d)`, at most `1 << 30`, instead of `1 << (31 - absolute depth)`. No negative values, the exponential shape
stays; at a fixed draft and without reductions the two differ only by a constant. So what changes: reduced and extended
lines count by what is left to search, later iterations no longer reuse the bonus of a ply, no overflow at the root. s = 20:
d = 1 gets 2^21, over the prior (piece step 1024). User: negative history and squared bonuses lost Elo in earlier tries.

Same scan as S17: late cuts -0.482 +- 0.272, move 4+ -0.064 +- 0.075, nodes (geomean) +9.0% +- 3.7. Games against off (same
binary, normal flavour, compiled eval, 10k nodes per move, 4 shards of 125 pairs, 6.5 minutes each here): 1000 games, W 353
D 348 L 299, +18.8 +- 17.4; shards +37.7, +8.3, +6.9, +22.3. The games are node limited, so the extra nodes per depth are
already paid.

More games at 10k nodes, against off, 1000 each (half slope: `epHRem` = -14, bonus `1 << (14 + (d + 2) / 2)`, no cap at
reachable depths, proposal of the user):

| Setting | Openings | W / D / L | Elo |
|---|---|---|---|
| s = 20 | 0-499 | 353 / 348 / 299 | +18.8 +- 17.4 |
| s = 20 | 500-999 | 327 / 328 / 345 | -6.3 +- 17.7 |
| s = 24 | 0-499 | 347 / 320 / 333 | +4.9 +- 17.8 |
| s = 16 | 0-499 | 317 / 325 / 358 | -14.3 +- 17.7 |
| half slope, 14 | 0-499 | 328 / 335 / 337 | -3.1 +- 17.6 |

All 5000 games together: 1672 / 1656 / 1672, exactly 50.0% (mixed settings). The first +18.8 was noise. The better late cuts did
not become Elo at 10k nodes. These games are shallow: the cap, the rescale, the aging and the weight of reduced lines
act more in deeper search (user), so a deeper result can differ.

At 50k nodes (s = 20, openings 1000-1499, 1000 games, about 55 minutes): 321 / 368 / 311, +3.5 +- 17.1; shards -29.3, +23.7, +22.3,
-2.8. No effect seen at 5 times the nodes either, within +- 17, which does not exclude a small one. Parked, no timed games.

**S19.** Counter moves (cmov, from expo, compiled off, off identical: 9919183 nodes): `epCMov` = 1: table by the move before
(after a null move none, the second half of the history vector, cleared per search). A quiet move which cuts, also a TT
cut, is stored; it is tried after the killers when legal, not a capture and not already the TT move or a killer.

Same scan as S17: late cuts -0.480 +- 0.137, move 4+ -0.247 +- 0.034, nodes (geomean) +4.1% +- 2.8: the clearest ordering
gain of this round. Games against off at 10k nodes: openings 0-499 331 / 337 / 332, -0.3 +- 17.5; openings 500-999 339 / 317 / 344,
-1.7 +- 17.8; together 2000 games about -1 +- 12. No timed games. 10k node games do not show ordering gains: they reach depth 4-5 (S3), and the threat sort
had +3.5 +- 3.2 there against +10.8 in the SPRT (S5).

**S20.** All ordering changes seemed to raise the nodes at fixed depth (user: counterintuitive; suspected the adaptive LMR, and the
openings as test positions). Test (lfix): the same settings at fixed level 11 and adaptive, depth 10, 16 chunks of `pos400.fen`
(openings) and 20 chunks of `noguard\roots-500.fen` (real games), geomean nodes against the base of the same row:

| Change | Adaptive, openings | Adaptive, real | Level 11, openings | Level 11, real |
|---|---|---|---|---|
| Counter moves | +4.1 | -4.5 | +6.6 | -11.5 |
| Remaining depth, s = 20 | +9.0 | -5.6 | +4.2 | -6.0 |
| Remaining depth, half slope | +6.2 | -9.0 | +3.7 | -1.4 |
| Threat sort removed | +6.8 | -2.9 | -0.9 | -6.4 |
| Prior removed | +7.7 | -7.5 | +3.4 | -11.0 |

Nearly all up on the openings, nearly all down on the real positions, also when good changes are removed. Placebo: the same engine, normal
flavour, 5 random history seeds: totals 78.0 - 80.2M on the openings, 75.4 - 79.4M on the real positions. The perf base has 75.5M and 80.3M: outside
on both sides. The deterministic perf run is one sample of a chaotic tree, and every variant is compared with that one sample. So
there is no general node increase from better ordering, and node differences of this size say nothing.

Noise floor from the placebo, paired against one seed run: late cuts up to +- 0.18 (one with 2 sigma), move 4+ up to +- 0.04. Standard errors over chunks do
not cover the shared base run. Move 4+:

| Change | Adaptive, openings | Adaptive, real | Level 11, openings | Level 11, real |
|---|---|---|---|---|
| Counter moves | -0.247 | -0.241 | -0.234 | -0.217 |
| Remaining depth, s = 20 | -0.064 | +0.166 | +0.034 | +0.059 |
| Remaining depth, half slope | +0.039 | +0.323 | +0.096 | +0.083 |
| Threat sort removed | +0.012 | +0.056 | +0.115 | +0.121 |
| Prior removed | -0.094 | +0.038 | -0.009 | +0.027 |

- Counter moves: the same real gain in all four columns.
- Remaining depth: no gain, worse on the real positions; the gain on the openings under the adaptive LMR (S18) came with the LMR.
- The fixed level does help the measuring (user): removing the threat sort, a proven gain, shows at level 11 in both sets (+0.115, +0.121), hardly
  at the adaptive level (+0.012, +0.056). The cause of the increase was not the adaptive LMR, but it blurs the cuts.

Method for ordering changes from now on: move 4+ at fixed level 11 on both sets, against the noise floor; several seeds where it is close; nodes only when
far outside the placebo spread. Files in `C:\Learn\Data\measurements\search\lfix`.

**S21.** History malus and gravity under the method of S20 (lfmal 581ea3c = hmal + `epLmrFix`, off identical: 9919183 nodes, level 11 base as in lfix:
81589698). Depth 10, perf build, 16 chunks of the openings, 20 of the real positions, against level 11:

| Setting | Move 4+ openings | Move 4+ real | Late openings | Late real |
|---|---|---|---|---|
| No root update | -0.046 | +0.031 | -0.026 | +0.230 |
| No root update, malus 1/2 | -0.005 | -0.059 | +0.138 | -0.018 |
| No root update, malus 1/4 | -0.049 | +0.016 | -0.092 | +0.228 |
| No root update, malus 1/8 | +0.036 | -0.054 | +0.149 | +0.182 |
| Gravity alone | +0.570 | +0.733 | +0.726 | +1.032 |
| Gravity, malus 1/8 | -0.054 | +0.138 | -0.328 | +0.254 |
| Threat sort removed (control) | +0.115 | +0.121 | -0.125 | +0.272 |

Placebo at level 11 (normal flavour, 4 seeds, against the first): move 4+ up to +-0.03 on the openings, +-0.065 on the real positions; late cuts up to
+-0.10 and +-0.17; node totals 80.2 - 84.8M and 83.1 - 88.4M, a spread of about 6%. So the real positions are noisier than S20 gave.

Against this floor the malus shows nothing, in no size, and the signs change between the sets; the late-cut gains of S17 do not repeat. Removing the root update
neither. Gravity alone is clearly worse, in both sets: it pulls the good values back without any malus to balance them. The control stays visible, on the real positions
at about 2 times the floor. Counter moves (-0.234, -0.217) are 3 to 8 times the floor. The malus is parked in this form: bonus and malus on the scale
`1 << (31 - absolute depth)`. No games.

**S22.** Two SPRTs on MEMO, 10+0.1, `open-moves-uniq-fm1.fen` (results in `C:\Learn\Data\results\memo\fsrt-expo-tc10`, `t100-ada-tc10`):

| Run | Change | Bounds | Games | Elo | LLR | Outcome |
|---|---|---|---|---|---|---|
| `fsrt` vs `expo` | filter before the quiet sort | [-5, 0] | 4422 | +6.68 +- 7.72 | 2.96 | H1: no regression |
| `t100` vs `lmfx` | RFP margin +100 when own pieces hang (`epRFutT=100`) | [0, 5] | 12000 | +4.75 +- 4.58 | 2.06 | no decision, ended at the game limit |

The filter was tested for no loss, so +6.68 is not a proven gain. T100 did not reach the bound; its error bar just excludes 0, LOS 97.9%.

**S23.** Continuation history (`chis` e2ad098: by the move before and the own move before, `epCHist` bits, `epCHistW` weight in quarters, off identical: 9919183 nodes,
`epCMov=1`: 9790448). Base: level 11 with counter moves on. Files in `C:\Learn\Data\measurements\search\chis`.

Screen, perf build, 15 settings, depth 10, 16 chunks of the openings, 20 of the real positions (`screen.txt`), best three by the mean of both sets:

| Setting | Move 4+ openings | Move 4+ real |
|---|---|---|
| `epCHist=1, W=4` | -0.115 | -0.205 |
| `epCHist=3, W=2` | -0.044 | -0.243 |
| `epCHist=2, W=16` | -0.028 | -0.194 |

Against one perf base run, and almost all 15 settings are down on the real positions (-0.058 to -0.243): the base shows in this pattern (S20).

The three and the base again, normal flavour, 4 random seeds each, means over the seeds (`seeds-fixed.txt`; spread = largest - smallest):

| Setting | Move 4+ openings (spread) | Move 4+ real (spread) | Against base |
|---|---|---|---|
| Base | 1.733 (0.035) | 1.968 (0.122) | |
| `epCHist=1, W=4` | 1.643 (0.088) | 1.917 (0.085) | -0.090, -0.052 |
| `epCHist=3, W=2` | 1.627 (0.080) | 1.898 (0.090) | -0.107, -0.070 |
| `epCHist=2, W=16` | 1.683 (0.131) | 1.873 (0.146) | -0.050, -0.095 |

All three gain in both sets, 1 to 3 times the single-run floor (S21), about a third to a half of what the counter moves gave (-0.234, -0.217); which setting is best is not resolved
by these numbers. Speed (`speed-fixed.txt`, tests.fen depth 10, 6 alternating pairs, `epCHist=3, W=2` against counter moves alone): -1.9% nodes/s, standard error 0.7, pairs +0.4% to -4.1%. The PC was
slower than usual in this run (about 390k against about 620k nodes/s in the runs before the games), so this is a rough number.

No games: the script took the group name for the setting (`epLmrFix`, from splitting at the first `=`), so the speed stage ran without the candidate and the candidate engine of the timed
games was started with `-p epLmrFix`. It lost all 4628 games on time, from the first move on. Stopped at 07:26 after 6.5 hours; the seed table was recomputed from the saved chunks, the speed stage rerun.

**S24.** New base `fbas` 24a69e7 = `expo` + filter always on (parameter removed) + `epRFutT=100` compiled + counter moves and continuation history compiled off. Same nodes as `fsrt` with
`epQFilt=1,epRFutT=100`, tests.fen depth 10: 11196580; `epCMov=1`: 11794874; `epCMov=1,epCHist=3,epCHistW=2`: 12086912. The filter and T100 were measured apart, against different parents, never
together. Smoke test of the continuation history against the counter moves: at 10+0.1 16 games, 8 points, no time losses; at 1+0.01 24 games, 22 time losses, 16 of them by the continuation history side, by a
few ms. Too few games to say more; the time losses are to be watched in the SPRT (the tables are cleared at the start of every search).

**S25.** Continuation history on top of the counter moves, SPRT on MEMO (`chis-cmov-tc10`, `fbas` 24a69e7, `epCMov=1,epCHist=3,epCHistW=2` against `epCMov=1`, the opponent
shows as `fbas`), 10+0.1, [0, 5]: +10.54 +- 7.22, 5010 games, LLR 2.96, H1 accepted, 11.4 hours. One time loss, by the opponent. The prediction was 0 to +4 (S24 register): the cut
gain at level 11 (S23, a third to a half of the counter moves) underrated it, and the speed cost of about 2% did not weigh much. Result in `C:\Learn\Data\results\memo\chis-cmov-tc10`.

**S26.** Counter moves alone against `fbas` (`cmov-fbas-tc10`, `epCMov=1`), 10+0.1, [0, 5], stopped by hand at 5398 games: +1.54 +- 6.80, LLR -0.40, no decision. The prediction was
+3 to +10. Stopped because the pair counter moves + continuation history is proven (S25) and the remaining question is whether the counter moves add anything on top of the
continuation history, which this match does not answer.

Cut gains and Elo, at fixed level 11 against timed games: counter moves -0.234 / -0.217 gave about +1.5; continuation history on top of them -0.107 / -0.070 gave +10.5. The cut rate
at move 4+ does not rank ordering ideas by their Elo: use it to see whether a change works at all, not to choose between changes.

New base `chcm` = `fbas` + `epCMov=1`, `epCHist=3`, `epCHistW=2` compiled. Same nodes as `fbas` with these parameters, tests.fen depth 10: 12086912.

**S27.** `chcm` 09fb3d4 against `expo` 1e7130b on MEMO (`chcm-expo-tc10`), 10+0.1, fixed 4000 games, concurrency 8: +22.53 +- 7.95, LOS 100%, 4h35. The steps since `expo` (filter, T100,
counter moves, continuation history), each measured against another parent, together: the expectation from their sum was +10 to +20. No sign that they cancel. Result in
`C:\Learn\Data\results\memo\chcm-expo-tc10`.

**S28.** Late move pruning on the new ordering: `lmpn` 256dbad = `chcm` + `lmpc` (compiled off). Off identical to `chcm`: 12086912 nodes on tests.fen depth 10; D3 M1 C1 10087341
(-16.5%). With the filter (S24) the moves futility would prune are mostly out of the list in nodes with the futility flag, so they no longer count towards the LMP limit there. Smoke
games against `chcm` at 10+0.1: 16 games 4 points, then 80 games 43 points, together 47 of 96, all ended normally, no time losses. SPRT on MEMO: `lmpn-chcm-tc10`, D3 M1 C1
against the `chcm` build, [0, 5].
Result: -7.60 +- 8.08, LOS 3.3%, H0 after 3888 games (LLR -2.97). Prediction (-5 to +5, about 0, H0 more likely): direction right, the loss a bit larger than expected; about
the same as D3 M1 with the old ordering (-9.4). Neither the better ordering nor the count for the LMR level (C1) made the pruning pay. Late move pruning stays out until
`improving` (and history in the LMR) is in. Result in `C:\Learn\Data\results\memo\lmpn-chcm-tc10`.

**S29.** Overflow of the history bonus at the root, fixed on the current base: `hroot` 1f0bec8 = `chcm` + `epHRoot` (compiled 0 = as before). Only the two history updates
of the root move loop (a new best move, a fail high in the aspiration window) change: 1 gives them the bonus of depth 1 (2^30, as the continuation history already does),
2 drops them. Off identical to `chcm`: 12086912 nodes on tests.fen depth 10; root as depth 1 11422571 (-5.5%), no root update 11605996 (-4.0%), move 4+ 1.43% and 1.44%
against 1.39% (one sample each, S20: says little). Smoke games against `chcm` at 10+0.1: 10 of 16 each, no time losses. SPRTs on MEMO, [0, 5], 10+0.1, concurrency 4 each,
against the `chcm` build: `hr1-chcm-tc10` (epHRoot=1) and `hr2-chcm-tc10` (epHRoot=2).
Prediction (2026-11-11, before the results): both about 0 to +3; H0 more likely than H1 for both; no clear order between them. Basis: the bug hits one entry per new root
best move and only the order in the deeper nodes; the cut rates of no root update were at the floor (S21).
Result: `hr1` (root as depth 1) -4.12 +- 6.52, LOS 10.8%, H0 after 5824 games (LLR -2.99); after 3400 games it stood at +4.80 +- 8.54. `hr2` (no root update)
-3.37 +- 8.67, LOS 22.3%, stopped after 3400 games (LLR -1.50), its slot went to `lmrh`. Both forms about -4. The plan was a non-regression test if both end in H0;
recomputed from the pentanomial counts, `hr1` under [-3, 2] has LLR -1.64 (`hr2` -0.73 at 3400 games): no form shows itself neutral, so neither is adopted, the
overflow stays as it is. Prediction (0 to +3, H0 more likely): H0 right, the value a bit lower. The overflow makes the root best move the worst in the history
of the quiet sort below the root; that seems to cost nothing, maybe it even helps (variety after a new best move). Results in
`C:\Learn\Data\results\memo\hr1-chcm-tc10` and `hr2-chcm-tc10`.

**S30.** How late move pruning interacts with the adaptive LMR (user's question after S28: too aggressive pruning, or an LMR that no longer reaches
its aggressive levels?). Diagnostics `lmpx` 9480c12 (from `lmpn`, `epDiag`, compiled off, off identical: 12086912): 1 = nodes by LMR level, 2 = LMP
shadow (nothing pruned, no count for the LMR, so the same tree as off; the moves LMP would prune are searched and counted). D3 M1, depth 10, perf
build, tests.fen (50) and roots-500 (real games). Results in `C:\Learn\Data\measurements\search\lmpx`.

The adaptive LMR restarts in every iteration (`alphaBeta` sets level 8, `lmrhi` 8500, counter 0) and climbs with the moves that fail low. So the
level is a ramp, not an equilibrium: on roots-500 (off) 28.7% of the nodes are searched at the start level 8, 15.4% at 9, 18.8% at the top 15, mean
10.83 (tests.fen 10.65). The end level of S15 (14.3) hid this.

| roots-500 | Nodes | Mean level | Level 8 | Level 15 |
|---|---|---|---|---|
| Off | 90007659 | 10.83 | 28.7% | 18.8% |
| D3 M1 C0 | 79164741 (-12.0%) | 10.55 | 30.6% | 15.1% |
| D3 M1 C1 | 76984920 (-14.5%) | 11.09 | 25.5% | 21.3% |

tests.fen the same way: 10.65, 10.59, 11.02. C0 lowers the level, C1 raises it above off: the count of a pruned move is more than the move would have
given (its subtree is gone, but it also cannot fail high and subtract). Both lost about the same (C0 -9.4 on `nmlm`, C1 -7.6 on `chcm`), with the
level moving in opposite directions: the level is not the main cause.

Shadow on roots-500, prunable moves by remaining depth and moves over the limit (k), fail high = LMP would have pruned the cut move:

| Depth | Nodes with limit | Prunable | Fail high | k 0 | k 3 | k 7+ |
|---|---|---|---|---|---|---|
| 1 | 7771050 | 5148166 | 83942 (1.63%) | 8.6% | 3.0% | 0.49% |
| 2 | 2413049 | 3565485 | 19217 (0.54%) | 2.2% | 1.1% | 0.23% |
| 3 | 1176236 | 2398921 | 4183 (0.17%) | 0.7% | 0.3% | 0.08% |

tests.fen: the same shape (d1 1.44%, d2 0.39%, d3 0.13%). Together 107342 lost cuts against about 292000 cuts at move 4+ (1.61% of 18111095): LMP turns
about a third of the late cuts into fail lows. 78% of them are at depth 1, where LMP only saves the quiescence search of the move (no LMR at depth 1)
and the limit is 3 moves from the last capture. The fail high rate falls steeply with k at every depth: the move number predicts well, but the first
moves over the limit still cut often.

Reading: the loss comes from the pruning itself (the first reading), most of it at depth 1 and just over the limit; the LMR level is a side effect,
in both directions depending on the count. Next for LMP, if again: no LMP at depth 1 or a larger limit there, or a limit by history. For the LMR: the
restart at level 8 in every iteration (29% of the nodes at level 8) is its own candidate: carry the level over from the previous iteration. Before
history in the LMR or a history limit for LMP: the same shadow by history value of the move, to see whether history separates the moves that cut.

**S31.** LMP shadow by history: does the history separate the moves that cut, beyond the move number? `lmpx` e8adbb4, `epDiag=6` (bit 4: each
shadow move with its history value h and its entries c1, c2 in the two continuation rows, read before the move is made), D3 M1, roots-500, depth
10: 11112572 moves, 107342 fail high (same counts as S30). Sort value as in the engine s = h + (c1 + c2) / 2; negative = good. Analysis
`shadow_hist.py`, results in `C:\Learn\Data\measurements\search\lmpx\shadow-hist-roots.txt`.

AUC for the moves that fail high (0.5 = no information, 1 = perfect):

| Depth | Move number k | h | c1 + c2 | s | s within the same k |
|---|---|---|---|---|---|
| 1 | 0.81 | 0.77 | 0.73 | 0.79 | 0.62 to 0.65 |
| 2 | 0.75 | 0.71 | 0.66 | 0.73 | 0.58 to 0.64 |
| 3 | 0.73 | 0.70 | 0.64 | 0.72 | 0.57 to 0.68 |

The move number and the sort value are about equally good, and they overlap (the moves are sorted by s). Within the same k, s still separates:
depth 1, k 0 to 2, fail high by quintile of s from best to worst 10.6%, 7.1%, 6.0%, 4.6%, 2.2%; k 3 to 6: 3.5% to 1.0%. Moves with no bonus in
this search (s >= 0, 46% of the moves at depth 1) fail high 0.37%, the largest values (-2^26 and more) 4.5% to 21%. The continuation rows add to h
(0.79 against 0.77).

For pruning: pruning the worst share of the prunable moves, ranked by k alone or by k and s together, loses almost the same share of their cuts:
depth 1, half of the moves: 10.0% by k, 9.7% by both; 90%: 54.7% and 52.6%; depth 2 and 3 alike. History does not make LMP safe: even the safest
half of the moves holds a tenth of the cuts.

For reductions: within the same move number the best fifth by history cuts 3 to 5 times as often as the worst fifth. That is information the LMR
does not use now (it reduces by the level and the move number only). Limits: only moves over the LMP limit at depth 1 to 3 in zero window nodes;
the values are on the depth-weighted scale (bonus 2^(31 - absolute depth), halved when full), so a reduction by history should use the position of
the value in the node (rank) or classes of size, not fixed values.

**S32.** Late move reduction by history: `lmrh` 4019537 (from `chcm`, compiled off). For a quiet move the LMR table reduces (reduction > 0), the
sort value s (history plus chw / 4 times the continuation rows in use, as in the quiet sort; negative = good) changes the reduction: one less with
s <= -2^`epLmrHG` (0 = off), one more with `epLmrHB` = 1 and s >= 0 (no bonus in this search). Size classes, not fixed small values (S31). From
S31 at depth 1: g = 24 takes about the best 20% of the late moves (fail high 3.2% and more against 1.6% on average), no bonus about 46% (0.37%).
The adaptive LMR answers through its counter.

Off identical to `chcm`: 12086912 nodes on tests.fen depth 10. Nodes and move 4+ at depth 10 (adaptive LMR, single samples, S20: they say little):

| Setting | tests.fen | Move 4+ | roots-500 | Move 4+ |
|---|---|---|---|---|
| Off | 12086912 | 1.39% | 90007659 | 1.61% |
| G22 | 12919519 | 1.37% | | |
| G24 | 12064619 | 1.50% | 93941046 (+4.4%) | 1.76% |
| G26 | 12267644 | 1.39% | | |
| B1 | 10669093 | 1.45% | 82554287 (-8.3%) | 1.73% |
| G24 B1 | 13023409 | 1.40% | 90238188 (+0.3%) | 1.74% |

Smoke games against `chcm` at 10+0.1, 16 each: G24 7 points, G24 B1 8 points, no time losses. For MEMO, once `hroot` is done: G24 B1 (both
directions, about the same nodes) and B1 alone against `chcm`, SPRT [0, 5], concurrency 4 each (`lmrh-gb-chcm-tc10`, `lmrh-b-chcm-tc10`).
Prediction (2026-11-11, before the results): G24 B1 0 to +8, most likely +3, H1 about as likely as H0; B1 alone -4 to +4, about 0, H0 more likely.
Basis: S31 shows a real separation by history within the same move number, but the classes are coarse (one ply, one threshold), and the adaptive
level takes back part of any change in the mean reduction.
Result G24 B1 (`lhgb`): -3.60 +- 6.30, LOS 13.1%, H0 after 6078 games (LLR -2.95). Prediction (0 to +8, most likely +3): wrong in direction, outside the
range. The two directions together (about the same nodes) do not pay; which part costs is open until B1 alone (`lmrh-b-chcm-tc10`, running) is in.
Result in `C:\Learn\Data\results\memo\lmrh-gb-chcm-tc10`.
To separate the two halves, G24 alone in the free slot: `lmrh-g-chcm-tc10` (`lhg`, epLmrHG=24), SPRT [0, 5], concurrency 4, against `chcm`.
Prediction (2026-11-13, before the result): G24 alone -6 to +2, most likely -2, H0 much more likely. Basis: +4.4% nodes on roots-500 without a clear
place where the extra depth pays; the moves with large values cut often anyway.
Result G24 alone (`lhg`): -9.93 +- 8.95, LOS 1.5%, H0 after 3114 games (LLR -2.96; fastchess was stopped by hand after the SPRT ended, the result is
complete). Prediction (-6 to +2, most likely -2): direction right, the loss larger. Less reduction for late moves with a large history value costs:
those moves cut often anyway at the reduced depth (a cheap cut, the re-search confirms it), so the extra depth buys little; besides, fewer failed
reductions let the adaptive level rise, so the other late moves are reduced more. Against G24 B1 (-3.6) this hints that B1 adds, open until B1 alone
is in. Result in `C:\Learn\Data\results\memo\lmrh-g-chcm-tc10`.
Result B1 alone (`lhb`): -2.57 +- 5.73, LOS 19.0%, H0 after 7584 games (LLR -2.96). Prediction (about 0, H0 more likely): right. All three forms lose
or are even at best: G24 -9.9, G24 B1 -3.6, B1 -2.6. History classes in the LMR do not pay in this form: the move number already carries most of the
history (the quiet moves are sorted by it, S31: AUC within the same k only 0.62 to 0.65), and the adaptive level answers any change in the mean
reduction. Results in `C:\Learn\Data\results\memo\lmrh-b-chcm-tc10`.

**S33.** LMR level carried over between iterations: `lmrc` 6894348 (from `chcm`, compiled off). Until now every iteration restarts the adaptive LMR
at level 8 with the node limit 8500 and counter 0, and climbs again (29% of the nodes at level 8, mean 10.8, S30). `epLmrCarry` 1: the next iteration
starts at the level where the last one ended (kept in `MyState`, the UCI loop and SelfPlay pass it on); 2: also the node limit (it grows only at level
15); draft 1 starts a new search as before.

Off identical to `chcm`: 12086912 nodes on tests.fen depth 10. Nodes and move 4+ at depth 10:

| Setting | tests.fen | Move 4+ | roots-500 | Move 4+ |
|---|---|---|---|---|
| Off | 12086912 | 1.39% | 90007659 | 1.61% |
| Carry 1 | 8413953 (-30.4%) | 1.33% | 68724425 (-23.6%) | 1.54% |
| Carry 2 | 8413953 (-30.4%) | 1.33% | 68638855 (-23.7%) | 1.54% |

Carry 2 is practically carry 1, so only carry 1 is played. Smoke games against `chcm` at 10+0.1, 16 each: 8.5 points each, no time losses. SPRT on
MEMO: `lmrc-chcm-tc10` (`lc`, epLmrCarry=1), [0, 5], concurrency 4, against `chcm`.
Prediction (2026-11-13, before the result): -10 to +8, most likely -1, H0 more likely than H1. Basis: a quarter fewer nodes at the same depth is
about half a ply at the same time, but it comes from more reduction, and more reduction has lost so far: fixed level 11 -8.6, 14 -36.4 (S13); the
restart at level 8 may work as a safety margin in every new iteration. The adaptive counter still pulls the level down where reductions fail.
Result carry 1 (`lc`): -24.97 +- 13.28, LOS 0.01%, H0 after 1366 games (LLR -2.95). Prediction (-10 to +8, most likely -1): direction right, the loss
much larger, outside the range. The restart at level 8 in every iteration is worth about 25 Elo: the early part of each iteration (the first root
moves, the new PV) is searched with less reduction, and a quarter fewer nodes at the same depth do not make up for it. Together with the fixed levels
(S13: 11 -8.6, 14 -36.4) and G24 alone (less reduction for some moves, but a higher level for the others, -9.9): every change that raises the
reduction where the search starts loses. Open the other direction: start lower than 8, or reduce less in PV nodes. Result in
`C:\Learn\Data\results\memo\lmrc-chcm-tc10`.

**S34.** Less reduction where an iteration starts, and in PV nodes (the other direction after S33): `lmrb` 5092839 (from `chcm`, compiled as before).
`epLmrIni` (8) the start level of the adaptive LMR in every iteration, `epLmrLim` (8500) its node limit to raise the level (higher = slower climb),
`epLmrPV` (0) so many plies less reduction for the late moves of PV nodes (the zero window searches after the first move there; the re-search is as
before).

Off identical to `chcm`: 12086912 nodes on tests.fen depth 10. Nodes and move 4+ at depth 10:

| Setting | tests.fen | Move 4+ | roots-500 | Move 4+ |
|---|---|---|---|---|
| Off | 12086912 | 1.39% | 90007659 | 1.61% |
| Ini 7 | 12842998 (+6.3%) | 1.39% | 95257685 (+5.8%) | 1.71% |
| Ini 6 | 13172044 (+9.0%) | 1.44% | 92409753 (+2.7%) | 1.82% |
| Lim 17000 | 13475972 (+11.5%) | 1.49% | 102076719 (+13.4%) | 1.75% |
| PV 1 | 13671482 (+13.1%) | 1.37% | 90592275 (+0.6%) | 1.81% |

Smoke games against `chcm` at 10+0.1, 16 each: Ini 7 7 points, PV 1 8.5 points, no time losses. SPRTs on MEMO, [0, 5], concurrency 4 each, against
`chcm`: `lmrb-i6-chcm-tc10` (`lbi6`, epLmrIni=6; the larger step, the nodes on real roots grow least) and `lmrb-pv1-chcm-tc10` (`lbpv`, epLmrPV=1).
Prediction (2026-11-13, before the results): Ini 6 -4 to +8, most likely +2, H0 more likely than H1; PV 1 -5 to +6, most likely +1, H0 more likely.
Basis: S33 (-25 for a higher start) shows the start matters, but the adaptive level climbs back from 6 within the iteration, so the change is much
smaller than the carry-over in the other direction; PV nodes are few, the late moves there are mostly searched by the re-search anyway.
Third form on the new Linux runner m3a (concurrency 6, time control by its calibration, built there from `lmrb`): `lmrb-l17-chcm` (`lbl17`,
epLmrLim=17000), SPRT [0, 5] against `chcm`. Prediction (2026-11-13, before the result): -5 to +6, most likely +1, H0 more likely than H1. Basis:
a slower climb keeps less reduction longer in every iteration, like a lower start, but over the whole iteration; +13% nodes on roots-500.
Result Ini 6 (`lbi6`, MEMO, 10+0.1): -8.13 +- 8.31, LOS 2.8%, H0 after 3506 games (LLR -2.95), pentanomial [137, 439, 653, 417, 107]. Prediction
(-4 to +8, most likely +2): wrong, the result is below the range. A lower start loses too, less than a higher one (carry-over -25, S33): the start
level 8 looks like a local optimum, and the loss in both directions is not symmetric. The extra nodes (+9% on tests.fen, +3% on roots-500) do not
buy enough depth quality for the time they cost. Expected from this for Lim 17000 (slower climb, +13% nodes): rather a loss as well. Result in
`C:\Learn\Data\results\memo\lmrb-i6-chcm-tc10`.
Result PV 1 (`lbpv`, MEMO, 10+0.1): -6.65 +- 7.72, LOS 4.6%, H0 after 4286 games (LLR -2.95), pentanomial [182, 521, 792, 493, 155]; one time
loss of `chcm`. Prediction (-5 to +6, most likely +1): wrong, just below the range. Less reduction for the late moves of PV nodes costs too,
although it adds few nodes on real positions (+0.6% on roots-500, +13% on tests.fen). With S33 (higher start -25) and Ini 6 (-8.1): every change of
the amount of reduction tried so far loses, in both directions; the adaptive LMR as it is sits near an optimum for its amount. What is left in
this form: the shape (growth, penalty, S35); outside it, a reduction that depends on depth. Result in
`C:\Learn\Data\results\memo\lmrb-pv1-chcm-tc10`.
Result Lim 17000 (`lbl17`, m3a, 32+0.32 = 10+0.1 on MEMO by the calibration factor 3.2): -6.72 +- 7.75, LOS 4.5%, H0 after 4084 games (LLR
-2.95, 19.5 hours, about 210 games per hour), pentanomial [161, 504, 763, 481, 133]; one time loss of `lbl17`. Prediction (-5 to +6, most likely
+1): wrong, just below the range, as expected after Ini 6. A slower climb (+13% nodes) loses like a lower start and PV 1: the third change
towards less reduction, all near -7. First result of m3a; its size and direction fit the MEMO results of the same series. Result in
`C:\Learn\Data\results\m3a\lmrb-l17-chcm`.

**S35.** The hard coded constants of the adaptive LMR as parameters: `lmrt` dea4035 (from `lmrb`, compiled as before): `epLmrGrw` (120) growth
of the reduction table with the move number (factor 1.2), `epLmrPen` (20) base of the re-search penalty 2^d in tenths, `epLmrNoD` (1) no LMR up
to this depth. Off identical to `chcm`: 12086912 nodes on tests.fen at depth 10 (perf build).

Node scan (`hist_scan.py`, depth 10, chunks, against `lmrt` off): node totals from -14% to +12% without a common sign on the two sets (Pen 25:
-8.4% on tests.fen, +5.3% on roots-500; Grw 110 and 130 both about -13% on tests.fen); the adaptive level absorbs most of a table change, the
scan does not select. Data in `C:\Learn\Tune\lmrt`. Reductions at level 8 by quiet move number w:

| Growth | w 5 | 10 | 15 | 20 | 30 | 40 | 60 |
|---|---|---|---|---|---|---|---|
| 1.1 | 0 | 1 | 1 | 2 | 3 | 4 | 5 |
| 1.2 (now) | 0 | 1 | 1 | 2 | 3 | 3 | 5 |
| 1.3 | 0 | 1 | 1 | 2 | 2 | 3 | 4 |

Smoke games against `lmrt` (same build, 10+0.1, 16 each) for Pen 15, Pen 30, Grw 110, Grw 130, NoD 2: all normal ends, no time losses.
SPRTs of this series play against `lmrt` with its defaults, the same binary: no speed difference from reading the parameters.
First: `lmrt-g110-lmrt-tc10` (`ltg110`, epLmrGrw=110) on MEMO, [0, 5]. Prediction (2026-11-13, before the result): -4 to +4, most likely 0, H0
much more likely than H1. Basis: at the levels the search uses (6 to 10) a growth of 1.1 changes the reductions only from about the 40th quiet
move on (one ply more), so few nodes see a difference; the level may settle a little lower in compensation. A weak test of the shape: if it is
even, growth 1.0 or a table with depth would be the stronger step.
Result Grw 110 (2026-11-15): H0 after 13964 games, -0.35 +- 4.30 (nElo -0.47 +- 5.76), +4034 -4048 =5882, Ptnml [547, 1692, 2534,
1646, 563], LLR -2.96. Prediction (-4 to +4, most likely 0, H0) right. The run was interrupted twice: by mistake after 8102 games (its
config.json overwritten by the other slot, rebuilt from games.pgn with lab `scripts/rebuild_config.py`, 4051 complete pairs, none
left out), and for the online tournament on 2026-11-14 (fastchess did not stop on Ctrl+C, ended in the task manager; the autosave is
at most 20 games behind games.pgn). The same SPRT continued both times (same machine, same time control). Reading: the growth of the
table hardly matters at the levels the search uses; the shape question goes to the cap and the bound by remaining depth (S40, LMR cap
review). Results in `C:\Learn\Data\results\memo\lmrt-g110-lmrt-tc10` (with `resume-note.txt`).
Second: `lmrt-p15-lmrt-tc10` (`ltp15`, epLmrPen=15) on MEMO, [0, 5], against `lmrt`. A failed reduction costs the counter 1.5^d instead of 2^d
(depth 4: 5 instead of 16, depth 8: 25 instead of 256): the level settles higher, most where the search is deep. A crude depth term, but also
more reduction overall. Prediction (2026-11-13, before the result): -12 to +4, most likely -4, H0 much more likely. Basis: every increase of
the amount lost so far (S33 -25, S13); the shift of the reduction towards deep subtrees, which strong engines use, may win back part of it.

**S36.** What the adaptive LMR level does in games: trace on `lmrt` 38658b1 (one log line per level change, search unchanged: 12086912 nodes),
8 games of the trace engine against itself at 10+0.1 on mini (concurrency 2, `-l 2`), 9423 iterations, 105M nodes, parsed with
BarbarossaTuning `lmr_trace.py`. Data in `C:\Learn\Tune\lmrt\trace`.

| Iterations | Nodes | Mean level | Level 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 |
|---|---|---|---|---|---|---|---|---|---|---|
| draft <= 8 | 31M | 8.85 | 55.5% | 20.7 | 12.5 | 6.9 | 3.2 | 1.0 | 0.2 | 0.0 |
| draft 9-12 | 65M | 10.38 | 26.8% | 19.3 | 14.5 | 11.0 | 8.1 | 6.7 | 4.9 | 8.8 |
| draft 13+ | 8M | 11.39 | 22.6% | 14.1 | 9.5 | 7.8 | 6.4 | 7.1 | 6.0 | 26.5 |

Steps up 6215, down 38 (one per 2.7M nodes; once two in a row); the drops came from penalty depths 1 to 11, the level was back after a median
of 12301 nodes. The level hardly adapts downwards: every node without a failure adds 1 to the counter, which restarts at 0 after a step up; a
failure costs 2^d, mostly 8 to 128, and only right after a step up does that exceed the credit. So the scheme is a ramp: every iteration starts at
level 8 and the level rises one step per about 8500 nodes up to the cap 15. The reduction grows with the nodes already searched in the iteration:
the first root moves and the new PV with level 8, the later root moves with more, large (deep) iterations at the cap for a quarter of their nodes.
That is an implicit depth term. It explains S33 (carry-over: the new PV searched at the level of the end of the last iteration, -25) and makes
Ini, Lim and the cap the knobs that matter; the re-search penalty matters little (S35 Pen 15: now expected about 0, against the recorded -4,
which assumed frequent drops). The premise of the drops being costly (user, 2026-11-13: a fail high lowers the level drastically for a while)
does not hold in this form; mini searches fewer nodes per move than MEMO, the shape should hold.
Pen 15 on MEMO stopped by hand after S36 (it hardly changes the search; result recorded when pasted). In its slot the faster ramp, the other
side of Lim 17000 on m3a: `lmrt-l6-lmrt-tc10` (`ltl6`, epLmrLim=6000), [0, 5], against `lmrt`. The level rises one step per about 6000 nodes
instead of 8500: the first root moves and the PV keep level 8 for fewer nodes, later root moves and deep iterations get more reduction (more
nodes at the cap). Prediction (2026-11-13, before the result): -10 to +4, most likely -3, H0 much more likely. Basis: every increase of the
reduction lost so far (S33, S13); this one leaves the start of each iteration nearly as it is, so it should cost less than the carry-over.
Pen 15 stopped after fewer than 140 games, no summary printed: no result.
Result Lim 6000 (2026-11-15): no decision at the game limit, 20000 games, +1.86 +- 3.50 (nElo +2.55 +- 4.82), +5830 -5723 =8447,
Ptnml [717, 2328, 3851, 2339, 765], LLR -1.00 [0, 5], LOS 85%. Prediction (-10 to +4, most likely -3, H0 much more likely): inside the
range, but the first result of S28 onwards above the most likely value. Reading: the faster ramp is neutral to slightly positive at
10+0.1 (not +5); with Lim 17000 (S34, -6.7 at an H0 stop, so "not +5", likely a few Elo less) the ramp speed is flat or falls
slowly towards slower ramps around the default 8500. At 10+0.1 the ramp hardly reaches the cap (S37), so this measures the climb
below it; at long time controls a faster ramp brings more nodes to the cap and acts together with it (LMR plan, step A3).
Check of S36 with more games (same trace engine, mini, concurrency 2): 16 games at 10+0.1 on other openings (28437 iterations, 321M nodes) and 8
games at 30+0.3 (15631 iterations, 453M nodes; about three times the nodes per move, beyond MEMO at 10+0.1):

| Games | Mean level all | d9-12 | d13+ | At cap 15 (all / d13+) | Steps up / down | Drops in a row |
|---|---|---|---|---|---|---|
| 8 at 10+0.1 | 10.01 | 10.38 | 11.39 | 7.6% / 26.5% | 6215 / 38 | 1: 36, 2: 1 |
| 16 at 10+0.1 | 10.07 | 10.34 | 10.86 | 7.9% / 15.4% | 18049 / 174 | 1: 158, 2: 8 |
| 8 at 30+0.3 | 11.97 | 11.73 | 12.62 | 32.2% / 39.6% | 20229 / 578 | 1: 398, 2: 66, 3: 8, 8: 3 |

The ramp is confirmed at 10+0.1 (the 16 games repeat the 8). At 30+0.3 the iterations are larger, the level climbs further: a third of all
nodes at the cap. Drops grow to 3% of the steps and come from deeper failures (penalty depth up to 18, 2^18 beats any credit); three cascades
went from 8 down to 0. So the scheme depends on the time control: the longer the search, the more of it runs at the cap, and parameters tuned
at 10+0.1 need not fit longer games. The cap and the ramp speed are the knobs; a ramp by depth instead of by nodes would not depend on the time
control. Data in `C:\Learn\Tune\lmrt\trace\tc10b`, `tc30`.

**S37.** The cap of the LMR ramp as a parameter and what it does at fixed time: `lmrm` 790aca7 (from `lmrt` with the trace), `epLmrMax` (15)
the highest level of the adaptive LMR (the table has levels 0 to 15 only). Off identical: 12086912 nodes on tests.fen, 90007659 on roots-500
(depth 10). Nodes at depth 10 on tests.fen: cap 14 +2.5%, 13 +11.5% (roots-500 +6.2%), 12 +20.9%. Smoke games cap 13 against `lmrm` at
10+0.1, 16: normal ends.

Fixed time on 200 positions of roots-500, all four caps in parallel on mini (BarbarossaTuning `fixed_time_scan.py`: the search is stopped
after the time, because the engine ignores `go movetime`), completed depth paired against cap 15, and the share of nodes at the cap
(`lmr_trace.py`):

| Time | Cap 15 depth | 14 | 13 | 12 | Nodes at cap 15 (all / d13+) |
|---|---|---|---|---|---|
| 0.5 s | 11.66 | +0.01 +- 0.06 | -0.09 +- 0.06 | -0.09 +- 0.05 | 2.7% / 1.7% |
| 1.5 s | 14.43 | -0.35 +- 0.09 | -0.76 +- 0.09 | -0.91 +- 0.10 | 35.0% / 40.7% |

With 0.5 s per move (about 200k nodes: close to a move of MEMO at 10+0.1) the cap hardly binds and lowering it costs no depth; with 1.5 s (600k
nodes) a third of the nodes run at the cap and every step down costs depth (13: three quarters of a ply). Depth is not Elo, but: a lower cap
would show little at 10+0.1 (an SPRT of cap 13 there probably ends near 0), and where the cap binds, more reduction at the end of large
iterations buys depth. The open question is above the cap: levels over 15 need a table extension (a level 15 + k reducing the moves level 15
reduces by k more), and a time control long enough for the cap to bind (MEMO 10+0.1 and m3a 32+0.32 search about as much as the 0.5 s
row). Engine gap found on the way: `go movetime` is parsed but not used (`Main/Barbarossa.hs`), the search runs without a limit.
Data in `C:\Learn\Tune\lmrt\capscan`.

**S38.** Caps above 15: `lmrm` e908bd8 extends the table to levels 16 to 19 (level 15 + k reduces the moves level 15 reduces by k plies
more; levels 0 to 15 unchanged), `epLmrMax` up to 19, default 15. Off identical: 12086912 nodes (depth 10, tests.fen); caps 17 and 19 -3.0% and
-2.4% there. Fixed time on the same 200 positions of roots-500 as S37, caps in parallel on mini, completed depth paired against cap 15, share
of the nodes at the top levels:

| Time | Cap 15 depth | 17 | 19 | At 15 with cap 15 | At 16-17 with cap 17 | At 18-19 with cap 19 |
|---|---|---|---|---|---|---|
| 1.5 s | 14.56 | +0.03 +- 0.13 | +0.03 +- 0.11 | 38.2% | 30.2% | 11.2% |
| 3 s | 14.69 | +0.16 +- 0.13 | +0.41 +- 0.13 | 50.1% | 40.7% | 23.6% |

With S37: the depth at fixed time rises with the cap up to 15 from 1.5 s on, and beyond 15 only at 3 s; the cap where more costs nothing in
depth grows with the time per move. At 3 s half the nodes run at cap 15. The node ramp makes the engine reduce differently at every time
control, and at long ones the cap holds it back. Depth is not Elo (more reduction also loses moves), but this is a candidate explanation for the
user's long-standing observation that Barbarossa plays relatively weaker at longer time controls (strength measured mostly at 1m+1s). A test
needs a long time control: a cap above 15 at 10+0.1 would show nothing (S37: at 0.5 s even cap 15 hardly binds). Data in
`C:\Learn\Tune\lmrt\capscan\h1500`, `h3000`.

**S39.** Does the adaptive LMR level follow the game phase (user, 2026-11-13: a reason the scheme works may be that it adapts to the phase,
where the number of moves differs)? The traces of S36 to S38 grouped by the pieces other than pawns and kings in the root position (BarbarossaTuning
`lmr_phase.py`), node weighted mean level, overall and at equal draft:

| Source | Pieces | Searches | All | d <= 8 | d 9-12 | d 13+ |
|---|---|---|---|---|---|---|
| 24 games 10+0.1 | 13-14 | 152 | 10.45 | 8.78 | 10.84 | 12.95 |
| | 9-12 | 658 | 10.52 | 8.99 | 10.86 | 12.46 |
| | 5-8 | 739 | 10.31 | 8.68 | 10.63 | 12.14 |
| | 0-4 | 2175 | 9.47 | 8.92 | 9.58 | 10.02 |
| 8 games 30+0.3 | 13-14 | 131 | 12.30 | 8.59 | 12.46 | 13.73 |
| | 9-12 | 186 | 12.11 | 8.72 | 12.09 | 13.75 |
| | 5-8 | 165 | 12.07 | 8.51 | 11.19 | 13.11 |
| | 0-4 | 677 | 11.76 | 9.95 | 10.83 | 12.28 |
| 200 roots, 3 s each, cap 15 | 13-14 | 18 | 13.07 | 9.00 | 12.69 | 13.97 |
| | 9-12 | 51 | 12.95 | 8.93 | 12.34 | 13.62 |
| | 5-8 | 39 | 13.22 | 8.74 | 12.62 | 13.76 |
| | 0-4 | 92 | 13.10 | 8.72 | 12.47 | 13.43 |

In games the endgame (0-4 pieces) runs at clearly lower levels at the same draft (10+0.1, d13+: 10.0 against 12.1 to 13.0; at 30+0.3 the gap
is smaller), the other phases hardly differ. In single searches from scratch (fixed 3 s, `ucinewgame` before each, same time for every phase)
the difference is gone. So the level does follow the phase in games, but not by the phase itself: through the size of the iterations. In a
game an endgame iteration of a given draft costs fewer nodes (transposition table from the moves before, less time per move), so the node ramp
climbs less and the endgame gets less reduction. Two readings for later: a ramp by depth (form #2) would lose this unless it takes the
phase or the move count in; and whether the endgame wants less reduction is open (the scheme gives it less by accident of the node count).
Group sizes are small for 13-14 pieces; d <= 8 shows no difference anywhere (the ramp has not started there).

**S40.** The cap at a long time control (2026-11-13/14). Machines: m3b, the second Linux box, set up on 2026-11-13, the same hardware as m3a
and the same speed (149k nodes per second alone, 143k with 6 in parallel; m3a 145k / 143k), so the same factor 3.2: 60+1 on MEMO = 192+3.2 on
the boxes, about 28 games per hour at concurrency 6. Run `lmrm-m19-lmrm-tc60`: `lmrm` e908bd8 `epLmrMax=19` (`lm19`) against `lmrm` (cap 15),
same binary, a fixed 2000 games planned. Prediction (before the result): -6 to +12, most likely +3.
Result: stopped by hand after 283 games as a clear loss (the one exception of the fixed-length rule): +50 -109 =124, 39.6%, about -74 Elo; the
last summary, at 200 games, -75.9 +- 35.9, LOS 0.00%, pentanomial [14, 37, 29, 18, 2]; all games ended normally, no time losses. Prediction
badly wrong. S38's +0.41 ply at 3 s per move is not strength: the levels above 15 reduce the late moves so much that the search misses moves
(depth is not Elo, as S38 warned). The weaker play at long time controls is then not a cap that holds the ramp back; if anything the reverse:
the ramp reaches the cap at long time controls and the reduction there may already be too strong. Test of that: the cap on both sides of 15 at
the same time control, a fixed 2000 games each, read once at the end: cap 13 on m3b (`lmrm-m13-lmrm-tc60`), cap 17 on m3a
(`lmrm-m17-lmrm-tc60`). Prediction (2026-11-14, before the results): cap 17 -45 to -5, most likely -25; cap 13 -15 to +15, most likely 0.
Result in `C:\Learn\Data\results\m3b\lmrm-m19-lmrm-tc60`.
Cap 13 at the short time control (2026-11-15): `lmrm` `epLmrMax=13` (`lm13`) against `lmrm` (cap 15, same binary) on MEMO, 10+0.1,
SPRT [-5, 0], run `lmrm-m13-lmrm-tc10b` (after a [-3, 2] run stopped at 4000 games, not used). Prediction (before the result): -3 to
+2, most likely -1; H1 much more likely. Result: H0, -9.08 +- 6.53, LOS 0.32%, 5664 games, LLR -2.96, pentanomial [224, 697, 1096,
633, 182]. Prediction wrong: cap 13 costs about 9 Elo at 10+0.1, although the cap binds in only about 3% of the nodes there (S37) and
S37's 0.5 s row shows no depth loss. So the level does not act only where it binds at the end of an iteration: a lower cap also
changes the ramp's earlier course (the level reaches the cap sooner, less room to climb back after penalties) or the moves at the cap
are the important ones. A fixed lower cap is out as the one setting for all time controls (branch C of the plan if cap 13 gains at
the long time control); the depth floor (S45), which keeps level 15, is the form to try first. Result in
`C:\Learn\Data\results\memo\lmrm-m13-lmrm-tc10b`.
Results of the two caps at 192+3.2 (2026-11-17, 2000 games each, 16 MB hash, read once at the end; about 72 hours each at
concurrency 6):
- Cap 17 (m3a, `lmrm-m17-lmrm-tc60`): -23.49 +- 10.96, nElo -32.7 +- 15.2, LOS 0.00%, +485 -620 =895 (46.6%), pentanomial
  [91, 266, 381, 211, 51]. Prediction (-25, range -45 to -5) right.
- Cap 13 (m3b, `lmrm-m13-lmrm-tc60`): +40.66 +- 10.85, nElo +57.6 +- 15.2, LOS 100%, +631 -398 =971 (55.8%), pentanomial
  [41, 182, 379, 299, 99]. Prediction (0, range -15 to +15) wrong, far too low (the interim of +36.7 at 798 games was not used
  for it, the prediction was written before).
Reading: the level curve at the long time control falls steeply from 13 over 15 (-41) to 17 (-64) and 19 (about -115, stopped):
about 20 to 30 Elo per level step of 2, and no flattening seen below 15. At 10+0.1 cap 13 costs 9 (above): the time control
dependence of branch C of the plan, not A. By the time control decision (Methods) cap 13 could be kept as it is (wins clearly at
the long time control, loses less than 10 at the short one). The floor f = 4 at cap 15 (S48: +16.7 at 60+1 on MEMO, the short
screen running near +4.6) gets less at the long time control but nothing lost at the short one; the two are measured on
different machines and hash loads, so the comparison is rough. Open: does the floor add to cap 13, and where is the bottom of
the cap curve. Results in `C:\Learn\Data\results\m3a\lmrm-m17-lmrm-tc60` and `C:\Learn\Data\results\m3b\lmrm-m13-lmrm-tc60`.

**S41.** Time use in games against other engines (2026-11-14): the Barbarossa log of an online tournament (ICS through WinBoard,
engine `0.8.0 w5`, hash 68 MB from WinBoard, one engine process for all games), 13 games of which 12 played (one ended after one
move). Time control 4+1: 246.6 s on the clock at the first move, about 0.97 s added per move. 29 to 90 moves per game, mean
completed draft 13 to 15 per game; at the end 3 games with a mate score for Barbarossa, 8 against it (the last search; result
tags are not in the log). The LMR level stood at the cap 15 at the last draft of 78 to 98% of the moves (S36 to S40: the cap
binds at long time controls).

| Game | Moves | Clock at move 20 | at move 40 | Moves under 10 s |
|---|---|---|---|---|
| 1 | 59 | 127 s | 20 s | 18 |
| 2 | 69 | 54 s | 9 s | 31 |
| 4 | 45 | 97 s | 1.7 s | 10 |
| 6 | 40 | 5 s | 1.7 s | 22 |
| 9 | 69 | 92 s | 6 s | 31 |
| 12 | 90 | 108 s | 41 s | 37 |

Half of the clock is gone by about move 20 (game 2: the first 8 moves took 154 of 246 s), the rest of the game is played on the
increment. Causes in `compTime` (`Main/Barbarossa.hs`, unchanged from 0.8.0 to `chcm`): (1) the maximum of a move is the target
plus a share of the rest (`remTimeFracIni` 0.15, rising to 0.5 near zero), and the searches reach it often (game 2 moves 2, 3, 5:
28.7, 32.5, 32.1 s at the maximum, against targets of 5 to 6 s); (2) the moves to go come from the score (`estimateMovesToGo`:
50 below 1 pawn, 36, 24, 15, 10 at 4 pawns, ..., 2 from 8 pawns): in game 6 the score was +4 to +8.5 from move 11 on, the engine
spent as for a game about to end and had 5 s left at move 20 (it won). The matches of the tuning work cannot see this: both sides
use the same time management. Against other engines it means many moves at about 1 s from the middlegame on, a candidate
explanation of the weaker play at long time controls (the user's observation, S38) besides the LMR cap. Register entry "Time
management at long time controls". Log in `C:\Learn\Data\logs\ics-4p1`.
`chcm` in the GUI at 60+1 against other engines (2026-11-10/11, 7 logs, 28 games, 1753 moves), the same measures (by
share of the starting clock; moves at the maximum: used >= 90% of `mxr`):

| Engine, time control | Clock at move 20 | at move 40 | Moves under 10% | Moves at the maximum | Used / target |
|---|---|---|---|---|---|
| `0.8.0 w5`, 4+1 (ICS) | 32% | 5% | 44% | 30% | 1.7 (5.4 s / 3.2 s) |
| `chcm`, 60+1 (GUI) | 46-74% by log, about 58% | 18-36%, about 26% | 5-35%, about 16% | 8-20%, about 12% | 1.0 (1.8 s / 1.8 s) |
| `chcm`, 4+1 (ICS) | 29% | 3% | 53% | 29% | 1.6 (4.6 s / 2.9 s) |

`chcm` keeps much more of its clock and uses about its target per move; 0.8.0 used 1.7 times its target, mostly by searches
running to the maximum. Two differences at once: the engine (the LMR explosion fixed since 0.8.0, user) and the time control
(at 60+1 the increment is about 40% of the time per move, at 4+1 about 15%), so the share of each is open; `chcm` at 4+1
(tournament of 2026-11-14) separates them. Still in `chcm`: a quarter of the clock left at move 40, single games with 3% there.
Logs in `C:\Learn\Data\logs\gui-60p1`. Script: BarbarossaTuning `time_use.py`.

`chcm` at 4+1 in the online tournament of 2026-11-14 (ICS through WinBoard, hash 516 MB = 512 + 4 MB EGTB cache, one engine
process, 11 games, 10 with 10 moves or more, 652 moves; last row of the table): the same picture as `0.8.0 w5`. Clock at move 20
11-53% by game, at move 40 1-3% in 7 of the 8 games that got there (one 14%), half of the moves under 10% of the start, 29% of the
moves at the maximum, 1.6 times the target (1.7 without the 184-move game, which was played almost all on the increment). The
completed draft is higher (mean 13 to 20 by game against 13 to 15 for 0.8.0); the LMR level at the last draft at the cap 15 in
64 to 94% of the moves. So the waste comes from the time control, not from the LMR explosion of 0.8.0: at 4+1 the maximum per
move (target plus 15% of the rest) is reached often and the clock is gone by the middlegame; at 60+1 the increment carries the
game. Log in `C:\Learn\Data\logs\ics-4p1-chcm`.

Playing on the increment in a decided game costs nothing (user): Barbarossa does not count the moves to mate in a mate score (on
purpose from the beginning), so with a mate found it does not take the shortest way, but it never loses the mate; it only makes
the game longer (the 184-move game). The clock at the first move under 10% of the start, by game: move 21 to 51, with a score of
-136 to -504 in 8 games (all 8 lost by mate later) and +936 / +1124 in the 2 won ones; a score of 1000 or more came 1 to 20 moves
after that point. So in the lost games the clock was low while the game was still open; whether the low clock made them lost is
not shown (the score was already negative). Reading (user): the time management fits short time controls (60+1), not longer ones.

**S42.** LMR census at fixed levels (M3 of the LMR cap review, `Search/LMR-CAP-REVIEW.md`; 2026-11-15). Branch `lmcs` 9b76970 (from `lmrm`):
build flag `lmrcensus` (SelfPlay only) counts the zero window searches of `zeroWithLMR` (not aborted) by the level, the remaining
depth d1, the reduced depth d' and w (moves since the last capture), with fail highs of the reduced search and confirmations by the
re-search at d1; `SelfPlay -P` prints them, BarbarossaTuning `lmr_census.py` makes the tables. Search unchanged: 12086912 nodes on
tests.fen at depth 10. Run: deterministic perf build, roots-500 (real games) at draft 12, `epLmrFix` 13, 15, 17, 19, one process per
level over the whole file (TT and history carried between positions, deterministic).

| Fixed level | Nodes | Reduced searches | d' = 1 | d' <= 2 | Pulled into the static range (d1 >= 7, d' <= 6) | Mean reduction | Zero window searches (LMR loop) per 1000 nodes |
|---|---|---|---|---|---|---|---|
| 13 | 589M | 34.6M | 78.3% | 88.6% | 3.9% | 1.86 | 348 |
| 15 | 327M | 16.9M | 89.9% | 94.4% | 7.4% | 2.41 | 195 |
| 17 | 270M | 13.3M | 96.6% | 98.3% | 9.2% | 2.69 | 174 |
| 19 | 619M | 12.5M | 99.2% | 99.7% | 9.6% | 2.78 | 69 |

Mean reduction by d1 (2-3 / 4-6 / 7-10 / 11+): level 13 1.3 / 2.8 / 3.4 / 2.6, 15 1.4 / 3.5 / 5.5 / 5.1, 17 1.4 / 3.7 / 6.2 / 6.8, 19
1.4 / 3.8 / 6.5 / 8.4; the share of reduced searches by d1 is about the same from 15 to 19 (18%, 76-77%, 80%, 48%), 13 reduces fewer
(12%, 63%, 70%, 38%).

Readings:
- Near the horizon the reductions collapse to depth 1 as the review said: 90% of the reduced searches end at d' = 1 at level 15,
  99% at 19 (78% at 13). Most reduced searches have d1 <= 6 at every level (the tree is widest there).
- The alarm does not go blind near the horizon: at equal d1 and w the share of reduced searches that fail high and are confirmed
  (`conf / red`) is about the same from 15 to 19 for d1 <= 6 (d1 4-6, w 3-5: 1.3%, 1.2%, 1.2%; d1 2-3, w 3-5: 2.3%, 2.7%, 2.8%).
  For d1 7-10 it falls a little and the alarm gets worse (`conf / fh` at w 3-5: 72% at 15, 56% at 17, 38% at 19; w 6-10: 37%, 27%,
  28%). So the "moves no longer found" lower bound shows no large loss; mechanism 3 of the review is not supported near the horizon,
  weakly in the middle of the tree. The census cannot say whether a reduction was wrong (it counts only what the alarm catches).
- New: fixed level 19 costs 619M nodes at draft 12, more than 15 (327M) and 17 (270M), although it does fewer searches in the LMR
  loop (42.8M against 63.8M at 15): the extra nodes are outside the reduced and normal zero window searches of the loop, i.e. in
  PV searches, full window re-searches after a zero window fail high, aspiration re-searches or QS. Not yet located; under the
  adaptive ramp (S38) cap 19 needed fewer nodes, but the ramp spends little time at the top in a draft 12 search. Possible link to
  the -74 of cap 19 (S40): an unstable search at the top level (scores jumping, repeated PV re-searches) would cost more than the
  depth suggests. Next: nodes per position for levels 15, 17, 19 (are a few positions exploding or all of them?) and the count of
  PV and aspiration re-searches.
- Level 13 fixed costs 1.8 times the nodes of 15 at the same draft (as S13: fixed 11 and 14 against the ramp).
Data: `C:\Learn\Data\measurements\search\lmcs` (raw outputs with the census lines, `census-fix-d12.txt` all tables).
Nodes per position (2026-11-15, `node_scan.ps1`, one process per position, fresh TT and history, roots-500, draft 12): totals
level 15 350M, 17 237M, 19 609M (as in the run over the whole file); per position, against level 15: level 17 geomean 0.81 (median
0.84), level 19 geomean 0.72 (median 0.76). The total of level 19 comes from a few positions: the 12 largest hold 84% of it, one
alone 70% (position 362, `8/8/8/8/3Q4/5k2/6r1/4K3 w`, queen against rook: 428M nodes against 384k at level 15, 1117 times; position
182, also queen against rook, 36 times). Without these 12, level 19 needs 100M against 323M (15) and 214M (17). The score of
position 362 is stable at both levels (+2.5 at every draft), so it is not an unstable score; the nodes jump at drafts 7 and 11
(level 19: 5.5k, 331k at 7, 726k at 10, 42.7M at 11, 428M at 12; level 15: 10k at 7, 358k at 11). At draft 11 level 19 does 15M beta
cuts (99.9% at the first move) but only about 0.1M searches in the LMR loop: the nodes are outside it (null move searches, QS, PV
nodes; not yet split). Reading: the higher level is cheaper in almost every position, and explodes in a few endings with few pieces
(queen against rook: many checks, long forced lines). In games the explosion would cost time where it happens and could be part
of the -74 of cap 19; S39 (endings run at lower levels under the ramp) limits how often a game gets there.
Next: split the nodes of such a position (QS, null move, PV, the LMR loop) to find the part that explodes.
Hypothesis (user, 2026-11-15): the check extension. In queen against rook the queen side checks almost every move. From the code:
every check is extended by one ply (`exten`, `Moves/Internal/Base.hs`: `inCheck p2 = 1`), without a limit by ply or by the number
of extensions, and repetition is the only stop; a check keeps the depth (d1 = d), a check of each side in turn (rook spite checks)
never lowers it. It fits "outside the LMR loop" if the extended lines run in full window nodes: in a full window node at d <= 2
(`minPvDepth`) every move is searched by `pvSearch` with the full window (no zero window, not counted by the census), and checks keep
such subtrees at d <= 2. Why the level changes it so much is open (a different PV and TT at level 19 may send the search into these
lines). Test, when it comes: nodes of position 362 split by in-check / extended / full window at d <= 2, or the same position with
the check extension limited (e.g. no extension beyond ply 2 * draft, or only while d1 is below the draft).
User's reading (2026-11-15): hardly important for the strength (4 pieces); of interest only (1) for what it shows about the self
regulating LMR, and (2) to keep such explosions out of the node statistics of experiments (totals are dominated by a few positions:
use per position ratios, geomean or median, as the scan above). The engine must still solve KQKR.

**S43.** Mate bound fixes, local checks (2026-11-15). Branch `mbnd` b7fb86e from `rel090` (search of `chcm`): no null move when
beta is a mate bound (`nearmate b` in `nullMoveFailsHigh`), no QS delta cut when alpha is a mate bound (findings 2 and 9 of
`Search/SEARCH-REVIEW-BUGS.md`). Deterministic perf builds of `rel090` (base) and `mbnd`. Perft unchanged (start position d5, Kiwipete
d4). tests.fen depth 10: 12086912 (base, as `chcm` and `lmcs`) against 12292105 nodes (+1.7%).
roots-500 at depth 12, one process per position: per position node ratio geomean 1.017, median 1.000, 27% of the positions above
1; 456 of 500 give the same score at every depth. Total 306M against 340M nodes, mostly from a few positions (one middlegame 4.5M to
33.5M, a queen ending 0.46M to 12.3M) with no mate score. Six positions show a mate score at some depth: the fix finds the mate one ply
earlier in two (d 9 to 8, 8 to 7), at the same depth in two, and in two where the base finds none by depth 12 (one of them -19744
in the base, a score just outside `nearmate`, not explained). In these six the fix needs more nodes (0.9 to 10.6 times): the null
move no longer cuts at the mate bounds.
The explosions of S42 (`lmcs` with the same two guards, not committed; depth 12, one process per position, `epLmrFix`):
| Position | Level 15 base / fix | Level 19 base / fix |
|---|---|---|
| 182 `5RK1/8/4k1q1/8/8/8/8/8 w` (KR against KQ) | 611k / 365k | 22.0M / 2.26M |
| 362 `8/8/8/8/3Q4/5k2/6r1/4K3 w` (KQ against KR) | 384k / 2.31M | 428M / 252M |
| 485 `1k2Q3/8/8/3K4/8/8/8/1r6 b` (KQ against KR) | 438k / 438k | 4.82M / 0.40M |
Readings: the mate bound bugs cause most of the level 19 explosion in two of the three positions (10 and 12 times fewer nodes),
less than half of it in 362, which still needs 650 times its level 15 count: there the check extension (register) stays the
candidate. The fixes change the search a little outside mate windows (geomean +1.7%) and find mates earlier or at all. Elo: to
be played (SPRT with no-loss bounds against `chcm`). Data: `C:\Learn\Data\measurements\search\mbnd`.

**S44.** The check extension in the KQ/KR explosions (2026-11-15; the user's hypothesis of S42). Two diagnostic builds on `lmcs`
with the mate guards of S43 (none committed): A without the check extension (`exten`: `inCheck p2 = 0`), B without any extension
beyond ply 2 * draft (in `pvInnerLoop`). Depth 12, `epLmrFix`, one process per position, nodes:
| Position | Guards only, level 15 / 19 | A: no check extension, 15 / 19 | B: no extension beyond ply 24, 15 / 19 |
|---|---|---|---|
| 182 (KR against KQ) | 365k / 2.26M | 214k / 31k | 365k / 2.26M |
| 362 (KQ against KR) | 2.31M / 252M | 32k / 474k | 3.53M / 8.88M |
| 485 (KQ against KR) | 438k / 402k | 224k / 11.7k | 438k / 402k |
Scores at depth 10 to 12 about the same in all variants (362: 248 to 260).
Readings:
- The rest of the explosion in 362 is the check extension: without it level 19 needs 474k nodes, with the ply limit 8.9M instead of
  252M (28 times fewer). In 182 and 485 the ply limit changes nothing: there the mate guards were enough (S43).
- Mechanism (inference): an extension is added to the reduced depth, so a reduced move that checks is not reduced on the checking
  line, and in queen endings nearly every move of one side checks. At level 19 almost every reduced search ends at d' = 1 (S42); a
  check chain keeps that depth, so the LMR cuts the quiet moves and leaves the tree to the checking lines, which only repetition or
  a ply limit ends. Without the check extension level 19 is cheaper than 15 in all three positions, as the LMR is meant to be.
- For the LMR statistics: node totals and fixed level scans of positions with a queen and few pieces measure the check chains, not
  the LMR. Report them apart (stratum, as in the method review) until the extension has a limit.
- Not measured: whether a limit costs strength (KQKR must still be won, mates by checks found), nor its effect outside these
  positions. The ply limit B is the simple form; others: a limit on the number of extensions per line, or no extension in a
  reduced line.
Data: `C:\Learn\Data\measurements\search\mbnd` (`expl-ceA-*`, `expl-ceB-*`).

**S45.** LMR depth floor, local checks (2026-11-15; design `Search/LMR-DEPTH-BOUND.md`). Branch `lmfl` 5463b64 (from `lmcs`, no mate
guards): `epLmrFlr` = f, reduced depth `d' = max dmin (d1 - r)`, `dmin = max 1 (d1 * f / 8)`, 0 = off. f = 0: 12086912 nodes on
tests.fen at depth 10 (identical); f = 4 under the adaptive ramp: 11367370 (-6%). Census as S42 (deterministic perf build with
`lmrcensus`, roots-500, draft 12, fixed level, one process per run; f = 0 from S42). Prediction (design): share at d' = 1 from 90% to
55-60% for f = 4 (80-85% for f = 3); `conf / fh` at d1 >= 4, w >= 6 towards level 13; nodes +30 to +100% for f = 4, +10 to +25% for f = 3.

| Level, f | Nodes | Reduced | d' = 1 | Mean red. | Mean red. d1 4-6 / 7-10 | `conf / fh` d1 4-6, w 6-10 / 11+ | `conf / fh` d1 7-10, w 6-10 / 11+ |
|---|---|---|---|---|---|---|---|
| 13, 0 | 589M | 34.6M | 78.3% | 1.86 | 2.8 / 3.4 | 67.5 / 31.1 | 75.0 / 31.3 |
| 15, 0 | 327M | 16.9M | 89.9% | 2.41 | 3.5 / 5.5 | 39.0 / 26.2 | 37.2 / 10.4 |
| 15, 3 | 386M | 16.6M | 79.8% | 2.29 | 3.3 / 4.6 | 41.0 / 29.2 | 35.8 / 21.3 |
| 15, 4 | 302M | 16.5M | 57.6% | 1.91 | 2.4 / 3.8 | 58.5 / 50.2 | 44.0 / 34.3 |
| 15, 5 | 404M | 20.2M | 61.5% | 1.70 | 2.1 / 3.0 | 63.6 / 54.3 | 57.4 / 52.0 |
| 19, 0 | 619M | 12.5M | 99.2% | 2.78 | 3.8 / 6.5 | 43.2 / 30.1 | 27.9 / 15.4 |
| 19, 3 | 473M | 13.3M | 82.7% | 2.53 | 3.6 / 5.2 | 45.7 / 33.3 | 40.2 / 29.6 |
| 19, 4 | 576M | 15.3M | 59.7% | 1.99 | 2.6 / 4.2 | 63.9 / 54.5 | 48.0 / 38.3 |
| 19, 5 | 882M | 16.7M | 62.3% | 1.78 | 2.2 / 3.2 | 64.6 / 55.0 | 57.9 / 48.4 |

Explosion positions (per position, fixed level, draft 12, nodes; f = 0 at 19 from S43, same build without guards):

| Position | 15: f 0 / 4 / 5 | 19: f 0 / 4 / 5 |
|---|---|---|
| 182 (KR against KQ) | 611k / 494k / 299k | 22.0M / 128k / 449k |
| 362 (KQ against KR) | 384k / 104k / 190k | 428M / 2.30M / 160M |
| 485 | 438k / 1.55M / 706k | 4.82M / 46.1M / 1.78M |

Readings:
- Share at d' = 1 and the mean reductions as predicted; f = 3 hardly acts (as predicted).
- The alarm: f = 4 lifts `conf / fh` at d1 >= 4 and w >= 6 most of the way to level 13 at d1 4-6 (58.5 against 39 and 67.5) and
  part of the way at d1 7-10 (44 against 37 and 75); at w 11+ above level 13. The same at level 19. `conf / red` hardly moves (0.4 to
  0.5%): more of the fail highs are real, not more found (the census still cannot count the misses).
- Nodes: not as predicted, f = 4 costs nothing at level 15 (-8%) and -7% at 19; f = 5 +24% and +43%. The totals of one process
  over the whole file are chaotic (TT and history carried, f = 3 above f = 4), so read them as "f = 4 cheap, f = 5 not": the floor
  makes fewer re-searches. Fits the -6% under the ramp at depth 10.
- The explosions are not removed, they move: at 19 f = 4 takes 182 and 362 from 22M and 428M to 128k and 2.3M, but 485 from 4.8M to
  46M; f = 5 leaves 362 at 160M. Without the mate guards and with the unlimited check extension (S43, S44) these stay chaotic;
  the floor is no repair for them.
- f = 4 is the form for the matches: about level 13's alarm with level 15's width, at no node cost. Next: the floor run on a box
  (f = 4 at cap 15 against cap 15, plan of 2026-11-15) needs a fast build of `lmfl` and a smoke match; a [-5, 0] screen at 10+0.1 on
  MEMO when a slot is free.
Data: `C:\Learn\Data\measurements\search\lmfl` (census outputs and tables, explosion CSVs).

**S46.** The screening at the long time control (2026-11-15/16; method review point 3, step A2 of the LMR plan). `chcm` against
`dev090` on MEMO, 60+1, a fixed 1000 games (500 rounds), read once at the end, run `chcm-dev090-tc60`, 16 MB hash. The short SPRTs
since `dev090` add up to about +80 at 10+0.1 (qthr +10.8, hpqt +5.8, rfut +17.6, nmlm +13.0, chcm +22.5, the filter step), a sum of
steps selected for passing. Prediction (before the result): +10 to +55, most likely +30.
Result: +82.47 +- 16.02, nElo +115.1 +- 21.5, LOS 100%, +413 -180 =407 (61.65%), pentanomial [17, 55, 185, 164, 79]. Interims (trends
only): +60.8 at 202 games, +73.5 at 398. Prediction wrong, too low, the first prediction since S28 below its result: the expected
inflation of a sum of selected steps and the expected loss of parameters tuned at 10+0.1 do not show at 60+1; the gain is about the
sum, if anything larger. So the 10+0.1 screening is valid for the long time control in sum, and the long runs keep their role for
single parameters whose effect depends on the size of the search (the LMR cap: cap 13 -9.1 at 10+0.1, S40, box results pending).
Limits: one batch of openings, the default 16 MB hash on both sides.
Result in `C:\Learn\Data\results\memo\chcm-dev090-tc60`.

**S47.** Stalemate after pruning and the eval clamp, local checks (2026-11-16). Branch `mbnd` 4708209 (on c2378e8, finding 7).
(1) Review finding 4 in the cheap form: the zero window loop counts its pruned moves (`prnno`); when a node ends with no legal move
searched (all pruned or illegal), `hasLegalMove` (`Moves/Core.hs`, pure: make each generated move, test the own king) decides
between the old fail low and `failHardNoValidMove` (stalemate draw, or mated in check). It runs only in such nodes; the quiet
filter at generation keeps one prunable quiet, so a node with prunable quiets always counts as pruned and gets the test.
(2) The eval clamp `matesc = 20000 - 255` (Eval/Core.hs, "also defined in Base.hs") is now `evalMax = 20000 - 256`, exported;
`nearmate` is `i > evalMax || i < -evalMax`, `isMating` `i > evalMax`: the same sets as before, and the static eval the same after
the rounding to the grain of 4 (19745 and 19744 both round to 19744). Decision: a clamped eval is an eval score, not a mate.
Checks (deterministic perf build): perft of the 5 standard positions unchanged; tests.fen depth 10: 12281346 nodes against
12292105 (c2378e8, -0.09%); 206 mate roots depth 10: 208800383 against 210389355 (-0.76%). The changes come from (1) only, by the
rounding argument (not checked by a separate build). Smoke `mbnd` against `chcm` at 10+0.1, 16 games: all ended normally.
Next: the [-5, 0] screen against `chcm` on MEMO (prediction in the register).

**S48.** LMR depth floor at the long time control (2026-11-16/17). `lmfl` da23a27 `epLmrFlr=4` (`lf4`) against `lmfl` (floor off,
same binary) on MEMO, 60+1, a fixed 1000 games (500 rounds), read once at the end, run `lmfl-f4-lmfl-tc60`, 16 MB hash.
Prediction (register, before the result): -10 to +25, most likely +8.
Result: +16.69 +- 15.54, nElo +23.2 +- 21.5, LOS 98.3%, +295 -247 =458 (52.4%), pentanomial [32, 98, 203, 124, 43]. Interim (trend
only): +13.2 at 502 games. Prediction right, in the range and above the most likely value. By the time control decision the floor
wins at 60+1 (LOS >= 95% at the planned number of games); it is kept if its 10+0.1 screen (`lmfl-f4-lmfl-tc10`, SPRT [-5, 0],
running) loses no more than about 10. It keeps level 15 and lifts the confirmations at d1 >= 4 (S45): the gain cap 13 promised at
the long time control without lowering the cap. Limits: one batch of openings, 16 MB hash, the error (+-15.5) still wide.
Result in `C:\Learn\Data\results\memo\lmfl-f4-lmfl-tc60`.

**S49.** Hash size of the matches and the engine's hash option (2026-11-17, main session; lab 49a0f82 added `run_timed -Hash <MB>`).
The engine starts with 4 MB (`newCache 1`, floor 2^18 entries x 16 bytes), not the 16 its UCI option announces: every match so far
played with 4 MB on both sides; "16 MB" in the entries above means 4 MB (the register is corrected). fastchess (trace log, 4 games, `option.Hash=64`)
sends `ucinewgame`, `isready`, `setoption name Hash value 64` before every game and keeps the engine processes. The engine applies
each one and does not free the old table: piped UCI test, 4 games of 2 searches at 300 ms, `setoption Hash 256` before each game,
all 4 applied ("Cache was set on 256 MB"), memory 261, 488, 725, 957 MB. Runs without `-Hash` are not affected (nothing is sent).
`-Hash` is usable only when both engines free the old table (register "Hash option of the engine").

**S50.** Fix of the hash option, merged (2026-11-17). Branch `hfix` 7bb0aee, merged into `rel090`: `setoption Hash` frees the old
table (kept when the number of entries is unchanged, `resizeCache`), new tables are zeroed (`callocArray`), the UCI option announces
default 4, min 4 (the start size, unchanged). Checks on `hfix` (register block "Hash option of the engine"): 12086912 nodes on
tests.fen depth 10 as on `rel090`, perft unchanged; piped UCI test with 256 MB before each of 6 games: 273 to 289 MB, flat (the
`sfen` build grew to 1267 MB); 64/256 alternating follows the size; fastchess smoke with `option.Hash=64`, 20 games: each engine
flat at 95 to 101 MB. `-Hash` is usable for builds from `rel090` on; branches made before (e.g. `lmfl`) need the fix merged in.

**S51.** Speed of the move generation, merged (2026-11-17). Branch `spdg` (a0a4575, 5c8afe4, 685161c), merged into `rel090`:
moves built in one step from the bitboards (no lists of pairs), a quick test in `genMovePromo`, insertion sort of the capture
keys in `genMoveCaptWL`. Engine unchanged: 12086912 nodes on tests.fen depth 10, 14 endgame positions 3445174, perft. Allocation
-6.2% per node; nodes/s +5.5% on mini (median of 7 alternating pairs, +4.4% to +7.9%), +4.5% on MEMO (7 pairs, +2.9% to +6.0%).
Details in `PROFILING.md` "Move generation without lists of pairs"; bench `scripts/speed_bench.ps1`. No games needed (same trees).

**S52.** LMR depth floor at the short time control (2026-11-15/17). `lmfl` da23a27 `epLmrFlr=4` (`lf4`) against `lmfl` (floor off,
same binary) on MEMO, 10+0.1, 4 MB hash (the header says 16MB: old `run_timed` text, S49), run `lmfl-f4-lmfl-tc10`. Planned as
SPRT [-5, 0], started without bounds, so it ran [0, 5] and ended undecided at the 20000 game cap.
Prediction (register, before the result): -5 to +4, most likely -1.
Result: +1.22 +- 3.51, nElo +1.67 +- 4.82, LOS 75.2%, +5772 -5702 =8526 (50.2%), pentanomial [701, 2407, 3770, 2365, 757],
LLR -2.01 on [0, 5]. Interim (trend only): +4.55 at 11600 games. Prediction right. The floor costs nothing at 10+0.1 (95% interval
-2.3 to +4.7; a [-5, 0] SPRT would have passed): with +16.7 at 60+1 (S48) it qualifies by the time control decision, unlike cap 13
(-9.1 at 10+0.1, S40). Which form is adopted waits for the box runs (floor on cap 13, cap 12 against cap 13).
Result in `C:\Learn\Data\results\memo\lmfl-f4-lmfl-tc10`.

**S53.** LMR depth floor adopted (2026-11-17, user's go). `lmfl` (da23a27) merged into `rel090`: the LMR parameters (`epLmrIni`,
`epLmrLim`, `epLmrPV`, `epLmrGrw`, `epLmrPen`, `epLmrNoD`, `epLmrMax` up to 19), the level trace, the census build flag, the floor
`epLmrFlr`, review findings 5 and 8. Conflicts only in the identity and in `Search/Core.hs` (`lmrFixLevel` came twice). Merge with the
floor off: 12086912 nodes on tests.fen depth 10, perft right. Then `epLmrFlr = 4` compiled in (S48 +16.7 at 60+1, S52 +1.2 at
10+0.1): new perf reference 11367370 nodes (-6.0%, as in S45), 12086912 with `-p epLmrFlr=0`. Cap stays 15; the box runs (floor on
cap 13 against cap 13, cap 12 against cap 13, `lmfl` with explicit parameters) can still change it. Runs against the old base use
`epLmrFlr=0` on the new build or an older binary.

**S54.** Mate bound fixes at the short time control (2026-11-17/18). `mbnd` 4708209 against `chcm` on MEMO, 10+0.1, SPRT
[-5, 0], 4 MB, `open-moves-uniq-fm1.fen`. `mbnd` = `rel090` 7afc8b6 (search of `chcm`; the only other code is `go movetime`, not
used by fastchess) + three commits: no null move and no QS delta cut at a mate bound (b7fb86e, S43), mated root moves no longer in
front (c2378e8, finding 7, nodes unchanged), stalemate test after pruning and the clamp as `evalMax` (4708209, S47).
Prediction (register, before the result): -4 to +3, most likely -1; H1 much more likely.
Result: -5.47 +- 4.40, nElo -7.48 +- 6.01, LOS 0.74%, +3572 -3774 =5476 (49.2%), pentanomial [489, 1588, 2449, 1406, 479],
LLR -2.95: H0 accepted after 12822 games. 1 time loss of `mbnd`, no crash. Interim (trend only): -7.41 at 3000 games.
Prediction wrong: the loss is larger than predicted (95% interval -9.9 to -1.1). The node cost (+1.7% on tests.fen, S43) explains
at most 1 to 2 Elo; the rest must come from a change of the search, and the mate guards are the one that changes it outside mate
windows (S43: geomean +1.7%, single positions up to 7 and 27 times the nodes, with no mate score), while the stalemate test changes
tests.fen by -0.09% (S47). Not separated by this run. Rejected in this form; the fixes are not merged.
Result in `C:\Learn\Data\results\memo\mbnd-chcm-tc10b`.

**S55.** Depth of `mbnd` in its games (2026-11-18). The PGN of `mbnd-chcm-tc10b` (S54, 12822 games, 921k moves per engine):
depth and time per move from the move comments, grouped by the engine's own score and by ply. Question: does `mbnd` search less
deep in real games, as the +11% total nodes on roots-500 (S43, geomean only +1.7%) suggest? Both use the same time per move
(0.226 s on average, also in every group), so a lower depth means more nodes per iteration.
| Group | moves per engine | depth `chcm` | depth `mbnd` | difference (+- 2 sigma, moves taken as independent) |
|---|---|---|---|---|
| all | 921k | 10.281 | 10.194 | 0.087 +- 0.009 |
| score < 1 | 483k | 10.950 | 10.901 | 0.049 +- 0.012 |
| score 1-3 | 171k | 10.614 | 10.563 | 0.051 +- 0.014 |
| score 3-10 | 95k | 10.735 | 10.668 | 0.067 +- 0.020 |
| score >= 10 | 32k | 10.036 | 9.970 | 0.066 +- 0.036 |
| score at the clamp (197.44) | 18k | 10.194 | 10.247 | -0.053 +- 0.054 |
| mate score | 122k | 6.908 | 6.542 | 0.366 +- 0.019 |
| ply 60-119 | 307k | 10.799 | 10.677 | 0.122 +- 0.014 |
Readings: `mbnd` is shallower in every group but the clamp, also in balanced positions (0.05 ply below a score of 1: the mate
windows come from single mated moves, not from decided games), and most where a mate is on the board (0.37 ply, 13% of the moves:
the games are played out). 0.09 ply at depth 10 is roughly 5 to 6% nodes and of the order of the -5.5 Elo: the loss of S54 is
explained by the search cost, without assuming that a found mate does harm. The stalemate test lowered the nodes (S47) and finding
7 left them unchanged, so the cost is the mate guards' (b7fb86e), by elimination, not by a separate build. The +- ignores the
correlation of the moves within a game; the differences are many times larger. Data: `C:\Learn\Data\results\memo\mbnd-chcm-tc10b`
(`games.pgn`, `depths.py`, `depths.txt`).

**S56.** `rel090` against `chcm` at the long time control (2026-11-17/18). `r90a` 771f656 (frozen `rel090` cf00634: `chcm` + LMR
depth floor f = 4 + the `spdg` speed) against `chcm` on MEMO, 60+1, 500 games fixed (250 rounds, openings from 1001, disjoint from
S48), 4 MB, read once at the end.
Prediction (register, before the result): -10 to +35, most likely +12.
Result: +11.82 +- 22.76, nElo +15.85 +- 30.45, LOS 84.6%, +143 -126 =231 (51.7%), pentanomial [17, 58, 88, 65, 22]. Prediction
right. Pooled with S48 (`lf4` against `lmfl`, the floor alone, 1000 games, pentanomial [32, 98, 203, 124, 43]): 1500 games,
+15.1 +- 12.8, LOS 98.9%; an upper estimate, because S48 is the batch that led to the adoption, and a lower one for the speed
(not in S48). Reading: the current base beats `chcm` at 60+1 by about +12 to +15; alone not significant. The header's
"4MB - 16MB" is the Hash option default as each engine reports it (`r90a` has the hash fix); both play with 4 MB.
Result in `C:\Learn\Data\results\memo\r90a-chcm-tc60`.

**S57.** Eval package `pegk` adopted (2026-11-18). `pegk` 3cdb421 (`r90a` + the `pawnEndGame` fix bf8d52b, the KPK bitbase
e04a604, the lone king endings below the eval clamp with KBNK to the bishop's corner 3cdb421; checks in the register: Syzygy,
play-outs, symmetry, node counts) against `r90a` on MEMO, 60+1, 100 games (openings from 3001), 4 MB, a sanity run only (user: the
gain is too small for a match of normal size). Result: +3.47 +- 45.94, +27 -26 =47, pentanomial [4, 9, 20, 16, 1], no time
losses, no crashes: clean. Merged into `rel090` (305043d), identity reset (550e38a); the merged code equals `pegk` apart from the
identity, so the perf reference stays 11367370 on tests.fen depth 10 (eval changes act only in the endings). Result in
`C:\Learn\Data\results\memo\pegk-r90a-tc60s`.

**S58.** `mbsp` sanity run (2026-11-18). `mbsp` 01a4667 (`r90a` + review findings 7 and 4 with the clamp `evalMax`, no mate
guards; register block "Null move and QS delta cut at a mate bound") against `r90a` on MEMO, 60+1, 100 games (openings from
3101), 4 MB, a sanity run only (user).
Prediction (register, before any game): -1 to +1, most likely 0.
Result: -20.87 +- 44.15, LOS 17.5%, +21 -27 =52, pentanomial [3, 13, 23, 9, 2]; no time losses, no crashes, all terminations
normal (34 repetitions, 48 mates, 13 insufficient material, 5 fifty moves). A true 0 scores this low or lower about once in 6.
Checks of the games (the user found the number not good): depth per move at the same time 14.094 against 14.080 (+0.014 +- 0.13:
no slowdown; 100 games resolve only about 0.13 ply); no move with a 0.00 of `mbsp` next to a score of 1.5 or more of `r90a`
(the sign a wrong stalemate draw would leave); the 27 losses turn gradually, both engines' scores agreeing (no sudden
misjudgement). Reading: noise, as the node counts (36 nodes on tests.fen) leave no room for an effect of this size; nothing odd.
Data: `C:\Learn\Data\results\memo\mbsp-r90a-tc60s` (`games.pgn`, `depths.txt`, `odd.txt`).

**S59.** TT replacement fix at 10+0.1 (2026-11-18). `ttrp` c64ad37 `epTTRep=2` (review rule: replace the smallest
(depth - 4 * age, type, nodes)) against `r90a` 771f656 (the old rule: smallest move code), MEMO, 10+0.1, 2000 games fixed (1000
rounds), 4 MB (engine default, no `-Hash`), read once at the end.
Prediction (register, before any result): -3 to +12, most likely +5.
Result: +11.99 +- 10.99, nElo +16.64 +- 15.23, LOS 98.4%, +604 -535 =861 (51.7%), pentanomial [63, 217, 389, 250, 81].
Prediction: direction right, the number at the upper edge of the range. Interim of the 60+1 run at the same time (500 of 1000
games, trend only): +128.6 +- 22.3, pentanomial [4, 13, 86, 96, 51].
Reading: the fix gains at both time controls, about ten times more at 60+1. That is the fill of the table: writes per move
(every node outside QS stores, an estimated 30 to 50% of the nodes; MEMO about 450k nodes per second, 3.2 times the boxes, S40)
against 262k entries in 4 MB are about 0.2 to 0.3 at 10+0.1 and 1.3 to 2 at 60+1, where the old rule throws out current deep
entries within the same move (register block "TT replacement": 1.65 ply deeper at 60+1). Under the time control rule
the 10+0.1 result only had to show no loss beyond about 10; it shows a gain. Estimate (not measured) for GUI games at 5m+1s:
about 1.1 to 1.9M writes per move against 16.8M entries with 256 MB (0.07 to 0.11) or 33.5M with 512 MB: the old rule probably cost
little there, comparable to or less than 10+0.1 at 4 MB; filling 256 MB in one move needs 75 to 120 s per move. So the bug
distorted mainly our own long time control matches at 4 MB, and GUI games only where no Hash was set (engine default 4 MB).
Result in `C:\Learn\Data\results\memo\ttrp-f2-r90a-tc10`; the interim in `...\ttrp-f2-r90a-tc60\interim-500.txt`.

**S60.** Aspiration windows at the root by node counts, and the PV node type after a fail low (2026-11-18, own session).
Branch `aspw` from `ttrp` c64ad37: 4fa3916 identity, 0e52350 aspiration as parameters (`epAspW` half window, 0 = off;
`epAspGrow` widening factor of the failing side, up to 2 more tries; `epAspMinD` first draft, 4; no aspiration at mate or
clamp scores; diagnostic counters), b54d34d `epPvsNt` (below); all compiled off, off identical to `ttrp` (11367370 nodes with
epTTRep=0, 11671539 with 2, tests.fen depth 10), perft unchanged. Report `Search/ASPIRATION.md` (copied to `rel090`; line
links there point into `aspw`). Found on the way: the window in use was 24 (`Uci/UciGlue.hs`), not 36 (`Search/Thread.hs`
is not in the cabal file, dead code).
Defect: a PV node moved its later moves to zero window only after a move raised alpha; when its first move failed low, all
moves were searched as PV nodes (full window, no LMR, no null move), recursively. With the full window it fires in PV
re-searches after a zero window fail high whose first move then fails low (search instability), with a narrow window in
every PV node of the first line below the window (tests.fen position 47, window 25: 3.2x the nodes without a root fail).
`epPvsNt=1` moves the node type on after the first move, as in PVS. Deterministic perf build, epTTRep=2, depth 10:
tests.fen 11671539 -> 10251160 (-12%), roots-500 83.2M -> 69.7M (-16%). Not a wrong score: the effort goes into the
unstable nodes; fewer nodes are not Elo, games decide.
Aspiration with epPvsNt=1 (roots-500, depth 10, chunks of 25 positions, geomean of the per-position ratios against off,
bootstrap 95%): window 15 +18.7% [16.1, 21.5], 25 +7.3% [5.1, 9.6], 36 +0.1 / -0.2 (growth 0 / 4), 50 +1.4 [-0.3, 3.3],
75 +0.4 / +0.5; tests.fen 50/4 -2.4% [-5.5, 0.5]; first 100 roots at depth 12: 36/4 +0.9, 50/4 -2.0 [-4.8, 0.9], 75/4 -1.1.
Without the fix every window cost (window 36: +67% total). Growth 4 against 0: no clear difference. The positions that fail
are those with large score swings (median swing 208 against 76); per draft 10-15% of the positions fail at depth 10, 4-10% at
depth 12. The per-position log ratio has an sd of about 0.2 even without a fail, so +-2% is no effect.
Reading: aspiration saves no nodes in any form here; `epPvsNt=1` is the find, a candidate for games on its own (against the
parent build, screened at 10+0.1, decided at 60+1). Raw data: `C:\Learn\Data\measurements\search\aspw`.

**S61.** `mbsp` trend games (2026-11-18). `mbsp` 01a4667 against `r90a` 771f656 on MEMO, 60+1, 200 games fixed (100 rounds,
openings from 3151, disjoint from S58), 4 MB (both the old TT rule).
Result: -8.69 +- 32.55, nElo -12.89 +- 48.15, LOS 30.0%, +45 -50 =105 (48.75%), pentanomial [4, 29, 42, 18, 7]; `r90a` lost one
game on time (in favour of `mbsp`), no crashes. Pooled with S58 (pentanomial [3, 13, 23, 9, 2]): 300 games, [7, 42, 65, 27, 9],
-12.8 +- 26.2, LOS 16.9%.
Reading: the prediction (-1 to +1, most likely 0) lies inside the interval; 300 games cannot tell 0 from -13 or from +13. The
node counts leave room for no effect of this size (36 nodes on tests.fen, one mate root changed), so the trend is read as noise,
as S58; nothing in the games was odd (S58). Result in `C:\Learn\Data\results\memo\mbsp-r90a-tc60b`.
Depths (2026-11-18, the user asked whether `mbsp` searches the same tree slower): `depths.py` on the 200 games and on all 300
(with S58), at the same time per move (1.721 s against 1.719 s): 300 games 13.959 against 13.975, -0.016 +- 0.073 ply (200 games:
-0.031 +- 0.089); below a score of 1 -0.085 +- 0.089, with a mate score +0.52 +- 0.29 (`mbsp` deeper there: finding 7 no longer
searches a mated root move in front). A 5 to 10% slower search would show as about 0.1 to 0.15 ply less; the interval reaches
-0.09, so a small slowdown is not excluded, a large one is. Oddities (`odd.py`, 200 games): no 0.00 of `mbsp` next to a clear
score of `r90a`, 50 losses all with a gradual turn, one time forfeit (`r90a`, 491 ms over). Data: `depths.txt`, `depths300.txt`,
`odd.txt` in the result folder.

**S62.** PV node type after a fail low at 10+0.1 (2026-11-18/19). `pvnt` 8c61267 (`epPvsNt = 1` compiled in) against `ttrp`
c64ad37, both `-p epTTRep=2` (the fixed TT rule; the only difference is the fix), MEMO, 10+0.1, 2000 games fixed (1000 rounds),
4 MB, read once at the end.
Prediction (register, before any game): -5 to +15, most likely +6.
Result: +2.78 +- 11.12, nElo +3.81 +- 15.23, LOS 68.8%, +566 -550 =884 (50.4%), pentanomial [76, 227, 372, 255, 70].
Prediction right (inside the range, below the most likely value, as most predictions so far).
Reading: about 12% fewer nodes at the same depth do not show as the 8 to 14 Elo of a speedup; the saved search in the unstable
nodes seems to have been worth part of its cost. No loss beyond about 10, so by the time control rule the 60+1 run decides:
`pvnt-t2-ttrp-tc60` started on MEMO (1000 games, openings from 4001). Result in `C:\Learn\Data\results\memo\pvnt-t2-ttrp-tc10`.

**S63.** TT replacement fix at 60+1 (2026-11-18/19). `ttrp` c64ad37 `epTTRep=2` (replace the smallest (depth - 4 * age, type,
nodes)) against `r90a` 771f656 (old rule: smallest move code), MEMO, 60+1, 1000 games fixed (500 rounds, openings from 2001),
4 MB (engine default), read once at the end.
Prediction (register, before any result): -5 to +25, most likely +10.
Result: +121.06 +- 15.63, nElo +180.77 +- 21.53, LOS 100%, +442 -107 =451 (66.75%), pentanomial [7, 37, 164, 198, 94].
Prediction wrong by an order of magnitude: the old rule was taken as close to random replacement, its cost at a full table was
not estimated. Mechanism (partial PGN, register block "TT replacement"): 1.65 ply deeper at the same time, growing with the game.
Together with S59 (+12.0 at 10+0.1): the gain grows with the fill of the 4 MB table (writes per move about 0.2 to 0.3 of the
entries at 10+0.1, 1.3 to 2 at 60+1). Adoption by the time control rule (LOS >= 95% at 60+1, no loss at 10+0.1). Consequence:
every earlier 60+1 match (and less so the 10+0.1 ones) was played with a crippled table; review of those matches as planned
(register summary), first the LMR floor replay `ttrp-f4-f0-tc60` (started 2026-11-19). Result in
`C:\Learn\Data\results\memo\ttrp-f2-r90a-tc60`.

**S64.** TT replacement merged (2026-11-19). `ttrp` merged into `rel090` (dc3fb20; the only conflict was the identity, reset to
`rel090`), `epTTRep = 2` compiled in. Checks, deterministic perf build of the merge: tests.fen depth 10 11671539 nodes (new
reference; with `epTTRep=0` 11367370, the previous `rel090`, so the merge brings nothing but the rule), perft of the 5 standard
positions right. The other 4 MB results of `rel090` are now measured with the old table; their review is the plan in the register
summary (first the floor replay `ttrp-f4-f0-tc60`, then cap 13 against 15). Matches against `rel090` from now on need a new
frozen build (the `r90a` builds have the old rule).

**S65.** Floor on cap 13 at the long time control, old TT (2026-11-17/19). `l13f` = `lmfl` da23a27 `epLmrMax=13,epLmrFlr=4`
against `lmfl` `epLmrMax=13` (same binary), m3a, 192+3.2 (= 60+1 on MEMO), 4 MB (engine default), concurrency 8, 2000 games
fixed, read at the end (interim at 1800 seen as a trend only).
Prediction (register, before the result): -12 to +18, most likely +3.
Result: +5.21 +- 10.28, nElo +7.72 +- 15.23, LOS 84.0%, +497 -467 =1036 (50.75%), pentanomial [43, 251, 400, 245, 61], 53.9 hours.
Prediction right (in the range, 2 above the most likely value). Not shown (LOS < 95%): the floor adds little or nothing to
cap 13, as expected (both act on the same deep reductions). Played with the old TT rule (4 MB full at this time control, S63):
its reading is limited to the old engine. Result in `C:\Learn\Data\results\m3a\lmfl-m13f4-m13`.
