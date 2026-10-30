# Search tuning - state and findings (0.9.0 development)

The record of the search experiments since 0.8.0: move ordering, pruning, and how to measure them. The HCE
tuning has its own record (`TUNING.md`, `TUNING-HISTORY.md`, cited there as plain numbers); the findings here
are cited as S1, S2, ... This file lives on the development base branch (`dev090`) only, as commits that
touch nothing else: the experiment branches carry code, so a merged or dropped branch never takes its
findings with it. Every finding names the branch and commit it came from.

Data of the measurements: `C:\Learn\Data\measurements\search` (see `C:\Learn\Data\DATA.md`); tools:
BarbarossaTuning (`sample_roots.py`, `node_scan.ps1`, `node_scan_report.py`, `rfp_shadow_report.py`).
Elo values are from the candidate's side, with 95% error bars.

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
| `lmfx` | nmlm | LMR level fixed by parameter `epLmrFix` (S13) | experiment, compiled = nmlm |

Only `hpqt` is pushed; the other branches are local, and the commits cited here exist only while the branches
are kept.

Open:
- LMR near-mate guard (S13): removal accepted by SPRT; mate finding not measured.
- LMR: no node minimum for a fixed level up to 15 (S13); fixed levels against the adaptive one need games.
- RFP: no cut with an opponent passer on rank 6/7 (S13): game test.
- Merge decisions for dev090 (hprf); the UCI engine ignores `go movetime` and `go nodes` (to implement in 0.9.0).
- HCE: threat features (S14).

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
| S13 | Node scan and shadow on real-game roots | shadow: endgame wrong cuts come from opponent passers on 6/7 (10x), own hanging 2.7x; node totals are ruled by the LMR near-mate guard, without it the 20 worst roots cost 344x less; without it no node minimum down to M10; removal SPRT +13.0 | guard removal accepted; passer filter open |
| S14 | HCE: the en-prise term and what RFP shows about it | | idea |

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
Two effects: at PV nodes the guard tests beta, near mate in every full-window search (after an aspiration
fail) without any mate; with `nearmate a` there 48 of the 50 roots have exactly the nodes of no guard (geomean -8%
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
