# LMR cap review: why cap 19 searches deeper and loses (2026-11-15)

Basis: `Search/Core.hs` on `lmrm` e908bd8; S36 to S40 in `SEARCH-TUNING.md` and the register entries "Retune of the adaptive LMR after
ordering changes" and "LMR cap depending on the size of the search" in `SEARCH-IDEAS.md` (`rel090`). Everything below is read from the code
and the recorded results; nothing was run except the reduction table. Statements about the cause are inferences, marked as such.

## Short answer

- Levels 16 to 19 are not "more of level 15". Up to 15 a level step widens the set of reduced moves; above 15 nothing is left to widen, so
  the table adds k plies to every reduced move. The smallest reduction jumps from 1 ply (level 15) to 5 plies (level 19), with a floor of
  depth 1.
- Most plausible mechanism (inference): a single reduction that is too large for the remaining depth. At level 19 every node with 6 plies
  or less to go searches the TT move, the captures, one quiet move and the passed pawn moves; every other move gets depth 1. Two such
  moves on a path end any line, so it also piles up, but the first one already does the damage.
- The scheme cannot notice: a reduced search must fail high to trigger a re-search, and a depth 1 search rarely does. The cost of
  re-searches is ruled out as the cause by S38 (fewer nodes, more depth). Null move, futility and RFP are amplifiers, testable by config.
- Encoding: this mechanism is local to the node, so the fitting variable is the remaining depth at the node (a bound on the reduction),
  not draft, node budget or time. Of the three, the draft is the nearest proxy; node budget and time fit only if the best level really
  depends on the size of the search, for which there is no evidence yet.
- First measurement: the census in `zeroWithLMR` (M3, search unchanged), then fixed levels by draft against a deep reference (M1) as
  the test of a repaired table before games. Both need a small code addition (section "Three points").

## What the code does

Reduction by level and by w = moves since the last capture in the node (`movno - spcno`; the first move has w = 0), `lmrTable 120`
([Core.hs:64](Core.hs:64)), computed with the formula of the source:

| Level | w=1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 13 | 17 | 21 | 26 |
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
| 8 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 1 | 1 | 1 | 1 | 2 | 2 |
| 11 | 0 | 0 | 0 | 0 | 0 | 1 | 1 | 1 | 1 | 1 | 2 | 2 | 3 | 3 |
| 13 | 0 | 0 | 0 | 1 | 1 | 1 | 2 | 2 | 2 | 2 | 3 | 4 | 4 | 5 |
| 14 | 0 | 0 | 1 | 1 | 2 | 2 | 2 | 3 | 3 | 3 | 4 | 5 | 6 | 7 |
| 15 | 0 | 1 | 2 | 3 | 3 | 4 | 4 | 5 | 5 | 6 | 7 | 8 | 9 | 10 |
| 17 | 0 | 3 | 4 | 5 | 5 | 6 | 6 | 7 | 7 | 8 | 9 | 10 | 11 | 12 |
| 19 | 0 | 5 | 6 | 7 | 7 | 8 | 8 | 9 | 9 | 10 | 11 | 12 | 13 | 14 |

- Two different dimensions. From 8 to 15 a step up mostly lowers the number of unreduced quiet moves after the last capture (level 8: 8,
  13: 3, 14: 2, 15: 1). At 15 this is exhausted, so 16 to 19 shift the whole row (`red l i = r + l - lmrLevTop`,
  [Core.hs:69](Core.hs:69)). Cap 13 against 15 and cap 15 against 19 therefore test different things; one does not extrapolate to
  the other.
- The reduced depth is `max 1 (d1 - r)` ([Core.hs:910](Core.hs:910)); no relation to the remaining depth d1. Examples for
  w = 2, 3, 4: at d1 = 6 level 15 searches to 5, 4, 3, level 19 to 1, 1, 1; at d1 = 10 level 15 to 9, 8, 7, level 19 to 5, 4, 3.
- Not reduced: the first move, captures and promotions, moves of a passed pawn (`noLMR = movePassed`), children at d1 <= `epLmrNoD`,
  root moves, and in a PV node the moves before the first one that raises alpha. Checks are extended by 1 but reduced like the rest.
- The level is one number for the whole tree (`lmrlv` in `PVState`), not per node or per ply: once the ramp is up, every ply of every
  path reduces by the same row. Each iteration (and each aspiration re-search) restarts at `epLmrIni`.
- Feedback ([Core.hs:913](Core.hs:913)): +1 per node searched without a failed reduction; a reduced search that fails high costs
  2^d' (d' the reduced depth), a confirmed one another 2^d1. At the cap the node limit is multiplied by 4 each time it is reached
  (8500, 34000, 136000, ...), so the credit that a failure must exceed grows.
- Re-search ([Core.hs:800](Core.hs:800)): only when the reduced zero window search returns >= beta; then zero window at d1 with the node
  types swapped, in a PV parent then the full window. All of it runs at the current level.
- The children of a reduced move are ordinary zero window nodes at d': RFP applies at d <= 6 (`epRFutD`, margin 50 per ply), futility
  at d <= 6, the null move from d >= 2 in nodes that are not All nodes, with a null move search depth of 0 (QS) up to d = 4
  (`nmDArr1`).
- The TT stores every node with its nominal depth d, whatever the level was ([Core.hs:659](Core.hs:659)).

## Why deeper and weaker

Deeper: the iteration draft counts the plies of the first moves. Level 19 removes 4 more plies from every late subtree, the iteration
costs fewer nodes (S38: -2.4% at depth 10, where the ramp is high only briefly; much more in large iterations), so more drafts complete
in the same time (+0.41 ply at 3 s). The draft says how far the lines of first moves go, not what was looked at beside them.

Weaker (inference): at 3 s per move 23.6% of the nodes run at levels 18 and 19 and more at 16 and 17 (S38), and these are the later
root moves of the large iterations, the place where a better move or a refutation of the current one would be found. There the tree is
the first line of every node at full depth and nearly everything else at depth 1 to 3. A result of -74 Elo with +0.41 ply means the
tree is worth roughly a ply or more less than its draft says (the Elo per ply at this depth is not measured here; order of magnitude only).

The mechanisms the question names, and what the code says about each:

1. Size of one reduction against the remaining depth (most plausible). The flat +k hits where most nodes are: near the horizon. With
   d1 <= 6 and level 19 every reduced move goes to depth 1, which is one ply and QS. A quiet move that needs two more quiet plies to
   show its point is not seen. At level 15 the same moves keep 2 to 5 plies. This is in the table itself, independent of the path.
2. Piling up along a path. Real, and stronger than at 15 (two moves with w = 2: 10 plies against 2), but mostly a consequence of 1:
   after the first reduced move little is left for the second to take. It would be the main cause only if the damage came from high in
   the tree (large d1), where one reduction of 5 to 7 plies still leaves a real search. M1 and M3 below separate the two.
3. Re-searches. As a cost: excluded, the node counts fall and the depth rises (S38). As a missing safeguard: likely. A reduced search of
   depth 1 to 3 fails high only on what the QS and the static cuts see, so fewer failures are detected the larger the reduction; a
   detected one costs 2^d', 16 times less at 4 plies less; at the cap the credit limit quadruples. The regulator reads less of the
   error the more it reduces, which is also why it behaves as a ramp (S36: 38 drops in 105M nodes).
4. Null move, RFP, futility. Not a cause of their own, but the reduction moves nodes into their range: a move 8 to 11 plies above the
   horizon lands at d' 2 to 6, where the child can fail high by RFP on the static score with the margin of d' (150 cp at d' = 3), by
   a null move that is a QS (d' <= 4), or have its quiet moves pruned by futility. At level 15 the same child is at d' 7 to 10, out
   of range. The parent's move then "fails low as expected" without a search. Testable by config (M2).
5. Not in the question, but in the code: (a) the PV is searched while the ramp is low, the later root moves at the top level, so the
   search is biased towards the move it already has, and a late root move that does come through is verified by a tree that
   reduces the opponent's defences as well; (b) TT entries written at level 19 carry the full depth and cut in the low-level part of
   the next iteration and of the next move, so the restart at level 8 (worth 25 Elo, S33) is partly undone.

## Which encoding fits

- Remaining depth at the node (not in the register's list): fits mechanisms 1 to 4. Forms: the extra plies of the levels above 15 only
  as far as d1 allows (for example reduced depth >= d1 / 2, or >= `epRFutD` + 1 when d1 is above it), or the level bounded by d1.
  Deterministic, hardware independent, measurable with node counts and the perf build. It scales with the time control by itself:
  longer searches have more nodes with a large d1, and only those get the larger reductions. It also keeps the node ramp, and with it the
  phase effect of S39.
- Draft of the iteration (register form 1): a proxy for the above, coarse, because a draft 16 iteration consists mostly of nodes with
  d1 <= 6. Note the direction: under mechanism 1 a bound that rises with the depth is wanted (large reductions only where there is
  room). A cap falling with the draft fits only mechanism 2 as the main cause. M1 decides the sign.
- Node budget of the move (form 2): fits only 5a, the share of an iteration that runs at the top level. It does not change what a level
  does to a subtree; it only decides how often the search gets there. Needed only if the best cap differs between short and long time
  controls, which nothing shows yet: S37 and S38 measure depth, S40 shows that depth misleads.
- Time per move (form 3): the same as the node budget plus the hardware. No mechanism above points to time itself.

Recommendation: do not encode the time control now. First repair the shape (a bound by remaining depth), then see whether a cap
question is left; if one is, node budget before time.

## Measurements before games

All deterministic or node based, so they can run in parallel on mini without timing noise. Cheapest first.

**M1. Fixed level by draft (needs the root move per position from `SelfPlay -P`; reference: see "Three points").**
Deterministic perf build, `epLmrFix` = 8 (reference), 13, 15, 17, 19 (the fixed level
indexes the same table, so 16 to 19 should work; to check), roots-500, drafts 8, 10, 12, 14. Per level and draft: nodes, and the share
of positions whose root move equals that of the reference at the same draft (and the score difference). Then the draft at which level
15 reaches the agreement that level 19 has at draft D: the "effective depth" of level 19. To check first: whether `SelfPlay -P` prints
the move and score per position. Reading:
- gap between 19 and 15 about constant over the drafts: horizon effect, mechanism 1; a draft encoding would be pointless;
- gap growing with the draft: piling up, mechanism 2; a cap falling with the draft fits;
- gap shrinking with the draft: too much for the depth available; a bound rising with the depth fits.
Limit: agreement on 500 positions has a standard error of about 2 points unpaired; paired differences are tighter, but more roots may
be needed. -74 Elo should be visible; a failure to see it would itself say that this measure is too blunt.

**M2. Ablation of the static cuts (config only).** M1's level 15 and 19 at one draft, with `epRFutD` = 0, then with the futility
margins set out of range, on both sides. If the gap between 19 and 15 shrinks clearly, mechanism 4 carries part of the loss and a floor
above the RFP depth is the form to try. The null move has no parameter (`useNullMove` is a constant); it needs M3.

**M3. Census in zeroWithLMR (small code change, search unchanged).** Counters by level, d1 and d': reduced searches, failed high,
confirmed. Check with identical node totals, like the trace of S36. Gives: the share of reduced searches that end at d' <= 2 and the
share moved from d1 > 6 to d' <= 6, at 15 against 19; and the re-search rate by size of the reduction at equal d1 and w. A rate that
falls with the reduction is mechanism 3 (the check goes blind). With one more field in `NodeState` (reductions so far on the
path) the same table by path history separates 1 from 2. With a tag on how the reduced child returned (RFP, null move, searched),
it measures 4 and the null move directly.

**M4. Sampled miss rate (measurement build, search changed by side effects).** For one in N reduced searches that failed low, search
again at d1 and count the fail highs: the moves the reduction lost, by level, d1, reduction and path history. This measures the
conjecture of S40 itself. It understates (the check runs at the same level below) and alters TT and history, so it is a rate, not a
node count identity.

**M5. Equal time, as S38, with quality.** `fixed_time_scan.py` at 3 s, caps 15, 17, 19: besides the completed depth, the agreement of
the move with a long reference search of a low fixed level. Shows whether the +0.41 ply is visible as a loss without games; timing
sensitive, so not in parallel with other load on mini.

After these, node limited games (node limit = the nodes of a move at 192+3.2) repeat the level profile of the long time control on any
machine and at any concurrency; they are the way to test a repaired shape without the long time control, and they take the time
management out.

## What the running cap runs can and cannot say

- Cap 17: on the same dimension as 19. About half of -74 or less would fit a loss per extra ply; clearly more than half would say the
  first extra plies do most of the damage (the collapse to depth 1 near the horizon happens early).
- Cap 13: the other dimension (3 unreduced quiet moves instead of 1, smaller steps). A gain would say that level 15 is past the optimum
  at long time controls; whether by the same collapse is for the census (M3, "Three points"). Early look: +39 +- 25 after 400 games.
- Neither run separates the encodings. That needs M1.

## Three points (user, 2026-11-15)

**1. `SelfPlay -P` prints totals only** (and scores per depth with `--scores`), not the root move per position
(`Main/SelfPlay.hs:760-830`). So M1 is not "config only": it needs one line per position with draft, root move, score and nodes. With
that its advantage in cost over M3 is gone; both are small additions, for another session.

**2. The reference of M1.** Right: level 8 at the same draft measures closeness to a less reduced tree, not correctness. Better: one
deep search per root at level 8 (the draft of the highest test draft + 4, or deeper), the same reference for every level and draft.
One run, once; its cost follows from the level 8 node counts at draft 14. Limits: it is the engine's own deeper opinion (same eval,
still an LMR search), and roots where the reference move changes between its last drafts are undecided and should be left out.
What it changes in the reading:
- Each level now has a curve "agreement by draft" that rises towards the reference. The gap between 19 and 15 is read horizontally, in
  plies (the draft at which 15 reaches the agreement of 19), not in points: near the reference draft all curves flatten, and a gap
  shrinking in points can be the ceiling.
- The three patterns keep their meaning in plies: a constant shift is the horizon effect, a growing shift is piling up, a shrinking
  shift says the reduction is too large for small depths only.
- New and more useful: the same curves over nodes instead of drafts compare the levels at equal effort, which is the question of S40
  (does level 19 buy correctness for its nodes or not), without games.
The level 8 runs at the test drafts stay as a second column (what the reduction removes at that draft); they answer the mechanism,
the deep reference answers the strength.

**3. M3 first.** Reasons: it is the direct look at the mechanism (where the reductions land), it keeps the search identical (node
totals as the check, like the trace of S36), it runs under the real ramp as well as at fixed levels, and it answers what the early
look at cap 13 makes the next question: whether level 15 has the same collapse in a milder form. Cap 13 at +39 +- 25 after 400 games
at 192+3.2 is an early look (the run is read at 2000), but it points the same way as cap 19: less reduction at the top of the ramp is
better at this time control. From 13 to 15 the width and the size change together (largest reduction up to w = 10: 2 against 6; at
d1 = 6 a move with w = 10 keeps depth 4 against 1), so the "reduction against remaining depth" reading covers it and the census can
show it. If cap 13 holds, the first fix is the cap itself (the register: one fixed cap if it is neutral at 10+0.1); the bound by
remaining depth comes second. M1 with the deep reference is then the check of a candidate table before games; M3 cannot say whether a
reduction was wrong, only where it went and how often it was caught.

Minimal counters, all in `zeroWithLMR`, not aborted searches only. Buckets: level 8 to 19 (below 8 in one), d1 in 2-3, 4-6, 7-10, 11+;
d' in 1, 2, 3-6, 7+; w in 2, 3-5, 6-10, 11+ (w has to be passed in; the reduction alone does not give it).
- A, by level, d1, d': `red`, the reduced searches.
- B, by level, d1, w: `red`, `fh` (the reduced search returned >= beta, a re-search starts), `conf` (the re-search at d1 returned
  >= beta).
- C, by level, d1: `full`, the zero window searches with d' = d1 (the denominator).
Not in the minimum: the nodes of the reduced searches per cell (`nodre` is already there), the reductions so far on the path, how
the reduced child returned (RFP, null move, searched).

Tables:
1. From A, by level: share of the reduced searches with d' = 1 and with d' <= 2, and the share pulled into the static range
   (d1 >= 7, d' <= 6). Levels 13, 15, 17, 19 side by side: the mild form at 15 against 13 is the question.
2. From A and C, by level and d1: share of the zero window searches that are reduced and the mean reduction.
3. From B, at equal d1 and w, by level: `fh / red` and `conf / red`. The same class of move, searched less deep at the higher level.
   Whether a move is good at depth d1 hardly depends on the level, so `conf / red` falling from 13 to 15 to 19 counts moves that are
   no longer found: a lower bound of the miss rate without M4. `conf / fh` (the quality of the alarm) beside it.

Runs: fixed levels 13, 15, 17, 19 (`epLmrFix`) at one draft on roots-500 in the deterministic build for the clean comparison (under
the ramp the level is tied to the place in the iteration: high levels see the late root moves); then the adaptive caps 13, 15, 19 at
3 s as in S38 for the profile of play. Check first: 12086912 nodes on tests.fen at depth 10 with the counters in.

## Not known

- Nothing here is measured; the ranking of the mechanisms is from the code and from S36 to S40.
- The share of nodes at levels 16 and 17 with cap 19, and the level profile of the S40 games themselves (the 3 s row of S38 is the
  nearest).
- Whether level 15 already has the same problem in a milder form (w = 4, 5 lose 3 plies, w = 10 loses 6). M1 with levels 13 and 15
  shows it.
