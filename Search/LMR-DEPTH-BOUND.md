# A depth bound for the LMR (2026-11-15, design only)

Read only: `Search/Core.hs` on `lmcs` 9b76970, `Search/LMR-CAP-REVIEW.md`, S36 to S44 and the LMR entries of the register (`rel090`),
`TIME-MATE-EXTENSION-REVIEW.md`, the census file `census-fix-d12.txt` (S42). Nothing built or run. "Inference" marks what is
concluded, not measured; the interim match values (cap 13 +37, 17 -24, 19 about -74) are trends, not results.

## 1. The form

**Formula.** A floor of the reduced depth that grows with the remaining depth d1, in eighths:

    d' = max dmin (d1 - r(level, w)),   dmin = max 1 (d1 * f / 8)   (integer division)

The same as r <= d1 - dmin. New parameter `epLmrFlr` = f, 0 = off (`max 1` as now, so the node counts stay 12086912). Start: f = 4.

| d1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 10 | 12 | 16 |
|---|---|---|---|---|---|---|---|---|---|---|
| floor, f = 3 (about the d1 / 3 of the plan) | 1 | 1 | 1 | 1 | 2 | 2 | 3 | 3 | 4 | 6 |
| floor, f = 4 | 1 | 1 | 2 | 2 | 3 | 3 | 4 | 5 | 6 | 8 |
| floor, f = 5 | 1 | 1 | 2 | 3 | 3 | 4 | 5 | 6 | 7 | 10 |
| largest reduction, f = 4 | 1 | 2 | 2 | 3 | 3 | 4 | 4 | 5 | 6 | 8 |

**Where.** Only in `reduceLmr` ([Core.hs:956](Core.hs:956)): `max 1` becomes `max dmin`, with one more field in `PVStRO`. Both
callers get it (`pvInnerLoopExten`, [Core.hs:783](Core.hs:783), and `pvInnerLoopExtenZ`, [Core.hs:799](Core.hs:799));
`zeroWithLMR` and the census need no change. For f < 8 the floor is below d1, so a reduced move stays reduced and the branch
`d' == d1` is reached as before.

**With the table and the cap.** Nothing else changes: the level still says which moves are reduced (the width) and how much the
table asks; the bound cuts what the table asks where there is no room. The penalty 2^d' of a failed reduction rises a little
(d' 2 to 3 instead of 1), which is nothing against a credit of 8500. With f = 4 the levels above 15 differ from 15 only for
w = 2, 3 and at large d1: near the horizon everything is at the floor already at 15.

**PV and zero window nodes alike.** Yes, one bound. Reasons: the census does not split them; reduced searches from PV parents are
few; and the only PV-specific test (one ply less in PV nodes, S34) lost 6.7 at 10+0.1. A separate PV constant only if the common
one passes.

**Constants from the census** (level 15, draft 12, 16.9M reduced searches):
- By d1: 57% of the reduced searches have d1 2-3, 35% d1 4-6, 8% d1 7-10 (sums of table 3). At d1 2-3 the mean reduction is 1.4 at
  every level: d' = 1 there is the geometry of the horizon, not a collapse, and no floor of this family touches it. So of the "90%
  at d' = 1" more than half is not the problem (correction of the reading in the cap review).
- The collapse is in d1 4-6: mean reduction 3.5 at level 15 against 2.8 at 13, so nearly all of that bucket ends at d' = 1 at 15
  (inference from the means; the census has no table of d' by d1). In d1 7-10: 5.5 against 3.4.
- Where the alarm suffers, 15 against 13 at equal d1 and w: `conf / fh` 39% against 68% (d1 4-6, w 6-10), 37% against 75% (d1 7-10,
  w 6-10), 10% against 31% (d1 7-10, w 11+); `conf / red` 0.4% against 0.6% and 0.3% against 0.4%. For w 3-5 the two levels find the
  same (1.3%, 0.9%). So what 13 finds and 15 does not is in the middle and late moves at d1 >= 4, the tail that the floor lifts,
  not in the early moves that only the width of 13 spares. Weak: the trees and the cell populations differ between the levels.
- f = 4 puts the largest reduction at 2 to 3 for d1 4-6 and 4 to 5 for d1 7-10: about the mean reduction of level 13 (2.8, 3.4)
  with the width of level 15. f = 3 hardly binds near the horizon (largest reduction 3 at d1 = 4, where 15 has a mean of 3.5): it
  acts from d1 = 6 on, on about a tenth of the reduced searches. f = 5 goes below level 13 in the middle of the tree.
- The interims fall by 12 to 25 Elo per level from 13 to 19, also between 17 and 19, where the share at d' = 1 is saturated (96.6%
  to 99.2%). What still grows there is the reduction at large d1 (11+: 5.1, 6.8, 8.4 at 15, 17, 19) and for w = 2, 3. So the floor
  has to bind in the middle of the tree as well, which a proportional one does and a constant one (d' >= 2) does not (inference).

**What the census predicts for f = 4 at level 15** (composition of the tree taken as unchanged, so rough):
- Share at d' = 1: from 90% to about 55 to 60% (only d1 2-3 are left); d' <= 2 from 94% to about 85 to 90%. With f = 3: 90% to
  about 80 to 85%.
- "Pulled into the static range" (7.4%) does not move: d' stays <= 6 up to d1 = 13. The measure to read instead is the mean d' for
  d1 7-10 (now about 2, then 3 to 5): RFP margins of 150 to 250 instead of 50 to 100.
- Confirmations: `conf / fh` for d1 >= 4, w >= 6 up towards the values of level 13 (from 26 to 39% towards 31 to 75%); `conf / red`
  in d1 4-6, w 6-10 from 0.4% towards 0.6%. Small counts (about 2000 more confirmed moves in 1M searches of that cell); the census
  still cannot count the misses.
- Nodes: not predictable from the census (it has no nodes per cell). Guess: +30 to +100% at a fixed draft for f = 4 (level 13
  costs +80% and spares more), +10 to +25% for f = 3; under the adaptive ramp less, the bound binds little at levels 8 to 12
  (level 11 at d1 = 4 only from w = 19). At equal time this is depth lost, as with cap 13 (-0.76 ply at 1.5 s, S37).

## 2. Relation to the cap

- Two different repairs. Cap 13 leaves three quiet moves unreduced and reduces the next ones gently, but still sends the tail to
  depth 1 (78% at d' = 1). The floor keeps the width of 15 (one unreduced quiet move) and gives the tail a depth. The census
  points to the tail (section 1), so the floor may get what cap 13 gets (inference, weak).
- It keeps level 15 and more for short searches: at 10+0.1 the cap binds in 3% of the nodes and the ramp is at its optimum there
  (S33, S34; Lim 6000 +1.9 says a faster ramp does not hurt). The floor leaves the ramp alone and acts mostly at the levels that
  long searches reach. Cap 13 has the same property only as long as 13 is harmless in short games (the [-5, 0] run).
- Could it replace a cap that depends on the time control: plausibly. With the floor the reduction is the same share of the
  depth in every search, and the levels above 15 can no longer collapse a subtree, so the cap loses most of its effect and with it
  the reason to make it depend on the size of the search. Not if the gain of cap 13 comes from the width.
- What separates them (long time control, 16 MB hash as the other points):
  1. Floor at cap 15 against cap 15, beside cap 13 against cap 15: does the floor reach the gain of cap 13?
  2. Floor at cap 13 against cap 13: a gain on top means two effects (width and tail), none means one, and the floor is the form
     to keep if it is the one that is harmless at 10+0.1.
  3. Cap 19 with the floor against cap 15: without the floor about -74. If the floor brings this near 0, the cap is no longer the
     sensitive parameter and no encoding is needed. The expected effect is large, so a few hundred games decide.
  Local, before games: the census of floor at 15 against plain 13 in the cells above, and M1 of the cap review (agreement with a
  deep reference at equal nodes) once `SelfPlay -P` prints the root move.

## 3. The check extension (S44, form D)

- The floor does not end check chains: a chain of mutual checks keeps depth 3 as well as depth 1. It changes where they start:
  fewer lines collapse to depth 1, each reduced line is deeper. The sign of the effect on the explosions is open; at level 19 the
  share of the tree left to checking lines should fall (inference).
- Form D (extend only while `ply + d >= draft - k`) and the floor move the same quantity, how far a line is below the draft. The k
  of the review (2 or 3) was set from the mean reductions of S42 (1.4 at d1 2-3, 3.5 to 3.8 at d1 4-6); the floor lowers the
  second to 2 to 3, so with the same k more reduced lines would keep the extension. k depends on the floor, not the reverse.
- Test them apart, the floor first, then D with k set on the table that was kept. A match of both together cannot say which one
  acted, and they act on the same lines. Different aims too: the floor is about strength, the limit about clean node statistics
  and time in queen endings (lower priority in the register).
- What the floor needs from that work now: node comparisons per position (geomean, median) with the queen endings as their own
  stratum, as in S42, and positions 182, 362, 485 at levels 15 and 19 with and without the floor as a check. The ply limit (form
  A) does not depend on the reductions and can go in at any time; the mate guards (`mbnd`) come before both, as planned.
- One pair worth running together: cap 19 against cap 15 with the guards and the limit (match 3 of the extension review) and cap
  19 against cap 15 with the floor (2.3 above). The pair says how much of the -74 is explosions and how much the collapse.

## 4. Test plan

Local, census build (deterministic, `epLmrFix`, roots-500, draft 12, as S42):
1. f = 0: 12086912 nodes on tests.fen at depth 10.
2. f = 3, 4, 5 at levels 15 and 19 (13 and 15 without the floor are there): census tables, and nodes per position against level 15
   without the floor (geomean, median, queen endings apart). Read: share at d' = 1, mean reduction by d1, `conf / fh` and
   `conf / red` for d1 >= 4 and w >= 6, node ratio. Go on with the f whose cells reach those of level 13 at the lowest node cost;
   stop if no f lifts `conf / fh` clearly.
3. The explosions (182, 362, 485) at 15 and 19 with that f.
4. Adaptive, fixed time as S37 and S38 (200 roots, 1.5 s and 3 s): completed depth of cap 15 and cap 19 with the floor, paired
   against cap 15. The cost in depth is the price the matches must pay back.
5. If the root move line exists: M1 at equal nodes, floor against none at levels 15 and 19. The only local check of correctness.

Matches (predictions written at the start of each; fixed matches read at the end):
1. MEMO, 10+0.1: floor against the base, SPRT [-5, 0]. A gain is not expected there, only no harm.
2. One box, 192+3.2, fixed 500 games (about 18 hours): cap 19 with the floor against cap 15 without. A large effect or none.
3. The other box, 192+3.2, 2000 games (3 days), in parallel: floor at cap 15 against cap 15. The main test; read beside the final
   result of cap 13 (same opponent, time control and hash).
4. Only if 3 and cap 13 both gain: floor at cap 13 against cap 13, 2000 games (3 days): one effect or two.
5. With the floor kept: the ramp speed (Lim 6000) and a cap above 15 at the long time control (step A3 of the plan), and the
   check extension limit with k set anew.

Against the plan in the register: step A1 (caps 11 and 12 against 15) measures the bottom of a curve whose shape the floor would
change. Suggestion: one box for cap 12, the other for match 3, instead of both for the caps; the cap curve is finished after the
floor is decided. Six box days in all for matches 2 to 4, three of them needed only in one outcome.

## Not known

- Whether the tail or the width carries the gain of cap 13: the census hint is weak, match 3 against the cap 13 result decides.
- The node cost of the floor, and so the depth it costs at equal time.
- Whether the Elo per level seen in the interims holds at 2000 games.
- The misses: no local measure counts them except M1.
