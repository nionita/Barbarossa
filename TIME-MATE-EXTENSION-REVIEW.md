# Mate bounds, check extension and time use: review

2026-11-15. Read only, nothing built or run. Basis: S41 to S44 of `SEARCH-TUNING.md` (`rel090`), `TIME-MANAGEMENT-REVIEW.md`,
`Search/SEARCH-REVIEW-BUGS.md`, the code of `rel090` (`Search/Core.hs`, `Moves/Internal/Base.hs`, `Main/Barbarossa.hs`), the diff
of `mbnd`, and the log `C:\Learn\Data\logs\ics-4p1-chcm` (`chcm` at 4+1, 10 games with 10 moves or more, 652 searches, 8 lost and
2 won by the last score). Numbers are from that one log; "at the maximum" is used >= 90% of `mxr`, as in S41. Inferences are marked.

## 1. Do the bugs contribute to the time problem?

**Answer: not to the clock problem, by the timing alone; they are visible in the lost and won endings, at a small cost.**

When the clock is lost:

| | Moves | At the maximum | Used | Used over target (sum) |
|---|---|---|---|---|
| All | 652 | 263 (40%) | 3025 s | 1551 s |
| Moves 1 to 10 | 100 | 27 | 1146 s | 709 s |
| Score between -150 and +150 | 219 | 61 | 1773 s | 1082 s |
| Score <= -500 or a mate score (either sign) | 285 | 129 | 322 s | 34 s (2%) |

- In the 8 lost games the score first reaches -500 with 4.8 to 22.7 s on the clock (2 to 9% of the start), a mate score with 1.1
  to 12.2 s. The lost phase is played on the increment: whatever the bugs cost there, the clock was gone before.
- The 15 longest last iterations (21 to 40 s, aborted at the limit) are all in moves 1 to 10, 23 or more pieces, scores within
  +-250, no mate bound near the root. 237 of the 263 moves at the maximum end in an aborted iteration.
- The path that costs is the score drop of the time management, not a search bug: a final score 24 to 300 under the last move's
  (`exte`, `stopByChance` returns `mx` and never stops) has 109 moves, 91 at the maximum, 1078 s (36% of all time), used / target
  3.7 (drop 24 to 100) and 2.6 (100 to 300). Without a drop: 461 moves, 31% at the maximum, 1.2. This is finding 1 of
  `TIME-MANAGEMENT-REVIEW.md`.
- Inference: the mate bound bugs do not cause these drops. The bug windows need alpha or beta at a mate bound; with root scores
  within +-250 they occur only deep in the tree, and S43 measured 456 of 500 positions with identical scores at every depth. Not
  proven for these positions (see "missing").

What the log shows of the three paths in the lost and decided phases:

- Score drops: in the lost phase (final -500 to -17000) 65 moves, 58 with `exte` at some draft, 45 at the maximum, 130 s, mean
  clock 3.8 s. It empties the last seconds, then every move has about 1 s.
- "Research" (root re-search): 1196 lines, 1.8 per move, 33 at a mate bound (in 20 moves, 27 s together). No relation to the time:
  38% at the maximum with none, 48% with 6 or more. One move with 56 (game 1 move 37, score -488 to -20000, 3.4 s).
- Mate scores unstable: a mate (20000) seen and gone at the next draft 6 times in 4 moves, all in the first drafts (a few ms);
  between two moves twice. This is the signature of findings 2 and 9, and it costs no time.
- Search after a mate score: the search does not stop at a mate. After the first draft with -20000 it spends 38.7 s (52 moves;
  14 end at draft 25, 18 at the limit), after +20000 135 s (154 moves). Since mate scores carry no distance, these drafts cannot
  change the move (inference; exception: a wrong mate, the 4 moves above). 174 s, 6% of all time, all on the increment.
- Slow iterations in queen endings: game 7 has a mate score for Barbarossa from move 41 to 184 (two queens against pawns, 6 to 9
  pieces). Final draft 7 to 16 (mean 12.8) in 0.86 s, 300k to 700k nodes per draft, never draft 25; 60 of 144 at the limit; the
  line passes ply 2 * draft in 66 of them (seldepth, main search only). The other mates reach draft 25 (9 of 10 moves in game 6).
  Of the 95 moves in the log with seldepth > 2 * draft, 69 are these won queen endings, 3 mated, 23 others (maximum 2.67 * draft).
  Inference: this is the S44 mechanism in a game; it lengthens a won game (144 moves, 50 move counter up to 58 plies), it did not
  cost the win here.

A side finding for S43: the "-19744, just outside `nearmate`" is the clamp of the static eval (`matesc = 20000 - 255` in
`Eval/Core.hs`, rounded to the grain), not a mate. `nearmate` is `>= 20000 - 255`, so the clamp value 19744 falls one grain outside
it. It is frequent: 151 draft scores of -19744 and 5 of the 8 lost games reach it before -20000. The guards of `mbnd` (and
`isRevFutil`, `isPruneFutil`) do not hold at this bound. Whether they should is a decision (it is an eval score), but the two
definitions should not differ by one.

Missing for more: results and opponent times (not in the log); nodes by type and a count of extensions per draft; the ply at the
abort. The log has every root position (first `NoEval` line of a search): a replay of the 652 roots with `rel090` and `mbnd` at the
logged draft would show directly whether the drops and the slow drafts change.

## 2. Which limit for the check extension

From the code: `d1 = d + exd - 1`, then `d' = reduceLmr spec d1 lv w = max 1 (d1 - r)`, no reduction at `d1 <= 1`. A checking move
is not exempt from LMR (`noLMR` is passed pawns only). So:
- a one-sided check chain costs one ply of depth per two plies: it ends by itself at about ply 2 * draft;
- checks of both sides in turn (rook spite checks, queens on both sides) cost nothing: only repetition ends them;
- a line reduced to depth 1 keeps extending: a check at depth 1 gives a node at depth 1. The reduction removes the quiet moves,
  the mutual checks stay. The higher the LMR level, the larger this share (S42: 90% of the reduced searches at d' = 1 at level 15,
  99% at 19). Inference, in line with S44.

| Form | State needed | Cuts | Risk |
|---|---|---|---|
| A: none beyond ply 2 * draft | none (built as B in S44) | mutual check chains only; 362: 252M to 8.9M, level 19 still 2.5 times level 15 | lowest: a one-sided mating chain of draft d ends before the limit |
| B: at most n extensions per line | counter in `NodeState` | as A when n grows with the draft; a fixed n is too loose at low drafts and too tight at high ones | deep mates by checks at high drafts with a fixed n |
| C: none in a reduced line | flag "reduced above" in `NodeState`, cleared in the re-search | the whole interaction with the LMR | the alarm of a reduced search no longer sees check sequences; all or nothing |
| D: only while `ply + d >= draft - k` | none (`absdp`, `d`, `draft` are at hand) | as C, graded: a line reduced by up to k plies still extends | as C, smaller, set by k |

`ply + d` is the draft for a line without reduction or extension, less after reductions, more after extensions; D measures "far
below the draft" without new state, and the full depth re-search of `zeroWithLMR` is back above the threshold by itself.

**Recommendation: D with the ply limit A as the backstop**, both as parameters compiled to "off" first (k, and the factor 2).
Reasons (inference, not measured): (1) it acts where S44 locates the explosion, the lines collapsed to depth 1, and more the
higher the level, so the level regulates the tree again (S44, build A: without the check extension level 19 is cheaper than 15
in all three positions); (2) the loss is bounded by the QS: a node in check is searched with all evasions in `qsInCheck`, so a
mate in one by a check is still seen in a collapsed line; (3) PV and little reduced lines (w small, the first moves) keep the
extension, which is what KQKR and mates by checks need. Start with k = 2 or 3 (S42: mean reduction 1.4 at d1 2-3, 3.5 to 3.8 at
d1 4-6 for levels 15 to 19). The last queen / last rook extension happens at most twice per line: leave it out of D.
A alone is the fallback if D loses mates: safe, already measured, but it leaves level 19 above 15.

## 3. Test plan (limit of 2 on top of `mbnd`)

Local, deterministic perf builds of `rel090`, `mbnd`, `mbnd` + limit (parameters off: node counts identical to `mbnd`):
1. Perft unchanged; tests.fen depth 10 and roots-500 depth 12: node ratio per position (geomean, median), share of positions
   with the same score at every depth.
2. Explosions: positions 182, 362, 485 at `epLmrFix` 15, 17, 19, depth 12. Target: level 19 not above level 15, scores as before.
3. Mates by check: the 6 mate positions of S43, the mate-escape set, and the roots of the log with a final mate score (52 against,
   154 for). Per position the first draft with the mate; none later than `mbnd` by more than one draft, none lost.
4. Queen endings from the game: the 144 roots of game 7 at a fixed node count (500k): draft reached (log: mean 12.8) and whether
   the mate score holds.
5. KQKR: 100 random legal KQKR positions (queen side to move, no immediate capture), candidate and `mbnd` as the queen side
   against the same defender (`mbnd`) at fixed nodes per move: wins, moves to the win, 50 move draws. No fewer wins, not longer.

Matches:
1. `mbnd` against `chcm`: SPRT [-5, 0] at 10+0.1 (planned in the register).
2. `mbnd` + limit against `mbnd`: SPRT [-5, 0] at 10+0.1, then a fixed match at 60+1 (the LMR level sits at the cap there).
3. Cap 19 against cap 15, both with the fixes, at 60+1: the -74 of S40 may contain the explosions; this is where a gain can show.
4. For the time use the self-play matches show nothing (same time management on both sides): a gauntlet against other engines
   at 4+1 with logs, before and after, same openings.

To read from the logs (base values from this log; script `time_use.py` plus the counts above):
- clock at move 20 and 40, moves at the maximum (40%), used / target: expected unchanged (question 1); a change here would
  contradict the reading;
- moves with seldepth > 2 * draft (95 of 652; 0 with the ply limit) and the final draft in roots with queens and 8 pieces or
  fewer (mean 12.8);
- mate found: moves from the first mate score to the end of the game (game 7: 144), searches ending at draft 25;
- "Research" lines at a mate bound (33) and mates seen and gone at the next draft (6): expected near 0 with `mbnd`.

Not part of the change but the larger lever for the decided phases (inference): end the search when the score is a mate (174 s
here), and treat the score drop rule of `TIME-MANAGEMENT-REVIEW.md` first: that is where the clock goes.
