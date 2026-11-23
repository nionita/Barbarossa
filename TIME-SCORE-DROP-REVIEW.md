# Score drop and time: what the extra time buys, and a rule for long time controls

2026-11-15. Read only, nothing built or run. Basis: `TIME-MANAGEMENT-REVIEW.md` (change A), `TIME-MATE-EXTENSION-REVIEW.md`,
`searchTheTree` / `stopByChance` in `Main/Barbarossa.hs` (`rel090`), the logs `C:\Learn\Data\logs\ics-4p1-chcm` (4+1, 652 moves,
3025 s) and `gui-60p1` (60+1, 7 logs, 1753 moves, 3305 s). Scripts: BarbarossaTuning `score_drop.py`, `score_drop2.py`, `score_drop3.py`.

Method (all counts below depend on it). The drop acts in two ways: `exte` (no stop at the target, limit `mx`) and `reds`
(`timeProlongation`: target and `mxr` times `1 + ln (drop / 8)`). "Without the drop" is a replay of the logged drafts with `exte`
off and `reds` = 1: the search stops at the first finished draft past the target, or is cut at its `mxr`. Approximations: only
the sure stops are replayed (not the random one); the change probability in `mxr` is taken as 0.5; a move found in an aborted
draft counts as not found; the increment is 1 s. Replaying the present rule this way misses the logged time by 15 s (4+1) and
22 s (60+1), so sums are good to about 1%. A correction to the first review: its 109 moves were selected by the final score;
by the mechanism the drop path is larger in moves and smaller in time (below).

## 1. What the extra time changed

Moves that got more time because of the drop, the result at the draft where the search would have ended against the end:

| | 4+1 | 60+1 |
|---|---|---|
| Moves with extra time from the drop | 124 of 652 | 196 of 1753 |
| Extra time | 609 s (20% of all time) | 315 s (9.5%) |
| Extra per move | 4.9 increments | 1.6 increments |
| Best move changed | 14 (11%) | 24 (12%) |
| Extra time in moves that kept the move | 550 s | 273 s |
| Score at the end: back within 24 of the last move's score | 24 (move changed in 3) | 80 (11) |
| partly back | 0 | 4 (0) |
| confirmed (within 24 of the drop) | 71 (5) | 87 (8) |
| deeper by 24 or more | 29 (6) | 25 (5) |

- The return is the same at both time controls: one move in nine changes. The price differs by a factor of three.
- The search confirms the drop or finds it worse in 100 of 124 moves at 4+1. Of the 24 recoveries, 21 keep the move: the score
  came back by itself, the time changed nothing on the board.
- The long extensions buy least. At 4+1: under 2 increments 64 moves, 50 s, 6 changes; 2 to 5: 24, 75 s, 4; 5 to 10: 15, 116 s,
  3; 10 or more: 21 moves, 368 s, 1 change. At 60+1 one move has 10 or more.
- A changed move comes early: at 4+1 the final move is settled within 3 increments after the no-drop stop in 10 of the 14,
  within 5 in 12; at 60+1 in 22 and 23 of the 24.
- By move number at 4+1: moves 1 to 20 take 420 s of the 609 s (36 moves, 9 changes); moves 21 on 189 s (88 moves, 5 changes).
- By size of the drop against the last move at 4+1: 24 to 50 cp 37 moves, 269 s, 4 changes; 50 to 100: 30, 175 s, 5; 100 to 300:
  31, 118 s, 2. The small drops cost most, per `1 + ln (drop / 8)` they already double the target.
- `exte` alone overrides a sure stop in only 15 moves at 4+1 (150 s) and 22 at 60+1 (53 s). The larger part comes from `reds`: it
  raises the target and lifts `mxr` to `mx`, and the draft then started is cut at the maximum (6 of the 15 `exte` cases finish
  no further draft at all).

Not in the log: whether a changed move was better. Only a match can say; the counts bound what can be lost.

## 2. Rules

Replay of each rule on both logs. "Other move": moves where the rule would have played something else than the log.

| Rule | 4+1: saved | other move | 60+1: saved | moves touched | other move |
|---|---|---|---|---|---|
| No drop handling at all (bound) | 602 s (20%) | 14 | 307 s (9.3%) | 177 | 21 |
| Change A alone | 604 s (20%) | 10 | 1 s | 6 | 0 |
| Drop against the last draft only (`exte`), `reds` as now | 139 s (4.6%) | 2 | 49 s (1.5%) | 18 | 2 |
| Stop when two drafts confirm (`exte`) | 126 s | 2 | 49 s | 18 | 2 |
| Cap of the drop extra, 3 increments over the target | 366 s (12%) | 10 | 105 s (3.2%) | 52 | 3 |
| the same, 5 increments | 309 s (10%) | 7 | 68 s (2.1%) | 28 | 1 |
| the same, 8 increments | 265 s (8.8%) | 5 | 17 s (0.5%) | 14 | 0 |

- Last draft only, and two confirming drafts: both act on `exte`, which is the small part (1). Little gain at 4+1, and they
  change 60+1 in 18 moves without a reason that belongs to the time control. Not recommended.
- Scale by game phase: at 4+1 the cost is in moves 1 to 20, but at 60+1 these moves also carry 156 s of the 315 s, and
  `reduceBegin` already halves the target in the first moves. A phase scale changes 60+1 unless it is tied to the clock in
  increments, and then it is the cap. Not recommended as its own rule.
- Cap in increments: fits 1 (changes come within 3 to 5 increments, the long extensions buy nothing). A flat cap breaks 60+1
  below about 9 increments, where it is the maximum of 60+1 itself. A cap that is exact at 60+1 is change A: it saves at 4+1 as
  much as dropping the whole drop handling (604 s), by cutting all three paths at a lower `mx`, and touches 6 moves at 60+1.

So A is the first score-drop rule. After A the drop still gets 362 s at 4+1 (122 moves, 9 changes; 15% of the time left) against
306 s at 60+1 (177 moves, 21 changes): per changed move 40 s against 15 s. That is the part a second rule can take.

**Recommended second rule, D: on a clock above 60 increments the drop gives at most 5 increments over the target.**

```
base = redp * ms                              -- the target without the drop
drop = exte || reds > 1
capd = base + epTmDropIncs * tpm              -- epTmDropIncs = 5; only if tpm > 0 and tim > epTmIncs * tpm (60)
msr  = min (reds * base) capd                 -- when drop
exte counts only while used < capd
mxr  = min mxr (max capd mxr0)                -- mxr0: the value without the drop (reds = 1, no exte)
```

- 60+1: identical while the clock is at or under 60 s, as A. In the logs the clock is above 60 s in 101 of 1753 moves (5.8%, a
  number A needs too); D would shorten 3 of them, 14 s (0.4%), no other move. Reason for "nearly": moves faster than the
  increment early in the game.
- 4+1, on top of A: 132 s more (5.5% of the time under A), 37 moves, 1 other move. With 3 increments 145 s and 3 other moves,
  with 8 increments 68 s and none: 5 is where the saving is nearly complete and the changes are kept (inference from 14 changes
  in one log, weak).
- 5+3: acts above 180 s; the cap is 15 s over the target (inference: milder, as for A).
- It is not continuous at 60 increments (under A the extra there is about 9 increments, D takes it to 5). If that shows in the
  logs, let the cap fall from 9 at 60 increments to 5 at 120.
- It does not cut the search below what a move without a drop gets (`max capd mxr0`).

Order: A first, alone, as planned in `TIME-MANAGEMENT-REVIEW.md`; D in a second run against A, both in the same build as
parameters (`epTmIncs`, `epTmDropIncs`, off = never binding). Two runs, not one: A moves 20% of the time, D 5%, and D's sign is
the open one (it takes time from the moves where the engine sees trouble). B of that review stays third; D and B both act in
lost positions, so not in the same run. If A loses, D is not a substitute: it then has to be judged without A (flat form above,
5 increments: 10% at 4+1, 2% at 60+1, so with its own 60+1 run).

## 3. What to count in a test run

Beyond the counts of the two reviews, per side, from the engine logs (Info level):
- Moves with a drop signal (`reds` > 1 or the score 24 under the last draft or the last move) and their time over the no-drop
  stop: sum, per move in increments, and the distribution (under 2, 2 to 5, 5 to 10, 10 or more). Base 4+1: 124 moves, 609 s,
  4.9; 21 moves with 10 or more.
- Of these: best move changed after the no-drop stop (base 11%), and how many increments after it the final move was settled.
  With D the changes must stay near 11% of the drop moves while the time falls; if they fall with the time, the cap is too low.
- Score at the end against the draft of the stop: recovered, confirmed, deeper (base 24 / 71 / 29).
- Drafts started after the stop and cut at the limit without a result (`timint` with no finished draft after the signal).
- The same moves by move number (1 to 20 against later) and by clock in increments (above and under 60).
- Moves where the cap bound (one flag in the `compTime` log line, as for A), at 60+1 as the proof of the constraint: expected
  about 0.2% of the moves.
- The move after a capped drop move: score change against the capped move. A capped move that was wrong shows as a further
  drop one move later; compare the rate with the uncapped side (inference: the only sign of quality the log can give).
- In lost games: the clock when the score first passes -150 and -500 (base 4+1: 5 to 23 s at -500).
