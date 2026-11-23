# Time management review: early spending at long time controls

2026-11-15. Read from the code (`compTime` and helpers in `Main/Barbarossa.hs`, branch `lmcs`), S41 in `SEARCH-TUNING.md` and
the register entry "Time management at long time controls" in `SEARCH-IDEAS.md` (both `rel090`). Nothing built, nothing
measured: all numbers below are computed from the formulas, the expectations are estimates.

Time controls: 4+1 is 4 min + 1 s, 5+3 is 5 min + 3 s, 60+1 is 60 s + 1 s.

## Result in short

- The target (`ctm`) is not the cause. At about the target it spends the same share of the clock at every time control.
- The cause is the maximum (`tmx`): the extra above the target is 16 to 18% of the remaining clock at every time control.
  At 60+1 that is 9.4 increments, at 4+1 it is 37 increments.
- Change A (recommended first): the extra above the target is computed from the clock capped at 60 increments. Identical at
  60+1 while the clock is at or under 60 s, for every score and repetition; at 4+1 the maximum at the start falls from 43.0 s
  to 16.0 s. (Corrected 2026-11-15: the first form, a flat bound of 9.44 increments, was not identical at 60+1, see below.)
- Change B (second, only if the logs of A still show it): on the losing side the shortened moves to go apply only to the first
  60 increments of the clock. Identical at 60+1; it slows the spending in the lost games.

## What compTime does

With `q = tim / tpm` (the clock in increments) and `mtg` from the score (`estimateMovesToGo`: 50 below 1 pawn, 36, 24, 15, 10,
7, 5, 3, 2 from 8 pawns, by the absolute score):

- Target: `ctm = tpm + tim / mtg`.
- Maximum: `tmx = ctm + rtimfrc * (tim - ctm)`, `rtimfrc = 0.15 + 0.35 * ctm / (tim - ctm)`, bounded by `tim - 100 ms`.

Three paths lead from the target to the maximum, all in `searchTheTree` / `stopByChance`:

1. Score drop (`exte`): the score is more than 24 cp (`extendScoreMargin`) under the last draft or under the final score of
   the last move. Then there is no check against the target at all, the search goes on until the maximum. While the score
   stays under the last move's score, this holds for every draft of the move.
2. `timeProlongation`: a drop of more than 8 cp against the last move multiplies the target by `1 + ln (drop / 8)` (2.1 at
   24 cp, 3.5 at 100 cp).
3. A draft started below the target (at about 2/3 of it or less) is only cut at `mxr`, which is between half of the maximum
   and the maximum.

Paths 1 and 2 fire move after move in a game where the score sinks, which is the picture of the 8 lost games.

## Which term causes the early spending

At the target the net cost of a move is `ctm - tpm = tim / mtg`: the increment part is paid back. With `mtg` = 50 that is 2%
of the clock per move at every time control: 67% left at move 20, 45% at move 40. The 60+1 logs (58%, 26%, used about the
target) are close to that. So the target would give 4+1 the same clock curve as 60+1.

The maximum is different. The numbers at the start (score under 1 pawn):

| Time control | q | Target | Maximum | Maximum / target | Extra in increments |
|---|---|---|---|---|---|
| 60+1 | 60 | 2.2 s | 11.6 s | 5.3 | 9.4 |
| 5+3 | 100 | 9.0 s | 55.8 s | 6.2 | 15.6 |
| 4+1 | 240 | 5.8 s | 43.0 s | 7.4 | 37.2 |

Later in the game, at 4+1: 32.5 s at 180 s on the clock, 22.1 s at 120 s, 16.9 s at 90 s. At 60+1: 6.4 s at 30 s (4.0 times
the target), 3.8 s at 15 s (2.9 times).

As a share of the clock a move at the maximum costs the same everywhere (16 to 18%). What differs is what the clock is worth:

- At 60+1 the increment is 45% of the first target. An engine that has spent its clock still gets 1 s per move against 2.2 s
  at the start: a factor of 2.2. The clock is a buffer for hard moves, and spending it early on hard moves is right.
- At 4+1 the increment is 17% of the first target (1 s against 5.8 s): a factor of 5.8. The clock is the main source of
  thinking time, and three or four moves at the maximum in the opening remove half of it.
- 5+3 is in between: 3 s against 9 s, a factor of 3.

So the term is `rtimfrc * (tim - ctm)`: the extra is a fixed share of the clock, without regard to how many increments the
clock holds. The measured 1.6 times the target at 4+1 fits: the room above the target is 7.4 times instead of 5.3 and shrinks
slowly, while at 60+1 it is under 4 from 30 s on.

A second term, smaller: `estimateMovesToGo` takes the absolute score, so the losing side also spends as for a short game. At
-3 pawns (`mtg` 15) with 100 s at 4+1 the target is 7.7 s and the maximum 24.1 s, a quarter of the clock. On the winning side
this is harmless (your point: playing a won game on the increment costs nothing); on the losing side it is the clock under
10% at -1.4 to -5 pawns.

## Change A: compute the extra above the target from the clock capped at 60 increments

```
timx    = min tim (epTmIncs * tpm)                   -- epTmIncs = 60, only when tpm > 0 and no fixed moves to go
frtimx  = max 0 (timx - ctm)                         -- ctm from the real clock, as now
rtimfrc = remTimeFracIni + remTimeFracDev * ctm / max frtimx ctm
tmxt    = ctm + rtimfrc * frtimx
```

The rule says: a hard move gets as much above its target as it would get with 60 increments on the clock. While
`frtimx >= ctm` the extra is `0.15 * timx + 0.2 * ctm`, above 60 increments `9 * tpm + 0.2 * ctm`.

Correction (2026-11-15, the user's check): the first form of A was a flat bound, `extra <= 9.44 * tpm`, and this review said
it was identical at 60+1 at every score. That was wrong. The extra is `0.15 * tim + 0.2 * ctm`, and `ctm` grows when `mtg`
shrinks: at 60 s the uncapped extra is 9.53 s at `mtg` 36, 10.0 s at `mtg` 15, 14.5 s at `mtg` 2 (`rtimfrc` 0.5) and 13.2 s
with a repetition (`mtr` 3). The flat bound would bind at 60+1 above about 59 s, 57 s, 40 s and 43 s of clock. The form
above replaces it.

- 60+1: identical by construction while the clock is at or under 60 s (`timx = tim`), for every score and repetition. It
  differs only when the clock rises above 60 s, which needs moves faster than the increment.
- 4+1: maximum 16.0 s at the start instead of 43.0 s, 2.8 times the target instead of 7.4. With 120 s on the clock 13.1 s
  instead of 22.1 s. From 60 s down as now. A move at the maximum costs 6 to 7% of the clock instead of 18%.
- 5+3: maximum 37.8 s at the start instead of 55.8 s, 4.2 times the target; with 240 s 36.4 s instead of 45.4 s. With 120 s
  (q = 40) unchanged, 24.5 s: from 180 s down as now. Less change than at 4+1, as it should be: the increment carries more.

All three paths to the maximum are covered, because all end at `mx`.

Limits of the form:

- With no increment `timx` would be zero, so it must not apply (`tpm > 0`). Sudden death stays as it is.
- Above 60 increments the extra falls when the target is large, and it is zero when the target reaches 60 increments: the
  maximum is then the target. At 4+1 with 240 s on the clock the extra is 10.2 s at `mtg` 50, 12.4 s at 15, 14.0 s at 10,
  5.5 s at 5, and zero at `mtg` 3 and 2 (from 7 pawns) and with a repetition (target 81 s, maximum 81 s instead of 133 s).
  This is the shape `compTime` already has at 60+1 when the target passes half of the clock, and these targets are already
  very large, so I see no harm; but a search can then be cut at `mxr` below the target (`mxr` is between half of the maximum
  and the maximum). Worth a look in the logs of decided positions.
- For very large q (15+1: q = 900) the extra gets small against the target, 1.7 times. If such time controls matter, the
  milder form is the better one there: bound the maximum by 5.29 times the target, the ratio at the start of 60+1 with `mtg`
  50. That one is identical at 60+1 for every score (the ratio falls when `ctm` grows), but at 4+1 it only takes the maximum
  from 43.0 s to 30.7 s (5+3: 55.8 s to 47.6 s). I would keep it as the fallback if A loses.

One thing to watch: the first root move of a new draft is cut at `mxr / 3`. With a maximum of 2.6 times the target that is
0.4 to 0.9 times the target, so a draft started late can be cut in its first move and its time is lost. The same happens
today in the 60+1 middlegame (3.3 times at 20 s), so it is not new, but it will happen more often at 4+1.

## Change B: the losing side keeps a longer horizon on the clock above 60 increments

Only for scores of -1 pawn and below, with an increment:

```
near = min tim (60 * tpm)
ctn  = tpm + near / mtg + (tim - near) / 50
```

The shortened moves to go divide only the first 60 increments of the clock, the rest is divided by 50.

- 60+1: identical (q is at most 60).
- 4+1 at -3 pawns: target 7.8 s instead of 14.3 s with 200 s on the clock, 5.8 s instead of 7.7 s with 100 s. At -5 pawns
  with 100 s: 10.4 s instead of 15.3 s.
- 5+3: acts only above 180 s on the clock; at 300 s and -3 pawns 17.4 s instead of 23.0 s.

It is continuous at q = 60 and weak by construction. The simple form (negative scores always use 50) is stronger and cleaner,
but it changes 60+1 in lost positions (30 s, -3 pawns: target 1.6 s instead of 3.0 s), so it breaks your constraint and would
need its own run at 60+1.

Order: A alone first. B only if the logs of the A run still show the clock under 10% at negative scores before the game is
decided. One change per run; earlier time-management changes all lost, so each step should be readable on its own.

## Test in self-play

Asymmetric: the same commit twice, the builds differ only in the constant (A off: `epTmIncs` so large that it never binds).
With A off `compTime` must give the same values as now. Each opening with both colours. Protocol and machines as in
`lab\MATCHES.md`.

Time controls:

| Run | Purpose | What is expected |
|---|---|---|
| 60+1 | The constraint | Moves with the clock above 60 s about zero in the log, result within the noise of an A/A match. A few hundred games with logs are enough; the log count is the proof, not the Elo. |
| 4+1 | The target | The main run, a fixed number of games read once at the end. |
| 5+3 | A second long control with a large increment | Smaller effect than at 4+1; shows whether the rule scales with the increment. |
| 2+1 (optional, first) | Cheap pilot, q = 120 | Half the game time of 4+1; the cap binds for the first half of the clock. Enough to see whether the clock curves move as computed. |

What to read from the logs, per side (the engine logs at Info level, so no `-l 5` for at least a part of the games; lines
`Time = `, `Draft ... ms ... used`, `Time factors (reds/redp)`, `compTime (ms/mx/mxr)`, `searchTheTree terminated in first if`):

- Clock at moves 20 and 40 as a share of the start clock, and the share of moves under 10%.
- Used / target, and better the time spent above the target per move, in total and for moves 1 to 20.
- Moves at the maximum (used at least 90% of `mxr`). Careful: with A this share will rise, because the maximum is lower and
  reached more often. It is not comparable between the sides; the time above the target is.
- How a search ended: `timint` and `over` against `justStop`, and cuts in the first move of a draft (the `mxr / 3` limit).
- Moves with `reds` above 1, and the time they took: the score-drop paths.
- The move at which the clock first falls under 10%, and the score there. In lost games: was the game decided by then.
- Losses on time, and games ended on the increment only.
- For A at 60+1: the number of moves with `timx < tim` (clock above 60 s), the only moves where the two sides can differ.
  This needs one flag in the `compTime` log line.
- For A at 4+1: decided positions and repetitions, where the extra is small or zero: searches cut below the target.

Estimates for A at 4+1, to be checked against the run (from a simple model, not measured): clock at move 20 from 29% to 45 to
55%, at move 40 from 3% to 15 to 25%, moves under 10% from 53% to under 30%. I have no estimate for the Elo; the sign is open:
A saves time for later moves by taking it from the hard early moves, and only the run can say which is worth more.

One limit of the method: self-play measures the new time management against the old one, with the same search on both sides.
The weakness was seen against other engines, where the opponent keeps its clock. A gauntlet at 4+1, old and new against the
same opponents, is the confirmation after a positive self-play result.
