# Method review: how we test search changes (2026-11-15)

Basis: the "Methods" table and the LMR entries of `SEARCH-IDEAS.md`, S34 to S42 of `SEARCH-TUNING.md` (`rel090`), the skill
`search-idea-test`, lab `MATCHES.md` / `MACHINES.md` and the `run_timed` scripts. No engine was run. Numbers marked (sim) are from a
simulation of the tests: normal approximation, standard deviation 177 Elo per game pair (pentanomial of `lbi6`, S34), alpha = beta =
0.05. The rest is read from the records; inferences are marked.

## Short answer

- "H0 near -7" is the signature of the SPRT, not a measurement: at an H0 stop the estimate is fixed by the number of games.
- [0, 5] fits new mechanisms, not steps of a tuned parameter. [-3, 2] is not a test of "neutral": a change of exactly 0 fails it in
  36% of the runs and takes 21000 games on average.
- 2000 long games decide only effects from about 15 Elo. An early look may always plan other runs, may stop a loser by a loose
  rule, and may stop a winner only at looks fixed before the start, by a strict rule.
- Node scans: per position ratios with an interval, one process per position, a node limit instead of dropped outliers, a placebo.
- Missing in the records: the gain of the short games checked at the long time control, the hash size of the long runs (16 MB),
  adjudication of decided games.

## What we misread

1. The estimate at an H0 stop. With [0, 5] the test stops for H0 at `estimate = 2.5 - K / games`, K about 38000 (37000 to 39000
   by the draw rate). Recomputed against the records: 3506 games -8.3 (-8.13), 4286 -6.4 (-6.65), 4084 -6.8 (-6.72), 1366 -25.3
   (-24.97), 13964 -0.2 (-0.35). The information is the number of games, and it is wide: a true -3 stops within 4300 games in 28% of
   the runs, -5 in 47%, -7 in 66%; the estimate at the stop is about 1.7 too low (sim). What stands: none of Ini 6, PV 1, Lim 17000
   gains 5, each probably loses 3 to 8. What does not: that the three cost the same, and that m3a "fits MEMO in size" (S34), since
   the size was forced. A box is validated by a fixed match of a known result (`lmrc`, -25: 600 games give +- 20), as `MATCHES.md`
   asks.
2. The predictions. In all 11 cases from S28 to S40 the "most likely" value was above the result. Point 1 explains about 2 Elo of
   that, not 6. A change of a search tuned as a whole starts at about -5, not at 0.
3. The time control. S37/S38 predicted the cap by depth, S40 answered -74. Likewise nothing recorded shows that the +80 or so of
   the short SPRTs since `dev090` (a sum of steps selected for passing, so inflated) arrives at 60+1.
4. The long runs use the default hash of 16 MB (`run_timed` passes no `option.Hash`) at about 0.8M nodes per move on the boxes:
   a table that fills (register, "TT replacement"), unlike real use (S41: 68 to 516 MB). Every long result so far, the caps
   included, is measured under that load; it may favour the smaller tree (a guess).

## SPRT bounds

Chance to pass / mean games by the true value (sim). A MEMO slot plays about 10500 games a day at 10+0.1, a box 4800.

| Bounds | -5 | -3 | -1.5 | 0 | +1.5 | +2.5 | +5 |
|---|---|---|---|---|---|---|---|
| [0, 5] | 0% / 5000 | 0% / 6700 | 1% / 9200 | 4% / 13700 | 25% / 20100 | 50% / 22200 | 95% / 13800 |
| [-3, 2] | 1% / 8300 | 5% / 13300 | 23% / 19800 | 64% / 21200 | 91% / 15600 | 97% / 11900 | 100% / 6700 |
| [-5, 0] | 5% / 13300 | 35% / 21500 | 76% / 19900 | 96% / 13200 | 99% / 9200 | 100% / 7400 | 100% / 5000 |
| [-8, 0] | 32% / 8400 | 67% / 8400 | 86% / 6900 | 95% / 5300 | 99% / 4000 | 99% / 3600 | 100% / 2600 |

- [0, 5]: keep it for new mechanisms (ordering, pruning, extensions); the steps of +10 and more passed it quickly. A parameter step
  predicted at 0 to +2 is its most expensive case and returns only "not +5". For "where is the optimum" play two or three settings
  as fixed matches against the same base (4000 games each: +- 8) and read the curve, or do not play.
- [-3, 2] asks for a gain of 2; 0 lies in its open middle. For a change claimed harmless put 0 at the upper bound and take as
  lower bound the loss we would accept: [-5, 0], or [-8, 0] when a known gain elsewhere pays for it (a cap that gains at 60+1).
  Fix that loss before the result. The running cap 13 SPRT ([-3, 2], predicted 0) is to be read by estimate and error: an H0 there
  can mean 0.

## Early looks at a fixed match

2000 games: +- 11, LOS >= 95% needs +9.2. Read once at the end, a true +5 is adopted in 22% of the matches, +10 in 56%, +15 in 84%,
+20 in 97%. Stopping at the first look with LOS >= 95% (every 100 games) adopts a change of 0 in 21% instead of 5% (sim).

- Planning what the other machines play next: always legitimate, it does not touch this match.
- Stopping a loser never adopts anything wrongly, it only costs power. At 500, 1000, 1500 games: stop below -2 standard errors
  (-22, -16, -13). A true -20 stops in 89%, after 990 games on average; a true 0 in 5%. Not when the size of the loss is wanted.
- Stopping a winner: only at these three looks, at +3 standard errors (+34, +24, +19). The false winners stay at 5.0%; it saves
  little below +20, and the estimate of a stopped match is too high.
- Cap 13, +39 +- 25 after 400 games, is 3.05 standard errors; without an effect some look gets there in 0.9% of the matches, so
  the sign is very probably right. The size is not: with the prediction as prior (0, standard deviation 10 to 15) expect +15 to
  +23. The look was not fixed before and the next step needs the size, so let it run. Legitimate now: queue cap 11 or 12 for the
  next free long slot, and stop cap 17 by the loser rule if it is there.
- After a passed screening the long match is a no-loss check: keep the change unless the LOS is below 20% (about -5). LOS >= 95%
  stays for changes that have only the long match.

## Spending the game time

| | 10+0.1 (boxes 32+0.32) | 60+1 (boxes 192+3.2) | 2000 long games |
|---|---|---|---|
| MEMO, per slot | 440 games per hour | about 60 (estimate of `MATCHES.md`, not measured) | 1.4 days |
| m3a, m3b, each | 200 | 28 | 3 days |

A long game costs seven short ones on every machine, so long games go to the questions short games answer wrongly. The kind of run
goes by its end: a fixed match ends at a known hour and fits a box that will be switched off; an SPRT of unknown length ([0, 5], true
0: one in ten takes over 26000 games) fits MEMO, which stays on and can resume.

Long slots, in this order:
1. `chcm` against `dev090` at 60+1, 1000 games (+- 16), 17 hours of a MEMO slot: it tests the screening itself (point 3).
2. The cap as a curve at one long time control (11 or 12, 13, 17 against 15), with the hash size settled first; after the running
   matches, since a change of the hash in between breaks the comparison.
3. The time management change (review of 2026-11-15) against the old one at 4+1 and 60+1: self-play sees it when the sides differ.

Short slots: SPRT [0, 5] for new mechanisms (`improving`, singular extensions, aspiration). No more single ramp steps at 10+0.1
after Lim 6000: the ramp is near its optimum there and acts differently in long games (S36).

Two inferences to check before use:
- A middle rung for ramp and cap. The cap binds by the nodes per move: 3% of the nodes at 0.5 s, a third at 1.5 s (S37) and at
  30+0.3 on mini (S36). 30+0.3 on MEMO costs 40% of 60+1 (2000 games in 14 hours per slot). Eight trace games show whether the cap
  binds there as at 60+1; if so, screen there and confirm at 60+1.
- Adjudication. `run_timed` plays every game out, and 58% of the searches in games have 4 pieces or fewer besides pawns and kings
  (S39), with long mates (S41). Their share of the time is smaller and not known; the stored `games.pgn` give the time saved and
  the results changed by a rule (both sides beyond 8 pawns for 4 moves; both within 0.15 for 8 moves after move 40) without a new
  game. Conversion (KQKR must be won) then needs its own position test.

## Node scans

The role stays as in the skill: identity check, anomaly detector, cost; not a selector.

- Statistic: per position the log of the node ratio to the base; report the median, the geometric mean with a bootstrap interval
  over the positions, and the share of positions above 1. Equal weight per position is right: in a game every move gets about the
  same time. The total serves the identity check only.
- Outliers are the finding (S42), not noise: none is dropped. A node limit per position of 30 times the base count, recorded as
  "30 or more" (bounded run time, the median does not move); every position beyond a factor 4 is listed and looked at.
- One process per position (`node_scan.ps1`), not chunks or the whole file: a carried TT and history let one explosion leak into
  its neighbours, and the interval needs independent positions.
- Placebo first (S20 again): the same engine with a change that does nothing (hash 16 against 17 MB) gives the spread of the per
  position ratio and so the floor. Without it "geomean 0.81" has no error bar.
- Position set: of the 200 roots of S38/S39, 92 have 4 pieces or fewer. Report by stratum (openings `pos400`, roots with more
  pieces, endings with pawns); positions with 5 men or fewer or a decided score apart, never in the headline.
- Equal effort instead of equal draft: draft 12 is 384k nodes in KQKR and many millions in a middlegame, and the ramp follows the
  size of the iteration (S36). Take per position the draft at which the base needs about the nodes of a move at the time control in
  question (0.2M at 10+0.1, 0.8M at 60+1).

## Not known

Whether a box replay of a known result was played; the games per hour of MEMO at 60+1; the time share of decided endings; how much
the 16 MB hash changes the long results. The simulation uses one variance for all runs (the partial cap results at 192+3.2 show
about the same error per game as 10+0.1).

## Measured afterwards: adjudication (2026-11-15)

On the 13984 games of Grw 110 (`lmrt-g110-lmrt-tc10`, MEMO 10+0.1), from the score and time comments of `games.pgn`: a resign rule
(both sides' scores beyond 8 pawns for 8 plies) would end 7898 games early and save 7.2% of the thinking time, and change 69
results; a draw rule (within 0.15 for 16 plies after move 40) 3272 games, 2.1%, 70 results changed. Together about 9% of the time
at 10+0.1 for about 1% changed results. Long time controls not measured (the cap runs on the boxes give the PGNs when they end).
