# `pawnEndGame`: the square of the pawn is used as its distance to promotion

Found by reading `Eval/Core.hs` on branch `lmrm` (lines 1167 to 1223). Nothing was built or run: the
numbers below are computed by hand from the code, not measured. Not fixed.

## Where

`pawnEndGame` is the special eval for positions with only kings and pawns (`specialsOf`). It looks for
"escaped" passed pawns (the enemy king is outside the square of the pawn) and returns a final score,
in centipawns, instead of the normal eval:

- only my side has an escaped pawn: `myrace`;
- only the other side has one: `yorace`;
- both have one: `dpr`, a comparison of who promotes first.

The helpers `escMeWhite`, `escYoWhite`, `escMeBlack`, `escYoBlack` return for every passed pawn

```haskell
(esc, (psq, dis))	-- escaped?, (square of the pawn, distance to the promotion square)
```

and `mescds`, `yescds` are the lists of `(psq, dis)` of the escaped pawns.

## The matter

```haskell
mim = fst $ minimumBy (comparing snd) mescds      -- who is promoting first?
miy = fst $ minimumBy (comparing snd) yescds
```

`minimumBy (comparing snd)` selects the right pawn, the one nearest to promotion. But `fst` then takes
its **square** (0 .. 63), not its distance (1 .. 6). `mim` and `miy` are used only as distances:

```haskell
dpr | mim < miy     = myrace
    | mim > miy + 1 = yorace
    | otherwise     = withQueens
myrace =  promoBonus - distMalus mim
yorace = -promoBonus + distMalus miy
distMalus x = unsafeShiftL x 3        -- to bring at least 8 cp per move until promotion
```

`Square` and the distance are both `Int`, so the compiler has nothing to say. All three uses are
affected, also the two cases with an escaped pawn on one side only, because `myrace` and `yorace`
contain `distMalus`.

## Effects

### One side has an escaped pawn

The score is `1000 - 8 * square` instead of `1000 - 8 * distance`:

| Pawn | Distance | Intended | White pawn (square) | Black pawn, mirrored (square) |
|---|---|---|---|---|
| a-file, 1 step to go | 1 | 992 | a7 (48): 616 | a2 (8): 936 |
| a-file, 3 steps to go | 3 | 976 | a5 (32): 744 | a4 (24): 808 |
| a-file, 5 steps to go | 5 | 960 | a3 (16): 872 | a6 (40): 680 |
| h-file, 1 step to go | 1 | 992 | h7 (55): 560 | h2 (15): 880 |

- **White: the score falls by 64 cp with every step forward**, where it should rise by 8. The best
  static score for White is the pawn far back.
- **Black: the score rises with every step**, in the right direction, but by 64 cp instead of 8.
- The file changes the score by 8 cp per file (56 cp between the a- and the h-file), which means
  nothing.
- The eval is not colour symmetric: a position and its colour flip get different scores (744 against
  808 in the second row).

The sign stays right in this case (the side with the escaped pawn is far ahead), which is why games do
not show it directly: the search finds the promotion a few plies later, and then the normal eval with
a queen takes over. What is lost is the gradient towards the promotion, and for White it is inverted.

### Both sides have an escaped pawn

Here the comparison decides the sign of a score of about 1000 cp, and it compares the square number of
a white pawn with the square number of a black pawn. With White to move the rule becomes:

- white square < black square: White wins the race;
- white square > black square + 1: Black wins the race;
- otherwise (pawns on neighbour squares in the numbering): both promote.

Advanced white pawns have high squares and advanced black pawns low ones. So when both pawns are far
advanced, which is the normal picture of a race, **Black is the winner whoever is faster and whoever
is to move**; when both are far back, White is. The distances do not enter.

Example (White to move, White promotes in 2 with check, Black needs 3):

```
7k/1K6/P7/8/7p/8/8/8 w - - 0 1
```

- intended: `mim = 2`, `miy = 3`, `2 < 3`, `myrace = 1000 - 16 = +984`;
- as coded: `mim = 40` (a6), `miy = 31` (h4), `40 > 32`, `yorace = -1000 + 8 * 31 = -752`.

A won position is scored as lost by 7.5 pawns. Its colour flip

```
8/8/8/7P/8/p7/1k6/7K b - - 0 1
```

gets, as coded, `mim = 16` (a3), `miy = 39` (h5), `16 < 39`, `myrace = 1000 - 8 * 16 = +872` for the
side to move: the right sign, by accident of the square numbers. The same position is -752 with White
and +872 with Black as the faster side. In the game itself the search repairs it once both
promotions are inside the horizon. The damage is earlier: at the leaves of a search from a piece ending
or a pawn ending, where the decision is whether to go into such a race (exchange the last pieces, push
or hold a pawn), the leaf score has the wrong sign and a size that overrules everything else.

## Why it was not seen

- The search hides the one-sided case (see above), and for Black the direction is right.
- The fit does not go through this code: `TuneSGD` uses `posExactEval`, which skips the special evals.
  The export `-X` writes the special score in its own column (`evalSpecialScore`), but the `eval`
  column, which the colour symmetry test of `TUNING.md` 3 d) compares, is the normal eval. The test
  sees the bug only if it compares the special score column too.
- Pawn endings with escaped pawns on both sides are rare as positions on the board, though less rare
  as leaves of the search.

## Fix

The two lines need the distance:

```haskell
mim = snd $ minimumBy (comparing snd) mescds
miy = snd $ minimumBy (comparing snd) yescds
```

or shorter, since the square is then not used at all: `mim = minimum $ map snd mescds`. With that the
helpers could return only `(esc, dis)`, and `Data.List (minimumBy)` and `Data.Ord (comparing)` are no
longer needed in the module if nothing else uses them.

This changes the engine (it is not a refactoring), so the node counts of the perf build will differ
wherever pawn endings are reached. Checks I would make:

- the example position above and its colour flip: the special score (the column of `TuneSGD -X`, or
  the static score through `posEval`), before and after: the same value for both after the fix, +984
  from the side to move;
- the positions of the table: the score must rise by 8 cp per step for both colours;
- the symmetry test of `TUNING.md` 3 d) extended to the special score column, on the pawn endings of
  a data set, as a permanent test for the special evals;
- a match is hardly able to measure it (rare positions); a set of pawn ending positions with known
  results (race won / lost / drawn) would be the direct test.

## Smaller points in the same function (not the bug)

- A pawn on its start rank has `dis = 6`, but it needs 5 moves (double step). The error is on the safe
  side for the escape test and one tempo wrong in the race.
- An own king or own pawn on the path of the pawn is not considered: the pawn needs more moves than
  its distance.
- In the equal race (`withQueens`) the score is only the pawn material difference; the comment in the
  code already names what is missing (promotion with check, capture of the new queen).
