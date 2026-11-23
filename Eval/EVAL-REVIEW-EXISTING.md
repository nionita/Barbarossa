# Eval review, part 1: two existing terms I would implement differently

Scope: `Eval/Core.hs` as on branch `lmrm`, with the compiled values of `Struct/Weights.hs` and
`Struct/Params.hs`. Everything here comes from reading the code: nothing was built, run or measured, so
the expected effects are judgement, not results. The order is by importance.

The two terms: the king attack (`kingSafe` / `ksSide`) and the passed pawns (`passPawns` /
`perPassedPawn`). Both are the largest non linear terms of the eval, both hang on one single weight, and
both have details in the feature itself that I think are wrong, not only untuned.

## 1. King attack: `kingSafe`, `ksSide`, `attCoef`

### What the code does

For each side, as attacker:

- no queen: the term is 0 (`myq == 0`);
- per piece type (P, N, B, R, Q, K) the union of its attacks is intersected with the 8 neighbours of the
  enemy king; a type that touches the zone counts 1 in `c` and adds `w * 2^(min 4 y - 1)` to `q`, with
  `y` the number of zone squares it touches and `w` = 1, 3, 3, 5, 7, 3;
- index = `c * q / 4 + c + 13 - freco`, with `freco` = zone squares that are not occupied by an
  unattacked piece of the defender;
- value = `attCoef` at the index: 8 zeros, then a cubic up to about 4500 units (562 cp);
- weight `ewKingSafe = (1, 0)`.

### What I do not agree with

1. **Attackers are counted by type, not by piece.** Two rooks on the zone are one attacker, a rook and
   a bishop are two. The number of attacking pieces is the strongest single predictor of a king attack,
   and here it is partly lost in the union bitboards.
2. **The quality grows exponentially with the squares one type touches**, not with the number of
   attackers: a queen alone touching 4 zone squares gives `q = 56`, the same queen touching 1 square
   gives 7. A queen next to a well defended king scores as a heavy attack.
3. **`freco` has the wrong sign for attacked squares.** `yok less (yop less mya)` is "free squares +
   attacked squares" (as the comment says: `freey + conce`), and it is subtracted. So every zone square
   the attacker controls lowers the index by one step, and every unattacked defender piece next to the
   king (the pawn shield f2, g2, h2) raises it. I would expect the opposite for both: what should lower
   the danger is the number of safe flight squares (empty or enemy occupied, and not attacked), and
   attacked squares should raise it. The edge effect (fewer neighbours on the back rank) is the only
   part of `freco` that points the right way.
4. **The defence is not in the term at all.** A zone square attacked once and defended three times
   counts like one defended only by the king. Engines with a comparable king danger get most of the
   value from "weak" zone squares: attacked, and defended by nothing but the king (or the queen).
5. **The queen gate is all or nothing.** Without a queen the term is 0 even for two rooks and two minors
   around the king; with a queen it is full, whatever else is left. It also makes the eval jump at a
   queen exchange offer.
6. **The term cannot be tuned.** The inner constants (type weights, shifts, `ksShift`, the cubic) are
   hand set, and the only weight is the integer 1 in the middle game and 0 in the end game: the fit can
   choose 0, 1 or 2. The same resolution problem holds for `ewSpace (1, 0)`, `ewKingOpen (3, 1)`,
   `ewKingPlaceCent (4, 1)` and `ewPassPawnLev (-1, 8)`: features with a large inner scale and a weight of
   a few units.

### What I would do

Keep the idea "danger units, then a convex map", but make the units countable and the map fitted.

Danger units of the attacker (all small integers, so they can be exported as columns):

- `nAtt`: number of attacking pieces (not types) with at least one attacked square in the zone, by
  piece type: 4 counts (N, B, R, Q); pawns and the king as two more;
- `nSq`: number of zone squares attacked, capped linear (not `2^y`);
- `nWeak`: zone squares attacked by the attacker and defended only by the king, plus those attacked
  twice and defended once (needs the bitboard of squares attacked twice, see below);
- `nFlight`: safe flight squares of the king (not own occupied, not attacked): enters with a negative
  coefficient;
- `noQueen`: attacker without queen: a negative offset instead of the gate;
- later the safe checks and the shelter / storm of part 2, in the same sum.

Zone: the 8 neighbours plus the 3 squares two ranks in front of the king (for a king on the back rank
the squares where the attack really lands: f3, g3, h3 for Kg1).

`danger = sum coef_i * unit_i`, `value = T(danger)`.

Fit, in two steps that fit the existing tooling:

1. With the inner coefficients fixed, bucket `danger` (for example 16 buckets) and give each bucket its
   own weight: one-hot features, linear in the weights, so `TuneSGD` and `hv_fit.py` learn the shape
   `T` instead of the hand made cubic. Monotony can be forced afterwards (isotonic smoothing), and the
   end game gets its own shape instead of weight 0.
2. The inner coefficients: export the units as columns (like the terms in `-X`) and fit
   `T(sum coef * unit)` in Python on the residual of the rest of the eval (Gauss-Newton or plain
   gradient steps on a handful of coefficients); alternate with step 1 once or twice.

Needed data: `MyPos` has only the union of attacks per piece type. For `nAtt` per piece the eval must
loop over the pieces (the slider attacks are computed per piece in `posLazy` anyway and then OR-ed). The
cheap way is to collect there, while OR-ing, a second bitboard per side "attacked at least twice"
(`att2 |= att .&. new; att |= new`). That one bitboard serves `nWeak`, the safe checks and a better
"hanging" in `enPrise`.

Immediate and cheap, without a redesign: divide `maxks` (and `spf` of the space term) by 16 and
multiply the weight by 16. The eval stays identical (check with `TuneSGD -X`), but the fit gets a
resolution of 6% instead of 100%.

## 2. Passed pawns: `passPawns`, `perPassedPawn`, `perPassedPawnOk`

### What the code does

Per passed pawn a product: `pmax(rank)` (quadratic, `epPassA/B/C`) times factors for the king distances
to the stop square, own and enemy pieces on the path, own and enemy control of the path squares, with a
rook or queen behind counting as control of the whole path. A pawn that is attacked, not defended and
whose owner is not to move gets the flat `epPassMin`. One weight: `ewPassPawnLev = (-1, 8)`.

### What I do not agree with

1. **One weight for a product of 9 hand set parameters.** The fit can only move the level. Its answer
   for the middle game is -1: the whole structure (blockers, control, rook behind) is switched off there
   and slightly inverted, and the PST plus `ewAdvPawn5/6` (-177, -414: "advanced but not passed") carry
   the passed pawn alone. That is the fit saying that the shape does not match the data in the middle
   game, and it cannot say which part.
2. **Blockers are counted on the whole path with the same weight.** `mblo` and `yblo` are popcounts
   over `way`: an enemy knight on the promotion square five squares away costs the same as one on the
   stop square, and two far blockers cost more than a real blockade. What matters is the stop square
   (can the pawn advance now, and is the advance safe), then the rest of the path.
3. **Own pieces on the path are a penalty whatever they are.** For a rook in front that is right; for
   the own king in front of the pawn in an end game it is the winning formation.
4. **Rook behind has no line of sight.** `rookBehind = behind .&. (rooks .|. queens)` takes any rook or
   queen on the file behind the pawn, also behind another pawn or piece, and then grants control of the
   whole path, including the squares behind an enemy blocker.
5. **The flat `epPassMin` for an attacked, undefended pawn.** It is a jump: a pawn on the 7th drops
   from about `pmax = 350` to 30 as soon as one enemy piece attacks it, whatever its rank and whatever
   else holds. The same fact is already charged by `ewEnpHanging`, `ewWepTotal` and `ewWepAttacked`.
6. **No relation between passed pawns.** Connected or protected passed pawns are worth much more than
   two single ones; here each pawn is alone (only `ewIsolPassed` looks at neighbours, as a penalty).

### What I would do

Break the product into additive features, each with its own mid / end weight, indexed by the relative
rank `r` of the pawn (ranks 2 and 3 merged, so 4 or 5 rank classes). The pattern is the one of Ethereal
and similar tuned engines, and it is linear in the weights, so the existing fit handles it:

| Feature | Index | Meaning |
|---|---|---|
| `passBase` | rank | passed pawn of that rank |
| `passCanAdv` | rank | stop square empty |
| `passSafeAdv` | rank | stop square empty and not attacked by the opponent (or defended) |
| `passFreePath` | rank >= 5 | no enemy piece on the path and no enemy attack on it |
| `passOwnK` | distance 1..7 | own king distance to the stop square, times a rank factor |
| `passOppK` | distance 1..7 | enemy king distance to the stop square, times a rank factor |
| `passConn` | rank | defended by an own pawn or with an own pawn beside it |
| `passRookOwn`, `passRookOpp` | - | rook or queen behind, with line of sight |

Notes:

- Line of sight: `rAttacs (occup p) sq .&. behind .&. (rooks p .|. queens p)`, one lookup per passed
  pawn, only when a rook or queen is on the file at all.
- King distances as two separate features (own and enemy) instead of the clipped difference `kdDist`:
  the two distances do not have the same value, and the fit can find that out.
- The stop square and path tests use `myAttacs` / `yoAttacs` as now; with the "attacked twice"
  bitboard of point 1, "safe" can become "not attacked, or attacked once and defended".
- Drop `epPassMin`; if the drop test says something is lost, a feature "attacked, not defended,
  opponent to move" by rank brings it back with a fitted value.
- About 30 weights instead of 1 weight and 9 parameters; the cost per pawn is about the same (the
  popcounts over `way` go away, one rook lookup is added).

A cheaper first step that keeps the current function: export the factors of the product per pawn and
fit `epPass*` by a non linear fit in Python. I would not stop there: points 2 to 4 are defects of the
features, which no parameter value repairs.

Check before and after, as in `AGENTS.md`: the term is not an identity refactoring, so the proof is the
drop test (`hv_fit.py --drop` of `ewPassPawnLev`, then the refit with the new features), the colour
symmetry test of `TUNING.md` 3 d), and games.

## Noticed in passing

Not part of the two terms, but found while reading:

- **`pawnEndGame`, lines 1186 to 1195: square instead of distance.** `mescds` is a list of
  `(square, distance)`; `mim = fst $ minimumBy (comparing snd) mescds` takes the *square* of the nearest
  pawn, and `mim`, `miy` are then used as distances: in the race comparison (`mim < miy`,
  `mim > miy + 1`) and in `distMalus`. It should be `snd`. Effects as read: for White the score of an
  escaped pawn falls by 64 cp with every step forward (for Black it rises), and with escaped pawns on
  both sides the comparison is between square numbers of a white and a black pawn. The search hides
  most of it, because the promotion itself is found a few plies later. The special evals are outside
  the tuning export (`posExactEval`), so the symmetry test of `TUNING.md` 3 d) would not see it: it
  needs `posEval` on flipped positions. Smaller points in the same function: the distance of a pawn on
  its start rank ignores the double step, and an own king on the path is not considered.
- **Mobility** (`mobDiff`) is linear in the popcount of the union of attacks per piece type. The value
  of mobility is strongly concave per piece (the step from 0 to 2 squares of a knight is worth several
  times a step at 6 squares), so a piece that is nearly trapped is undervalued and the doubled squares
  of two rooks are lost. A count per piece and one weight per (piece type, count) is linear in the
  weights and would also cover "trapped pieces" of the candidate list. This was my third candidate.
- **`evalBishops`**: the bad bishop feature counts the pawns of both colours on the bishop's squares.
  Own pawns there restrict the bishop; enemy pawns there are its targets, mainly in the end game. Two
  features (own, enemy) would let the fit separate them.
