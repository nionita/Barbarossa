# Eval review, part 2: the two most important terms that are missing

Scope and limits as in part 1 (`EVAL-REVIEW-EXISTING.md`): read from `Eval/Core.hs` on branch `lmrm`,
nothing built or measured. The code below is a sketch in the style of the module, not compiled.

Both terms are about the king. That is on purpose: the king attack is where the eval is thinnest
(one integer weight, see part 1), and it is the part of a hand crafted eval where the search helps
least, because the damage is many plies away. Neither term is in the candidate list of `TUNING.md` 3 b).

## 1. Safe checks

### What is missing

The king attack counts attacks on the neighbours of the king. It does not know whether the attacker
can give check at all. A queen and a knight that have a safe checking square each are a different
position from the same pieces without one, with the same zone attacks. In engines with a comparable
king danger this is one of the largest single contributions to it, and the quiescence search does not
replace it: it looks at captures, and the checks that matter here are quiet moves.

### Definition

For the attacker (me) against the king of the defender (yo), per piece type:

- checking squares: the squares from which that piece type attacks the king, computed from the king
  square (`nAttacs yks`, `bAttacs occ yks`, `rAttacs occ yks`, queen: both sliders);
- reachable: in the attack set of my pieces of that type (`myNAttacs` ...), not occupied by my pieces;
- safe: not attacked by the defender. Better, with the "attacked twice" bitboard of part 1: not
  attacked, or attacked only by the king and by me twice (the contact check that cannot be taken).

Feature per type: number of safe checking squares, capped at 2 (the second check adds little, the
tenth nothing).

### Sketch

```haskell
------ Safe checks ------

safeChecks :: MyPos -> EvalWeights -> MidEnd -> MidEnd
safeChecks p !ew = mad (ewSafeCheckN ew) nd .
                   mad (ewSafeCheckB ew) bd .
                   mad (ewSafeCheckR ew) rd .
                   mad (ewSafeCheckQ ew) qd
    where !mks = kingSquare (kings p) $ me p
          !yks = kingSquare (kings p) $ yo p
          (!mn, !mb, !mr, !mq) = scSide (occup p) yks (me p) (yoAttacs p)
                                        (myNAttacs p) (myBAttacs p) (myRAttacs p) (myQAttacs p)
          (!yn, !yb, !yr, !yq) = scSide (occup p) mks (yo p) (myAttacs p)
                                        (yoNAttacs p) (yoBAttacs p) (yoRAttacs p) (yoQAttacs p)
          !nd = mn - yn
          !bd = mb - yb
          !rd = mr - yr
          !qd = mq - yq

-- Safe checking squares per piece type of the attacker (capped at 2) against the king on ksq
scSide :: BBoard -> Square -> BBoard -> BBoard -> BBoard -> BBoard -> BBoard -> BBoard
       -> (Int, Int, Int, Int)
scSide !occ !ksq !own !oatts !na !ba !ra !qa = (cnt nc, cnt bc, cnt rc, cnt qc)
    where !safe = complement $ own .|. oatts
          !kn = nAttacs ksq
          !kb = bAttacs occ ksq
          !kr = rAttacs occ ksq
          !nc = na .&. kn .&. safe
          !bc = ba .&. kb .&. safe
          !rc = ra .&. kr .&. safe
          !qc = qa .&. (kb .|. kr) .&. safe
          cnt = min 2 . popCount
```

To add: 4 weights in `EvalWeights` / `Struct/Weights.hs` (start at 0, so the eval is unchanged until
fitted), the term in the list of `evalTermSum`, the columns in the export.

### Points to decide

- **Side to move.** A safe check of the side to move can be played now; one of the other side is a
  threat that can still be parried. `enPrise` already is a term for the side to move only, so two
  weight sets (mine / yours) instead of the difference are in the style of the eval. I would start with
  the difference and test the split as a second step.
- **Occupancy.** A queen in front of its own rook or bishop hides the check behind it; `occ` without
  the attacker's queens (x-ray) finds a few more. Small, later.
- **Linear first, then inside the danger.** As four linear features the term can go through the score
  test of `TUNING.md` 3 b) (gradient at weight 0, in Python on exported columns) before any Haskell,
  and through the normal fit. Its natural place afterwards is the danger sum of the king attack of
  part 1: a safe check with three attackers around the king is worth far more than one alone, which a
  linear feature cannot say.
- **Cost.** Two slider lookups per side from the king squares with the full occupancy. `kingPlace`
  already does two per side with the pawns only occupancy (`ewKingOpen`), so the order of cost is
  known; the four bitboards could also live in the lazy bits next to the attack sets.

## 2. Pawn shelter and pawn storm on the king's files

### What is missing

What exists about the pawns in front of the king:

- `ewKingPlaceCent`: king near a1/b1 or g1/h1 (and mirrored), times own pawns on the 2nd and 3rd rank
  of the files a-c or f-h, times the opponent's material, only when the opponent has a queen. It is a
  corner bonus more than a shelter: a king on e1, d2 or c2 has none, the files are fixed and not the
  king's own, and which of the three pawns is missing does not matter.
- `ewKingOpen`: the rays from the king over the pawns of both colours, times the opponent's rooks and
  queens.
- `ksSide`: own unattacked pieces next to the king raise the danger index (part 1, point 3).

Missing completely: the **enemy pawns** in front of the king. A pawn storm (g4, h4, h5 against a king
on g8) and the lever it prepares, an enemy pawn on the 6th rank next to the king, and the difference
between a storm pawn that is blocked by a shelter pawn and one that is not: none of it is in the eval,
and the PST cannot have it, because it depends on where the king is. Missing as well: the shelter as a
function of the king's own file, with the half open and open files next to the king. `TUNING.md` (idea
1 and 6) already asks for "a simpler shelter feature plus PST" in place of `ewKingPlaceCent`; this is
that feature, with the storm as the part that is new.

### Definition

For each side, the three files around the king: the king file clipped to b..g, and its two neighbours.
Per file, seen from the king's own side (rank 1 = own back rank):

- shelter: relative rank of the own pawn nearest to the back rank, among the pawns not behind the king;
  0 = no own pawn on the file;
- storm: relative rank of the enemy pawn nearest to the back rank on that file; 0 = none;
- blocked: the enemy pawn stands directly in front of the own pawn (the storm pawn cannot advance
  and does not open the file).

Features, counts over the three files (the same for both sides, the difference goes into `mad`):

| Feature | Index | Count of files with |
|---|---|---|
| `kShelter` | 0, 2, 3, 4+ | own pawn missing / on the 2nd / 3rd / further |
| `kShelterEdge` | - | the same file being a or h with the pawn missing (worth less than a centre file) |
| `kStorm` | 0, 3, 4, 5, 6+ | enemy pawn missing / on relative rank 3 (close) ... 6 and more (far) |
| `kStormBlocked` | - | enemy pawn blocked by the shelter pawn |
| `kOpenFile` | - | no pawn of either colour on the file |

About 12 weights, mid and end each; the end values will fit near zero by themselves. All features are
counts, so the term is linear in the weights.

### Sketch

To avoid the White / Black pairs of functions, the bitboards of Black are mirrored vertically
(`byteSwap64`, `BBoard` is a `Word64`) and one function for a king on the lower side does the work.

```haskell
------ Pawn shelter and storm on the king's files ------

data KFront = KFront !Int !Int !Int !Int !Int !Int !Int !Int !Int !Int !Int

-- From the point of view of White: own pawns, enemy pawns, king square. For Black call it with
-- byteSwap64 of the pawn bitboards and ksq `xor` 56
kingFront :: BBoard -> BBoard -> Square -> KFront
kingFront !ownp !oppp !ksq = foldl' perFile kf0 [kf - 1, kf, kf + 1]
    where !kf    = max 1 $ min 6 $ fileOf ksq
          !front = complement $ rowsBelow (rankOf ksq)	-- the king's rank and all ranks above
          perFile acc f = acc'
              where !fbb  = fileBB f
                    !own  = ownp .&. fbb .&. front
                    !opp  = oppp .&. fbb
                    !sr | own == 0  = 0
                        | otherwise = rankOf (firstOne own) + 1		-- nearest to the back rank
                    !tr | opp == 0  = 0
                        | otherwise = rankOf (firstOne opp) + 1
                    !blk  = own /= 0 && tr == sr + 1
                    !open = (ownp .|. oppp) .&. fbb == 0
                    acc'  = ...		-- increment the counters of sr, tr, blk, open, edge
```

`rowsBelow r` is the mask of the ranks under rank `r` (`(1 << (8 * r)) - 1`). The term itself, like
the others:

```haskell
kingFrontDiff :: MyPos -> EvalWeights -> MidEnd -> MidEnd
kingFrontDiff p !ew = mad (ewKShelter0 ew) (ms0 - ys0) . ... . mad (ewKOpenFile ew) (mop - yop)
    where (KFront ms0 ..., KFront ys0 ...)
              | moving p == White = (kingFront mp yp mks, kingFront (byteSwap64 yp) (byteSwap64 mp) (yks `xor` 56))
              | otherwise         = (kingFront (byteSwap64 mp) (byteSwap64 yp) (mks `xor` 56), kingFront yp mp yks)
          mp = pawns p .&. me p
          yp = pawns p .&. yo p
```

### Points to decide

- **Dependence on the attacker's material.** `ewKingPlaceCent` multiplies with the opponent's material
  and needs his queen. The mid / end pair of the weights already scales with the pieces on the board
  (`gamePhase` counts both sides); I would start with that alone and add "times the opponent has a
  queen" as a second set of features only if the bias tables (`hv_bias.py`, by opponent queen) show a
  residual.
- **Against `ewKingPlaceCent` and `ewKingOpen`.** Once fitted, the drop tests of finding 39
  (`hv_fit.py --drop`) say whether the two old terms still carry anything; the expectation is that
  `ewKingPlaceCent` goes (the corner part is PST), `ewKingOpen` stays for the diagonals.
- **Into the danger sum.** As for the safe checks: missing shelter and a close storm are worth much
  more when pieces attack too. The linear features come first (cheap, fitted with the existing
  tools); afterwards their weighted sum can enter the danger index of the king attack as one more
  unit.
- **Cost.** The term depends only on the pawns and the two king squares. If its cost shows in the
  speed check, it is the first candidate for a small cache by pawn structure and king squares; before
  that, three files with two `firstOne` each per side are less work than `kMatBonus` does now.
- **Symmetry.** The mirror by `byteSwap64` makes the term colour symmetric by construction; the test of
  `TUNING.md` 3 d) confirms it. The file mirror (a <-> h) holds too, because the clipped king file and
  its neighbours are symmetric.

## What I considered and put behind these two

- Doubled pawns: missing completely, cheap and linear, but small in every tuned eval I know, and
  already on the list of `TUNING.md`.
- Knight (and bishop) outposts: also on the list; partly overlapping with `ewWeakSq` / `ewWeakSqA`.
- Threats against the side not to move: mostly resolved by the quiescence search, which is why the
  `enPrise` term for the side to move only is reasonable.
- Mobility per piece with a table: an existing term done differently, see the end of part 1.
