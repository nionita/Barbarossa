# Eval terms for future forks

The question: should the eval have terms for forks that are one move away, for example "the side to
move can push a pawn and attack two pieces"? The check whether such a move exists should be cheap, so
the cost stays small.

Short answer: worth trying, starting with the pawn push fork, with three caveats (side to move, safety
of the push, a value well below a piece). Nothing here is measured: it is judgement, the code is a
sketch and not compiled.

## Why it fits this engine

- It is a known blind spot: `TUNING.md`, Problems: "The eval does not see forks or one-move tactics
  (pawn push forks, check and capture)". A fork is a three ply tactic (push, reply, capture), and the
  push is a quiet move, so a quiescence search of captures never sees it. At a leaf the eval is the
  only place where it can count.
- There is precedent: the classical eval of Stockfish had terms for attacks by safe pawns and for
  threats by a pawn push, and they stayed because they gained.
- The on-policy fit suits such a term. The leaf is where the own search stopped, so the fitted weight
  is the value that the search misses, not the full tactical value. That is the opposite of the case
  of finding 17, where foreign search labels counted the captures twice.
- The futility test uses the static eval of the parent. A side that has the fork available gets the
  bonus there, so the fork push is less often pruned as a hopeless quiet move.

## The caveats

1. **Side to move.** If the side to move has the fork, it can play it now and usually wins something.
   If the other side has it, the side to move normally uses its move to parry. So two features with
   own weights (mine, yours), as `enPrise` is a term of the side to move, not a difference.
2. **Safety of the push.** The fork is real only if the pawn survives on the push square:
   - the square is empty (double push: the square in between too);
   - not attacked by an enemy pawn;
   - not attacked at all, or defended. One of the forked pieces (bishop, queen, king) may be able to
     take the pawn itself; if the pawn is defended, that is an exchange in our favour.
   Not covered by cheap masks: a pinned pawn, the en passant capture after a double push.
3. **Value.** The opponent often escapes: he moves one piece with check, with a capture or with a
   threat of his own, or one forked piece defends the other. The fitted weight will be well below the
   value of the cheaper piece. The term also makes the static eval jump with the side to move, which
   the cp margins of the search (futility, reverse futility, null move) feel. So the weight should
   come from the fit, not from the piece values.

## Cost

No gate is needed: the detection is a handful of bitboard operations without a loop. For White:

```haskell
push   = (myPawns `unsafeShiftL` 8) .&. complement (occup p)   -- squares of a single push
tgt    = yo p `less` pawns p                                    -- enemy pieces, the king too
both   = ((tgt .&. notFileA) `unsafeShiftR` 9)                  -- squares from which a pawn
     .&. ((tgt .&. notFileH) `unsafeShiftR` 7)                  -- attacks a piece on each side
safe   = complement (yoPAttacs p) .&. (complement (yoAttacs p) .|. myAttacs p)
forkSq = push .&. both .&. safe                                 -- nonzero: a fork is available
```

`notFileA`, `notFileH` are the complements of the edge files (the names are assumed). The double
push adds one line (`push` of the pawns on the 2nd rank, shifted once more, both squares empty). Black
is the mirror image; with `byteSwap64` of the bitboards one function serves both colours, as proposed
for the shelter term in `EVAL-REVIEW-MISSING.md`.

Features, all counts or flags, so the term is linear in the weights:

| Feature | Value |
|---|---|
| `forkPawnMe` | the side to move has a safe pawn push fork (0 / 1) |
| `forkPawnYo` | the other side has one (0 / 1) |
| `pushThreatMe`, `pushThreatYo` | number of enemy pieces that a safe pawn push would attack (one piece is enough) |

The last pair is the wider pattern: a safe push that attacks one piece gains a tempo and space. It is
far more frequent than the true fork, and in other engines it carried most of the value. It uses the
same bitboards with an "or" in place of the "and" in `both`. If the fork alone turns out to be too
rare to fit, this is the form that remains.

A later refinement, only if the first form shows a gain: split the fork by its cheaper target (two
minors; at least a rook on both sides; the king as one target, which is a check and forces the reply).

## Knight forks

The natural second step, and the more decisive one when the king is a target. They cost more: for
each knight the safe landing squares (at most 8), and per landing square the knight attacks from
there against the valuable targets (king, queen, rook, undefended pieces), two or more of them. That
is a loop, so here a gate makes sense: a knight exists and the opponent has at least two such targets.
Bishop and rook forks, skewers and discovered attacks are a different order of cost and I would leave
them to the search.

## What to do first

1. The score test of `TUNING.md` 3 b), in Python on the on-policy data ("fork patterns" is already on
   that list): how often the pattern occurs at the leaves, and whether the results are better than the
   eval predicts when the side to move has it. The frequency bounds the gain: a correct term in very
   few leaves is worth little.
2. The same test for the wider push threat, and for both sides separately (mine, yours).
3. Only the winners in Haskell, with weights 0 until fitted, the symmetry test of 3 d), the fit, the
   speed number, matches at depth.

## The alternative: the search instead of the eval

The same detection can feed the search:

- the fork push as a quiet move in the first ply of the quiescence search (like checks in other
  engines), only when `forkSq` is not empty;
- no futility pruning and no late move reduction for such a push;
- ahead in the order of the quiet moves (next to what `epQThr` already does).

This resolves the tactic exactly where the eval term adds an average bonus, and it has no effect on the
static eval and its margins. It is a search change, to be tested by the search test procedure (cut
rates, 10k node games, SPRT). The two ways do not exclude each other, but I would not introduce both
at once: the eval weight fitted without the search change is too high with it.

## Expectation

A small gain, limited by how rare the pattern is at the leaves; the wider push threat has the better
chance. The risk is low: the term costs almost nothing, has weight 0 until the fit moves it, and the
score test answers most of the question before any engine code is written.
