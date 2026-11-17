# Speed and simplification ideas (for later)

Older ideas, checked against the code of branch `lmfl` (2026-11-17). Each change must leave the engine
unchanged (see "Verification of Changes" in AGENTS.md) unless it is a search idea; measure speed with direct
timers, not with the GHC profiler (PROFILING.md).

## Open

1. **Per-colour constant records.** Many functions branch on `moving p` to pick colour constants (last rank,
   7th rank, pawn push direction, castle masks, promotion rank, ...). Instead: two static records, one for
   White and one for Black, and `MyPos` holds references to them (`meConst`, `yoConst`), swapped by
   `makeMove`. Removes the colour branches and makes the code simpler. Cost: two more pointer fields copied
   per move. Mainly a simplification; the speed gain is uncertain, measure it.

2. **Generate moves directly into vectors.** Move ordering already sorts in an unboxed vector
   (`MovesToSort`, `Moves/History.hs`), but the generators build lists first, which are then copied into the
   vector. Generating directly into a (mutable) unboxed vector would save the list allocations. Largest
   expected gain of this list, also the largest change (all generators in `Moves/Core.hs`). Idea 4 is the
   cheap first step.

4. **Move coding and the generation pipeline** (narrower look at the old "simple moves + table for the rest"
   idea). Where the cost really is:
   - The coding itself is not the problem: normal moves are already decoded first and cheaply
     (`moveIsNormal` is one mask and compare, the first guard in `doFromToMove` and `movePiece`).
   - Allocations do not depend on the coding: `Move` is a newtype over `Word16`, boxed in every list
     anyway. They come from the generator pipeline, e.g. in `genMoveNCapt`: `bbToSquares` (unfoldr, a
     `Just` and a tuple per square unless fused), `srcDests` (a `(from, to)` tuple per move),
     `moveFromTo`, `map (moveAddPiece ..)`, `++`, then a second `map (moveAddColor c)` over everything.
   - Cheap fix, same coding: compute the tag `colour + piece` once per piece type and build each move in
     one step, `Move (tag .|. from<<6 .|. to)`, directly from the destination bitboard (no tuples, no
     second `map`). Check in GHC Core (`-ddump-simpl`) which lists are fused today, and measure
     allocation (`+RTS -s`) and nodes/second before and after.
   - Keep colour and piece in normal moves: they are needed by the history address (`moveHisAdr`,
     `moveHisOfs`), `moveChecksDirect`, the threat classification and the legality check of TT and killer
     moves (protection against hash collisions); without them each would need a board lookup.
   - Rare special moves: a table could replace the case on `src`/`dst` for the rook of a castle and the
     `fromSquare` reconstruction of promotions. Small gain, rare moves. Micro: `fromSquare` tests
     `moveIsPromo` (2-4 compares) before the simple case; test `moveIsNormal` first.

3. **Information from earlier plies (search stack).** The search passes only the two previous moves down
   (`prvmv`, `prvmv2`). A per-ply array (as in Stockfish) would give access to more of the path, above all
   the static eval two plies back ("improving" flag for pruning and LMR). Note: in Haskell the recursion is
   the stack and the position list in `Status` is cheap; the value here is the search idea, not fewer
   allocations. Test as a search idea (skill search-idea-test).

## Dropped

- **`pawnBeatAt` / `pawnBlockAt` with `myPAttacs`**: `pawnBeatAt` already returns early via `myPAttacs`;
  both functions run only in check evasions, which are rare, so the gain would be negligible.
- **Beam search**: in alpha-beta this means a hard limit on the number of moves searched per node (forward
  pruning). Barbarossa's adaptive LMR already does the soft, re-searchable form of this; a hard beam misses
  tactics, the known weakness of forward pruning.
