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
   expected gain of this list, also the largest change (all generators in `Moves/Core.hs`).

3. **Information from earlier plies (search stack).** The search passes only the two previous moves down
   (`prvmv`, `prvmv2`). A per-ply array (as in Stockfish) would give access to more of the path, above all
   the static eval two plies back ("improving" flag for pruning and LMR). Note: in Haskell the recursion is
   the stack and the position list in `Status` is cheap; the value here is the search idea, not fewer
   allocations. Test as a search idea (skill search-idea-test).

## Dropped

- **`pawnBeatAt` / `pawnBlockAt` with `myPAttacs`**: `pawnBeatAt` already returns early via `myPAttacs`;
  both functions run only in check evasions, which are rare, so the gain would be negligible.
- **Move coding with tables for special moves** (ep, castle, promotions by index): moves are already 16 bits,
  with ep, castle and promotions coded inline; a table lookup would add an indirection and gain nothing.
- **Beam search**: in alpha-beta this means a hard limit on the number of moves searched per node (forward
  pruning). Barbarossa's adaptive LMR already does the soft, re-searchable form of this; a hard beam misses
  tactics, the known weakness of forward pruning.
