# A KPK bitbase (2026-11-17, design only)

Read only: `Eval/Core.hs` (`specialsOf`, `pawnEndGame`, `evalNoPawns`, `scoreToMate`), `Moves/Internal/Moves.hs`,
`Moves/Internal/Magics.hs`, `Struct/Struct.hs` on `rel090`. Nothing built or run; times are estimates.
Related: `Search/TB-SYZYGY.md` (full tablebases), `Eval/PAWN-ENDGAME-BUG.md` (`pawnEndGame`).

## 1. Why and what

KPK is not decidable by a few rules (opposition, key squares, rook pawn, stalemate tricks), but it is small: an
exact table of "the side with the pawn wins / does not win" for every position costs 24 KB and is computed in a
fraction of a second. Today Barbarossa has no KPK knowledge: `specialsOf` sends king and pawn endings to
`pawnEndGame`, which only judges pawn races (a pawn outside the square of the enemy king), otherwise the normal
eval decides, which cannot tell a won from a drawn KPK.

Effort: about 150-250 lines in a new module `Eval/KPK.hs`, about one day with the full check of section 5.

## 2. Index (normalised positions, 1 bit each)

Normalisation, so that the strong side (the one with the pawn) is white and the pawn on the files a-d:

- the pawn is black: flip the ranks of all three squares (`sq xor 56`) and swap the side to move;
- the pawn is on files e-h: mirror the files of all three squares (`sq xor 7`).

```haskell
-- stm: 0 = the strong side moves; wk, bk: strong / weak king; psq: pawn on files a-d, ranks 2-7
kpkIndex :: Int -> Int -> Int -> Int -> Int
kpkIndex stm wk bk psq = wk .|. bk `unsafeShiftL` 6 .|. stm `unsafeShiftL` 12
                       .|. (psq .&. 7) `unsafeShiftL` 13 .|. (6 - psq `unsafeShiftR` 3) `unsafeShiftL` 15

kpkSize :: Int
kpkSize = 2 * 64 * 64 * 4 * 6       -- 196608 positions = 3072 Word64 = 24 KB

kpkWin :: Int -> Int -> Int -> Int -> Bool
kpkWin stm wk bk psq = kpkBits `unsafeAt` (i `unsafeShiftR` 6) `testBit` (i .&. 63)
    where !i = kpkIndex stm wk bk psq
```

Only "win" is stored: everything else (draw, invalid) is a 0 bit. The probe is a few shifts and one memory read.

## 3. Generation (retrograde, by passes)

States per position during the generation (an `STUArray Int Word8`): invalid, unknown, draw, win.

**First classification** (squares after normalisation, `kAttacs`, `pAttacs` from `Moves.Internal.Moves`):

- invalid: the kings on the same or adjacent squares, a king on the pawn square, or the strong side moves while
  the pawn attacks the weak king;
- win: the strong side moves, the pawn is on rank 7, the promotion square is free of both kings, and it is not
  attacked by the weak king or it is defended by the strong king;
- draw: the weak side moves and has no legal king move (stalemate; its targets minus the squares attacked by the
  strong king and the pawn are empty), or it can take the pawn and the strong king does not defend it;
- unknown: all the rest.

**Passes** over the unknown positions until a pass changes nothing:

- strong side to move: successors are the king moves (not to a square next to the weak king, not onto the pawn),
  the pawn push by one (target free) and by two from rank 2 (both squares free); one successor won -> win; all
  successors draw -> draw;
- weak side to move: successors are the king moves (not next to the strong king, not attacked by the pawn, the
  capture of the defended pawn is illegal anyway); one successor drawn -> draw; all successors won -> win;
- what is still unknown at the end is a draw.

```haskell
kpkBits :: UArray Int Word64
kpkBits = packBits $ runSTUArray $ do
    a <- newArray (0, kpkSize - 1) stUnknown
    forM_ [0 .. kpkSize - 1] $ \i -> writeArray a i (initial i)
    let pass = do
            n <- foldM (\ !c i -> do
                    s <- readArray a i
                    if s /= stUnknown
                       then return c
                       else do
                           s' <- classify a i          -- reads the states of the successors
                           if s' == stUnknown then return c else writeArray a i s' >> return (c + 1)
                ) (0 :: Int) [0 .. kpkSize - 1]
            when (n > 0) pass
    pass
    return a
```

Cost: 15-25 passes over 196608 positions with at most 8 king moves plus 2 pawn moves, some 30 million steps:
estimated 50-200 ms compiled (strict loops, unboxed arrays; `foldM` over a list fuses or must be written as
a loop). The generator is plain Haskell over squares, no `MyPos` and no move generator needed.

## 4. Where the table comes from: three variants

1. **Top level CAF, computed at run time (recommended).** `kpkBits` as above. Laziness gives "computed only when
   needed": an engine that never reaches KPK never pays; the first KPK eval of a process pays once. To keep that
   out of a timed search, force it at `isready` (`seq` on one word of `kpkBits`); SelfPlay processes pay it
   once each, negligible. No generated file, no build change except the module list.
2. **Generated static module** `Eval/KPKData.hs`, as `Moves/Internal/Magics.hs` (whose magics came from a C
   program): a SelfPlay switch or a small program prints `kpkBits = listArray (0, 3071) [0x..., ...]`, 3072
   literals. Zero run time cost (the array is built from the literals in microseconds at first use), the same
   24 KB in the binary. The risk of a stale file is small (KPK does not change); a test compares the generated
   and the compiled table. An `Addr#` string literal would avoid even the array building, not worth the
   unsafe code.
3. **Template Haskell**: the generator in a module of its own (stage restriction), spliced into the data module;
   the package already depends on `template-haskell`. It runs at every compile of the splicing module and makes
   the build more fragile; no gain over 2.

Variant 1 first; 2 only if the startup or first use cost shows up anywhere.

## 5. Use in the eval

`specialsOf` (`Eval/Core.hs`): with exactly one pawn, before `pawnEndGame` (the bitbase is exact, the race is not):

```haskell
peg | kings p .|. pawns p == occup p = [kpkEval | popCount (pawns p) == 1] ++ [pawnEndGame]
    | otherwise                      = []

kpkEval :: SpecialEval
kpkEval p
    | kpkWin stm wk bk psq = Score (if strongMoves then sc else -sc)
    | otherwise            = Score 0
    where -- normalise (section 2) from the colour of the pawn and moving p
          sc = kpkWinScore + 10 * relRank psq   -- relative rank of the pawn, 1..6
```

Score points:

- The special scores are in centipawns from the point of view of the side to move (as `pawnEndGame`, which
  uses `promoBonus = 1000`).
- **Progress**: without the rank term all winning moves are equal and the search may shuffle; the rank term
  (and optionally a small term for the strong king near the pawn) makes the pawn walk. Mates are found by the
  search once the queen is there.
- **Promotion must stay attractive**: the win score must be below the eval just after the promotion. KQK goes
  through `scoreToMate` with `(winBonus + ...) * 10`, `winBonus = 1200`, i.e. over 10000 cp: any KPK win score
  around 1000-2000 is far below, nothing to tune here.
- **Draw = 0**: the engine then avoids trades into a drawn KPK and seeks the won ones, which is the main gain
  (the decision is made in KPPK, KRPKR and similar, many plies before).

Side effects:

- Tuning unchanged: `posExactEval` (TuneSGD) does not use the specials.
- The engine changes only where a search reaches KPK: the node counts of `tests.fen` most probably stay
  12086912 (to check), those of the endgame set change; new references there.
- The self-play data generation changes in those games (better endgame play); record it with the round.

## 6. Checks (when implemented)

- **Complete check against Syzygy**: python-chess (jacob env) with the 3 men files `KPvK.rtbw` (a few KB): all
  196608 indices, both sides to move, invalid ones skipped; the WDL of KPK has no cursed values, so win <-> WDL 2
  for the strong side, everything else <-> not win. One script, a full proof of the table.
- Symmetry: the eval of a position and of its colour flipped / file mirrored twin must be equal and opposite.
- Classical positions (opposition, rook pawn with the defending king in the corner, the key squares): by hand.
- Strength: a few Elo at most overall (KPK decisions are rare), more visible in an endgame opening set; a
  non-regression SPRT is enough.

## 7. Later

The same scheme (normalise, index, passes) works for other small endings, but the index grows quickly: KRKP,
KQKP (the rook pawn / bishop pawn on the 7th draws) or KBPK (wrong bishop) would each need 64^3 x pawn squares
x 2 = 12-25 MB as states during generation and 1.5-3 MB as bits, and generation times in seconds. Worth it only
if one of them shows up as a weakness; KPK is the one with the best cost/benefit.
