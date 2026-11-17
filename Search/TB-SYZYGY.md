# Syzygy tablebase probes (2026-11-17, design only)

Read only: `Struct/Struct.hs`, `Moves/Internal/Base.hs`, `Search/Core.hs`, `Hash/TransTab.hs`, `Eval/Core.hs`,
`Main/Barbarossa.hs`, `Barbarossa.cabal` on `rel090` 5d53bec. Nothing built or run. Effort figures are estimates.

## 1. Summary

| Way | Work | Risk | Speed |
|---|---|---|---|
| A. FFI to Fathom (C, MIT licence), compiled in by cabal `c-sources` | 2-4 days: C shim, ~250 lines Haskell, options, search hooks | low: the probing code is mature | C speed |
| B. Port of the Syzygy decoder to Haskell | 3-5 weeks: ~1200-1800 lines | high: format details, DTZ subtleties | probably 1.5-3x slower per probe, unimportant (probes are rare and memory bound) |

Recommendation: **A first.** The search integration (sections 4-6) is the same for A and B and is where the Elo
is; B is only worth it if a pure Haskell build is a goal in itself. The engine side fits well: bitboards with
a1 = 0 (as Fathom and Syzygy), separate piece bitboards (`rooks` without queens), side to move, 50 move counter,
castle rights and the en passant square all in `MyPos`, the `Game` monad is IO based (`liftIO` already used).

Off by default (no `SyzygyPath`): the engine stays unchanged, the perf node count (12086912; 11367370 since the floor, S53) must not move.

## 2. What a probe needs (both ways)

- **WDL** (`.rtbw`): win / cursed win / draw / blessed loss / loss for the side to move, assuming the 50 move
  counter is 0. Used inside the tree.
- **DTZ** (`.rtbz`): distance to zeroing move (capture or pawn move) under the 50 move rule. Used only at the root,
  to pick moves that really convert (a pure WDL win can be lost by the 50 move rule).
- Preconditions: no castle rights; at most `TB_LARGEST` pieces (5 for the 939 MB of 3-4-5 men, 6 or 7 with
  the large sets); en passant handled by the prober (Fathom checks it itself).

Mapping from `MyPos` (all fields exist, no conversion of squares):

```haskell
white  = occup p .&. complement (black p)
kings p, queens p, rooks p, bishops p, knights p, pawns p   -- as computed by updatePos
rule50 = get50Moves p                                       -- WDL in the tree only when 0
cast   = epcas p .&. caRiMa                                 -- must be 0
epSq   = epcas p .&. epMask                                 -- bit -> square index (0 = none), check: the bit
                                                            -- is the passed square, as Fathom wants
turn   = moving p == White                                  -- Fathom: true = white to move
```

## 3. Way A: Fathom by FFI

Fathom (`tbprobe.c`, `tbprobe.h`, `tbchess.c`, `stdendian.h`, ~3000 lines C99, MIT) is used by many engines and
compiles with MinGW gcc (Windows, the one stack brings) and gcc/clang on Linux. It maps the files itself
(`CreateFileMapping` / `mmap`), so parallel SelfPlay processes share the pages in the OS cache, important on
this machine (31 GB) with 8+ processes.

**Cabal.** `Cabal-version >= 1.8` has no common stanzas, so each executable that contains `Search.Core`
(Barbarossa, SelfPlay, and the tuning programs if they link the search) gets:

```
  C-sources:     tb/tbprobe.c, tb/tbshim.c
  Include-dirs:  tb
  CC-options:    -O2 -std=gnu99 -msse4.2
```

**C shim.** `tb_probe_wdl` and `tb_probe_root` are `static inline` in `tbprobe.h`, not linkable; a small file
exports them under fixed names:

```c
#include "tbprobe.h"
unsigned bb_tb_wdl(uint64_t w, uint64_t b, uint64_t k, uint64_t q, uint64_t r, uint64_t bi,
                   uint64_t n, uint64_t p, unsigned ep, int turn)
{ return tb_probe_wdl(w, b, k, q, r, bi, n, p, 0, 0, ep, turn); }

unsigned bb_tb_root(uint64_t w, uint64_t b, uint64_t k, uint64_t q, uint64_t r, uint64_t bi,
                    uint64_t n, uint64_t p, unsigned r50, unsigned ep, int turn, unsigned *res)
{ return tb_probe_root(w, b, k, q, r, bi, n, p, r50, 0, ep, turn, res); }

unsigned bb_tb_largest(void) { return TB_LARGEST; }
```

**Haskell module** `Search/Syzygy.hs` (explicit export list, as required):

```haskell
module Search.Syzygy (TbWdl(..), tbInit, tbLargest, tbProbeWdl, tbProbeRoot) where

foreign import ccall safe   "tb_init"       c_tb_init    :: CString -> IO CBool
foreign import ccall unsafe "bb_tb_largest" c_tb_largest :: IO CUInt
foreign import ccall unsafe "bb_tb_wdl"     c_tb_wdl     :: Word64 -> Word64 -> Word64 -> Word64 -> Word64
                                            -> Word64 -> Word64 -> Word64 -> CUInt -> CInt -> IO CUInt
foreign import ccall safe   "bb_tb_root"    c_tb_root    :: ... -> Ptr CUInt -> IO CUInt

data TbWdl = TbLoss | TbBlessedLoss | TbDraw | TbCursedWin | TbWin | TbFail

tbProbeWdl :: MyPos -> IO TbWdl
tbProbeWdl p = decode <$> c_tb_wdl w (black p) (kings p) (queens p) (rooks p) (bishops p)
                                   (knights p) (pawns p) ep (if moving p == White then 1 else 0)
    where w  = occup p .&. complement (black p)
          ep = case epcas p .&. epMask of 0 -> 0; b -> fromIntegral (countTrailingZeros b)
          decode r | r == 0xFFFFFFFF = TbFail      -- TB_RESULT_FAILED
                   | otherwise       = toEnum' r   -- 0..4 = loss .. win
```

`unsafe` for the WDL probe: no GC synchronisation, ~100 ns call overhead less. Caveat: with `-threaded -N1` an
`unsafe` call that waits for a page fault (first touch of a table block, a disk read) blocks the whole RTS,
also the UCI reader and the time check, for up to milliseconds. Acceptable for WDL (short, and the pages are
hot after a while); `tb_init` and the root probe are `safe`.

**Init.** UCI option `SyzygyPath` (string, `<empty>`), plus `SyzygyProbeDepth` (spin, 1) and `SyzygyProbeLimit`
(spin, 7); `doSetOption` knows only `Hash` today, `guiUciOptions` gets the three entries. The path goes to
`tb_init` (paths separated by `;` on Windows, `:` on Linux, Fathom handles both), the result `tbLargest` into
`Changing` / `PVStRO` so the search reads a pure `Int` (0 = no tables). SelfPlay gets a command line option (e.g.
`--syzygy <path>`) for tests and data generation. Fathom keeps global state: one `tb_init` per process, before the
first search, never while searching (the "working" check of `doSetOption` already blocks that).

## 4. Search integration: scores

Barbarossa codes mates as exactly `±mateScore` (20000) with the distance taken from the path length
(`scoreToExtern`); `nearmate` means `|s| >= 19745`, the eval is clamped to `±matesc = 19745`. TB results need a
band between the eval and the mates, not dependent on the ply (the TT stores plain 16 bit scores, the mates are
ply free for the same reason):

```haskell
tbWin :: Int
tbWin = 19500           -- WDL win; loss = -tbWin; cursed win / blessed loss = +1 / -1, draw = 0

tbScore :: TbWdl -> Int
tbScore TbWin = tbWin; tbScore TbLoss = -tbWin
tbScore TbCursedWin = 1; tbScore TbBlessedLoss = -1; tbScore _ = 0

decisive :: Int -> Bool     -- for the pruning guards which today use nearmate
decisive s = s >= tbWin - 255 || s <= -tbWin + 255
```

Points to decide:

- `matesc` should go below the TB band (e.g. 19000), else an absurd eval can pass a TB win. Inference: no real
  eval comes near, the node count should not change; to check with the perf build.
- `isPruneFutil` and `isRevFutil` test `nearmate`; with `decisive` they also stay off when alpha / beta is a TB
  score (futility with margins of a few hundred makes no sense there). Same for `insertToPvs` (root order).
- A ply free TB win does not prefer a shorter way into the TB; harmless: once the root itself is in the TB, the
  DTZ filter (section 6) makes the progress. Optionally `tbWin - absdp` like Stockfish, then it must not go to
  the TT unadjusted (Barbarossa has no TT score adjustment today, so better not).
- UCI shows `score cp 19500`; the time management (`compTime`, score drop extension) sees a big jump when a TB
  win appears; check `TIME-SCORE-DROP-REVIEW.md` logic for big scores (no extension wanted on a jump up).

## 5. Search integration: WDL in the tree

Place: `pvSearch` and `pvZeroW`, after the TT probe failed to cut, before null move / generation; not in QS
(as Stockfish, the probe costs more than the QS of such small positions).

```haskell
-- in pvZeroW, else branch, after: pos <- lift getPos
tbl <- gets (tblarge . ronly)
let pcs = popCount (occup pos)
if tbl > 0 && pcs <= tbl && get50Moves pos == 0 && epcas pos .&. caRiMa == 0
       && (pcs < tbl || d >= tbDepth)
   then do
       w <- lift $ liftIO $ tbProbeWdl pos
       case w of
           TbFail -> searchAsNow
           _      -> do
               let !sc = tbScore w
                   !tp | sc > 1 = 1 | sc < -1 = 0 | otherwise = 2  -- win: lower b., loss: upper b.
               lift $ ttStore (d + tbDepthBonus) tp sc (Move 0) 0
               tbHit
               if tp == 2 || tp == 1 && sc >= b || tp == 0 && sc < b
                  then return $! pathFromScore (trimax bGrain b sc)
                  else searchAsNow     -- e.g. TB win but beta is a mate score
   else searchAsNow
```

Remarks:

- **`get50Moves == 0`** is Fathom's condition for `tb_probe_wdl` and fits: every path into the TB region goes
  through a capture (or promotion), which resets the counter, so the entry nodes are all probed; deeper nodes
  of the same material are cut by the TT entry written at the entry. Positions reached by quiet moves inside the
  TB region are searched normally (their WDL is already known above them).
- `doMove` already knows the capture (`Exten _ True _`); the probe could be triggered from there, but the node
  needs the bound check against alpha / beta anyway, so the node start is the natural place.
- **PV nodes** (`pvSearch`): cut only on a usable bound as above; otherwise Stockfish raises alpha to the TB
  value (win) or limits the best value (loss) and searches on. For Barbarossa the simpler variant (cut or search
  as now) is enough to start.
- **TT**: depth `d + 6` (as Stockfish) keeps the entry over normal ones; move 0 (`Move 0` is the "no move"
  coding already used by `counterOf`); check that a TT entry with move 0 and `hdeep > 0` does not become a
  `ttPathMoves` / TT move (today `mttmv` takes `e` when `hdeep > 0`): needs a guard, else an illegal TT move,
  as with the type 0 TT moves that caused the illegal PVs.
- **Draw before probe**: `checkRemisRules` (repetition, 50 moves) runs in `doMove`, before the child node, so
  a TB probe never overrides a real draw.
- **Statistics**: a counter `sTbHits` in `SStats` and `tbhits` in the UCI info line (GUIs show it).
- `SyzygyProbeDepth` (d >= depth for the largest piece count only) limits disk traffic with 6/7 men; with 5 men
  in RAM, 1 is right.

## 6. Search integration: the root

If the root position itself is in the TB (no castling, pieces <= largest), `tb_probe_root` gives the DTZ
optimal move and per move WDL / DTZ (array `results`, `TB_MAX_MOVES` entries). Two ways:

1. **Filter** (as Stockfish): keep only the root moves with the best WDL (and, for wins, a DTZ that still
   converts within the 50 moves: with the counter `r50`, `dtz + r50 <= 100`), give them as `rootmvs` to
   `searchTheTree` and search as normal, with the WDL probes in the tree switched off (all positions below are
   TB positions with known result, the search should find the mate or the fastest conversion). The search then
   mates as fast as it can see and never spoils the win by the 50 move rule.
2. **Play the DTZ move directly** (no search): simplest, always correct, but the moves look strange (DTZ optimal,
   not mate optimal) and are often slower to mate.

Recommendation: 1; it reuses `rootmvs` (the root move list exists already in `ABControl` and is passed
between drafts). Reported score at the root: the TB score of the best move (or the mate when the search finds
it). `tb_probe_root` is not thread safe in Fathom; the engine searches in one thread, fine.

Conversion of the Fathom result moves: from / to / promotion piece out of the result word, matched against the
moves of `genMoves` at the root (no own move building needed).

## 7. Way B: a Haskell decoder

What has to be ported (from Fathom, MIT, not from Stockfish or python-chess, both GPL, Barbarossa is BSD3):

| Part | Content | Size (Haskell, est.) |
|---|---|---|
| File access | mmap of `.rtbw` / `.rtbz`, lazy per table, magic check | 100-150 (+ mmap package or C shim) |
| Table registry | material key (counts per piece type and colour) -> table, symmetric / mirrored lookup | 150 |
| Header parsing | piece order, norm, factor, pawn file tables, flags | 200-300 |
| Index | binomials, `KK_idx`, triangle / diagonal mirroring, leading pawn files, `ptwist` | 300-400 |
| Decompression | sym tables, canonical codes with RE-PAIR symbol pairs, block index, size table | 250-350 |
| WDL probe | capture resolution (`probe_ab`), en passant correction | 150 |
| DTZ probe | one sided tables (one ply search for the other side), DTZ map, root ranking | 250-350 |

Haskell notes: all on `Ptr Word8` / `Word64` with `peekByteOff`, unboxed `UArray` for the static tables;
`Data.Map` (or a sorted `UArray` with binary search) for the material keys. The memory per process must be
mmap, not `ByteString.readFile`: 1 GB (5 men) per SelfPlay process times 8 is too much, mmap shares. On Hackage
`mmap` (Windows and POSIX) is not in LTS 24.12: an `extra-deps` entry, or a 20 line C shim with
`CreateFileMapping` / `mmap` under CPP (then little is won against way A).

The decoder is pure in principle (the files never change), so `unsafePerformIO` for the lazy table load behind
an `IORef` per table is acceptable, but the probe stays in `IO` for the same page fault reasons as in A.

Verification oracle: python-chess (`chess.syzygy`, in the jacob env) probes the same files; random positions
of every table (from SelfPlay endgames or generated) must give the same WDL / DTZ. Only as an oracle, not as
source.

## 8. Further problems and remarks

- **Elo expectation**: commonly some Elo for 5 men at fast TC, more in endgame heavy openings; it must be
  measured (SPRT, both sides with the same TB access, or Barbarossa with TB against itself without). Many
  testing setups adjudicate with TB anyway (`fastchess -tb`), which hides most of the gain: to measure, no TB
  adjudication on the engine side.
- **Specials in the eval** (`mateKBNK`, `knnk`, `kbbk`, `evalNoPawns`) stay; they matter without TB and for
  more pieces than the tables.
- **Tuning data**: TB WDL could label the endgame positions of the training data (exact result instead of
  the game result), and the SelfPlay data generation with TB changes the data: keep it off for the data rounds
  unless intended, and record it in the identity / config of a run.
- **Determinism**: probes do not change the node counts when off; with TB on the perf counts change by design,
  new references needed.
- **Files**: the 3-4-5 men set (939 MB) is the practical start; 6 men are 150 GB (disk speed then matters,
  `SyzygyProbeDepth` > 1), the match machines need their own copies (MEMO, m3a).
- **Release builds**: the C files are part of the source, `build-prod.sh` unchanged; `-optl-s` and the static
  link of the release work with C sources as with the RTS.
