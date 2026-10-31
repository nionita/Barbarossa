# Profiling Barbarossa

How to measure where the time and the allocation go in the engine, and what was found. First measurement 2026-10,
version 0.9.0 in development (branch `expo`, from `nmlm`). GHC 9.10.3, Ryzen 7 7840HS, single thread.

All numbers: deterministic perf build of SelfPlay, `SelfPlay -P -i tests/data/tests.fen -d 10` (50 positions, a local
file), 9919183 nodes in every build, 16.6 s and 67.07 GB allocated without timers: 1.68 us and 6.76 KB per node.

## Summary

- The module structure does not limit the speed: more unfoldings or fewer exports bring nothing.
- The GHC profiler gives right call counts and wrong times. Use direct timers in the optimized build.
- No large hot spot. Eval 30%, move generation 19%, quiet sort 7%, make move 6%, TT 6%, the search itself about 30%.
- SEE (2%) and the TT (6%) are fine. The eval is called at 96% of the nodes, mostly for the stand pat.
- Allocation: half is in the search itself, a quarter in the move generation. GC is only 2% of the time, but allocation
  still costs: with a large nursery the engine gets slower.
- Proven gain so far: the statistics counters which the search does not read cost 2.2% of the time.

## Method

### Comparing speeds

This PC throttles: the same build ran between 400k and 600k nodes/s in one afternoon. Only runs alternating between two
builds in the same period can be compared; the node count must be identical. Spread about 1% when not throttling.

### Module structure

| Build | Nodes/s | Result |
|---|---|---|
| Current | about 590k | |
| `-fexpose-all-unfoldings -fspecialise-aggressively` | about 590k | no gain |
| Explicit export list of Moves.Core (before: everything) | 559k against 567k | noise |

In the Core of Search.Core no monad dictionaries are left and few calls into other modules (`betaCut`, `doNullMove`,
`qsDelta`, `legalMove`, `moveChecks`, `retrieveEntry`, `writeCache`). To check it: the flag `ddump` (default on) writes
`*.dump-simpl` next to the objects in `.stack-work/dist`. Merging modules was not built; these results give it little
chance. Import lists have no effect on the optimization, only export lists.

### GHC profiler

Late cost centres are inserted after the optimization and are the best the profiler offers:

```
stack build --library-profiling --executable-profiling --flag Barbarossa:-qscollect --flag Barbarossa:reproselfplay \
    --flag Barbarossa:-ddump --ghc-options "-fno-prof-auto -fprof-late" Barbarossa:exe:SelfPlay
```

- The stanza needs `Other-extensions: TemplateHaskell` (only for this build), otherwise: "cannot find object file ...
  while linking an interpreted expression" (Struct.Status).
- `SelfPlay ... +RTS -p -RTS` writes the `.prof`.
- It runs 3 times slower (155k nodes/s). The vector and primitive operations stay as cost centres with hundreds of
  millions of entries, also with `configure-options: $everything: [--profiling-detail=none]` in `stack.yaml`.
- So it overstates what uses vectors: the quiet sort looked like 30%, it is 7%. Use its entry counts, not its times.
- Use a separate worktree: the flags rebuild everything.

### Direct timers

In the normal optimized build (a worktree, not for committing), around the functions of interest:

```haskell
timedIO :: Int -> IO a -> IO a
timedIO i act = do
    a0 <- getAllocationCounter          -- GHC.Conc, counts down
    t0 <- now                           -- foreign import ccall unsafe "QueryPerformanceCounter"
    r <- act
    !_ <- evaluate r
    t1 <- now
    a1 <- getAllocationCounter
    ...                                 -- add t1 - t0, 1 and a0 - a1 to global counters of i

timed :: Int -> a -> a                  -- NOINLINE
timed i x = unsafeDupablePerformIO $ timedIO i (evaluate x)
```

- Rename `f` to `f'` and add `f a = timed n (f' a)`; pure results must be forced first (lists to the end, for the move
  lists with the elements), but what the timed function consumes must be forced before the timer starts.
- Runs agree within 1%, also the node count stays identical.
- Calibrate with an empty `timed`: here 28 ns and 176 bytes are measured per call inside the timer, the whole cost of
  a timed call is 93 ns. Subtract the inner part per call and, for nested timers, the whole cost of the inner calls.
- A timer stops inlining and worker/wrapper across it, so it can move allocation a bit (see `doFromToMove`).
- Timed lists are fully forced, a little more than a lazy search would consume.

The scripts of this measurement (`patch_timing*.py`, `patch_nostats.py`, `speed_ab.sh`) and the raw output are in
`C:\Learn\Data\measurements\search\speed`.

## Results

Shares of 16.6 s and 67.07 GB, corrected as above. The time of `genMoveCaptWL` is without SEE and `theAttacs`, assuming
all `theAttacs` calls are in it.

| Part | Calls | Per call | Time | Bytes per call | Allocation |
|---|---|---|---|---|---|
| `posEval` | 9.57M | 513 ns | 29.5% | 472 | 6.7% |
| **Move generation** | | | **19.0%** | | **24.9%** |
| `genMoveCaptWL`, own | 4.38M | 251 ns | 6.6% | 1610 | 10.5% |
| `genMoveNCapt` | 0.52M | 1107 ns | 3.5% | 7520 | 5.9% |
| `genMoveFCheck` | 0.51M | 733 ns | 2.2% | 3400 | 2.6% |
| `seeMoveValue` | 6.94M | 45 ns | 1.9% | 200 | 2.1% |
| `genMovePromo` | 4.39M | 65 ns | 1.7% | 203 | 1.3% |
| `genMoveNCaptToCheck` | 0.83M | 308 ns | 1.5% | 1416 | 1.8% |
| `theAttacs` | 10.7M | 21 ns | 1.4% | 49 | 0.8% |
| `genMoveCast` | 0.58M | 51 ns | 0.2% | | 0.0% |
| **Quiet sort** | | | **7.0%** | | **8.9%** |
| `dirSort` (depth up to 2) | 0.59M | 1268 ns | 4.5% | 6200 | 5.4% |
| `makeMTS`, selection steps | 3.32M | 123 ns | 2.5% | 694 | 3.4% |
| `doFromToMove` with `updatePos` | 9.66M | 109 ns | 6.3% | at least 439 | at least 6.3% |
| **TT** | | | **6.1%** | | **0.6%** |
| `ttRead` | 9.93M | 93 ns | 5.5% | 37 | 0.5% |
| `ttStore` | 2.03M | 47 ns | 0.6% | | 0.1% |
| `moveChecks` | 12.1M | 28 ns | 2.1% | | 0.0% |
| `legalMove` | 1.12M | 72 ns | 0.5% | | 0.1% |
| **Not timed: search loop, state, repetition, ...** | | 490 ns per node | **about 30%** | 3500 per node | **about 52%** |

Findings:

- **TT.** Fast as expected: 93 ns per read, probably mostly the memory access of the cell (not measured separately).
- **SEE.** 45 ns per call, about 200 bytes: little to gain. The list `acc` could go,
  at most about 2% of time and allocation together.
- **Move generation.** Lists everywhere: `genMoveNCapt` allocates about 250 bytes per move (the pair of squares, the move,
  the list cells over `concatMap`, `map`, `++`). `genMoveCaptWL` builds and sorts lists of `LMove` by `Data.List.sort`,
  1.6 KB per call, called in nearly every QS node. Largest allocation among the timed parts.
- **Eval.** Lazy already (`staticScore`), but needed at 96% of the nodes, mostly as the stand pat. The attack fields of the
  position are lazy (`lazyBits`), so part of their cost is counted in the first function using them, often the eval.
- **Search itself.** Half of the allocation, 3.5 KB per node. The state is copied at every change: `PVState` (`absdp`,
  `maxdp`, `lmrrs`, ...) and with every counter also `SStats` (16 words). Further `NodeState`, `Path` and the closures of
  the two nested CPS state monads (`Search = CState PVState Game`, `Game = CState MyState CtxIO`).

## Experiments on allocation

### Nursery size (`+RTS -A`), 3 alternating runs each

| `-A` | Time | GC |
|---|---|---|
| 256k | 18.6 s | 1.8 s |
| 512k | 17.6 s | 1.2 s |
| 1m | 17.2 s | 0.7 s |
| 4m (default) | 16.8 s | 0.3 s |
| 16m | 18.8 s | 0.2 s |
| 64m | 20.1 s | 0.1 s |

The default is the best of these. From 16m the engine gets slower although the GC time falls: allocating through more
memory than the caches hold costs cache misses. So allocation costs more than the 2% GC time shows; reducing it should
help more than that. Not tested: other sizes near 4m, and the other PC (other caches).

### Statistics counters off

The counters the search does not read (`reTrieve`, `reFail`, `incBeta`, `incRedu`, `incReSe`, `incReBe`, `incReMi`)
as no-ops; `sNodes`, `sNodesQS`, `sRSuc` stay. 4 alternating runs: same nodes, allocation -4.6% (67.07 to 64.00 GB), time
-2.2% (16.73 to 16.37 s). The statistics are needed by `SelfPlay -P` (beta cut rates for search comparisons), so a build
flag would be the way, off for the engine. Done (expo): `-DNO_SEARCH_STATS` in the stanza of the engine, SelfPlay keeps them.

## Generated against consumed quiet moves

Counted in the interior nodes (`patch_gencount.py`; expected node type, not the real one). Of 2.01M move loops 498k (25%)
generate the quiet moves: the staged laziness works, captures, TT move and killers cut before. Once generated, the sort
forces all of them, on average 32.5.

| Loops with generated quiet moves | Generations | Moves | Consumed | Cut after generation |
|---|---|---|---|---|
| No futility, depth up to 2 | 106752 | 3664978 | 36% | 70% |
| No futility, depth over 2 | 67431 | 2299618 | 57% | 46% |
| Futility on | 323894 | 10226873 | 95% | 8% |

- With futility on (65% of the generations, 63% of the moves) nearly all moves are just pruned: 11.9M pruned moves in these
  loops. They are generated and sorted, then rejected one by one; only captures, checks, passers and new queen attacks are
  searched (`canPruneMove`). Filter first, sort only the rest.
- The expected node type does not separate the cases: of the generations in expected all nodes without futility 56% end
  with a cut (expected cut nodes: 69%).
- Without futility at depth up to 2 only 36% of the moves are used, but `dirSort` sorts all: a lazy selection could help.

## Filter before the quiet sort (fsrt)

Zero window nodes with futility pruning: only the moves `canPruneMove` rejects not, plus the best pruned move (so that
the test for no valid moves and the best move of a fail low stay as they were), are sorted. Parameter `epQFilt`.

| Build | Nodes | Time | Nodes/s | Allocation per node |
|---|---|---|---|---|
| expo | 9919183 | 16.56 s | 599k | 6.76 KB |
| fsrt, filter off | 9919183 | 16.99 s | 584k | 6.82 KB |
| fsrt, filter on (same binary) | 10169685 | 16.60 s | 613k | 6.22 KB |
| fsrt, filter hard coded | 10169685 | 16.48 s | 617k | |

- Same binary against itself looks better than it is: +5.3% nodes/s, while the off side is slower than expo (parameter
  read or code layout). Hard coded against expo: +3.0% nodes/s (estimate from the share of moves was 4%).
- 2.5% more nodes: the remaining moves have a smaller move number in the LMR, so they are reduced less. Time to depth
  gains only 0.5%. Whether the extra nodes are worth anything only games can tell.
- Compare builds also against the parent build, not only by a parameter in one binary.

## Candidates, not tested

By expected gain. Except 5 and 6 without changing the search (same node counts), so no games are needed:

1. Search state: counters and the small fields (`absdp`, `maxdp`, LMR counters) in a mutable unboxed array instead of
   a record copied at every change. Up to several percent of allocation per field group; the statistics alone gave 4.6%.
2. Quiet move generation without pairs and lists of pairs (`srcDests`), e.g. directly from the bitboards to moves.
3. `genMoveCaptWL`: a small insertion sort on an unboxed buffer instead of `Data.List.sort` on lists. It must keep the order
   of the moves (the sort key contains the move, so equal keys do not occur).
4. `genMovePromo`: 65 ns and 203 bytes in nearly every QS node, nearly always empty: a quick test of the pawns first.
5. `dirSort`: heap sort at depth up to 2 with about 30 moves, 6.2 KB per call. It must keep the order for equal history
   values to stay the same engine, otherwise the node counts change and games are needed.

6. Futility on: filter by `canPruneMove` before the sort. Built, see below. Checks and passers directly from the bitboards
   would also save most of the generation there, but that is much more work.

A realistic total is maybe 10-20% speed for considerable work. By the usual rule of thumb (not measured here) that is about
10-15 Elo.
