# Aspiration windows at the root, and the PV node type after a fail low (2026-11-18)

State (2026-11-18): built on `aspw` (from `ttrp` c64ad37), measured by node counts only, no games. Aspiration saves no
nodes at any window. Measuring it found a defect of the PV search, independent of aspiration: as the parameter `epPvsNt`
it saves 12-16% of the nodes without aspiration. Both are compiled off. Not yet in SEARCH-TUNING.md / SEARCH-IDEAS.md.

Commits: 4fa3916 identity `aspw`; 0e52350 aspiration (`epAspW`, `epAspGrow`, `epAspMinD`, counters); b54d34d `epPvsNt`.
Raw results: `C:\Learn\Data\measurements\search\aspw` (README.txt there). "Inference" marks what is concluded, not measured.

## 1. What the plan did not know

- **The PV node type after a fail low** (section 3): every narrow window was much more expensive than its re-searches.
  Without the fix, aspiration measures this defect, not itself.
- **The window in use was 24, not 36.** UCI and SelfPlay call `Uci/UciGlue.hs` `bestMoveCont`, `aspirWindow = 24`.
  `Search/Thread.hs` (36) is not in `Barbarossa.cabal`: dead code. Both constants and the `window` field of `ABControl`
  are removed; the window is now `epAspW`.
- **The eval clamp is the near-mate boundary**: `posEval` clamps to +-(20000 - 255) (`Eval/Core.hs` `matesc`), and
  `nearmate` is |s| >= 20000 - 255. One test (`nearmate sp`) covers mate scores and the clamp.

## 2. The aspiration form

`alphaBeta` ([Core.hs:308](Core.hs:308)), with the last score sp of the previous draft:

| Parameter | Compiled | Meaning |
|---|---|---|
| `epAspW` | 0 | half window in score units; 0 = off: full window, the engine as before |
| `epAspGrow` | 0 | on a fail, widen only the failing side by this factor, for up to 2 more tries (`aspMoreTries`); 0 or 1 = straight to the full window (the old form) |
| `epAspMinD` | 4 | first draft with aspiration (never below 2) |

- No aspiration when sp is near mate or at the eval clamp: full window.
- A fail high searches its move first in the next search (as before); a fail low the last PV.
- Every try starts from the same fresh search state as before (`pvs0`).
- Counters (diagnostic, `SStats.sAsp`, key draft * 8 + kind, `aspCount` [Core.hs:400](Core.hs:400)): searches with a window,
  fail lows, fail highs, full window re-searches, nodes of the failed searches, sum of the score changes against the last
  draft (all drafts, also off), aborts. `SelfPlay -P` prints them per draft (`asp,draft,kind,n`) and one line per position
  (`pos,n,nodes,score,move,FL,FH,RS,failed nodes,swing`). They do not change the search: off gives identical nodes.

## 3. The defect: PV node type after a fail low (`epPvsNt`)

A PV node searches its later moves with zero window (as Cut nodes, `nxtnt = nextNodeType`) only after a move has
raised alpha (`checkFailOrPVLoop` [Core.hs:911](Core.hs:911), `checkFailOrPVRoot` [Core.hs:505](Core.hs:505)). If its
first move fails low, all its moves are searched as PV nodes: full window, no LMR, no null move, and the same again in
their PV children, recursively.

- **With the full window** the first line of the search starts at alpha = -inf, so the first move always raises alpha;
  the root is never affected. Inner PV nodes are: a later move of a PV node, which failed high in its zero window search,
  is searched again as a PV node with a real alpha (`pvInnerLoopExten` [Core.hs:803](Core.hs:803), at the root
  `pvInnerRootExten`); if the first move of that child fails low (search instability: reductions, TT, pruning), the defect
  fires. How often is not counted; the node effect says: often.
- **With a narrow window** it fires in every PV node of the first line whose score falls under the window, also when the
  root does not fail. tests.fen position 47, depth 10, window 25: 3.2x the nodes without any root fail, 3.7x with a fixed
  LMR level (`epLmrFix=11`), so not the adaptive LMR.
- **`epPvsNt = 1`**: after the first move the node type moves on also when it failed low, as in PVS. Position 47 with
  window 25: 389k nodes instead of 1.32M (one fail low).
- **Without aspiration**: tests.fen depth 10, epTTRep=2: 11671539 -> 10251160 (-12%); roots-500 depth 10 (chunks):
  83.2M -> 69.7M (-16%).
- It is an efficiency defect, not a wrong score: the subtrees are searched more expensively, with the effort exactly in the
  unstable nodes. Fewer nodes are not Elo (it may play better with the extra search there); games decide.

## 4. Abort paths and time management

- **Nodes of failed searches are counted**: every root search adds its statistics (`reportStats` -> `draftStats`), so the
  totals include the failed searches.
- **Timeout in the narrow search** without a move: the last PV and the last score are returned (as before).
- **Timeout in a re-search after a fail high** (changed): before, it returned the last PV and lost the move that failed
  high. Now the failed-high move with the bound as score (`aspFallback` [Core.hs:393](Core.hs:393)), but only when the
  re-search found no move.
- **A fail low never reaches time management as a score**: a failed search returns no path, only completed searches give
  a score. An aborted re-search after a fail low returns the last score, so a root fail low also never extends the time
  (`exte` in `searchTheTree` sees completed drafts only). Not new; a possible later change: extend the time on a root fail
  low.
- **Not changed, noted**: each try starts the adaptive LMR again at `epLmrIni`; the best move changes (`rbmch`) of the failed
  searches are lost for the time management; under `AbortByNodes` (node-limited SelfPlay games) a re-search starts its
  node budget again (the budget is per search state). The root move number (`rootmn`, for the first-move time limit) is
  reset per move, not per draft: aspiration does not touch it.

## 5. Measurement

Deterministic perf build (`scripts/build-perf.sh`), all with epTTRep=2. `asp_scan.py` (in the measurement folder): chunks
of 25 positions, one process per chunk and variant (the TT and history carry over within a chunk), 4 workers; geomean of
the per-position node ratios against off with a 2000-sample bootstrap interval over positions.

Checks: off identical to `ttrp` on tests.fen depth 10, one process: 11367370 (epTTRep=0), 11671539 (epTTRep=2), after both
commits. Perft of the 5 standard positions unchanged (after 0e52350; b54d34d does not touch move execution).

**roots-500 (C:\Learn\Tune\hvar\nodescan\noguard), depth 10**

| Window, growth | epPvsNt=0: total / geo % | epPvsNt=1: total / geo % [95%] | FL / FH / RS (epPvsNt=1) | failed nodes |
|---|---|---|---|---|
| off | 83.2M | 69.7M | - | - |
| 15, 0 | +282% / +86 | +21.6% / +18.7 [16.1, 21.5] | 723 / 560 / 1283 | 20% |
| 15, 4 | +341% / +91 | +22.1% / +19.1 [16.0, 22.3] | 844 / 665 / 36 | 22% |
| 25, 0 | +108% / +33 | +7.6% / +7.3 [5.1, 9.6] | 344 / 273 / 617 | 10% |
| 25, 4 | +121% / +33 | +7.1% / +7.3 [4.9, 9.8] | 376 / 329 / 22 | 10% |
| 36, 0 | +67% / +20 | -0.3% / +0.1 [-1.7, 1.9] | 206 / 173 / 379 | 4.7% |
| 36, 4 | +74% / +22 | -0.6% / -0.2 [-1.9, 1.5] | 233 / 211 / 12 | 5.8% |
| 50, 0 | +35% / +10 | +1.1% / +1.4 [-0.3, 3.3] | 105 / 105 / 210 | 2.9% |
| 50, 4 | +33% / +10 | +0.8% / +1.4 [-0.4, 3.2] | 126 / 131 / 11 | 3.6% |
| 75, 0 | +4.3% / +5.0 | -0.3% / +0.4 [-1.0, 1.8] | 61 / 58 / 119 | 1.9% |
| 75, 4 | +3.7% / +5.4 | -0.5% / +0.5 [-0.9, 2.0] | 77 / 82 / 11 | 2.3% |

FL / FH count every failed try (with growth also the widened ones), RS the full window re-searches.

**tests.fen (50), depth 10, epPvsNt=1** (off 10.29M): 15/0 +18.0% (geo +20.3), 25/4 +6.1% (+4.0), 36/4 -1.0% (-1.3),
50/4 -3.9% (-2.4 [-5.5, 0.5]), 75/4 -2.7% (-1.8). epPvsNt=0: 15/4 +598%, 25/4 +29%, 50/4 +3.1%, 75/4 +8.3%.

**First 100 roots, depth 12, epPvsNt=1** (off 33.3M): 25/4 +8.3% (geo +7.4 [3.0, 12.2]), 36/4 +1.0% (+0.9 [-2.7, 4.4]),
36/0 +1.0% (+1.2), 50/4 -1.0% (-2.0 [-4.8, 0.9]), 75/4 +0.2% (-1.1 [-3.7, 1.7]).

## 6. Reading

- **Aspiration saves no nodes here** (with epPvsNt=1): windows 36-75 are within about +-2% at depth 10 and 12, narrower
  ones cost. Growth 4 against growth 0: no clear difference.
- **Where it gains and loses** (roots, window 36/4): the 318 positions without a fail save 2.4%, the 182 with a fail cost
  3.7%; net zero. With window 75/4: 76 failing positions +5.2%, the others -0.3%.
- **Which positions fail**: those with large score swings, median sum of the draft-to-draft changes 208 against 76 (window
  36). The largest increases: roots 170 (x2.9, swing 532), 487 (x2.7-3.2, swing 372), 462 (x1.9, score -1800, swing
  1540). 6 of the roots have mate scores: aspiration is off there.
- **By draft**: at depth 10, window 36/4, 10-15% of the positions fail per draft from draft 4 to 10 (most at drafts 7-8);
  at depth 12 (100 roots) 4-10% per draft, without a trend to fewer at the deep drafts.
- **Noise**: the per-position log ratio has an sd of about 0.2 even in positions without a fail (the TT and history carried
  over in a chunk, the chaotic tree), so a geomean within +-2% is no effect. The best moves at depth 10 agree with off in
  83-88% of the positions.
- Inference: aspiration gains elsewhere by its cut-offs in the first line and the TT bounds it leaves; here the defect hid
  that, and with the fix the gain is used up by the failing positions. Whether the narrower first line helps the move
  choice is not a node question.

## 7. Next

1. **Game test of `epPvsNt=1` alone**, best on its own branch from `ttrp` with the value compiled in and its own identity,
   against the parent build (one change at a time; no parameter read favouring one side). Screen at 10+0.1, decide at 60+1.
2. **Aspiration only on top of it, low priority**: window 50 or 75, growth 4. The nodes promise nothing; a game would
   show only what node counts cannot (TT bounds, move order after a fail, the fallback move on timeout).
3. Possible later: a counter of how often the defect fires without aspiration; extending the time on a root fail low.
