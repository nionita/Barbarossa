# Search ideas - the register (0.9.0 development)

One block per idea, always current: what was tried in which form, what came out, how it is read today. The numbers and how they
were obtained are in the log `SEARCH-TUNING.md` (S1, S2, ...; speed: `PROFILING.md`); this file holds the interpretation, which may
change with more evidence. Both live on `dev090` only. How to test and how to keep this file: skill `search-idea-test`.

Status: open (not tried), running, promising, adopted, parked, rejected in this form. "Parked" and "rejected" name the form tested, not
the idea. Elo from the candidate's side with 95% error bars. "Adopted" means in the best branch, nothing is merged into `dev090` yet.

Older branches (before 0.8.0, e.g. `lmrfix`, `lmrfix14`, `hlmr`, `slmr`, `ttlmr`, `hiss`, `mpru`) hold earlier tries of some of these ideas; they
are not reviewed here. The user's experience: negative history and squared bonuses lost Elo each time.

## Now (2026-10-31)

- Best version by games: `nmlm` (+13.0 vs `hprf`, which has both orderings and RFP). `expo` = `nmlm` with cleanups, same search.
- Running on MEMO: filter before the quiet sort (`fsrt-expo-tc10`), RFP threat margin T100 (`t100-ada-tc10`).
- Next for games: counter moves. Next to measure: continuation history.

## Methods

Readings rest on the method of their time; when a method falls, the readings resting on it are marked below.

| Method | For | Status | Why | Ref |
|---|---|---|---|---|
| Move 4+ cuts at fixed LMR level 11, two position sets, against a placebo floor | ordering | current | a proven change shows clearly | S20 |
| Cuts under the adaptive LMR | ordering | blurred | removing the threat sort hardly shows (+0.012, +0.056); on the older base it was -0.174 (S5), the difference is not explained | S20 |
| Late cuts | ordering | weak | placebo up to +-0.18, one at 2 sigma | S20 |
| Node totals at fixed depth | search quality | identity check only | one perf run is a single sample; placebo spread 3-5%; 50 positions: +29% was noise | S2, S18, S20 |
| Node minimum at fixed depth | pruning parameters | refuted | the M38 was a blow-up effect, no minimum for RFP or LMR | S10, S13 |
| Worst 1% of the roots | reductions | no selector | right on 14, wrong on 11 against adaptive | S13 |
| Overlap of the top 1% roots | anomalies | useful | 79% with the guard, 9-12% without | S13 |
| 10k-node games | large effects, pruning | limited | depth 4-5 only; threat sort +3.5 against +10.8; agreed on RFP | S3, S5, S8 |
| 1000 games, one batch of openings | anything | too weak | +18.8 then -6.3 | S18 |
| Timed SPRT on MEMO, 10+0.1 | strength | decides | still shallower than rating list time controls | |
| GHC profiler | time shares | counts only | 3x slower, sort 30% against 7% | S16, `PROFILING.md` |
| Direct timers in the optimized build | time, allocation | current | | `PROFILING.md` |

To review under the current method: the cut numbers of S4 to S7, S17 (malus), all measured under the adaptive LMR.

## Move ordering

### Threatened pieces first (status: adopted)
Now: the best supported ordering change, by games. 2026-10-31.
Forms:
| Form | Branch, commit | Parameters | Differs by |
|---|---|---|---|
| Escapes first | `qthr` f5324a5 | `epQThr=2` | fast path when nothing is threatened, speed -2..3% |
| Softer variants, moves to attacked squares last | working tree | | none better (S6) |
Evidence:
| Date | Form | Test | Result | Ref | Reading then |
|---|---|---|---|---|---|
| 2026-10 | Escapes first | 10k nodes, 32k games | +3.5 +- 3.2 | S5 | small |
| 2026-10 | Escapes first | SPRT [0, 5], 10+0.1 | +10.8 +- 7.3 | S5 | worked |
| 10-31 | Removed | move 4+, level 11 | +0.115, +0.121 | S20 | good control |
Use: the control for measuring ordering changes.

### History prior from piece order and PST (status: adopted)
Now: gain in games together with the threat sort; its own effect is small and not visible in the current cut measure. Not sure on its own. 2026-10-31.
Forms: `hpst` 5919678, `epHistPst=3`, `epHistPstSh=-4`. Untested: a to-from key (S4).
Evidence:
| Date | Form | Test | Result | Ref | Reading then |
|---|---|---|---|---|---|
| 2026-10 | PST alone | 16k games: 1000 nodes, 10k nodes | +6.3 +- 4.9, +2.0 +- 4.5 | S4 | small gain |
| 2026-10 | With threat sort, against `qthr` | SPRT [0, 5], 10+0.1 | +5.8 +- 4.6 | S7 | worked |
| 10-31 | Removed | move 4+, level 11 | -0.009, +0.027 | S20 | below or near the floor |

### Counter moves (status: promising)
Now: real ordering gain, the same in all four measures. Elo unknown: 10k-node games cannot show it. 2026-10-31.
Forms:
| Form | Branch, commit | Parameters | Differs by |
|---|---|---|---|
| After the killers | `cmov` adbce56 | `epCMov=1` | also stored on TT cuts; none after a null move; table cleared per search |
Evidence:
| Date | Form | Test | Result | Ref | Reading then |
|---|---|---|---|---|---|
| 10-31 | After killers | move 4+, adaptive | -0.247, -0.241 | S19, S20 | real |
| 10-31 | After killers | move 4+, level 11 | -0.234, -0.217 | S20 | real, far above the floor |
| 10-31 | After killers | 10k nodes, 2000 games, 2 batches | about -1 +- 12 | S19 | not informative |
Interpretation history: 10-31: "no Elo" withdrawn, the test cannot see ordering gains (S3, S5).
Review when: continuation history is in; LMR retune.
Next: SPRT at 10+0.1, [0, 5], when MEMO is free.

### History malus (status: open)
Now: unclear. Earlier readings used the old method. The user's earlier tries lost Elo. Low priority. 2026-10-31.
Forms: `hmal` f10edb1, `epHMal=n`: quiet moves failing low before a cut get the bonus shifted right by n - 1.
Evidence:
| Date | Form | Test | Result | Ref | Reading then |
|---|---|---|---|---|---|
| 10-31 | 1/2, 1/4, 1/8, no root update | late cuts, adaptive, openings | -0.31, -0.35, -0.42, about +-0.2 | S17 | direction good |
| 10-31 | 1/8, no root update | 16 games | noise | | no crash |
Interpretation history: 10-31: the 2 sigma above is within what the placebo showed (S20).
Next: move 4+ at level 11 on both sets, before any games.

### History bonus by remaining depth (status: parked)
Now: no gain in cuts under the current method, worse on real positions; no effect in games at 10k and 50k nodes. Deep play not tested. 2026-10-31.
Forms:
| Form | Branch, commit | Parameters | Differs by |
|---|---|---|---|
| `2^(s+d)`, cap 2^30 | `hrem` f28fa1b | `epHRem=s` | s = 16, 20, 24 |
| Half slope, per own ply | `hrem` f802556 | `epHRem=-14` | no cap (user) |
Evidence:
| Date | Form | Test | Result | Ref | Reading then |
|---|---|---|---|---|---|
| 10-31 | s = 20 | 10k nodes, batch 1 | +18.8 +- 17.4 | S18 | "promising", wrong |
| 10-31 | s = 20 | 10k nodes, batch 2 | -6.3 +- 17.7 | S18 | noise |
| 10-31 | s = 24, s = 16, half | 10k nodes, 1000 each | +4.9, -14.3, -3.1, each +- 18 | S18 | no effect |
| 10-31 | s = 20 | 50k nodes, 1000 games | +3.5 +- 17.1 | S18 | no effect |
| 10-31 | s = 20 | move 4+, level 11 | +0.034, +0.059 | S20 | no gain |
| 10-31 | half slope | move 4+, level 11 | +0.096, +0.083 | S20 | slightly worse |
Interpretation history: 10-31: the gain on openings under the adaptive LMR (S18) came with the LMR (S20).
Review when: timed games at depth are cheap; a better history; separating aging from the weight of reduced lines (bonus `2^(r + ply - draft)`, not built).

### Overflow of the history bonus at the root (status: open)
Now: a real bug (at absolute depth 0 the Int32 overflows and makes the move the worst), removing it shows no clear gain. Clean up when the code is touched. 2026-10-31.
Forms: `hmal` f10edb1: `epHGrav=0` as before, `2` root as depth 1 (still 2^30), `3` no root update.
Evidence: no root update, move 4+ -0.043 under the adaptive LMR on openings (S17). No games, not at level 11.
Review when: the bonus is changed for another reason.

### History gravity (status: open)
Now: little evidence. In one shared-TT run without error bars it looked worse alone. Waits for the malus. 2026-10-31.
Forms: `hmal` f10edb1, `epHGrav=1`.

### Continuation history (status: open)
Now: not built. Expected to cost speed (more table reads per move), so it needs timed games. 2026-10-31.
Next: after counter moves; move 4+ at level 11 first.

## Pruning

### Reverse futility pruning (status: adopted)
Now: a clear gain; D6 / M50 compiled. The margin comes from games; the node method behind M38 is refuted. 2026-10-31.
Forms: `rfut` 9b7a4a1, `epRFutD=6`, `epRFutM=50`.
Evidence:
| Date | Form | Test | Result | Ref | Reading then |
|---|---|---|---|---|---|
| 2026-10 | M100, M50, M25 | 10k nodes, 8000 games each | +11.1, +25.2, +21.7, each +- 6.4 | S8 | peak about M50 |
| 2026-10 | D6 / M50 | SPRT [0, 5], 10+0.1 | +17.6 +- 9.9 | S8 | worked |
| 2026-10 | Sublinear margins | node scan | no gain against a linear one | S13 | old method |
Interpretation history: S10's method looked good, then S13 showed the blow-ups behind it.
Review when: the margin measured without the LMR guard (S8 ran with it); timed games for M25 and M38.

### RFP: more margin when own pieces hang (status: running)
Now: skipping RFP then lost; a moderate margin is untested in games. 2026-10-31.
Forms: `rfth` 7693aa9, `lmfx` 32c4bb8, `epRFutT`.
Evidence:
| Date | Form | Test | Result | Ref | Reading then |
|---|---|---|---|---|---|
| 2026-10 | T10000 (no RFP) | 10k nodes, 8000 games | -12.5 +- 6.3 | S12 | skipping is wrong |
| 2026-10 | T25 .. T200 | node scan | noise | S12 | old method |
Next: result of `t100-ada-tc10`.

### RFP: no cut with an opponent passer on rank 6/7 (status: open)
Now: the sharpest signal of the shadow diagnostic, never played; wrong cuts can be cheap. 2026-10-31.
Evidence: wrong rate 5.3x, 10.4x in endgames; gives up 10% of the decisions for 38% of the wrong cuts (S13).
Next: build, 10k-node games (pruning shows there), then SPRT.

### RFP: cut more when an opponent piece hangs (status: open)
Now: wrong rate 0.3x (S13). User: only after the RFP SPRTs. 2026-10-31.

### Late move pruning (status: rejected in this form)
Now: loses, more with depth. Without better history and `improving` late moves are not reliably bad. 2026-10-31.
Forms:
| Form | Branch, commit | Parameters | Differs by |
|---|---|---|---|
| Limit A + M d d / 2 from the last capture | `lmpr` 484b2e6 | `epLmpD`, `epLmpA=3`, `epLmpM` | zero window, not in check |
| A pruned move counts for the adaptive LMR | `lmpc` cae8bcd | `epLmpC` | keeps the LMR level |
Evidence:
| Date | Form | Test | Result | Ref | Reading then |
|---|---|---|---|---|---|
| 2026-10 | D3 M1 | SPRT [0, 5], 10+0.1 | -9.4 +- 8.8 | S15 | rejected |
| 10-31 | D5 M1 | SPRT [0, 5], 10+0.1 | -25.4 +- 13.4 | S15 | rejected, deeper costs more |
| 10-31 | With the count | LMR level, nodes | C1 restores most of the level | S15 | not played |
Review when: better history and `improving` are in.

## Reductions

### LMR without the near-mate guard (status: adopted)
Now: a bug fix with a clear gain. Rare missed mates not measured. 2026-10-31.
Forms: `nmlm` 5795f3f. Evidence: SPRT [0, 5] +13.0 +- 8.2; the 20 worst roots cost 344x less (S13).

### Fixed LMR level (status: rejected in this form)
Now: adaptive is stronger than 11 and 14. Useful for measuring ordering. 2026-10-31.
Forms: `lmfx` 32c4bb8, `epLmrFix`; with ordering changes `lfix` fbfb381.
Evidence: SPRT [0, 5] level 14 -36.4 +- 15.7, level 11 -8.6 +- 8.5 (S13). Levels 12, 13 not played.
Review when: ordering has changed much.

### Retune of the adaptive LMR after ordering changes (status: open)
Now: user's point: the adaptive parameters (`lmrInitLim`, weight 2^d of a re-search) were set for the old ordering. No data. 2026-10-31.

### History and depth in the LMR (status: open)
Now: the reduction depends only on the level and the move number. Largest expected effect after better history, not built. 2026-10-31.
Next: after counter moves.

## Other search

### Aspiration windows (status: open)
Now: code present, off since 2013 (`useAspirWin`). Not tested in this round. 2026-10-31.
Next: 10k-node games may be enough (user).

### `improving` (status: open)
Now: not built; `staticScore` two plies back is on `stack`, so cheap. 2026-10-31.

### Singular extensions (status: open)
Now: not built, needs good ordering and TT first. 2026-10-31.

### Threaded search (status: open)
Now: after the lower fruits (user). Per-thread state arrays would fit. 2026-10-31.

## Speed

Search unchanged, so no games are needed where nodes stay identical. Shares and method: `PROFILING.md`.

### Fewer modules, smaller interfaces (status: rejected in this form)
Now: no effect on speed; more unfoldings neither. Explicit exports stay for cleanliness (`expo` 3227638). Merging modules not built. 2026-10-31.

### Statistics counters off in the engine (status: adopted)
Now: -2.2% time, -4.6% allocation measured in SelfPlay, same nodes; on `expo` 1e7130b, not re-measured in the engine. 2026-10-31.

### Filter before the quiet sort (status: running)
Now: +3.0% nodes/s against `expo`, 8% less allocation per node, but 2.5% more nodes (LMR move numbers), time to depth -0.5%. Games decide. 2026-10-31.
Forms: `fsrt` 696882e, `epQFilt=1`. Next: non-regression SPRT [-5, 0] against `expo`.

### Other speed candidates (status: open)
Now: each small, `PROFILING.md` lists them: search state in mutable arrays by depth (user; half of the allocation is in the search itself), quiet generation
without pairs, insertion sort for captures, pawn check in `genMovePromo`, lazy selection in `dirSort`. A realistic total of maybe 10-20% speed. 2026-10-31.

## Eval

### Threat features in the HCE (status: open)
Now: after the search work (user). S14.

### NNUE (status: open)
Now: not on the list (user).
