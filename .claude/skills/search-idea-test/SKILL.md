---
name: search-idea-test
description: How to test an idea for Barbarossa's search (move ordering, history, killers, LMR, pruning, extensions, aspiration, speed changes) and how to read and record the results. Use it whenever an engine search change is prepared, measured or judged, whenever cut rates, node counts, 10k-node games, an SPRT or a MEMO result of such a change are discussed, and before calling a change good or bad, even if the user only says "try this" or pastes a result.
---

# Testing a search idea (Barbarossa)

Each rule here comes from a real wrong conclusion; the details are in `SEARCH-TUNING.md` on dev090 (S2, S3, S5, S13, S16-S20) and
`PROFILING.md`. A quick test answers only some questions. Say which question it answers, and call a result preliminary until the
right test has confirmed it. Only timed games decide about strength.

## 1. Build it so that it can be measured

- One experiment branch per idea from the current best version, identity = branch name (`AGENTS.md`).
- A parameter, compiled off. Off must give identical nodes with the deterministic perf build (`scripts/build-perf.sh`,
  `SelfPlay -P -i tests/data/tests.fen -d 10`) against the parent. If off differs, find out why: that is how the history overflow at
  the root was found.
- One change at a time. A change often carries a side effect: late move pruning lowered the adaptive LMR level, the filter before the
  sort changed the LMR move numbers. Look for them in the code before reading the numbers.

## 2. Choose the test by the kind of change

| Change | Measure | Not this |
|---|---|---|
| Refactoring, speed, same search | identical nodes, then alternating runs against the parent build | a parameter inside one binary alone (the off side can be slower by the read or the layout); GHC profiler times (right counts, wrong times: `PROFILING.md`) |
| Move ordering | cuts at move 4+ at fixed LMR level 11, on two position sets, against the noise floor; then timed games | node totals; cuts under the adaptive LMR; 10k-node games |
| Pruning, reductions | games; 10k-node games are useful for large effects | cut rates (pruning removes the easy cut nodes); node minimum (cost, not loss) |

Node totals serve as identity checks and as detectors of anomalies. Worst-1% and similar metrics have not earned a place as selectors.

### Ordering cuts

Use `hist_scan.py` (BarbarossaTuning): `hist_scan.py <exe> <fen> 10 <outdir> <base> <variants...>`, first variant = base, `-` = no
parameters, deterministic perf build. Sets: `C:\Learn\Data\measurements\search\rfp-scan-400\pos400.fen` (openings) and
`C:\Learn\Tune\hvar\nodescan\noguard\roots-500.fen` (real games): they can give opposite signs. Fixed level: `epLmrFix=11` (branch `lfix`,
from `lmfx`). The adaptive LMR blurs the cuts: removing the proven threat sort shows at level 11 (+0.115, +0.121), hardly under the adaptive
one (+0.012, +0.056).

Noise floor, same engine with other random seeds (normal flavour): move 4+ about +-0.04, late cuts about +-0.18, node totals a spread of
3-5%. The standard errors over chunks do not cover the shared base run: one perf run is a single sample of a chaotic tree, and all variants
are compared with it. So a pattern over all variants can be the base (S20: all up on openings, all down on real positions). Rerun the
placebo when the method changes. Controls with known outcome: `epQThr=0` (threat sort removed, must look worse), counter moves (`epCMov=1`).

### Games

- 1000 games: about +-17 Elo; 2000: +-12. Two batches on separate openings, never one: +18.8 was followed by -6.3 on new openings.
- 10k nodes (6.5 minutes per 1000 games here with 4 shards, `run_group.ps1`, normal flavour, `-Config`): with the node margin depth 4-5 only
  (S3). Ordering gains hardly show: threat sort +3.5 there, +10.8 in the SPRT. So no result there does not refute an ordering change. Large
  gains and losses of pruning show (RFP). Node-limited games do not count speed costs.
- 50k nodes: 55 minutes per 1000 games here, deeper but still +-17.
- Depth matters by itself: caps, rescaling, aging, the LMR level act more in deep search (user).
- Timed SPRT on MEMO at 10+0.1 decides: `run_timed.ps1` (watchdog against the fastchess hang). `[0, 5]` for changes worth it, D3 M1 took 8
  hours for a -9; `[0, 10]` needs about a quarter and may lose a +3; non-regression `[-5, 0]` when the aim is speed or allocation. Play against
  the parent build when a parameter read could favour one side.
- Prior experience (user): negative history and squared bonuses lost Elo each time.

## 3. Reading results

- Before a result is called anything, ask: what would the same number look like with no change at all? Placebo first when in doubt.
- Do not call a first result a find. 2 sigma happened in the placebo.
- A chunk's standard error of exactly 0, or a chunk that takes minutes when seconds are expected: the test is not doing what is assumed.
  (`SelfPlay -P` ignores `-s` and `-f`: chunks need their own files, as `hist_scan.py` does.) Look at partial results early.
- Better cuts are not Elo, more nodes at fixed depth are not a loss. Each needs its own evidence.
- Record the prediction before the result comes (S13). If two explanations remain, design the test that separates them.
- Say what a result does not exclude: +3.5 +- 17 does not mean zero.
- State estimates of time with their basis and correct them when they were wrong.

## 4. Environment

- This PC throttles, MEMO is for long runs; speeds only from alternating runs. Copy binaries under own names, use a worktree (`git worktree add
  --detach C:\Learn\Tune\modx <branch>`) for experiments with other flags, and remove it afterwards.
- Patches by scripts in the scratchpad; after a branch switch working files can have CRLF: normalize with `sed -i 's/\r$//'` before a commit.
- MEMO: zip to `Z:\nicu\transf`, command `Expand-Archive -Path Z:\nicu\transf\<name>.zip -DestinationPath C:\ -Force`, plain `.\run_timed.ps1 ...`,
  quote values with commas (`-CandParams "a=1,b=2"`), give the SHA-256 start of new exes (Defender flags them; its settings are the user's).
- `TaskStop` leaves a script's SelfPlay children running: end them explicitly.

## 5. Recording

All on dev090 in document-only commits, the experiment branches carry only code. Raw results to
`C:\Learn\Data\measurements\search\<idea>` or `C:\Learn\Data\results\memo\<run>\result.txt`.

- `SEARCH-TUNING.md`: the log. A finding S<n>: what was done, branch and commit, conditions, numbers. Facts stay; a later correction is added
  as a dated note, not by rewriting.
- `SEARCH-IDEAS.md`: the register, one block per idea, always current. Update it with every result; revise the interpretation there, with the
  date and the reason, keeping the old lines:

```markdown
### <Idea> (status: open | running | promising | adopted | parked | rejected in this form)
Now: <current reading in one or two sentences, how sure, date>
Forms:
| Form | Branch, commit | Parameters | Differs by |
Evidence:
| Date | Form | Test (method, conditions) | Result | S<n> | Reading then |
Interpretation history: <date>: <what changed and why>
Review when: <what could reopen it, e.g. a better history, deeper games, a new implementation>
Next: <the test that would decide>
```

"Parked" and "rejected" name the form tested, not the idea: what failed under an old method or at low depth may deserve a new test.
When the method changes, go through the register for readings that rested on the old one.

The source of this skill is `.claude/skills/search-idea-test` on dev090; the copy in `~/.claude/skills/search-idea-test` makes it available on
all branches. Change both.
