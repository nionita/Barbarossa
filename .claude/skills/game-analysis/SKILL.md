---
name: game-analysis
description: How to analyse games and engine logs of Barbarossa - match PGNs from fastchess or cutechess (results, depth, time and nps per engine and game phase, terminations, time losses), Barbarossa UCI logs from GUI games (clock, think time, planned time, hard limit, long moves), and the mistakes in lost games (eval drops, replay of the suspect positions with an engine). Use it whenever the user pastes or names a PGN or a barbarossa-*.log, asks about time use, depths, speed, a lost game, a bad move or the time management in real games, even if they only say "look at these games" or "what went wrong here".
---

# Analysing games and logs (Barbarossa)

Three scripts in BarbarossaTuning do the work; write no scratch parsers. Each rule below comes from a mistake made with ad hoc
scripts (2026-11-21). The scripts' `--help` explains every column; read it before quoting a number.

## 1. Which script

| Question | Script (BarbarossaTuning) | Input |
|---|---|---|
| Score, terminations, time losses; depth, time per move, nps per engine and phase | `game_stats.py <pgn> [--games]` | fastchess or cutechess PGN |
| Clock and think time per game and block, planned time, hard limit, long moves | `time_log.py <log>` | Barbarossa UCI log at Info level |
| The same from the move comments of a PGN (no plan, no limit) | `time_log.py <pgn>` | PGN |
| Where a lost game turned, suspect moves, replay | `bad_moves.py <pgn> [--engine-exe <exe> --time 15]` | PGN with evals |

Run them with the conda env's python (the user's `CLAUDE.md`), with `-I` when the input lies on a share; the scripts find their
shared module `pgn_games.py` also then. Copy large inputs from shares to a scratch folder first, never write next to them.
Engine names come from the PGN tags; the engine of interest is the regex `--engine` (default `barbarossa`; for self-play files
give the branch name, e.g. `--engine dcap`).

## 2. Traps

- Side to move from the game's FEN, never from the ply parity: the openings start with Black to move. The scripts use the
  board; a scratch count of plies swapped the engines once.
- The log parser splits searches at the exact line `searchTheTree starts draft 1`. A prefix match also takes drafts 10-19 and
  cost 40% of the think times. Think time is draft 1 line to `bestmove`; the GUI's overhead is not in it.
- Logs need Info level (branch builds by default); `-l 5` writes no search lines. Games split at `New game` and at a clock jump
  (`--jump`); with moves-to-go time controls raise `--jump`.
- `mx` (hard limit) changes during a search: the scripts test the hit against the last `compTime` line, the plan (`ms`) is the
  first. "at plan ... same" means the time over the plan did not change the move.
- Evals of different engines are on different scales, depths of different engines are not comparable (pruning, reductions,
  how depth is counted). Compare an engine with itself: phases, games, versions.
- PGN times are what the GUI measured; the clock left is estimated from `TimeControl` (upper bound). cutechess writes no
  comment for some moves (ponder hits): those moves are missing from depth, time and drop lists ("gap").
- A drop marks the move before it: the search of the suspect did not see what came. A drop at a mate score or below `--lost`
  is not a mistake hunt any more.
- A replay with Barbarossa itself shows only what Barbarossa sees with more time; it cannot find what it never sees. A stronger
  engine is the better reference when available; its scores are still not Barbarossa's scale. The UCI search is not
  deterministic: a replay can name another best move of the same value on a second run (game 15, 27...Nb7: Rf5 once, Qc8 once).
- Barbarossa has no MultiPV: asking for it hangs python-chess. The replay analyses the position before and after the move,
  single PV. Barbarossa replays run with `-l 5` (no log) in a temporary working directory.

## 3. Reading

- `game_stats.py`: points and W/D/L per engine; depth and nps by phase show where an engine slows (endgame nps rises with
  fewer pieces). Time losses need `Termination` or the last comment.
- `time_log.py`: per game the clock at the last move and the total think; per block where the clock goes. Long moves: `MX` =
  stopped by the hard limit, the factor (`reds`) tells whether the score drop rule stretched it, the draft ranges show whether
  the move changed after the plan. Note the long moves and the clock at moves 20 and 40 for the time management block.
- `bad_moves.py`: per lost game "tactical" (most of the decline in big drops: look at those moves) or "drift" (many small
  steps: positional, or a horizon too short for each step; no single culprit). With a replay: `miss` = the replay engine
  prefers another move by more than `--drop` at that time; "same move" = the problem lies later or beyond the replay's horizon.
- Say how many games a reading rests on. A few GUI games show behaviour, not strength.

## 4. Where findings go

The integration session records them in the register `SEARCH-IDEAS.md` on rel090 (in a feature session: hand them over, do not
edit the register): time use under "Time management at long time controls", mistakes under "Mistakes in lost games: tracing
the refutation through the tree", depth caps and speed under their own blocks. Quote the script's numbers with the input file
name, the engine build and the time control. Input files stay where the user put them; results worth keeping go to the user's data
folder (listed in its `DATA.md`), not into this repository.

The source of this skill is `.claude/skills/game-analysis` on rel090; the integration session copies it to `~/.claude/skills`
after the merge. Change both.
