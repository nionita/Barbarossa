# Changelog

Changes since the last release. Older history: the git log and the tags.

## 0.9.0 (2026-11)

About 110 Elo stronger than 0.8.0: 53% in 1200 games at 60+1 s against six engines rated about 2710 on the CCRL
Blitz list, which puts 0.9.0 near 2730 there. The gain comes from the search.

### Search

- The replacement rule of the transposition table is fixed: a long-standing bug chose the entry to replace by its
  move code, ignoring depth and age, which hurt most at long time controls and with small hash tables.
- PV nodes whose first move fails low continue with zero window searches, as in the rest of the PVS: fewer wasted
  nodes in unstable positions.
- Better move ordering: quiet moves of threatened pieces first, a history initialised from the piece square table
  at every search, counter moves and a continuation history.
- Reverse futility pruning, with more margin when own pieces hang.
- Late move reductions: no special treatment near mate scores, and a lower bound for the reduced depth.
- Mate scores: a node in which all moves were pruned recognises stalemate, and a mated move no longer stays first
  in the root move list.
- Maximal search depth raised to 40.
- One piece value table for exchange evaluation, delta pruning in the quiescence search and the special endgame
  evaluations.

### Evaluation

- An exact KPK bitbase for king and pawn against king.
- Pawn endings: the distance to promotion is computed correctly.
- Lone king endings are won faster; with bishop and knight the king is driven to the right corner.

### UCI

- Pondering (option `Ponder`).
- `go movetime`.
- Changing the hash size frees the old table (before, every change kept the old one in memory).
- FEN strings in the log are standard.

### Speed

- Move generation builds the moves in one step, captures are sorted in place, and the quiet moves are sorted only
  when they will be searched: about 5 to 10% more nodes per second.

## 0.8.0 (2026-10)

About 115 Elo stronger than the previous version (300 games at 10+1 s), the largest part from the new
evaluation.

### Evaluation

- The evaluation weights and a piece square table are tuned on positions of Barbarossa's own games and of its
  games against other engines, labelled with the real game results. This is the main source of the strength
  gain.
- Material and some simple placement terms (rook pawns, centre knights, centre attacks of pawns, knights and
  king) are now part of the piece square table: fewer evaluation terms, the same knowledge.
- The piece square table is updated incrementally with the moves, so it costs almost no speed.
- New scaling of drawish endgames, fitted from game results: few pawns of the stronger side, opposite
  coloured bishops, pawns on one wing only, rook and queen endings. It replaces the former hand-set rules.
- Further evaluation terms and refactored known endgames and scale factors.

### Search

- The principal variation sent to the GUI contains only legal moves.
- The quiescence search stays within the search window.
- Maximal search depth raised to 25.
- Fixed a rare crash at high search depths (null move depth index).

### Time management and UCI

- Fixed: with very little time left (100 ms or less) or with an increment-only time control the search
  could run without a time limit and lose on time.
- Fixed: the full move number of a FEN in the position command was read as one character, which could make
  the engine ignore the moves after the FEN.
- More robust end of a search (search completion owned by one thread).

### Development tools

- SelfPlay: matches with SPRT (save and resume), node-limited matches, perft, performance tests, training
  data output.
- TuneSGD: export of the evaluation terms for offline fitting, configuration based tuning.
- New tools LogScan and UCIScores; option -d of the engine writes training data from its games.
- Build flavours and deploy script; the Python tuning tools are in the separate repository
  BarbarossaTuning.

### Code

- Modules reorganised (core and internals), explicit exports, no compiler warnings, built with GHC 9.10
  (Stackage LTS 24.12).
