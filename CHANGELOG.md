# Changelog

Changes since the last release. Older history: the git log and the tags.

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
