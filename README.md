Barbarossa - a chess engine written completely in Haskell
=========================================================

Barbarossa is a UCI chess engine written completely in Haskell. UCI is one of 2 protocols used in the computer
chess scene to communicate between a chess GUI and a chess engine. This way it is possible to write just the
chess engine, which then works with any chess GUI.

I started in 2009 to write a chess engine under the name Abulafia. In 2012 I decided to rewrite the evaluation
and search parts of the engine under the new name, Barbarossa.

My motivation was to demonstrate that even in a domain in which the raw speed of a program is very important,
as it is in computer chess, it is possible to write competitive software with Haskell. The speed of Barbarossa
(measured in searched nodes per second) is still far behind comparable engines written in C or C++. Nevertheless
Barbarossa can compete with many engines - as it can be seen on the CCRL rating lists*, where version 0.7.0 is
listed with a strength of about 2500 Elo.

Barbarossa uses a few techniques which are well known in the computer chess scene:
- in evaluation: material, king safety, piece mobility, pawn structures, tapered evaluation, piece square
  tables, scaling of drawish endgames and a few other features; the weights and the piece square tables are
  tuned on positions of Barbarossa's own games and of its games against other engines, with the real game
  results
- in search: principal variation search, transposition table, quiescence search, null move pruning, killer
  moves, futility pruning, late move reduction, internal iterative deepening.

I still have a lot of ideas which could improve the strength of the engine, some of which address a higher speed
of the calculations, and some, new chess related features, which may reduce the search tree.

The last released version is Barbarossa 0.8.0 from October 2026; the changes are in `CHANGELOG.md`, how to build
it is in `BUILDING.md`. Besides the engine the repository contains development tools (SelfPlay, TuneSGD,
LogScan, UCIScores); the Python tools for the tuning are in the separate repository BarbarossaTuning.

*) https://computerchess.org.uk/ccrl/404/
