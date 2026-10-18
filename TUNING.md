# Tuning the HCE evaluation - state of 2026-10-17

What was tried and its results: `TUNING-HISTORY.md` (summary table and findings 1-39, cited here by number).
The data (raw self-play data, engine games, openings, configs, results) and what it is: `C:\Learn\Data\DATA.md`
(the backup set; derived data in `C:\Learn\Tune\hvar` can be regenerated from it).
The procedure of a tuning round: skill `.claude/skills/on-policy-tuning`; commands and scripts:
BarbarossaTuning (`README.md`, section "On-policy tuning of the HCE").

## Where we stand

- Engine (branch `scale`): HCE with 43 weights (`Struct/Weights.hs`) plus an antisymmetric piece square table
  (`Struct/Pst.hs`, incremental, packed mid/end sums in the position), both from on-policy fits; material and
  the other pure piece-square terms are in the PST (33). Compiled: round 2 (`r2-both`, 34) and the drawishness
  scale `scale-c-lin` (37, about +4 Elo; `epScaleNew = 0` gives the old scaling, neutral config
  `compiled-scale.cfg`).
- Candidate (branch `w5`): weights and PST refitted under the new scaling with the own games and the engine
  games weighted x5 (38): +22.7 +- 6.5 at 10k nodes and +27.3 +- 6.0 timed (8000 games) against `scale`;
  against 6 other engines +25.5 +- 16.4 (paired, gauntlet ug-d), better against each of them.
- Gain of the on-policy tuning: +90 Elo timed against the old eval, +78.6 against `boas` (10+0.1, 31, 32);
  round 2 added little (+7 at 10k nodes), round 3 nothing (36).
- The self-play iteration has converged: rounds 1, 2, 3 gave -1.85%, -0.39%, -0.22% loss on the yardstick
  (36). New information came from a missing variable (drawishness, 37) and from games against other engines (38).
- Search: the futility margins (`epFutil1 .. 6`, 24 << (d-1)) are a local optimum, all 5 tested directions
  lose (35).

## Method that works: on-policy tuning

1. The compiled engine plays itself (`SelfPlay --data`, neutral config, 10k-50k nodes per move) and other
   engines (fastchess gauntlet, `run_uci_games.ps1`, Barbarossa with `-d`, joined by `uci_data.py`); every
   evaluated leaf of a move is stored with the real game result.
2. `TuneSGD -X` exports the eval decomposition (exactly linear in the weights, the compiled PST in `const`);
   `hv_fit.py` fits the HCE weights and PST corrections jointly (Texel loss, q = 0, k fixed at the baseline,
   ridge prior on the PST chosen on held-out games); every data source (self-play round, opponent) gets its own
   offset and k (`--sources`, `--source-k`), engine games a higher weight (`--weight`).
3. Yardstick: loss of each candidate's real eval on held-out games (self-play and engine games); then
   node-limited matches (1000 and 10k nodes), timed games and gauntlets; compile in with identity checks.

## General knowledge

- The labels must be our own results: positions of our games (own search, own leaves) with the real results.
  Every source with foreign labels improved the loss and lost in play: another engine's PV leaves (13, 25),
  Stockfish labels (16, 17), adjudicated results of own games (26). Positions from games against other engines
  are fine (our leaves, real results): they hold information the self-play games do not have (38).
- The loss form hardly matters: q = 0 (result only) is best (15, 18). With free HCE weights the eval scale and
  k are degenerate: fix k at the baseline. Sources of different strength or predictability need their own
  offset and k, else the fit bends the eval to explain the opponents (38: k 105 .. 185 between opponents).
- Loss and strength: the yardstick ranks candidates well but has an offset: about -0.8 .. -1% of loss is
  needed before a candidate gains in play (29, 34, 36). Loss decreases in weakly determined directions do not
  buy strength (11); loss increases do cost it (10, 20). The engine-game yardsticks predicted a gain for a
  refit that did not play better (r4, 38): the matches decide.
- A loss gain is not always a strength gain, even beyond the yardstick offset: a feature can predict the result
  without being a good lever for move choice (the strong 50 moves scale, 37).
- Keep the cp scale of the middle game unchanged with every eval change (factors 1 in normal positions): the
  margins and other cp constants of the search depend on it (normalization error: -12.8 Elo, 37).
- Drawishness (pawns of the stronger side, opposite bishops, one wing, R v R, Q v Q) is a hidden variable of the
  linear HCE: a fitted scale of the whole eval gains 2% loss, +10.6 Elo at 10k nodes, +3.7 timed (37).
- Capacity: the linear HCE + PST (about 820 parameters) can only learn an average; weighting the engine games
  x5 (26% of the loss) gave +22.7 (timed +27.3), x15 (51%) only +9.9, x1 (6%) nothing (38). Richer knowledge needs new terms.
- Weaker opponents give less informative data (their errors decide the results, few positions under
  pressure); stronger ones expose our blind spots (38).
- The compiled weights are not in a flat region: random changes of +-1 or 10% cost 10-16 Elo (20). Sensitive
  are material and passed pawns (10).
- The PST helps only when fitted jointly with the HCE weights; alone it takes over the material values (28).
- Gains grow with search depth: test candidates at 10k nodes and more, not only at 1000 (29, 30, 34, 37).
- Deeper games are more predictable: k 148 on 50k-node data against 163 on 10k-node data, same eval (36).
- Data volume: round 1 gained about 0.33% loss per doubling of the data (34); at the fixed point more own data
  gives nothing (36).
- Eval scale and search: the margins are in cp, tuning changes the eval scale (pawn 89 cp middle game, 130
  end game against 100 / 100), so the risk of fixed margins moves with every eval change. Equal risk per depth
  does not predict strength; the transition phase needs the largest margins (35).
- Speed: an eval change must be measured in nodes/s (idle PC, alternating order, several rounds); a list
  based `drawScale` cost 3% and showed in timed games (37).
- Costs: 1000 nodes 20k games +-4.5 Elo; 10k nodes 8000 games +-6.3 (about 3 hours on this PC with 2 shards,
  35 minutes on MEMO with 4); 50k nodes 400 games +-27 (MEMO, 55 minutes); timed 10+0.1 2000 games +-12.5, 8000
  games +-5.7 (MEMO, 12 hours); gauntlet 4800 games against 6 engines about 7 hours (MEMO, 6 in parallel).

## Problems

- The eval does not see forks or one-move tactics (pawn push forks, check and capture). The en-prise terms
  are mover-only and linear in piece values.
- The loss is a static predictor while the engine uses the eval as a leaf function of a search with fixed
  margins (futility, null move, LMR) and at interior nodes (pruning), which are not quiet and not in the data.
- Self-play shares its blind spots: positions where a misjudgment of the eval could be exploited are not
  reached when both sides misjudge them the same way (games against other engines help, 38).
- Small differences (a few Elo) need tens of thousands of games, at depth very expensive.
- Some engines are unusable as opponents: Luna (an illegal move, illegal PVs), Zurichess (illegal promotions with
  upper case pieces); fastchess warns about many harmless PVs after repetitions (Tantabus, Supernova, Weiawaga).

## Ideas to try

1. Eval simplification (branch `esimp`, 39): the centre attacks of pawns, knights and king are in the PST
   (40 weights, +6.6 at 10k nodes against w5); `ewKingPlaceCent` could become a simpler shelter feature plus
   PST. Then the next refit with the engine data of ug-d (w5 and scale against the stronger opponents).
2. More and better engine data: gauntlets against stronger opponents, a weight per opponent by strength in the
   fit (`hv_fit.py --weight 'GreKo|Supernova=10;...'`), enough games of each strong opponent before giving it
   a high weight.
3. New eval terms (the capacity of the linear HCE is the limit, 38):
   a) bias tables (`hv_bias.py`) on the engine games, separately for strong and weak opponents: descriptors
      with a bias against the strong ones but not in self-play are our blind spots;
   b) score test of candidate features (gradient of the loss at weight 0, no refit), computed in Python:
      doubled pawns, connected / protected passers, king distance to passers and the square rule, pawn
      majorities, knight outposts, rook on the 7th, trapped pieces, bishop vs knight by pawn count, threats
      against the side not to move, simplified en-prise (number of own attacked pieces), fork patterns,
      nonlinear king attack; remaining drawish endings of the material signature (rook vs minor, Q v R, 37);
   c) headroom of a general model on the residuals (gradient boosting, small network on piece-square
      inputs): an upper bound of what new features can give, and where;
   d) before fitting new terms, a symmetry test of the eval implementation: eval(p) = eval(colour flip of p)
      for all positions of a data set (flipped FENs exported with `TuneSGD -X`, eval columns compared); every
      new term must pass it;
   e) data augmentation by the file mirror (a <-> h), exact only without castling rights: it adds no new results,
      only the prior "the position is worth the same mirrored"; it may help rare PST squares and rare patterns
      of new terms (validation unmirrored; worth it only if the yardsticks improve). The colour flip adds
      nothing (the eval is colour symmetric by construction).
   Implement only the winners in Haskell (each costs speed), fit, match.
4. Off-policy positions, lower priority: middle game positions of other engines' games (chilo-g4) as start
   positions of our self-play; TD(lambda) / TDLeaf targets from the .meta root scores.
5. Search after the next eval change: repeat the margin measurement (`hv_margins.py --quiet-rest
   --by-phase`), margins in eval units (pawn value of the phase), phase dependent margins; always confirmed
   by matches at depth.
6. Remaining terms that are partly piece-square: `ewAdvPawn5/6` (non passed pawns), `ewKingPlaceCent`
   (opponent material, queens, pawn shelter).
7. A longer time control test (60+1) of the current engine against `boas`.
