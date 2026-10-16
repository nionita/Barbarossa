# Tuning the HCE evaluation - state of 2026-10-15

What was tried and its results: `TUNING-HISTORY.md` (summary table and findings 1-37, cited here by number).
The data (raw self-play data, engine games, openings, configs, results) and what it is: `C:\Learn\Data\DATA.md`
(the backup set; derived data in `C:\Learn\Tune\hvar` can be regenerated from it).
The procedure of a tuning round: skill `.claude/skills/on-policy-tuning`; commands and scripts:
BarbarossaTuning (`README.md`, section "On-policy tuning of the HCE").

## Where we stand

- Engine (branch `fold`): HCE with 43 weights (`Struct/Weights.hs`) plus an antisymmetric piece square table
  (`Struct/Pst.hs`, incremental, packed mid/end sums in the position), both from on-policy fits; material and
  the other pure piece-square terms are in the PST (33). Compiled: round 2 (`r2-both`, 34).
- Gain of the on-policy tuning: +90 Elo timed against the old eval, +78.6 against `boas` (10+0.1, 31, 32);
  round 2 added little (+7 at 10k nodes), round 3 nothing (36).
- The iteration has converged: rounds 1, 2, 3 gave -1.85%, -0.39%, -0.22% loss on the yardstick. Neither more
  nor deeper data of the engine's own games moves the HCE + PST any more (36).
- Search: the futility margins (`epFutil1 .. 6`, 24 << (d-1)) are a local optimum, all 5 tested directions
  lose (35).

## Method that works: on-policy tuning

1. The compiled engine plays itself (`SelfPlay --data`, neutral config, 10k-50k nodes per move) and writes the
   evaluated leaf of every move with the real game result.
2. `TuneSGD -X` exports the eval decomposition (exactly linear in the weights, the compiled PST in `const`);
   `hv_fit.py` fits the HCE weights and PST corrections jointly (Texel loss, q = 0, k fixed at the baseline,
   ridge prior on the PST chosen on held-out games).
3. Yardstick: loss of each candidate's real eval on held-out games of the same engine; then node-limited
   matches (1000 and 10k nodes) and timed games; compile in with identity checks.

## General knowledge

- Data must be on-policy: positions of the engine's own games with the real results. Every off-policy source
  improved the loss and lost in play: another engine's PV leaves (13, 25), Stockfish labels (16, 17),
  adjudicated results of own games (26). The same refit that lost on chilo data gained on own data (28).
- The loss form hardly matters: q = 0 (result only) is best (15, 18). With free HCE weights the eval scale and
  k are degenerate: fix k at the baseline.
- Loss and strength: the yardstick ranks candidates well but has an offset: about -0.8 .. -1% of loss is
  needed before a candidate gains in play (29, 34, 36). Loss decreases in weakly determined directions do not
  buy strength (11); loss increases do cost it (10, 20).
- The compiled weights are not in a flat region: random changes of +-1 or 10% cost 10-16 Elo (20). Sensitive
  are material and passed pawns (10).
- The PST helps only when fitted jointly with the HCE weights; alone it takes over the material values (28).
- Gains grow with search depth: test candidates at 10k nodes and more, not only at 1000 (29, 30, 34).
- Deeper games are more predictable: k 148 on 50k-node data against 163 on 10k-node data, same eval (36).
- Data volume: round 1 gained about 0.33% loss per doubling of the data (34); at the fixed point more data
  gives nothing (36).
- Eval scale and search: the margins are in cp, tuning changes the eval scale (pawn 89 cp middle game, 130
  end game against 100 / 100), so the risk of fixed margins moves with every eval change. Equal risk per depth
  does not predict strength; the transition phase needs the largest margins (35).
- Costs: 1000 nodes 20k games +-4.5 Elo; 50k nodes 400 games +-27 (MEMO, 4 shards,
  55 minutes); timed 10+0.1 2000 games +-12.5.

- A loss gain is not always a strength gain, even beyond the yardstick offset: a feature can predict the result
  without being a good lever for move choice (the strong 50 moves scale, 37). Test eval changes at 10k nodes or
  more; keep the cp scale of the middle game unchanged (factors 1 in normal positions, 37).
- Drawishness (pawns of the stronger side, opposite bishops, one wing, R v R, Q v Q) is a hidden variable of the
  linear HCE: a fitted scale of the whole eval gains 2% loss, +10.6 Elo at 10k nodes, +3.7 timed (37).

## Problems

- The eval does not see forks or one-move tactics (pawn push forks, check and capture). The en-prise terms
  are mover-only and linear in piece values.
- The loss is a static predictor while the engine uses the eval as a leaf function of a search with fixed
  margins (futility, null move, LMR) and at interior nodes (pruning), which are not quiet and not in the data.
- Self-play shares its blind spots: positions where a misjudgment of the eval could be exploited are not
  reached when both sides misjudge them the same way.
- Small differences (a few Elo) need tens of thousands of games, at depth very expensive.

## Ideas to try

1. Hidden variables at the fixed point (the weights are no longer what is wrong, the residual is the
   features' limit), on the round 3 data (2.6M positions, `C:\Learn\Data\raw\selfplay\r3-gen3`):
   a) bias tables without a model: mean residual by material signature (drawish endings, opposite bishops,
      imbalances), by phase (k per phase: is the mid / end scale consistent? exchanges depend on it), by side
      to move, check, attacked pieces;
   b) score test of candidate features (gradient of the loss at weight 0, no refit), computed in Python:
      doubled pawns, connected / protected passers, king distance to passers and the square rule, pawn
      majorities, knight outposts, rook on the 7th, trapped pieces, bishop vs knight by pawn count, threats
      against the side not to move, simplified en-prise (number of own attacked pieces), fork patterns,
      nonlinear king attack;
   c) headroom of a general model on the residuals (gradient boosting, small network on piece-square
      inputs): an upper bound of what new features can give, and where.
   Implement only the winners in Haskell (each costs speed), fit, match.
2. Off-policy positions with on-policy labels:
   a) games against other engines (Barbarossa writes `--data` also in UCI mode, fastchess, only our side's
      positions): opponents with another eval steer into our blind spots;
   b) middle game positions of other engines' games (chilo-g4) as start positions of our self-play;
   c) older Barbarossa versions as opponents (cheap, but similar blind spots);
   d) lower priority now (variance is not the limit): TD(lambda) / TDLeaf targets from the .meta root scores,
      a fit on other data as the center of the ridge prior.
3. Search after the next eval change: repeat the margin measurement (`hv_margins.py --quiet-rest
   --by-phase`), margins in eval units (pawn value of the phase), phase dependent margins; always confirmed
   by matches at depth.
4. Remaining terms that are partly piece-square: `ewAdvPawn5/6` (non passed pawns), `ewKingPlaceCent`
   (opponent material, queens, pawn shelter).
5. A longer time control test (60+1) of the current engine against `boas`.
