# Review of the tuning method of the HCE

Read for this: `TUNING.md`, `TUNING-HISTORY.md` (findings 1-39), `hv_fit.py`, `hv_cand_eval.py`,
`hv_prep_sp.py` and the README of BarbarossaTuning, `Struct/Weights.hs`, `Struct/Params.hs`. Not read in
detail: `Main/TuneSGD.hs`, `hv_scale.py`, `hv_bias.py`. Nothing was run: what follows is judgement from
the documents and the code, and every proposal is a hypothesis until a match says otherwise.

## Verdict

The method is sound, and it is better controlled than most tuning setups I know. Its limit is the one
`TUNING.md` names itself: the on-policy iteration has reached its fixed point, and the linear model has
no capacity left for what the engine games show. I agree with that reading. So the improvements I see
are less about the loss and more about three things around it:

1. the fit reaches only the linear weights, while the largest terms hang on untuned inner parameters;
2. the fit gives a point without error bars, so real signal and weakly determined directions move
   together;
3. variables that predict the result but must not steer the moves sit in the eval instead of in the
   link of the loss.

## What I would keep as it is

- Own positions with own real results, and the rule behind it (foreign labels gain loss and lose
  games: 13, 16, 17, 25, 26). The controls that led there (random configs, synthetic data without
  hidden variables, 20, 22) are what makes the history trustworthy.
- The exact linear export and the joint L-BFGS fit of weights and PST, with k fixed when the weights
  are free, and the conditioning by RMS units of the features.
- Offset and k per data source, and the weight per source.
- Validation by games held out as whole shards, the bootstrap over games in the yardstick, and
  matches as the last word, at depth.
- Keeping the cp scale of the middle game with every eval change.

## What I would do differently

Ordered by what I expect from it.

### 1. A tangent export, so that every parameter can be fitted

The fit sees the 40 weights and the PST. It does not see `epPassA/B/C`, `epPassKingProx`,
`epPassBlockO/A`, `epPassMyCtrl/YoCtrl`, `epMater*`, `epMaterBonusScale`, `epPawnBonusScale`, nor the
constants inside `ksSide` and `attCoef`, `spaceVals`, `proxyBonusArr`, `pawnBonusArr`, `matKCArr`. These
shape the king attack, the king placement and the passed pawns: the terms the history calls sensitive
(10) and "fixed shapes" (5). They are still hand set or from Clop runs on older code (9).

The eval is not linear in them, but it is smooth enough for a linearisation:

`eval(p + dp) ~ eval(p) + sum_j (d eval / d p_j) * dp_j`

`TuneSGD -X` can write the derivative columns by finite differences (the eval with `p_j + 1` minus the
eval with `p_j - 1`, per position; for the table constants one column per knot or per shape
parameter). For the fit these columns are ordinary features with start weight 0: `hv_fit.py` needs no
change. One round moves the parameters a bounded step (a ridge on `dp`, because the linearisation holds
only near the start), then export again; two or three rounds. The yardstick already checks the result
with the real eval, so a bad linearisation shows before any game is played.

This makes the existing machinery reach the whole eval, and it is the general form of what
`hv_scale.py` did by hand for the drawishness scale.

Belongs to the same point: **the integer resolution**. `hv_fit.py` writes `round(new)` into the config.
For `ewKingSafe (1, 0)`, `ewSpace (1, 0)`, `ewKingOpen (3, 1)`, `ewKingPlaceCent (4, 1)`,
`ewKingPlacePwns (-4, 7)`, `ewPassPawnLev (-1, 8)` the rounding is a change of up to 50% of the
weight, and finding 20 measured that +-1 on the tiny weights is exactly what costs Elo. Two remedies:
rescale the features inside the eval (divide the tables by 16, multiply the weight: identical eval,
16 times the resolution), and until then round the small weights first, fix them, and refit the rest
around the rounded values (bounds in L-BFGS-B, as already done for k).

### 2. Error bars for the fit, and steps only where the data decides

The central finding of the first phase (11, 13) is that the loss minimum is displaced in weakly
determined directions, and that moving there costs strength. The on-policy data removed the main cause,
but the fit still returns one point, and near the fixed point (round 3: -0.2%) most of the movement is
noise. Today the only guard is "the yardstick must gain 0.8 to 1%".

What the fit can give in addition, cheaply:

- **Standard errors clustered by game.** The positions of a game share the result, so the effective
  sample is the games (about 90 rows each). A bootstrap over games with warm started refits (the
  optimum is near, a refit needs few iterations), or the sandwich estimator with per game sums of the
  gradient, gives a standard error per weight and the covariance.
- **Shrink the step by its signal.** Report `delta / se` for every weight; take a weight's step fully
  only when it is clear, shrink the others towards the compiled value (empirical Bayes, or simply
  dropping the directions with small eigenvalues of the Gauss-Newton Hessian, where "small" is against
  the game clustered noise). This is a prior on the HCE weights, which today only the PST has (`tau`).
  In round 1 it would have changed little; at the fixed point it separates "nothing to learn" from
  "small but real".
- **The step length as one more parameter, decided by games.** The fit gives a direction; the
  yardstick has an offset and the loss cannot choose the length (11). Three matches on the line
  (0.5, 1, 1.5 of the step) cost what three candidates cost and answer the question the loss cannot.

### 3. Nuisance variables into the link, not into the eval

Finding 37 has the lesson already: the strong 50 moves scale predicts the result and loses 15 Elo as an
eval term. The general form: a variable can be a good predictor of the result without being a lever for
the choice of the move. The per source offset and k are exactly such variables, and they are kept out
of the engine. I would extend that:

- k (and an offset) as a function of the 50 moves counter, the number of plies played, and the search
  depth of the data, fitted in the loss only. Then the weights no longer have to explain "long games
  are draws" through terms that correlate with long games (end game weights, few pawns).
- **A draw model.** The loss treats a draw as half a win under a two outcome sigmoid. With
  `P(win) = sigmoid((e - d) / k)`, `P(loss) = sigmoid((-e - d) / k)` the draw margin `d` can depend on
  the drawishness descriptors (pawns of the stronger side, opposite bishops, one wing, the counter).
  That separates "who is better" (`e`, the lever) from "how often is this drawn" (`d`, not compiled
  in, or compiled in only where it helped as a scale, as `scale-c-lin` did). The refits under the new
  scaling (38) would then run against a cleaner target.

I expect a small direct gain and a real indirect one: weights that are less bent, and a yardstick
whose offset shrinks, because part of today's loss gains are nuisance prediction.

### 4. Less noise in the labels

Every leaf of a game carries the final result. For a position 60 moves before the end that label is
close to a coin; all positions weigh the same, and long games weigh more than short ones.

- **Weight per game.** A fixed number of positions per game, or weights `1 / sqrt(rows of the game)`.
  Long drawn games with a high counter are over represented now, which is part of the drawishness
  story. Cheap to test: one more weight vector in `Problem`.
- **TD targets from the own later searches** (`TUNING.md` idea 4, where it is low priority; I would
  raise it). The root score of the same search is no target: it is the eval of this very leaf. But
  the root scores of the following own moves are: target = `lambda` mix of the result and the scores
  some moves later, from the `.meta` files. It is own play and own labels, so it does not break the
  rule of finding 17 (foreign search labels), and it replaces a noisy label by a less noisy one for
  the early positions. The risk is that it reinforces the own blind spots, so with `lambda` near the
  result end first, and judged by matches. With the learning curve of 0.33% per doubling (34), a
  label with half the variance is worth as much as doubling the games.

### 5. Stronger and different opponents from the own engine

Finding 38: the engine games carry what self-play does not, stronger opponents expose the blind
spots, weaker ones give poor labels. External engines are slow to run and limited in number. Two
sources that cost only self-play time:

- **Node odds:** the compiled engine with 10k nodes against the same engine with 40k or 80k. The
  deeper side refutes what the shallow side misjudges; the leaves of the shallow side are positions
  under pressure with real results. As its own source with offset and k, weighted like the engine
  games.
- **Other evals in the same binary:** the old eval (`pst.off = 1`, old weights), `boas`, earlier
  rounds. They are weaker, so they count less, but they disagree with the current eval in known
  ways, and games between different evals reach positions that identical evals avoid together.

### 6. Screening of new features

The score test of `TUNING.md` 3 b) (gradient at weight 0) ranks a candidate feature by itself. A
feature that is nearly a combination of existing ones (and of the PST) shows a large gradient and
gains nothing in the refit. With the Gauss-Newton Hessian the expected gain after a refit is
`g' S^-1 g / 2`, with `S` the Schur complement of the new columns against the existing ones: one
linear solve, no refit, and with the game clustered noise of point 2 it comes with a z value. I would
rank candidates by that.

### 7. Decisions by matches

- Several accepted steps were about one standard error (+6.8 +- 6.5, +6.6 +- 6.4, +3.7 +- 5.7). Each
  decision is defensible, and the history says "small" honestly. But accepted 1 sigma gains do not
  add up to their sum. An anchor match of every compiled state against a fixed reference (`boas`, and
  the gauntlet) keeps the sum honest; idea 7 of `TUNING.md` is this.
- Fixed game counts spend the same on a clear result as on a close one. A sequential test (the
  pentanomial SPRT that SelfPlay and fastchess have) with bounds for "at least not worse" spends the
  games where the decision is close.
- Node limited matches do not see speed. The rule is in `TUNING.md`; I would make the speed number a
  fixed line of every candidate report, next to the yardstick.

## What I would not spend time on

- Loss forms (q, cross entropy against squared error): 15 and 18 settle it, and with on-policy data
  the question is the label, not the loss.
- More self-play data of the same kind: 36 settles it.
- The coordinate search of `TuneSGD -O`: the export and the Python fit replace it.

## If only one thing

The tangent export of point 1. The capacity limit of 38 is partly a limit of what the fit can reach:
the king attack and the passed pawns have one weight each and a dozen constants nobody has fitted on
on-policy data. Reaching them needs no new term in the engine and no new fitting code, and it prepares
the ground for the term changes of `EVAL-REVIEW-EXISTING.md` and `EVAL-REVIEW-MISSING.md`.
