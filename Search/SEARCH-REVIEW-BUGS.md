# Search review: correctness bugs (branch lmrm, e908bd8)

Read only: `Search/Core.hs`, `Moves/History.hs`, and what they call (`Moves/Internal/Base.hs`,
`Hash/TransTab.hs`, `Uci/UciGlue.hs`). Nothing was built or run: every finding is from reading the
code, none is measured. Style and ideas are left out.

| # | Where | What | Weight |
|---|-------|------|--------|
| 1 | `Moves/History.hs:194`, `Search/Core.hs:493,505` | history bonus overflows Int32 for root moves (absdp 0) | known (S17, S29): fix measured about -4, not adopted |
| 2 | `Search/Core.hs:665-699` | null move "proves" not mated without information when beta is mated + grain | medium |
| 3 | `Hash/TransTab.hs:242` | TT replacement compares the move bits instead of generation / type / depth / nodes | medium (outside the two files) |
| 4 | `Search/Core.hs:733`, `648` | stalemate with only pinned pieces not seen in futility pruned zero window nodes | low |
| 5 | `Hash/TransTab.hs:251` | node count over 32 bits runs into depth / type bits of the TT entry | low |
| 6 | `Moves/History.hs:300` | quiet moves beyond 128 are dropped silently | low |
| 7 | `Search/Core.hs:513-525` | `insertToPvs`: a move which gets mated is ordered like a mating move | low (order only) |
| 8 | `Search/Core.hs:80-82, 319, 796` | parameter values which index out of range or overflow | low (non default values only) |
| 9 | `Search/Core.hs:1095, 1110` | QS delta cut always fires when alpha is mate - grain: the QS cannot mate there | low to medium |

## 1. History bonus of root moves overflows (wrong sign)

`Moves/History.hs:194-197`

```haskell
histw !d = 1 `unsafeShiftL` dm
    where !dm = max 0 (maxd - d)
          maxd = 31
```

The comment says "d is absolute depth, root = 1", but the search counts the root as 0 (`pvsInit`,
`absdp = 0`). `checkFailOrPVRoot` calls `betaCut (absdp sst) e` (`Search/Core.hs:493` and `505`) after
`absdp` was set back to the root value (`Core.hs:441`), so `toHist h m 0` and `histw 0 = 1 << 31`,
which is `minBound :: Int32` (-2^31), not +2^31.

`addHist` (`History.hs:203-215`) then computes `oh - p` with `p = minBound`:

- `oh < 0` (the usual case with `epHistPst = 3`: a knight or bishop move to a good square starts
  negative): the entry becomes `oh + 2^31`, about +2^31, the worst possible value. The best root move
  gets the worst history.
- `oh >= 0`: wraps to about -2^31, below `lowLimit`. Every later cut on that entry takes the rescale
  branch and halves the whole history table (the entry tends to -2^30 from below with the ply 2
  bonus 2^29, so it stays under the limit for about 30 updates).

Path: any position where the root best move is quiet, e.g. start position, 1. Nf3. This happens for the
first root move of every iteration and for every new best root move. The entry is per colour, piece
and to square, so it is the one used for the same kind of move at plies 2, 4, ... of the tree: those
moves are sorted last (or first) among the quiet moves whatever their real history, and in the second
case the history of all other moves decays.

`contUpdate` already guards against this (`histw (max 1 d)`, `History.hs:122`); `toHist` does not.

Fix: `toHist h m d = addHist h (adr m) (histw (max 1 d))`. (Passing `absdp + 1` would match the
comment, but changes all weights.)

Already known and measured (SEARCH-TUNING.md on `rel090`: found in S17, tested in S29 on `chcm`):
root as depth 1 (this fix) -4.12 +- 6.52 after 5824 games, no root update -3.37 +- 8.67 after 3400
games, both H0 under [0, 5] and not neutral under [-3, 2]. Neither form was adopted, the overflow
stays. The weight "high" above is the weight of the defect in the code, not of a gain: the measured
effect of removing it is zero to slightly negative.

Not in S17 / S29, from this reading: the overflow has a second branch. With `oh >= 0` the entry wraps
to about -2^31, and from then on every cut on that entry halves the whole history table. S29 names
only the first branch (root best move becomes the worst). If the fix loses anything, the lost
halvings (an aging of the history after a new root best move) are a candidate cause next to the
"variety" guess of S29. Not measured: a count of the rescale events with and without the fix would
tell whether this branch is frequent enough to matter.

## 2. Null move at a mated bound always "fails high"

`Search/Core.hs:665-699`: `nullMoveFailsHigh` has no test for a mate bound, while `isRevFutil`
(`Core.hs:964`, `nearmate b`) and `isPruneFutil` (`Core.hs:952`, `nearmate a`) have one.

With `b = mated + scoreGrain = -19996` (the node is asked "are you not mated?"):
`nmb = b - 8 = -20004`, `nma = -20008`, the null move child is searched with the window
(20004, 20008). No score reaches it, the child fails low with 20004, so `val = -20004 >= nmb`
(`Core.hs:683`) is always true and the node returns `b` without having learned anything. The trigger
`v < b + nulTrig * scoreGrain` never stops it at this bound.

Because mate scores carry no distance, this window occurs every time a side has only mated moves so
far: a node with alpha = -20000 searches the next moves with (-20000, -19996), their children with
b = 20000, the grandchildren with b = -19996.

Path where it matters: root, first move scores -20000. Second root move, child Q (b = 20000,
CutNode), grandchild G (b = -19996, AllNode: no null move, so the first, reduced search is real). A
late quiet move of Q is reduced, the reduced search of G finds G mated (`sr >= b`,
`Core.hs:813`), the re-search at full depth swaps the node types (`Core.hs:822`): G is now a CutNode,
not in check, has a piece, `d1 >= 2`, so the null move returns "not mated" and the mate found by the
reduced search is taken back by its own verification. Q fails low, the root sees an escape and
re-searches with the full window ("Research" in the log). The same repeats at every level below.
Result: forced mates by quiet moves are denied in zero window nodes and only found through PV
re-searches, at higher depth or cost.

Fix: `| nearmate b = return NoNullMove` in the guards of `nullMoveFailsHigh` (as in `isRevFutil`).

## 3. TT replacement looks at the wrong bits (outside the two files)

`Hash/TransTab.hs:242`

```haskell
lowm = lowc .&. 0xFFFF	-- mask the move
```

The comment and the layout (generation, type, depth, nodes above the move) want the move masked away:
this keeps only the move. `scoreReplaceLow` therefore replaces the entry of the cell with the smallest
move code: entries with white moves always before those with black moves (bit 15), then by piece and
squares. Generation, bound type, depth and nodes play no role, except for the wrap test
`generation > gen`.

Path: any full cell (4 entries with other keys): a depth 1 entry of the current search survives, a
deep entry with a white pawn move is overwritten.

Minimal fix: `lowm = lowc .&. complement 0xFFFF`. Changes the search: needs measurement.

### The order after the minimal fix

With the move masked away the order is generation > type > depth > nodes, as the comments of
`writeCache` and `scoreReplaceLow` say. Two dominances follow, none of them measured:

- Generation over everything: any entry of the previous move is replaced before a depth 1 entry of the
  current search. Defensible (old entries help mostly the first, cheap iterations, many are off the new
  tree), but absolute: a current depth 1 entry costs a handful of nodes to recompute. Entries are not
  refreshed by a hit (`ttRead` does not write): an old entry which still cuts stays old.
- Type over depth, inside the current generation: every upper bound entry (type 0) is replaced before
  any lower bound entry, e.g. a depth 15 fail low entry (the most expensive kind to recompute) to keep
  a depth 1 cut entry. This is active all the time once the cells are full, not only across moves, and
  is the more doubtful of the two.

`generation > gen` is only the wrap of the 8 bit counter: an entry is written with the current `gener`,
which only grows (`newGener`, no reset). The one other case is memory never written (`mallocArray`
does not zero). Side effects of that test: it ends at the first such entry without looking at the
other slots of the cell for the same key (the key can then be twice in the cell; the newer one is in
front and is the one found, a slot is wasted), and after a wrap an entry more than 256 searches old
can look young (negligible).

### Proposed rule to test first

Age as a penalty against depth, type and nodes only as tie breaks, no special case for the wrap:

- age = (gen - generation of the entry) mod 256, by the Word64 subtraction
- value = depth - 4 * age
- replaced: the entry with the smallest (value, type, nodes)

```haskell
scoreReplaceLow gen lowc crt rep sco cont
    | key < sco = cont crt key
    | otherwise = cont rep sco
    where age   = (gen - (lowc .&. generMsk)) `unsafeShiftR` 56	-- 0 .. 255
          depth = (lowc `unsafeShiftR` 48) .&. 0x3F
          typ   = (lowc `unsafeShiftR` 54) .&. 0x3
          nodes = (lowc `unsafeShiftR` 16) .&. 0xFFFFFFFF
          val   = 1024 + depth - 4 * age	-- always > 0
          key   = (val `unsafeShiftL` 34) .|. (typ `unsafeShiftL` 32) .|. nodes
```

(The `term` argument and its call in `writeCache` go away.)

The 4 plies per generation are an estimate, not a known optimum: 2 plies of the old depth are used up
by the 2 moves played, the rest is a discount for entries which cannot be reached any more. With it an
entry of the previous move with depth 5 or more survives against a current depth 1 entry, entries 2 or
more moves old go first almost always. Second value to try: 8 (near to generation first, but deep
entries survive one move).

Test:

- This rule directly against the current engine, not the minimal fix first: that one carries the type
  over depth dominance, its result says little about either question.
- With a hash size at which the table fills at the time control of the test (minimum 2^18 entries,
  4 MB): otherwise full cells are rare and the match measures nothing.
- The generation advances only in the UCI engine (`posNewSearch`, `Main/Barbarossa.hs:523`); SelfPlay
  never calls it, the age is always 0 there. Node limited SelfPlay games and the perf node counts see
  only the depth, type, nodes part of the rule: the penalty per generation needs timed UCI games.
- The perf node counts differ from the baseline: expected here.
- A refresh of the generation at a TT cut is a possible second step, not part of the first test.

## 4. Stalemate not seen in a futility pruned zero window node

`Search/Core.hs:733-734`

```haskell
if prune && (zw || movno nst > 1) && canPruneMove (cpos nst) e
   then return $! nst { movno = movno nst + 1 }
```

In a zero window node the pruned moves are counted without being made, so `movno nstf == 1`
(`Core.hs:648`) no longer means "no legal move". King moves are generated legal, so this needs a
position where the only pseudo legal moves are those of pinned pieces.

Position: `kn1R4/8/1K6/8/8/8/8/8 b - - 0 1` (stalemate: Na6, Nc6, Nd7 are illegal, the knight is
pinned). Nd7 "gives check" and is tried (illegal), Na6 and Nc6 are pruned when static + margin <=
b - grain: the node fails low and stores an upper bound below 0 with depth d, although the value is 0.
It matters when b <= 0 for the stalemated side, i.e. when the other side looks for more than a draw:
the stalemating move looks winning in remaining depths 1 to 6. The PV search is right
(`movno nst > 1` there needs a legal move first).

Fix: count a pruned move only after a legal move was seen in the node (a flag in `NodeState`, set
after the first successful `doMove`; before that, make the move and prune after the legality test).
`quietFilter` (`Base.hs:109-112`) keeps one pruned move in the list for the same test and needs the
same care.

Related, not counted as a bug: `isPruneFutil` does not exclude positions in check. The result stays a
correct bound there (mated <= alpha), but the static score in check is the one the QS does not trust
(`Core.hs:1101`).

## 5. TT node count is not limited to its 32 bits

`Hash/TransTab.hs:251`: `(fromIntegral nodes `unsafeShiftL` 16)` with `nodes :: Int64`. From 2^32 nodes
on the upper bits are or-ed into depth (bits 48-53) and type (54-55): the entry gets a greater depth
and possibly type 3. Type 3 is no cut in `pvSearch` / `pvZeroW`, but `qSearchFound`
(`Core.hs:1069-1071`) treats everything which is not 1 as an upper bound.

Paths: at the root `nodes'` includes `sRSuc` (`Core.hs:475-477`), the sum of the node counts of all TT
hits, which passes 2^32 long before the real node count; in the tree `deltan` passes it for the
nodes near the root in searches of more than about 4.3 * 10^9 nodes (long analysis).

Fix: `fromIntegral (min 0xFFFFFFFF nodes)` in `quintToCacheEn`.

## 6. More than 128 quiet moves: the rest is dropped

`Moves/History.hs:300-301` (`maxMoves = 128`), used by `U.fromListN maxMoves` in `makeMTS` (`318`),
`dirSort` (`363`) and `dirSortC` (`290`). `fromListN` cuts the list: quiet moves after the 128th of a
sort group are never searched. The comment accepts it as rare; it is still a wrong result when it
happens (positions with several queens, test positions with 200+ moves).

Fix: `U.fromList` (or `fromListN (length ms)`), or sort the first 128 and append the rest unsorted.

## 7. insertToPvs: mated counts as mate

`Search/Core.hs:523-524`: `pmate = pnearmate ...` is true for both signs. A root move with score
-20000 (possible only while alpha is still `alpha0`, i.e. the first moves) is kept in front of all
normal moves (`not pmate && qmate = q : ...`), so the next iteration searches the move which gets
mated second. Order only for d > 1. At d == 1 the head of `pvsl` is the result (`Core.hs:390-391`):
this is the known open issue (fail low moves with score == alpha and more nodes in front), not
repeated here.

Fix: `pmate = pathScore (pvPath p) >= mateScore - 255` (same for `qmate`).

## 8. Parameters without range check

Only with values other than the defaults:

- `Core.hs:319-320`: `epLmrFix` and `epLmrIni` are used as level without the clamp which `epLmrMax`
  gets (`315`): a value over 19 (or `epLmrIni < 0`) is an index error in `lmrtb ro ! (lmrlev, w)`
  (`910`).
- `Core.hs:796` with `epLmrNoD = 0`: for `d1 == 0` `reduceLmr` returns `max 1 (0 - r) = 1`, the
  "reduced" search is one ply deeper than the full one (QS) and, when it does not fail low, the QS is
  searched after it. `pvInnerLoopExten` has `min d1` (`780`), `pvInnerLoopExtenZ` not.
- `Core.hs:80-82`: `floor ((p / 10) ^ d)` is converted to `Int` before the `min`: over 2^63 (base 3.0
  from d = 40) the result is undefined. Also `2^63` for d = 63 with the default base is `minBound`.
  Both depths are out of reach today. Fix: `min` on the `Double`, `2^(min 50 d)`.

## 9. QS delta cut at a mate bound

Two windows at the mate bounds, used below:

- L = (-20000, -19996): "are you not mated?"
- H = (19996, 20000): "can you mate?"

`Search/Core.hs:1110` (`qsNormal`) and `1095` (`qsInCheck`):

```haskell
!dcut <- qsDeltaCut $ (qsScore qsa) - (staticScore pos) - qsDeltaMargin
```

At H, with a = 19996, the argument is far over a queen: `qsDelta` (`Moves/Internal/Base.hs:343-356`)
returns True at `matPiece White Queen < a` and the QS returns alpha before any capture or front check
is generated; in check, before the evasions. With a normal window the QS does find a mate given by its
own move (capture or front check, the child is in check without escape and returns `mated`): at H it
never can.

Path: a node with alpha = -20000 (its first move gets mated) searches the next move with the zero
window; the child Q has H and remaining depth 0, and the opponent mates by a capture (or a quiet check
in the first QS level). The QS of Q returns 19996, "no mate": the move looks like an escape, the full
window re-search finds the mate (normal window in the QS) and fails low after all. Where no PV
re-search follows (zero window nodes deeper in the tree) the mate is seen one iteration later. The
"no mate" bound is used again only at H windows, it does not get into normal scores.

Fix: no delta cut when alpha is a mate bound (`nearmate (qsScore qsa)`), as `isPruneFutil` has it.

## Mate bounds: the other places (checked, sound)

| Place | Window | Result without search? | Correct? |
|---|---|---|---|
| Null move | L | yes, always fails high | no (finding 2) |
| QS delta cut | H | yes, always cuts | no (finding 9) |
| QS stand pat | L | yes, at once | yes |
| LMR re-search | L, H | no | yes |
| IID | L | no result, only no move | harmless |

- QS stand pat at L (`Core.hs:1106`): static >= -19996 is always true, the QS returns at once. Correct
  for what the QS can know: not in check at the horizon is not mated now. In check at L the first
  legal evasion ends the node the same way (its child has H and is cut by delta at once). At H the
  stand pat never fires.
- LMR re-search: every step is a real search. A mate found at reduced depth is taken without
  re-search, and that is sound: at L the moves of the defender are not futility pruned (`nearmate a`),
  and its null move and stand pat can only say "not mated". The only fault is the swap of the node
  types in the re-search, which makes the null move of finding 2 possible. While alpha = -20000 most
  other moves beat alpha in the reduced search: the re-searches are systematic there and push the
  adaptive level down (the LMR mate escape idea, not a correctness matter).
- IID: gives a move, never a result. In a CutNode at L its depth 2 `pvZeroW` runs into the null move
  of finding 2 and returns no move: wasted, gone with that fix.
- After a mate was found in a PV node (alpha = 20000) every further move is refuted in a few nodes:
  nothing is over mate. A shorter mate cannot replace the first one: this follows from the mate scores
  without distance.
- TT entries of normal windows cut at the mate windows: any lower bound at L, any upper bound at H,
  also upper bounds of futility pruned nodes. Normal TT use, but "no mate" at H can come from a
  pruned search.

## Mate scores without distance: consistency

- TT: consistent. A mate is always exactly +-20000 whatever the ply, so stored and probed scores need
  no adjustment, and the 16 bit score field holds the whole range (+-20001). All returned scores are
  cut to the window (`failHardNoValidMove`, `trimax`, the QS), stores use `b`, `b - grain` or an in
  window score with the matching type.
- Search: the mate tests are `nearmate` in futility and reverse futility; the null move lacks one
  (finding 2), `insertToPvs` does not look at the sign (finding 7) and the QS delta cut does not
  exclude a mate bound (finding 9).
- Outside the search: the UCI mate distance comes from the PV length (`Struct/Context.hs:231`), and a
  PV ends early at TT cuts in alpha/beta nodes (`Core.hs:552`, path `[e]`) or at type 0 entries: the
  announced distance can be too short.

## Checked, no bug found

- Re-search conditions: zero window (a, a + grain) and (b - grain, b), reduced, full depth, full
  window: each fires once, the comparisons fit the fail hard returns (`Core.hs:783`, `813`, `461`).
- Bound types: `rbmch` 0 / 1 / 2 at the end of `pvSearch` and `pvZeroW` matches the score stored; the
  in window store during the loop is a lower bound (`845`); the move of a type 0 entry is kept out of
  paths (`ttPathMoves`).
- TT probes: `tp == 0 && hsc < b` and `tp == 1 && hsc >= b` in `pvZeroW`, `<= a` / `>= b` in
  `pvSearch`; QS limit improvement keeps a < b and is sound with fail hard.
- Abort: every path undoes its move, results after an abort are not stored and not used.
- History and continuation tables: index ranges, saturation of `contUpdate` and `contVal`.
- Wasteful, not wrong: the TT move or a killer can be in the list twice (`bestFirst`, `Core.hs:1287`:
  `kl` is not filtered against `e` and `es1`); the root stores its best so far as exact (type 2,
  `Core.hs:503`), which is only a lower bound until the loop ends (remains after an abort).
