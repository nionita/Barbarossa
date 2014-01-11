module POI where

-- POI is a point of interest, i.e. a data structure around an important square
-- together with some information of a another type (e.g. for SEE move generation, or for king safety, etc.)
data POIProto
    = POIProto {
        poiCenter   :: !Square,	-- central square of the poi
        poiWAttacks :: !BBoard,	-- BB of white pieces attacking the poi center, inclusiv x-ray
        poiBAttacks :: !BBoard,	-- BB of black pieces attacking the poi center, inclusiv x-ray
        poiBlockers :: !BBoard,	-- BB of blocking squares (which, when freed, could influence the poi)
        poiKInf     :: !BBoard,	-- BB of all squares on which a comming king will influence the poi
        poiRInf     :: !BBoard,	-- BB of all squares on which a comming queen or a rook will influence the poi
        poiBInf     :: !BBoard,	-- BB of all squares on which a comming queen or a bishop will influence the poi
        poiNInf     :: !BBoard,	-- BB of all squares on which a comming knight will influence the poi
        poiWPInf    :: !BBoard,	-- BB of all squares on which a comming black(!) pawn will influence the poi
        poiBPInf    :: !BBoard	-- BB of all squares on which a comming white(!) pawn will influence the poi
      }

type POI a = (POIProto, a)

-- We must have three arrays of bitboards, which should be precalculated (every taking
-- 2^6 x 2^6 bbs = 4K bbs = 32 KB)
-- one of the form bbBetween!(sq1,sq2): bitboard of all squares between the two square indices
-- one of the form bbDirection!(sq1,sq2): bitboard of all squares from sq1 to sq2 and further
-- one of the form bbShadowFromTo!(sq1,sq2): shadow of sq2 seen from sq1 (sq2 not included)
-- (actually some could be calculated from the others - e.g. as intersection sq1->sq2 and sq2->sq1, but for performance...)

poiProto :: MyPos -> Square -> POIProto
poiProto p sq = POIProto {
                     poiCenter = sq, poiWAttacks = poiaw, poiBAttacks = poiab, poiBlockers = poib,
                     poiKInf = poiki, poiRInf = atrs, poiBInf = atbs, poiNInf = poini,
                     poiWPInf = poipw, poiBPInf = poipb
                }
    where ocpb = occup p `less` (bishops p .|. queens p .|. bpinf .|. wpinf)
          ocpr = occup p `less` (rooks p   .|. queens p)
          atrs = rAttacs sq ocpr	-- rook attacs from sq (with line/column xray)
          atbs = bAttacs sq ocpb	-- bishop attacs from sq (with diagonal xray)
          atqs = atrs .|. atbs		-- queen attacs from sq (with all xray)
          bpinf = poiBPInf opoi .&. pawns p .&. white p	-- black & white trick because we look
          wpinf = poiWPInf opoi .&. pawns p .&. black p	-- from the sq point of view
          poiki = kAttacs sq
          poini = nAttacs sq
          poipw = pAttacs sq White
          poipb = pAttacs sq Black
          poia  = poiki .&. kings p
              .|. poini .&. knights p
              .|. bpinf .|. wpinf
              .|. atqs  .&. queens p
              .|. atrs  .&. rooks p
              .|. atbs  .&. bishops p	-- all attacks
          poiaw = poia .&. white p
          poiab = poia .&. black p
          poib  = atqs .&. occup p `less` allMarginBits	-- blocker at board margin are irelevant

-- Recalculate a poi given a recalculation function for the user data, a position and the old poi
-- The decision to recalculate comes from the function poiIsChanging (see below)
-- From the point of view of the POI internals (all fields except poiData) the effort to
-- recalculate is the same as when creating a new POI (including allocation, as we must
-- recalculate at least the blockers)
-- The only difference is for the user data (poiData) which will be recalculated only when
-- the attackers (white & black) are changing. From this it follows one invariant which should
-- hold (not checked - we could do it by the type of the user function, but that is clumsy now):
--
-- The value of the user data of the POI must depend only on the center, white and black attackers of the POI
--
-- Center and influence squares for non-sliding pieces never change (so they are copied from
-- the old poi)
-- Blockers and influence squares for slider have always to be recomputed
-- The rest, only when the attackers change (so attackers have also to be recalculated,
-- at least for that check)
poiRecalc :: (MyPos -> POIProto -> a) -> MyPos -> POI a -> POI a
poiRecalc f p (opoi, a)
    | poiWAttacks opoi == poiWAttacks npoi || poiBAttacks opoi == poiBAttacks npoi = (npoi, a)
    | otherwise                                                                    = (npoi, a')
    where sq   = poiCenter opoi
          npoi = poiProto p sq
          a'   = f p npoi

-- When moving a piece, there is always a chance that this piece will become itself
-- the center of a new poi, and for the king and adiacent squares, this is always the case
-- This function returns a new poi when either it's a permanent one or only when there
-- are attackers
poiCreate :: (MyPos -> POI a -> a) -> MyPos -> Square -> Bool -> Maybe POI
poiCreate f p sq force
    | force || poia /= 0 = Just (npoi, a)
    | otherwise          = Nothing
    where npoi = poiProto p sq
          poia = poiWAttacks npoi .|. poiBAttacks npoi
          a    = f p npoi

{--
    POI {
                          poiCenter = sq, poiWAttacks = poiaw, poiBAttacks = poiab, poiBlockers = poib,
                          poiKInf = poiki, poiRInf = atrs, poiBInf = atbs, poiNInf = poini,
                          poiWPInf = poipw, poiBPInf = poipb, poiSEEPcs = poisee, poiSEEMvs = poim
                       }
          ocpb = occup p `less` (bishops p .|. queens p .|. bpinf .|. wpinf)
          ocpr = occup p `less` (rooks p   .|. queens p)
          atrs = rAttacs sq ocpr	-- rook attacs from sq
          atbs = bAttacs sq ocpb	-- bishop attacs from sq
          atqs = atrs .|. atbs		-- queen attacs from sq
          atsl = atqs .&. queens p .|. atrs .&. rooks p .|. atbs .&. bishops p	-- all attacking sliders
          bpinf = poiBPInf opoi .&. pawns p .&. white p	-- black & white trick because we look
          wpinf = poiWPInf opoi .&. pawns p .&. black p	-- from the sq point of view
          poiki = kAttacs sq
          poini = nAttacs sq
          poipw = pAttacs sq White
          poipb = pAttacs sq Black
          poia =     poiki .&. kings p
                 .|. poini .&. knights p
                 .|. poipb .&. pawns p .&. white p	-- black & white trick because we look
                 .|. poipw .&. pawns p .&. black p	-- from the sq point of view
                 .|. atsl
          poib = atqs .&. occup p
          (poisee, poim) = calcPoiSee p sq poia
--}

-- Given the slider bitmap, a poi, a piece, an action (come or go) and a square:
-- when does the action influence the poi, so that it is necessary to recalculate it?
-- Case 1: square is the central square of the poi
-- Case 2: square is one of the attackers of the poi
-- Case 3: piece comes in the influence zone of a slider (at least blockers change)
-- Case 4: non-slider piece comes in the influence zone corresponding to the piece type
-- Case 5: one blocker leaves (square is in blockers and action is leave)
-- If we check exactly in this order then we don't need to be very exact about blockers and influence zones

data PieceAction = Come | Leave deriving EQ
type PosChange = (Square, (Piece, PieceAction))

poiIsChanging :: BBoard -> POI a -> PosChange -> Bool
poiIsChanging bsli poi (sq, (piece, pa))
    =    sq == poiCenter poi				-- case 1
      || poiWAttacks poi .&. bsq /= 0
      || poiBAttacks poi .&. bsq /= 0			-- case 2
      || pa == Come && (ro || bi || pi || ni || ki)	-- case 3 & 4
      || pa == Leave && poiBlockers poi .&. bsq /= 0
                     && discoverSlider bsli (poiCenter poi) sq	-- case 5
    where ro = poiRInf poi .&. bsq /= 0
          bi = poiBInf poi .&. bsq /= 0
          pi = piece == Pawn && (poiWPInf poi .&. bsq /= 0 || poiBPInf poi .&. bsq /= 0)
          ni = piece == Knight && poiNInf poi .&. bsq /= 0
          ki = piece == King && poiKInf poi .&. bsq /= 0
          bsq = 1 `unsafeShiftL` sq

-- When moving a blocker, it can be that a slider behind that blocker comes free
-- and attacs the center
-- This function checks if this can be the case
-- We check at least if there is a slider in the correct place, but it could be either
-- obscured or the wrong type (e.g. rook on the diagonal). We do not check exactly,
-- because this is as hard as recalculating the poi, which we prefer (at the risk that
-- the recalculation was not necessary).
-- We could at least check the slider type, but then we would need to code also the
-- direction between center and blocker - this could be the next improvement
discoverSlider :: MyPos -> Square -> Square
discoverSlider pos csq sq = further .&. sliders pos /= 0
    where further = bbShadowFromTo!(csq, sq)

-- Now the philosophy of keeping pois up to date:
-- The list of pois is part of the position (should be a map or an array?)
-- When making a move, the following has to take place:
-- 1. mark for deletion all the pois which are "moved", i.e. the from-square is the center
--    (normally one poi, but for en passant 2 pois are involved, and for castle - 10
--    - one for the rook and 9 for the king! By the way, a good reason to sort king moves
--    at the end, in the hope they will not be searched.
--    Another idea: the 8 neighbours of the king should be calculated only when there is
--    enough material for king safety)
-- 2. create a list of all triplets piece - piece action - square involved in the move
--    and run poiIsChanging for everyone, collecting a list of pois to be recalculated
-- 3. for non captures: create (conditionally) a poi for the new locations involved in
--    the move (same remark as pct.1)
-- 4. Modify the list of pois accordingly: delete, add (poiCreate), recalculate (poiRecalc)

-- So why are the pois good?
-- 1. First, for move generation: we can generate the winning and losing moves once per poi,
--    and then just copy them as needed - as log as the poi is stable, we don't need
--    recalculation
-- 2. For evaluation of:
--    2a. king safety - counting the winning attacs on the king neighbours and the non
--        winning attacs (which should be rated lower) we gain a more clear view of the
--        king safety
--    2b. position stability - counting equal or just slightly loosing exchanges can give
--        us an idea how stable our position is (if it's stable, the positional factors
--        are more trustfully)
-- 3. Finding weak points: this could guide the move ordering

data SEEPoi	-- a data used for SEE - to be defined

makeSEEPoi :: MyPos -> POI SEEPoi -> SEEPoi
makeSEEPoi p poi = undefined

updatePois :: MyPos -> [PosChange] -> [POI SEEPoi] -> [POI SEEPoi]
updatePois p trips pois = newpois ++ map (poiRecalc makeSEEPoi p) chg ++ nochg
    where newpois = catMaybes $ map (\sq -> poiCreate makeSEEPoi p sq False) $ filter isnew come
          isnew = notIn $ map poiCenter respois
          (come, leave) = map fst $ partition ((== Come) . snd . snd) trips
          respois = filter (notIn leave . poiCenter) pois	-- remaining pois
          (chg, nochg) = partition isChanging respois
          isChanging = \poi -> or $ map (poiIsChanging (sliding p) poi) trips
          notIn = not . flip elem
