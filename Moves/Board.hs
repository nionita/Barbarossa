{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses, PatternGuards, BangPatterns #-}
module Moves.Board (
    posFromFen, initPos,
    isCheck, inCheck,
    movePassed, moveIsCapture,
    castKingRookOk, castQueenRookOk,
    genMoveCast, genMoveNCapt, genMovePromo, genMoveFCheck, genMoveCaptWL,
    genMoveNCaptToCheck,
    updatePos, leftInCheck, moveChecks,
    legalMove, alternateMoves,
    doFromToMove, reverseMoving
    ) where

import Data.Array.Base (unsafeAt)
import Data.Bits
import Data.List (sort, foldl')
import Data.Word

import Struct.Struct
import Moves.Moves
import Moves.BitBoard
import Moves.ShowMe
import Eval.BasicEval
import Hash.Zobrist
import Moves.Fen

-- When we make the move to arrive in a new position, we want to check legality
-- of the move (the generator delivers pseudo-legal moves)
-- This is the first calculation we do on the new position, so we don't have the
-- new attacks, and this is the method to check if the king of the moving color
-- was left in check
{-# INLINE leftInCheck #-}
leftInCheck :: MyPos -> Bool
leftInCheck p = atts .&. me p /= 0
    where atts = unsafeAt (attacked p) $ firstOne $ kings p .&. yo p

{-# INLINE inCheck #-}
inCheck :: MyPos -> Bool
inCheck p = atts .&. yo p /= 0
    where atts = unsafeAt (attacked p) $ firstOne $ kings p .&. me p

-- Is color c in check in position p?
{-# INLINE isCheck #-}
isCheck :: MyPos -> Color -> Bool
isCheck p c = unsafeAt (attacked p) ksq .&. them /= 0
    where (us, them) | c == White = (white, black p)
                     | otherwise  = (black p, white)
          white = occup p `less` black p
          ksq = firstOne $ kings p .&. us

{--
goPromo :: MyPos -> Move -> Bool
goPromo p m
    | moveIsPromo m  = True
    | movePassed p m = True
    | otherwise      = False
--}

{-# INLINE movePassed #-}
movePassed :: MyPos -> Move -> Bool
movePassed p m = passed p .&. (uBit $ fromSquare m) /= 0
    --           && target   .&. (uBit $ toSquare   m) /= 0
    -- where target = 0x00FF00000000FF00

moveGenAscendent :: Bool
moveGenAscendent = True

genMoveNCapt :: MyPos -> [Move]
genMoveNCapt !p
    | moveGenAscendent
      = map (moveAddColor c) $ nGenNC ++ bGenNC ++ rGenNC ++ qGenNC ++ pGenNC1 ++ pGenNC2 ++ kGenNC
    | otherwise
      = map (moveAddColor c) $ qGenNC ++ rGenNC ++ bGenNC ++ nGenNC ++ pGenNC1 ++ pGenNC2 ++ kGenNC
    where pGenNC1 = map (moveAddPiece Pawn . uncurry moveFromTo)
                      $ pAll1Moves c (pawns p .&. me p .&. traR) (occup p)
          pGenNC2 = map (moveAddPiece Pawn . uncurry moveFromTo)
                      $ pAll2Moves c (pawns p .&. me p) (occup p)
          nGenNC = map (moveAddPiece Knight . uncurry moveFromTo)
                      $ concatMap (srcDestsAtt p ncapt)
                      $ bbToSquares $ knights p .&. me p
          bGenNC = map (moveAddPiece Bishop . uncurry moveFromTo)
                      $ concatMap (srcDestsAtt p ncapt)
                      $ bbToSquares $ bishops p .&. me p
          rGenNC = map (moveAddPiece Rook   . uncurry moveFromTo)
                      $ concatMap (srcDestsAtt p ncapt)
                      $ bbToSquares $ rooks p .&. me p
          qGenNC = map (moveAddPiece Queen  . uncurry moveFromTo)
                      $ concatMap (srcDestsAtt p ncapt)
                      $ bbToSquares $ queens p .&. me p
          kGenNC = map (moveAddPiece King   . uncurry moveFromTo)
                      $            srcDestsAtt p (ncapt . legal)
                      $ firstOne $ kings p .&. me p
          !noccup = complement (occup p)
          ncapt = ((.&.) noccup)
          !nyoa = complement $ yoAttacs p
          legal = ((.&.) nyoa)
          !traR = complement $ if c == White then 0x00FF000000000000 else 0xFF00
          c = moving p

-- Generate only promotions (now only to queen) non captures
-- The promotion captures are generated together with the other captures
genMovePromo :: MyPos -> [Move]
genMovePromo !p = map (uncurry (makePromo Queen)) pGenNC
    where pGenNC = pAll1Moves c (pawns p .&. myfpc) (occup p)
          !myfpc = me p .&. traR
          !traR = if c == White then 0x00FF000000000000 else 0xFF00
          c = moving p

{-# INLINE srcDests #-}
srcDests :: (Square -> BBoard) -> Square -> [(Square, Square)]
srcDests f !s = zip (repeat s) $ bbToSquares $ f s

-- Having the attacks from every square, it is easy to generate the moves
{-# INLINE srcDestsAtt #-}
srcDestsAtt :: MyPos -> (BBoard -> BBoard) -> Square -> [(Square, Square)]
srcDestsAtt p f !s = zip (repeat s) $ bbToSquares . f $ attacks p `unsafeAt` s

-- This one should be called only for normal moves
{-# INLINE moveChecksDirect #-}
moveChecks :: MyPos -> Move -> Bool
moveChecks !p !m = moveChecksDirect p m || moveChecksIndirect p m

moveChecksDirect :: MyPos -> Move -> Bool
moveChecksDirect !p !m
    | fig == Pawn   = pAttacs (moving p) t .&. yok /= 0
    | fig == Knight = nAttacs t .&. yok /= 0
    | fig == Bishop = ba .&. yok /= 0
    | fig == Rook   = ra .&. yok /= 0
    | fig == Queen  = ba .&. yok /= 0 || ra .&. yok /= 0
    | otherwise     = False	-- king can't check directly
    where !fig = movePiece m
          !t   = toSquare m
          !yok = kings p .&. yo p
          occ = occup p
          ba  = bAttacs occ t
          ra  = rAttacs occ t

-- This one can be further optimised by using two bitboard arrays
-- for the attacks on empty table
moveChecksIndirect :: MyPos -> Move -> Bool
moveChecksIndirect !p !m
    =  (eba .&. bq /= 0) && (ba .&. bq /= 0)
    || (era .&. rq /= 0) && (ra .&. rq /= 0)
    where !ksq = firstOne $ kings p .&. yo p
          !b   = bishops p .&. me p
          !r   = rooks p   .&. me p
          !q   = queens p  .&. me p
          !bq  = b .|. q
          !rq  = r .|. q
          !eba = emptyBAttacs `unsafeAt` ksq
          !era = emptyRAttacs `unsafeAt` ksq
          fb   = uBit $ fromSquare m
          tb   = uBit $ toSquare m
          occ  = (occup p .|. tb) `less` fb
          ba   = bAttacs occ ksq
          ra   = rAttacs occ ksq

data CheckInfo = CheckInfo !Piece !Square

-- Finds pieces which check
findChecking :: MyPos -> [CheckInfo]
findChecking !pos = concat [ pChk, nChk, bChk, rChk, qChk ]
    where pChk = map (CheckInfo Pawn)   $ bbToSquares $ pAttacs (moving pos) ksq .&. p
          nChk = map (CheckInfo Knight) $ bbToSquares $ att .&. n
          bChk = map (CheckInfo Bishop) $ bbToSquares $ att .&. b
          rChk = map (CheckInfo Rook)   $ bbToSquares $ att .&. r
          qChk = map (CheckInfo Queen)  $ bbToSquares $ att .&. q
          !myk = kings pos .&. me pos
          !ksq = firstOne myk
          !att = attacked pos `unsafeAt` ksq
          !b = bishops pos .&. yo pos
          !r = rooks pos   .&. yo pos
          !q = queens pos  .&. yo pos
          !n = knights pos .&. yo pos
          !p = pawns pos   .&. yo pos

-- Generate move when in check
genMoveFCheck :: MyPos -> [Move]
genMoveFCheck !p
    | null chklist        = error $ "genMoveFCheck:\n" ++ showMyPos p
    | null $ tail chklist = r1 ++ kGen ++ r2	-- simple check
    | otherwise           = kGen		-- double check, only king moves help
    where chklist = findChecking p
          kGen = map (moveAddColor (moving p) . moveAddPiece King . uncurry moveFromTo)
                     $ srcDestsAtt p legal ksq
          !ksq = firstOne kbb
          !kbb = kings p .&. me p
          !ocp1 = occup p `less` kbb
          !call = complement $ me p .|. yoAttacs p .|. excl
          legal = ((.&.) call)
          !excl = foldl' (.|.) 0 $ map chkAtt chklist
          chkAtt (CheckInfo f s) = fAttacs s f ocp1
          -- This head is safe becase chklist is first checked in the pattern of the function
          (r1, r2) = case head chklist of	-- this is needed only when simple check
                 CheckInfo Pawn   sq -> (beatAtP p (uBit sq), [])  -- cannot block pawn
                 CheckInfo Knight sq -> (beatAt  p (uBit sq), [])  -- or knight check
                 CheckInfo pt     sq -> beatOrBlock pt p sq

-- Generate moves ending on a given square (used to defend a check by capture or blocking)
-- This part is only for queens, rooks, bishops and knights (no pawns and, of course, no kings)
defendAt :: MyPos -> BBoard -> [Move]
defendAt p !bb = map (moveAddColor $ moving p) $ nGenC ++ bGenC ++ rGenC ++ qGenC
    where nGenC = map (moveAddPiece Knight . uncurry moveFromTo)
                     $ concatMap (srcDestsAtt p target)
                     $ bbToSquares $ knights p .&. me p
          bGenC = map (moveAddPiece Bishop . uncurry moveFromTo)
                     $ concatMap (srcDestsAtt p target)
                     $ bbToSquares $ bishops p .&. me p
          rGenC = map (moveAddPiece Rook   . uncurry moveFromTo)
                     $ concatMap (srcDestsAtt p target)
                     $ bbToSquares $ rooks p .&. me p
          qGenC = map (moveAddPiece Queen  . uncurry moveFromTo)
                     $ concatMap (srcDestsAtt p target)
                     $ bbToSquares $ queens p .&. me p
          target = (.&. bb)

-- Generate capture pawn moves ending on a given square (used to defend a check by capture)
pawnBeatAt :: MyPos -> BBoard -> [Move]
pawnBeatAt !p bb = map (uncurry (makePromo Queen))
                       (concatMap (srcDestsAtt p pcapt) (bbToSquares promo))
                ++ map (moveAddColor (moving p) . moveAddPiece Pawn . uncurry moveFromTo)
                       (concatMap (srcDestsAtt p pcapt) (bbToSquares rest))
    where !yopi = bb .&. yo p
          pcapt = (.&. yopi)
          (promo, rest) = promoRest p

-- Generate blocking pawn moves ending on given squares (used to defend a check by blocking)
pawnBlockAt :: MyPos -> BBoard -> [Move]
pawnBlockAt p !bb = map (uncurry (makePromo Queen))
                        (concatMap
                              (srcDests (block . \s -> pMovs s (moving p) (occup p)))
                              (bbToSquares promo))
                 ++ map (moveAddColor (moving p) . moveAddPiece Pawn . uncurry moveFromTo)
                        (concatMap
                              (srcDests (block . \s -> pMovs s (moving p) (occup p)))
                              (bbToSquares rest))
    where block = (.&. bb)
          (promo, rest) = promoRest p

promoRest :: MyPos -> (BBoard, BBoard)
promoRest p
    | moving p == White
                  = let prp = mypawns .&. 0x00FF000000000000
                        rea = mypawns `less` prp
                    in (prp, rea)
    | otherwise   = let prp = mypawns .&. 0xFF00
                        rea = mypawns `less` prp
                    in (prp, rea)
    where !mypawns = pawns p .&. me p

beatAt :: MyPos -> BBoard -> [Move]
beatAt p !bb = pawnBeatAt p bb ++ defendAt p bb

-- Here we generate a possible en passant capture of a pawn which maybe checks
beatAtP :: MyPos -> BBoard -> [Move]
beatAtP p !bb = genEPCapts p ++ pawnBeatAt p bb ++ defendAt p bb

blockAt :: MyPos -> BBoard -> [Move]
blockAt p !bb = pawnBlockAt p bb ++ defendAt p bb

-- Defend a check from a sliding piece: beat it or block it
beatOrBlock :: Piece -> MyPos -> Square -> ([Move], [Move])
beatOrBlock f !p sq = (beat, block)
    where !beat = beatAt p $ uBit sq
          !aksq = firstOne $ me p .&. kings p
          !line = findLKA f aksq sq
          !block = blockAt p line

-- Find pinning lines for a piece type, given the king & piece squares
-- The queen is very hard, so we solve it as a composition of rook and bishop
-- Here we only need bishop/rook attacks on empty table (no occupancy) - this could be
-- further optimised by 2 bitboard arrays with those attacks
{-# INLINE findLKA #-}
findLKA :: Piece -> Square -> Int -> BBoard
findLKA Queen !ksq !psq
    | emptyBAttacs `unsafeAt` ksq .&. bpsq /= 0 = findLKA0 Bishop ksq psq
    | otherwise                                 = findLKA0 Rook   ksq psq
    where !bpsq = uBit psq
findLKA pt !ksq !psq = findLKA0 pt ksq psq

findLKA0 :: Piece -> Square -> Int -> BBoard
findLKA0 pt ksq psq
    | pt == Bishop = (emptyBAttacs `unsafeAt` ksq) .&. (emptyBAttacs `unsafeAt` psq)
    | pt == Rook   = (emptyRAttacs `unsafeAt` ksq) .&. (emptyRAttacs `unsafeAt` psq)
    | otherwise    = 0	-- it will not be called with other pieces

genMoveNCaptToCheck :: MyPos -> [Move]
genMoveNCaptToCheck p = genMoveNCaptDirCheck p ++ genMoveNCaptIndirCheck p

-- TODO: check with pawns (should be also without promotions)
genMoveNCaptDirCheck :: MyPos -> [Move]
genMoveNCaptDirCheck p
    | moveGenAscendent
      = map (moveAddColor $ moving p) $ nGenC ++ bGenC ++ rGenC ++ qGenC
    | otherwise
      = map (moveAddColor $ moving p) $ qGenC ++ rGenC ++ bGenC ++ nGenC
    where nGenC = map (moveAddPiece Knight . uncurry moveFromTo)
                      $ filtQPSEE p Knight $ concatMap (srcDestsAtt p (target nTar))
                      $ bbToSquares  $ knights p .&. me p
          bGenC = map (moveAddPiece Bishop . uncurry moveFromTo)
                      $ filtQPSEE p Bishop $ concatMap (srcDestsAtt p (target bTar))
                      $ bbToSquares  $ bishops p .&. me p
          rGenC = map (moveAddPiece Rook   . uncurry moveFromTo)
                      $ filtQPSEE p Rook   $ concatMap (srcDestsAtt p (target rTar))
                      $ bbToSquares  $ rooks p .&. me p
          qGenC = map (moveAddPiece Queen  . uncurry moveFromTo)
                      $ filtQPSEE p Queen  $ concatMap (srcDestsAtt p (target qTar))
                      $ bbToSquares  $ queens p .&. me p
          target b = (.&. b)
          !nocp = complement $ occup p
          !ksq  = firstOne $ yo p .&. kings p
          !nTar = fAttacs ksq Knight (occup p) .&. nocp
          !bTar | me p .&. (bishops p .|. queens p) == 0 = 0
                | otherwise                              = fAttacs ksq Bishop (occup p) .&. nocp
          !rTar | me p .&. (rooks   p .|. queens p) == 0 = 0
                | otherwise                              = fAttacs ksq Rook   (occup p) .&. nocp
          !qTar = bTar .|. rTar

-- TODO: indirect non capture checking moves
genMoveNCaptIndirCheck :: MyPos -> [Move]
genMoveNCaptIndirCheck _ = []

-- Generate the castle moves
genMoveCast :: MyPos -> [Move]
genMoveCast p
    | inCheck p = []
    | otherwise = kingside ++ queenside
    where (cmidk, cmidq, cattk, cattq)
              | c == White = (caRMKw, caRMQw, caRAKw, caRAQw)
              | otherwise  = (caRMKb, caRMQb, caRAKb, caRAQb)
          kingside  = if castKingRookOk  p c && (occup p .&. cmidk == 0) && (yoAttacs p .&. cattk == 0)
                         then [caks] else []
          queenside = if castQueenRookOk p c && (occup p .&. cmidq == 0) && (yoAttacs p .&. cattq == 0)
                         then [caqs] else []
          caks = makeCastleFor c True
          caqs = makeCastleFor c False
          c = moving p

{-# INLINE castKingRookOk #-}
castKingRookOk :: MyPos -> Color -> Bool
castKingRookOk !p White = epcas p .&.  b7 /= 0 where b7 = uBit 7
castKingRookOk !p Black = epcas p .&. b63 /= 0 where b63 = uBit 63

{-# INLINE castQueenRookOk #-}
castQueenRookOk :: MyPos -> Color -> Bool
castQueenRookOk !p White = epcas p .&.  b0 /= 0 where b0 = 1 
castQueenRookOk !p Black = epcas p .&. b56 /= 0 where b56 = uBit 56

data ChangeAccum = CA !ZKey !Int

-- Accumulate a set of changes in MyPos (except BBoards) due to setting a piece on a square
accumSetPiece :: Square -> Color -> Piece -> MyPos -> ChangeAccum -> ChangeAccum
accumSetPiece sq c f !p (CA z m)
    = case tabla p sq of
        Empty      -> CA znew mnew
        Busy co fo -> accumCapt sq co fo znew mnew
    where !znew = z `xor` zobPiece c f sq
          !mnew = m + matPiece c f

-- Accumulate a set of changes in MyPos (except BBoards) due to clearing a square
accumClearSq :: Square -> MyPos -> ChangeAccum -> ChangeAccum
accumClearSq sq p i@(CA z m)
    = case tabla p sq of
        Empty      -> i
        Busy co fo -> accumCapt sq co fo z m

accumCapt :: Square -> Color -> Piece -> ZKey -> Int -> ChangeAccum
accumCapt sq !co !fo !z !m = CA (z `xor` zco) (m - mco)
    where !zco = zobPiece co fo sq
          !mco = matPiece co fo

accumMoving :: MyPos -> ChangeAccum -> ChangeAccum
accumMoving _ (CA z m) = CA (z `xor` zobMove) m

-- Take an initial accumulation and a list of functions accum to accum
-- and compute the final accumulation
chainAccum :: ChangeAccum -> [ChangeAccum -> ChangeAccum] -> ChangeAccum
chainAccum = foldl (flip ($))

{-
changePining :: MyPos -> Square -> Square -> Bool
changePining p src dst = kings p `testBit` src	-- king is moving
                      || slide p `testBit` src -- pining piece is moving
                      || slide p `testBit` dst -- pining piece is captured
-}

{-# INLINE clearCast #-}
clearCast :: BBoard -> BBoard -> (BBoard, ZKey)
clearCast cas sd
    | sdposs == 0 || sdcas == 0 = (0, 0)	-- most of the time
    | otherwise = clearingCast sdcas cas	-- complicated cases
    where !sdposs = sd .&. caRiMa	-- moving from/to king/rook position?
          sdcas = sdposs .&. cas	-- first time touched

{-# INLINE clearingCast #-}
clearingCast :: BBoard -> BBoard -> (BBoard, ZKey)
clearingCast sdcas cas = (cascl, zobcl)
    where (casw, zobw) | casrw == 0 = (0, 0)	-- cast rights & changes for white
                       | casrw == wkqb = if sdcas .&. wkqb /= 0
                                            then (wkqb, zobCastQw)
                                            else (0, 0)
                       | casrw == wkkb = if sdcas .&. wkkb /= 0
                                            then (wkkb, zobCastKw)
                                            else (0, 0)
                       | otherwise     = if sdcas .&. wkbb /= 0
                                            then (wkqb .|. wkkb, zobCastQw `xor` zobCastKw)
                                            else if sdcas .&. wqrb /= 0
                                                    then (wqrb, zobCastQw)
                                                    else if sdcas .&. wkrb /= 0
                                                            then (wkrb, zobCastKw)
                                                            else (0, 0)
          (casb, zobb) | casrb == 0 = (0, 0)	-- cast rights & changes for white
                       | casrb == bkqb = if sdcas .&. bkqb /= 0
                                            then (bkqb, zobCastQb)
                                            else (0, 0)
                       | casrb == bkkb = if sdcas .&. bkkb /= 0
                                            then (bkkb, zobCastKb)
                                            else (0, 0)
                       | otherwise     = if sdcas .&. bkbb /= 0
                                            then (bkqb .|. bkkb, zobCastQb `xor` zobCastKb)
                                            else if sdcas .&. bqrb /= 0
                                                    then (bqrb, zobCastQb)
                                                    else if sdcas .&. bkrb /= 0
                                                            then (bkrb, zobCastKb)
                                                            else (0, 0)
          !casr  = cas .&. caRiMa
          !casrw = casr .&. 0xFF
          !casrb = casr .&. 0xFF00000000000000
          !cascl = casw .|. casb
          !zobcl = zobw `xor` zobb
          wkqb = 0x11	-- king & queen rook for white
          wkkb = 0x90	-- king & king rook for white
          wkbb = 0x10	-- white king
          wqrb = 0x01	-- white queen rook
          wkrb = 0x80	-- white king rook
          bkqb = 0x1100000000000000	-- king & queen rook for black
          bkkb = 0x9000000000000000	-- king & king rook for black
          bkbb = 0x1000000000000000	-- black king
          bqrb = 0x0100000000000000	-- black queen rook
          bkrb = 0x8000000000000000	-- black king rook

-- Just for a dumb debug: a quick check if two consecutive moves
-- can be part of a move sequence
alternateMoves :: MyPos -> Move -> Move -> Bool
alternateMoves p m1 m2
    | Busy c1 _ <- tabla p src1,
      Busy c2 _ <- tabla p src2 = c1 /= c2
    | otherwise = True	-- means: we cannot say...
    where src1 = fromSquare m1
          src2 = fromSquare m2

-- This is used to filter the illegal moves coming from killers or hash table
-- but we must treat special moves (en-passant, castle and promotion) differently,
-- because they are more complex
-- This legality is still incomplete, as it does not take pinned pieces into consideration
-- Now, when in check, reject all
legalMove :: MyPos -> Move -> Bool
legalMove p m
    | inCheck p          = False	-- reject TT & killers when in check
    | moveColor m /= mc  = False	-- wrong color
    | me p `uBitSet` dst = False	-- destination is occupied by me
    | Busy col fig <- tabla p src,
      col == mc,
      fig == movePiece m =
         if moveIsNormal m
            then canMove fig p src dst	-- figure can move like that
            else specialMoveIsLegal p m	-- special move legality check
    | otherwise = False
    where src = fromSquare m
          dst = toSquare m
          mc  = moving p

specialMoveIsLegal :: MyPos -> Move -> Bool
specialMoveIsLegal p m | moveIsCastle m = elem m $ genMoveCast p
specialMoveIsLegal p m | moveIsPromo  m = canMove Pawn p (fromSquare m) (toSquare m)
specialMoveIsLegal p m | moveIsEnPas  m = elem m $ genEPCapts p
specialMoveIsLegal _ _ = False

{-# INLINE moveIsCapture #-}
moveIsCapture :: MyPos -> Move -> Bool
moveIsCapture p m = occup p `uBitSet` toSquare m

canMove :: Piece -> MyPos -> Square -> Square -> Bool
canMove Pawn p src dst
    | (src - dst) .&. 0x7 == 0 = elem dst $
         map snd $ pAll1Moves col pw (occup p) ++ pAll2Moves col pw (occup p)
    | otherwise = pAttacs col src `uBitSet` dst
    where col = moving p
          pw = uBit src
canMove King p src dst = kAttacs src `uBitSet` dst && not (yoAttacs p `uBitSet` dst)
canMove fig p src dst  = fAttacs src fig (occup p) `uBitSet` dst

-- See http://stackoverflow.com/questions/47981/how-do-you-set-clear-and-toggle-a-single-bit-in-c-c
-- I combined "Checking a bit" with "Changing the nth bit to x"
-- We have also to clear dst bit
mvBit :: Square -> Square -> BBoard -> BBoard
mvBit !src !dst !w = (w `xor` mx) .&. (complement $ uBit src)
    where !mx = ((complement ((w `unsafeShiftR` src) .&. 1) + 1) `xor` w) .&. (uBit dst)

{-# INLINE moveAndClearEp #-}
moveAndClearEp :: BBoard -> BBoard
moveAndClearEp bb = bb `xor` (bb .&. epMask) `xor` mvMask

{-# INLINE epClrZob #-}
epClrZob :: BBoard -> BBoard
epClrZob bb
    | epLastBB == 0 = 0
    | otherwise     = epSetZob $ head $ bbToSquares epLastBB	-- safe because epLastBB /= 0
    where epLastBB  = bb .&. epMask

{-# INLINE epSetZob #-}
epSetZob :: Square -> BBoard
epSetZob = zobEP . (.&. 0x7)

-- Copy one square to another and clear the source square
doFromToMove :: Move -> MyPos -> MyPos
doFromToMove m !p | moveIsNormal m
    = updatePos (Just (srcbb .|. dstbb)) p {
          black = tblack, slide = tslide, kkrq  = tkkrq,  diag  = tdiag,
          epcas = tepcas, zobkey = tzobkey, mater = tmater, mmove = Just m
      }
    where src = fromSquare m
          dst = toSquare m
          tblack = mvBit src dst $ black p
          tslide = mvBit src dst $ slide p
          tkkrq  = mvBit src dst $ kkrq p
          tdiag  = mvBit src dst $ diag p
          !srcbb = uBit src
          !dstbb = uBit dst
          !pawnmoving = pawns p .&. srcbb /= 0	-- the correct color is
          !iscapture  = occup p .&. dstbb /= 0	-- checked somewhere else
          (clearcast, zobcast) = clearCast (epcas p) (srcbb .|. dstbb)
          !irevers = pawnmoving || iscapture || clearcast /= 0
          !tepcas' = moveAndClearEp $ epcas p `less` clearcast
          !tepcas  = setEp $! if irevers then reset50Moves tepcas' else addHalfMove tepcas'
          -- For e.p. zob key:
          !epcl = epClrZob $ epcas p
          (setEp, !epst)
              | pawnmoving && (src - dst == 16 || dst - src == 16)
                  = let !epFld = (src + dst) `unsafeShiftR` 1
                        !epBit = uBit epFld
                    in ((.|.) epBit, epSetZob epFld)
              | otherwise = (id, 0)
          !zob = zobkey p `xor` epcl `xor` epst `xor` zobcast
          CA tzobkey tmater = case tabla p src of	-- identify the moving piece
               Busy col fig -> chainAccum (CA zob (mater p)) [
                                   accumClearSq src p,
                                   accumSetPiece dst col fig p,
                                   accumMoving p
                               ]
               _ -> error $ "Src field empty: " ++ show m ++ " in pos\n"
                                 ++ showTab (black p) (slide p) (kkrq p) (diag p)
                                 ++ "resulting pos:\n"
                                 ++ showTab tblack tslide tkkrq tdiag
doFromToMove m !p | moveIsEnPas m
    = updatePos (Just (uBit src .|. uBit dst .|. bdel)) p {
          black = tblack, slide = tslide, kkrq  = tkkrq,  diag  = tdiag,
          epcas = tepcas, zobkey = tzobkey, mater = tmater, mmove = Just m
      }
    where src = fromSquare m
          dst = toSquare m
          del = moveEnPasDel m
          bdel = uBit del
          nbdel = complement bdel
          tblack = mvBit src dst (black p) .&. nbdel
          tslide = mvBit src dst (slide p) .&. nbdel
          tkkrq  = mvBit src dst (kkrq p) .&. nbdel
          tdiag  = mvBit src dst (diag p) .&. nbdel
          tepcas = reset50Moves $ moveAndClearEp $ epcas p
          Busy col fig  = tabla p src	-- identify the moving piece
          !epcl = epClrZob $ epcas p
          !zk = zobkey p `xor` epcl
          CA tzobkey tmater = chainAccum (CA zk (mater p)) [
                                accumClearSq src p,
                                accumClearSq del p,
                                accumSetPiece dst col fig p,
                                accumMoving p
                            ]
doFromToMove m !p | moveIsCastle m
    = updatePos (Just (srcbb .|. uBit dst .|. uBit csr .|. uBit cds)) p {
          black = tblack, slide = tslide, kkrq  = tkkrq,  diag  = tdiag,
          epcas = tepcas, zobkey = tzobkey, mater = tmater, mmove = Just m
      }
    where src = fromSquare m
          dst = toSquare m
          (csr, cds) = case src of
              4  -> case dst of
                  6 -> (7, 5)
                  2 -> (0, 3)
                  _ -> error $ "Wrong destination for castle move " ++ show m
              60 -> case dst of
                  62 -> (63, 61)
                  58 -> (56, 59)
                  _ -> error $ "Wrong destination for castle move " ++ show m
              _  -> error $ "Wrong source for castle move " ++ show m
          tblack = mvBit csr cds $ mvBit src dst $ black p
          tslide = mvBit csr cds $ mvBit src dst $ slide p
          tkkrq  = mvBit csr cds $ mvBit src dst $ kkrq p
          tdiag  = mvBit csr cds $ mvBit src dst $ diag p
          !srcbb = uBit src	-- source clears cast rights
          (clearcast, zobcast) = clearCast (epcas p) srcbb
          tepcas = reset50Moves $ moveAndClearEp $ epcas p `less` clearcast
          Busy col King = tabla p src	-- identify the moving piece (king)
          Busy co1 Rook = tabla p csr	-- identify the moving rook
          !epcl = epClrZob $ epcas p
          !zob = zobkey p `xor` epcl `xor` zobcast
          CA tzobkey tmater = chainAccum (CA zob (mater p)) [
                                accumClearSq src p,
                                accumSetPiece dst col King p,
                                accumClearSq csr p,
                                accumSetPiece cds co1 Rook p,
                                accumMoving p
                            ]
doFromToMove m !p | moveIsPromo m
    = updatePos (Just (uBit src .|. dstbb)) p0 {
          black = tblack, slide = tslide, kkrq = tkkrq, diag = tdiag,
          epcas = tepcas, zobkey = tzobkey, mater = tmater, mmove = Just m
      }
    where col = moving p	-- the new coding does not have correct fromSquare in promotion
          srank = if col == White then 6 else 1
          sfile = fromSquare m .&. 0x7	-- see new coding!
          src = srank `unsafeShiftL` 3 .|. sfile
          dst = toSquare m
          !pie = movePromoPiece m
          p0 = setPiece src col pie p
          tblack = mvBit src dst $ black p0
          tslide = mvBit src dst $ slide p0
          tkkrq  = mvBit src dst $ kkrq p0
          tdiag  = mvBit src dst $ diag p0
          !dstbb = uBit dst	-- destination could clear cast rights!
          (clearcast, zobcast) = clearCast (epcas p) dstbb
          tepcas = reset50Moves $ moveAndClearEp $ epcas p `less` clearcast
          !epcl = epClrZob $ epcas p0
          !zk = zobkey p0 `xor` epcl `xor` zobcast
          CA tzobkey tmater = chainAccum (CA zk (mater p0)) [
                                accumClearSq src p0,
                                accumSetPiece dst col pie p0,
                                accumMoving p0
                            ]
doFromToMove _ _ = error "doFromToMove: wrong move type"

reverseMoving :: MyPos -> MyPos
reverseMoving p = updatePos Nothing p { epcas = tepcas, zobkey = z, mmove = Nothing }
    where tepcas = moveAndClearEp $ epcas p
          !epcl = epClrZob $ epcas p
          !zk = zobkey p `xor` epcl
          CA z _ = chainAccum (CA zk (mater p)) [
                       accumMoving p
                   ]

-- The new SEE functions (swap-based)
-- Choose the cheapest of a set of pieces
chooseAttacker :: MyPos -> BBoard -> (BBoard, Int)
chooseAttacker pos !frompieces
    | p /= 0 = p1 `seq` (p1, seeValue Pawn)
    | n /= 0 = n1 `seq` (n1, seeValue Knight)
    | b /= 0 = b1 `seq` (b1, seeValue Bishop)
    | r /= 0 = r1 `seq` (r1, seeValue Rook)
    | q /= 0 = q1 `seq` (q1, seeValue Queen)
    | k /= 0 = k1 `seq` (k1, seeValue King)
    | otherwise = (0, 0)
    where p = frompieces .&. pawns pos
          n = frompieces .&. knights pos
          b = frompieces .&. bishops pos
          r = frompieces .&. rooks pos
          q = frompieces .&. queens pos
          k = frompieces .&. kings pos
          p1 = lsb p
          n1 = lsb n
          b1 = lsb b
          r1 = lsb r
          q1 = lsb q
          k1 = lsb k

-- Data structure to keep the status for the incremental calculation
-- of the new attacks during SEE
data Attacks = Attacks {
                   atAtt, atOcc, atBQ, atRQ, atRst :: !BBoard
               }
 
-- The new attacks are calculated once per central square with this function,
-- which is more heavy, and then updated with newAttacs incrementally, which is cheaper
theAttacs :: MyPos -> Square -> Attacks
theAttacs pos sq = axx
    where occ = occup pos
          b = bishops pos
          r = rooks pos
          q = queens pos
          n = knights pos
          k = kings pos
          p = pawns pos
          !bq  = b .|. q                -- bishops & queens
          !rq  = r .|. q                -- rooks & queens
          !ats = attacked pos `unsafeAt` sq	-- all attackers
          !rst = ats .&. (n .|. k .|. p)
          !axx = Attacks ats occ bq rq rst      -- this is result and state for the next step
 
newAttacs :: Square -> BBoard -> Attacks -> Attacks
newAttacs sq !moved !atts = axx
    where !mvc = complement moved
          !occ = atOcc atts .&. mvc     -- reduce occupancy
          !bq  = atBQ  atts .&. mvc     -- reduce bishops & queens
          !rq  = atRQ  atts .&. mvc     -- reduce rooks & queens
          !rst = atRst atts .&. mvc     -- reduce pawns, knights & kings
          !bqa = bAttacs occ sq .&. bq  -- new bishops & queens can arise because reduced occupancy
          !rqa = rAttacs occ sq .&. rq  -- new rooks & queens can arise because reduced occupancy
          !ats = bqa .|. rqa .|. rst    -- these are all new attackers
          !axx = Attacks ats occ bq rq rst      -- this is result and state for the next step

xrayAttacs :: MyPos -> Square -> Bool
xrayAttacs pos sq = sa1 /= sa0
    where sa1 = (attacked pos `unsafeAt` sq) .&. (bishops pos .|. rooks pos .|. queens pos)
          sa0 = (emptyBAttacs `unsafeAt` sq) .&. (bishops pos .|. queens pos)
            .|. (emptyRAttacs `unsafeAt` sq) .&. (rooks   pos .|. queens pos)

unimax :: Int -> [Int] -> Int
unimax = foldl' (\a g -> min g (-a))

usePosXRay, useMayXRay :: Bool
usePosXRay = False
useMayXRay = True

data SEEPars = SEEPars {
                   seeGain, seeVal :: !Int,
                   seeAtts, seeFrom, seeMovd, seeDefn, seeAgrs :: !BBoard,
                   seeAttsRec :: !Attacks
               }

-- Calculate the value of a move per SEE, given the position,
-- the source square of the first capture, the destination of the captures
-- and the value of the first captured piece
seeMoveValue :: MyPos -> Attacks -> Square -> Square -> Int -> Int
seeMoveValue pos !attrec sqfirstmv sqto gain0 = v
    where v = go sp0 [gain0]
          go :: SEEPars -> [Int] -> Int
          go seepars acc =
             let !gain'   = seeVal  seepars -     seeGain seepars
                 !moved'  = seeMovd seepars .|.   seeFrom seepars
                 !attacs1 = seeAtts seepars `xor` seeFrom seepars
                 (!from', !val') = chooseAttacker pos (attacs1 .&. seeAgrs seepars)
                 attacs2  = newAttacs sqto moved' (seeAttsRec seepars)
                 acc' = gain' : acc
                 seepars1 = SEEPars { seeGain = gain', seeVal = val', seeAtts = attacs1,
                                      seeFrom = from', seeMovd = moved', seeDefn = seeAgrs seepars,
                                      seeAgrs = seeDefn seepars,
                                      seeAttsRec = seeAttsRec seepars }
                 seepars2 = SEEPars { seeGain = gain', seeVal = val', seeAtts = atAtt attacs2,
                                      seeFrom = from', seeMovd = moved', seeDefn = seeAgrs seepars,
                                      seeAgrs = seeDefn seepars,
                                      seeAttsRec = attacs2 }
             in if from' == 0
                   then unimax (minBound+2) acc
                   -- With the new attacks: is it perhaps better to recalculate always?
                   else if usePosXRay
                           then if posXRay && seeFrom seepars .&. mayXRay /= 0
                                   then go seepars2 acc'
                                   else go seepars1 acc'
                           else if useMayXRay
                                   then if seeFrom seepars .&. mayXRay /= 0
                                           then go seepars2 acc'
                                           else go seepars1 acc'
                                   else go seepars1 acc'
          !mayXRay | useMayXRay = pawns pos .|. bishops pos .|. rooks pos .|. queens pos  -- could be
                   | otherwise  = 0
          posXRay = xrayAttacs pos sqto  -- calculated only once, as it is per pos (but it's cheap anyway)
          !moved0 = uBit sqfirstmv
          attacs0 = newAttacs sqto moved0 attrec
          (!from0, !valfrom) = chooseAttacker pos (atAtt attacs0 .&. yo pos)
          sp0 = SEEPars { seeGain = gain0, seeVal = valfrom, seeAtts = atAtt attacs0,
                          seeFrom = from0, seeMovd = moved0, seeDefn = yo pos, seeAgrs = me pos,
                          seeAttsRec = attacs0 }

-- This function can produce illegal captures with the king ??
-- This should be fixed now (see perCaptFieldWL comment below)
genMoveCaptWL :: MyPos -> ([Move], [Move])
genMoveCaptWL !pos = (map f $ sort ws, map f $ sort ls)
    where capts = myAttacs pos .&. yo pos
          epcs  = genEPCapts pos
          (ws, ls) = foldr (perCaptFieldWL pos (me pos) (yoAttacs pos)) (lepcs, []) $ bbToSquares capts
          lepcs = map (moveToLMove Pawn Pawn) epcs
          f = moveAddColor (moving pos) . lmoveToMove

type LMove = Word32

-- We want to sort MVVLVA, which means first victim, then attacker
-- Victim is "negate" so that the normal sort will pick higher victims first
-- We rely here on the fact that the piece type enumeration
-- is from low to high value (i.e. pawn, knight, bishop, rook, queen, king)
-- Otherwise this will not work!
{-# INLINE moveToLMove #-}
moveToLMove :: Piece -> Piece -> Move -> LMove
moveToLMove attacker victim (Move w)
    =   (vicval `unsafeShiftL` 24)
    .|. (attval `unsafeShiftL` 16)
    .|. fromIntegral w
    where kingval  = fromEnum King
          !vicval  = fromIntegral $ kingval - fromEnum victim	-- pseudo negate
          !attval  = fromIntegral $ fromEnum attacker

{-# INLINE lmoveToMove #-}
lmoveToMove :: LMove -> Move
lmoveToMove = Move . fromIntegral . (.&. 0xFFFF)

genEPCapts :: MyPos -> [Move]
genEPCapts !pos
    | epBB == 0 = []
    | otherwise = map (\s -> makeEnPas s dst) $ bbToSquares srcBB
    where epBB  = epcas pos .&. epMask
          dst   = head $ bbToSquares epBB	-- safe because epBB /= 0
          srcBB = pAttacs (other $ moving pos) dst .&. me pos .&. pawns pos

perCaptFieldWL :: MyPos -> BBoard -> BBoard -> Square -> ([LMove], [LMove]) -> ([LMove], [LMove])
perCaptFieldWL pos mypc advdefence sq mvlst
    | hanging   = let mvlst1 = foldr (addHanging  pos pcto sq) mvlst  reAgrsqs
                  in           foldr (addHangingP     pcto sq) mvlst1 prAgrsqs	-- for promotions
    | otherwise = let mvlst1 = foldr (perCaptWL pos myAttRec False pcto valto sq) mvlst  reAgrsqs
                  in           foldr (perCaptWL pos myAttRec True  pcto valto sq) mvlst1 prAgrsqs
    where !myAttRec = theAttacs pos sq
          -- Avoid illegal capture with the king if not hanging
          myattacs | hanging   = mypc .&. atAtt myAttRec
                   | otherwise = mypc .&. atAtt myAttRec `less` kings pos
          Busy _ pcto = tabla pos sq
          valto = seeValue pcto
          hanging = not (advdefence `uBitSet` sq)
          prAgrsqs = bbToSquares prPawns
          reAgrsqs = bbToSquares reAtts
          (prPawns, reAtts)
              | sq >= 56 && moving pos == White
                  = let prp = myattacs .&. pawns pos .&. 0x00FF000000000000
                        rea = myattacs `less` prp
                    in (prp, rea)
              | sq <=  7 && moving pos == Black
                  = let prp = myattacs .&. pawns pos .&. 0xFF00
                        rea = myattacs `less` prp
                    in (prp, rea)
              | otherwise = (0, myattacs)

approximateEasyCapts :: Bool
approximateEasyCapts = True	-- when capturing a better piece: no SEE, it is always winning

perCaptWL :: MyPos -> Attacks -> Bool -> Piece -> Int -> Square -> Square
          -> ([LMove], [LMove]) -> ([LMove], [LMove])
perCaptWL !pos !attrec promo vict !gain0 !sq !sqfa (wsqs, lsqs)
    | promo = ((moveToLMove Pawn vict $ makePromo Queen sqfa sq) : wsqs, lsqs)
    | approx || adv <= gain0 = (ss:wsqs, lsqs)
    | otherwise = (wsqs, ss:lsqs)
    where ss = moveToLMove attc vict $ moveAddPiece attc $ moveFromTo sqfa sq
          approx = approximateEasyCapts && gain0 >= v0
          Busy _ attc = tabla pos sqfa
          v0  = seeValue attc
          adv = seeMoveValue pos attrec sqfa sq v0

-- Captures of hanging pieces are always winning
addHanging :: MyPos -> Piece -> Square -> Square -> ([LMove], [LMove]) -> ([LMove], [LMove])
addHanging pos vict to from (wsqs, lsqs)
    = ((moveToLMove apiece vict $ moveAddPiece apiece (moveFromTo from to)) : wsqs, lsqs)
    where Busy _ apiece = tabla pos from

addHangingP :: Piece -> Square -> Square -> ([LMove], [LMove]) -> ([LMove], [LMove])
addHangingP vict to from (wsqs, lsqs) = ((moveToLMove Pawn vict $ makePromo Queen from to) : wsqs, lsqs)

filtQPSEE :: MyPos -> Piece -> [(Square, Square)] -> [(Square, Square)]
filtQPSEE !pos piece = filter (quietPositiveSEE pos v0)
    where v0 = seeValue piece

quietPositiveSEE :: MyPos -> Int -> (Square, Square) -> Bool
quietPositiveSEE !pos !v0 (!sqfa, !sq) = adv <= 0
    where !allAttRec = theAttacs pos sq
          adv = seeMoveValue pos allAttRec sqfa sq v0
