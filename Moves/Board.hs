{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses, PatternGuards, BangPatterns #-}
module Moves.Board (
    posFromFen, initPos,
    movePassed, moveIsCapture,
    genMoveNCapt, genMovePromo, genMoveFCheck, genMoveCaptWL,
    genMoveNCaptToCheck,
    updatePos, checkOk, moveChecks,
    legalMove, alternateMoves,
    doFromToMove, reverseMoving
) where

import Data.Bits
import Data.List (sort, foldl')
import Data.Word

-- import Debug.Trace (trace)

import Struct.Struct
import Struct.Status (EvalState(..))
import Moves.Pattern
import Moves.Moves
import Moves.ShowMe
import Eval.BasicEval
import Eval.Eval (pieceToIdx)
import Eval.NNUE
import Moves.Fen

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
      = map (moveAddColor c) $ nGenNC ++ bGenNC ++ rGenNC ++ qGenNC ++ pGenNC ++ kGenNC
    | otherwise
      = map (moveAddColor c) $ qGenNC ++ rGenNC ++ bGenNC ++ nGenNC ++ pGenNC ++ kGenNC
    where pGenNC = map (moveAddPiece Pawn . uncurry moveFromTo)
                      $ targetPawnMoves c (pawns p .&. me p) (occup p) regPawnsBB
          nGenNC = map (moveAddPiece Knight . uncurry moveFromTo)
                      $ concatMap (srcDests (ncapt . nAttacs))
                      $ bbToSquares $ knights p .&. me p
          bGenNC = map (moveAddPiece Bishop . uncurry moveFromTo)
                      $ concatMap (srcDests (ncapt . bAttacs (occup p)))
                      $ bbToSquares $ bishops p .&. me p
          rGenNC = map (moveAddPiece Rook   . uncurry moveFromTo)
                      $ concatMap (srcDests (ncapt . rAttacs (occup p)))
                      $ bbToSquares $ rooks p .&. me p
          qGenNC = map (moveAddPiece Queen  . uncurry moveFromTo)
                      $ concatMap (srcDests (ncapt . qAttacs (occup p)))
                      $ bbToSquares $ queens p .&. me p
          kGenNC = map (moveAddPiece King   . uncurry moveFromTo)
                      $            srcDests (ncapt . legal . kAttacs)
                      $ firstOne $ kings p .&. me p
          !noccup = complement (occup p)
          ncapt = ((.&.) noccup)
          !nyoa = complement $ yoAttacs p
          legal = ((.&.) nyoa)
          c = moving p

-- Generate only non capture promotions
-- The promotion captures are generated together with the other captures
genMovePromo :: MyPos -> ([Move], [Move])
genMovePromo !p = (toQueen, toRook ++ toBishop ++ toKnight)
    where toQueen  = map (uncurry (makePromo Queen))  ftlist
          toRook   = map (uncurry (makePromo Rook))   ftlist
          toBishop = map (uncurry (makePromo Bishop)) ftlist
          toKnight = map (uncurry (makePromo Knight)) ftlist
          ftlist   = targetPawnMoves (moving p) (pawns p .&. me p) (occup p) promoline
          !promoline | moving p == White = row8
                     | otherwise         = row1

{-# INLINE srcDests #-}
srcDests :: (Square -> BBoard) -> Square -> [(Square, Square)]
srcDests f = \s -> zip (repeat s) $ bbToSquares $ f s

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
moveChecksIndirect !p !m = ba .&. bq /= 0 || ra .&. rq /= 0
    where !ksq = firstOne $ kings p .&. yo p
          !b   = bishops p .&. me p
          !r   = rooks p   .&. me p
          !q   = queens p  .&. me p
          !bq  = b .|. q
          !rq  = r .|. q
          !fb  = uBit $ fromSquare m
          !tb  = uBit $ toSquare m
          !occ = (occup p .|. tb) `less` fb
          ba   = bAttacs occ ksq
          ra   = rAttacs occ ksq

-- Because finding the blocking square for a queen check is so hard,
-- we define a data type and, in case of a queen check, we give also
-- the piece type (rook or bishop) in which direction the queen checks
data CheckInfo = NormalCheck Piece !Square
               | QueenCheck Piece !Square

-- Finds pieces which check
findChecking :: MyPos -> [CheckInfo]
findChecking !pos = concat [pChk, nChk, bChk, rChk, qbChk, qrChk]
    where pChk  = map (NormalCheck Pawn)   $ bbToSquares $ pAttacs (moving pos) ksq .&. p
          nChk  = map (NormalCheck Knight) $ bbToSquares $ nAttacs ksq .&. n
          bChk  = map (NormalCheck Bishop) $ bbToSquares $ ba .&. b
          rChk  = map (NormalCheck Rook)   $ bbToSquares $ ra .&. r
          qbChk = map (QueenCheck Bishop)  $ bbToSquares $ ba .&. q
          qrChk = map (QueenCheck Rook)    $ bbToSquares $ ra .&. q
          occ = occup pos
          !ksq = firstOne $ kings pos .&. me pos
          !b = bishops pos .&. yo pos
          !r = rooks   pos .&. yo pos
          !q = queens  pos .&. yo pos
          !n = knights pos .&. yo pos
          !p = pawns   pos .&. yo pos
          ra = rAttacs occ ksq
          ba = bAttacs occ ksq

-- Generate move when in check
genMoveFCheck :: MyPos -> [Move]
genMoveFCheck !p
    | null chklist        = error "genMoveFCheck"
    | null $ tail chklist = r1 ++ kGen ++ r2	-- simple check
    | otherwise           = kGen		-- double check, only king moves help
    where chklist = findChecking p
          kGen = map (moveAddColor (moving p) . moveAddPiece King . uncurry moveFromTo)
                     $ srcDests (legal . kAttacs) ksq
          !ksq = firstOne kbb
          !kbb = kings p .&. me p
          !ocp1 = occup p `less` kbb
          !call = complement $ me p .|. yoAttacs p .|. excl
          legal = ((.&.) call)
          !excl = foldl' (.|.) 0 $ map chkAtt chklist
          chkAtt (NormalCheck f s) = fAttacs s f ocp1
          chkAtt (QueenCheck f s)  = fAttacs s f ocp1
          -- This head is safe becase chklist is first checked in the pattern of the function
          (r1, r2) = case head chklist of	-- this is needed only when simple check
                 NormalCheck Pawn   sq -> (beatAtP p (uBit sq), [])  -- cannot block pawn
                 NormalCheck Knight sq -> (beatAt  p (uBit sq), [])  -- or knight check
                 NormalCheck Bishop sq -> beatOrBlock Bishop p sq
                 NormalCheck Rook   sq -> beatOrBlock Rook p sq
                 QueenCheck  pt     sq -> beatOrBlock pt p sq
                 _                     -> error "genMoveFCheck: what check?"

-- Generate moves ending on a given square (used to defend a check by capture or blocking)
-- This part is only for queens, rooks, bishops and knights (no pawns and, of course, no kings)
defendAt :: MyPos -> BBoard -> [Move]
defendAt p !bb = map (moveAddColor $ moving p) $ nGenC ++ bGenC ++ rGenC ++ qGenC
    where nGenC = map (moveAddPiece Knight . uncurry moveFromTo)
                     $ concatMap (srcDests (target . nAttacs))
                     $ bbToSquares $ knights p .&. me p
          bGenC = map (moveAddPiece Bishop . uncurry moveFromTo)
                     $ concatMap (srcDests (target . bAttacs (occup p)))
                     $ bbToSquares $ bishops p .&. me p
          rGenC = map (moveAddPiece Rook   . uncurry moveFromTo)
                     $ concatMap (srcDests (target . rAttacs (occup p)))
                     $ bbToSquares $ rooks p .&. me p
          qGenC = map (moveAddPiece Queen  . uncurry moveFromTo)
                     $ concatMap (srcDests (target . qAttacs (occup p)))
                     $ bbToSquares $ queens p .&. me p
          target = (.&. bb)

-- Generate capture pawn moves ending on a given square (used to defend a check by capture)
-- The bitboard represents the piece which checks (only 1 bit set)
pawnBeatAt :: MyPos -> BBoard -> [Move]
pawnBeatAt !p !bb
    | bb .&. myPAttacs p == 0 = []	-- none of my pawns attack the checking piece
    | bb .&. lastline /= 0 		-- pawn attacks on last line: generate promotions
        =  map (uncurry (makePromo Queen))  ftlist
        ++ map (uncurry (makePromo Rook))   ftlist
        ++ map (uncurry (makePromo Bishop)) ftlist
        ++ map (uncurry (makePromo Knight)) ftlist
    | otherwise = map (moveAddColor (moving p) . moveAddPiece Pawn . uncurry moveFromTo) ftlist
    where lastline | moving p == White = row8
                   | otherwise         = row1
          checksq = firstOne bb
          attpwbb = pAttacs (other $ moving p) checksq .&. pawns p .&. me p
          ftlist  = map (\s -> (s, checksq)) $ bbToSquares attpwbb

-- Generate blocking pawn moves ending on given squares (used to defend a check by blocking)
-- The bitboard has one or more quares
-- The moves generated here cannot be captures!
-- When we block on the last line, we generate a promotion - in this case the whole blocking
-- line is on the last line - so we can have either promnotion blocks or normal ones,
-- but never both of them
pawnBlockAt :: MyPos -> BBoard -> [Move]
pawnBlockAt p !bb
    | bb .&. lastline /= 0
        =  map (uncurry (makePromo Queen))  ftlist
        ++ map (uncurry (makePromo Rook))   ftlist
        ++ map (uncurry (makePromo Bishop)) ftlist
        ++ map (uncurry (makePromo Knight)) ftlist
    | otherwise = map (moveAddColor (moving p) . moveAddPiece Pawn . uncurry moveFromTo) ftlist
    where lastline | moving p == White = row8
                   | otherwise         = row1
          ftlist = targetPawnMoves (moving p) (pawns p .&. me p) (occup p) bb

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
    where beat = beatAt p $ uBit sq
          aksq = firstOne $ me p .&. kings p
          line = findLKA f aksq sq
          block = blockAt p line

genMoveNCaptToCheck :: MyPos -> [Move]
genMoveNCaptToCheck p = genMoveNCaptDirCheck p ++ genMoveNCaptIndirCheck p

-- Todo: check with pawns (should be also without promotions)
genMoveNCaptDirCheck :: MyPos -> [Move]
genMoveNCaptDirCheck p
    | moveGenAscendent
      = map (moveAddColor $ moving p) $ nGenC ++ bGenC ++ rGenC ++ qGenC
    | otherwise
      = map (moveAddColor $ moving p) $ qGenC ++ rGenC ++ bGenC ++ nGenC
    where nGenC = map (moveAddPiece Knight . uncurry moveFromTo)
                      $ filtQPSEE p Knight $ concatMap (srcDests (target nTar . nAttacs))
                      $ bbToSquares  $ knights p .&. me p
          bGenC = map (moveAddPiece Bishop . uncurry moveFromTo)
                      $ filtQPSEE p Bishop $ concatMap (srcDests (target bTar . bAttacs (occup p)))
                      $ bbToSquares  $ bishops p .&. me p
          rGenC = map (moveAddPiece Rook   . uncurry moveFromTo)
                      $ filtQPSEE p Rook   $ concatMap (srcDests (target rTar . rAttacs (occup p)))
                      $ bbToSquares  $ rooks p .&. me p
          qGenC = map (moveAddPiece Queen  . uncurry moveFromTo)
                      $ filtQPSEE p Queen  $ concatMap (srcDests (target qTar . qAttacs (occup p)))
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

-- Accumulate Zobrist changes in MyPos due to setting a piece on an empty square
accumSetPiece :: Square -> Color -> Piece -> ZKey -> ZKey
accumSetPiece sq c f = xor (zobPiece c f sq)

-- Accumulate Zobrist changes in MyPos due to clearing a square
-- This is the same as when setting the piece on an empty square
accumClearSq :: Square -> Color -> Piece -> ZKey -> ZKey
accumClearSq = accumSetPiece

accumMoving :: ZKey -> ZKey
accumMoving = xor zobMove

-- Take an initial accumulation and a list of functions accum to accum
-- and compute the final accumulation
chainAccum :: ZKey -> [ZKey -> ZKey] -> ZKey
chainAccum = foldl (flip ($))

-- When we update the NNUE accumulators incrementally, we have pairs of indices
-- (white, black) with 2 possible signs:
-- adding a piece is with plus
-- clearing a piece is with minus
-- Per move we have more such pairs and we need to collect them together
data AccumChange = AccumChange Bool Int Int
    -- deriving Show

pieceToAccumAdd :: Piece -> Color -> Square -> AccumChange
pieceToAccumAdd piece pccol sq = AccumChange True x y
    where x = pieceToIdx White sq piece pccol
          y = pieceToIdx Black sq piece pccol

pieceToAccumSub :: Piece -> Color -> Square -> AccumChange
pieceToAccumSub piece pccol sq = AccumChange False x y
    where x = pieceToIdx White sq piece pccol
          y = pieceToIdx Black sq piece pccol

updateAccumulators :: NNUE -> Accum -> Accum -> [AccumChange] -> (Accum, Accum)
updateAccumulators model whacc blacc acs = -- trace tr $
    foldr f (whacc, blacc) acs
    where f (AccumChange isadd x y) (wha, bla)
              | isadd     = (addIndex model x wha, addIndex model y bla)
              | otherwise = (subIndex model x wha, subIndex model y bla)
          tr = "ACS:" ++ concatMap (showAccumChange model) acs

showAccumChange :: NNUE -> AccumChange -> String
showAccumChange model (AccumChange plus x y)
    = " ACh " ++ show plus ++ " W " ++ show x ++ " -> " ++ show (accumVal model x)
        ++ " B " ++ show y ++ " -> " ++ show (accumVal model y)

{-
changePining :: MyPos -> Square -> Square -> Bool
changePining p src dst = kings p `testBit` src	-- king is moving
                      || slide p `testBit` src -- pining piece is moving
                      || slide p `testBit` dst -- pining piece is captured
-}

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
legalMove :: MyPos -> Move -> Bool
legalMove p m
    | moveColor m /= mc   = False
    | me p `uTestBit` dst = False
    | Busy col fig <- tabla p src,
      col == mc,
      fig == movePiece m =
         if moveIsNormal m
            then canMove fig p src dst
            else specialMoveIsLegal p m
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
moveIsCapture p m = occup p .&. (uBit (toSquare m)) /= 0

canMove :: Piece -> MyPos -> Square -> Square -> Bool
canMove Pawn p src dst
    =  (not $ null $ targetPawnMoves (moving p) (uBit src) (occup p) (uBit dst))
    || pAttacs (moving p) src `uTestBit` dst
canMove fig p src dst = fAttacs src fig (occup p) `uTestBit` dst

-- See http://stackoverflow.com/questions/47981/how-do-you-set-clear-and-toggle-a-single-bit-in-c-c
-- I combined "Checking a bit" with "Changing the nth bit to x"
-- We have also to clear dst bit
{-# INLINE mvBit #-}
mvBit :: Square -> Square -> BBoard -> BBoard
mvBit src dst w = (w `xor` mx) .&. (complement $ uBit src)
    where mx = ((complement ((w `unsafeShiftR` src) .&. 1) + 1) `xor` w) .&. (uBit dst)

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
doFromToMove :: EvalState -> Move -> MyPos -> MyPos
doFromToMove (EvalState model) m !p | moveIsNormal m
    = updatePos p {
          black = tblack, slide = tslide, kkrq  = tkkrq,  diag  = tdiag,
          epcas = tepcas, zobkey = tzobkey, whAccum = twhacc, blAccum = tblacc
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
          (dstclear, accs1) = case tabla p dst of
              Empty      -> ([], [])
              Busy co fo -> ([accumClearSq dst co fo], [pieceToAccumSub fo co dst])
          (tzobkey, accs2)
               = case tabla p src of	-- identify the moving piece
                   Busy col fig -> (chainAccum zob $ dstclear ++ [
                                       accumClearSq  src col fig,
                                       accumSetPiece dst col fig,
                                       accumMoving
                                   ], [
                                       pieceToAccumSub fig col src,
                                       pieceToAccumAdd fig col dst
                                   ]
                                   )
                   _ -> error $ "Src field empty: " ++ show m ++ " in pos\n"
                                 ++ showTab (black p) (slide p) (kkrq p) (diag p)
                                 ++ "resulting pos:\n"
                                 ++ showTab tblack tslide tkkrq tdiag
          (twhacc, tblacc) = updateAccumulators model (whAccum p) (blAccum p) $ accs1 ++ accs2

doFromToMove (EvalState model) m !p | moveIsEnPas m
    = updatePos p {
          black = tblack, slide = tslide, kkrq  = tkkrq,  diag  = tdiag,
          epcas = tepcas, zobkey = tzobkey, whAccum = twhacc, blAccum = tblacc
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
          !epcl = epClrZob $ epcas p
          !zk = zobkey p `xor` epcl
          (tzobkey, accs2)
              | Busy col Pawn <- tabla p src,	-- identify the moving piece
                Busy co1 Pawn <- tabla p del,
                col /= co1
                = (chainAccum zk [
                                accumClearSq  src col Pawn,
                                accumClearSq  del co1 Pawn,
                                accumSetPiece dst col Pawn,
                                accumMoving
                              ],
                   [
                       pieceToAccumSub Pawn col src,
                       pieceToAccumSub Pawn co1 del,
                       pieceToAccumAdd Pawn col dst
                   ])
              | otherwise = error "doFromToMove en-passant"
          (twhacc, tblacc) = updateAccumulators model (whAccum p) (blAccum p) accs2

doFromToMove (EvalState model) m !p | moveIsCastle m
    = updatePos p {
          black = tblack, slide = tslide, kkrq  = tkkrq,  diag  = tdiag,
          epcas = tepcas, zobkey = tzobkey, whAccum = twhacc, blAccum = tblacc
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
          !epcl = epClrZob $ epcas p
          !zob = zobkey p `xor` epcl `xor` zobcast
          (tzobkey, accs2)
              | Busy col King <- tabla p src,	-- identify the moving piece (king)
                Busy co1 Rook <- tabla p csr,	-- identify the moving rook
                col == co1			-- should be same color
                = (chainAccum zob [
                                accumClearSq  src col King,
                                accumSetPiece dst col King,
                                accumClearSq  csr co1 Rook,
                                accumSetPiece cds co1 Rook,
                                accumMoving
                              ],
                   [
                       pieceToAccumSub King col src,
                       pieceToAccumAdd King col dst,
                       pieceToAccumSub Rook co1 csr,
                       pieceToAccumAdd Rook co1 cds
                   ])
              | otherwise = error "doFromToMove King + Rook"
          (twhacc, tblacc) = updateAccumulators model (whAccum p) (blAccum p) accs2

doFromToMove (EvalState model) m !p | moveIsPromo m
    = updatePos p0 {
          black = tblack, slide = tslide, kkrq = tkkrq, diag = tdiag,
          epcas = tepcas, zobkey = tzobkey, whAccum = twhacc, blAccum = tblacc
      }
    where srank = if moving p == White then 6 else 1
          sfile = fromSquare m .&. 0x7	-- see new coding!
          src = srank `unsafeShiftL` 3 .|. sfile -- new coding doesn't have correct fromSquare in promotion
          dst = toSquare m
          !pie = movePromoPiece m
          -- This trick with p0 is a bit odd, but it works & does not affect the accumulator calculations
          p0 = setPiece src (moving p) pie p
          tblack = mvBit src dst $ black p0
          tslide = mvBit src dst $ slide p0
          tkkrq  = mvBit src dst $ kkrq p0
          tdiag  = mvBit src dst $ diag p0
          !dstbb = uBit dst	-- destination could clear cast rights!
          (clearcast, zobcast) = clearCast (epcas p) dstbb
          tepcas = reset50Moves $ moveAndClearEp $ epcas p `less` clearcast
          !epcl = epClrZob $ epcas p0
          !zk = zobkey p0 `xor` epcl `xor` zobcast
          (dstclear, accs1) = case tabla p dst of
              Empty      -> ([], [])
              Busy co fo -> ([accumClearSq dst co fo], [pieceToAccumSub fo co dst])
          (tzobkey, accs2) = (chainAccum zk $ dstclear ++ [
                                accumClearSq  src (moving p) Pawn,
                                accumSetPiece dst (moving p) pie,
                                accumMoving
                             ], [
                                pieceToAccumSub Pawn (moving p) src,
                                pieceToAccumAdd pie  (moving p) dst
                             ]
                             )
          (twhacc, tblacc) = updateAccumulators model (whAccum p) (blAccum p) $ accs1 ++ accs2

doFromToMove _ _ _ = error "doFromToMove: wrong move type"

reverseMoving :: MyPos -> MyPos
reverseMoving p = updatePos p { epcas = tepcas, zobkey = z, whAccum = twhacc, blAccum = tblacc }
    where tepcas = moveAndClearEp $ epcas p
          epcl = epClrZob $ epcas p
          zk = zobkey p `xor` epcl
          z = chainAccum zk [accumMoving]
          twhacc = whAccum p
          tblacc = blAccum p

-- Find pinning lines for a piece type, given the king & piece squares
-- the queen is very hard, so we solve it as a composition of rook and bishop
-- and when we call findLKA we always know as which piece the queen checks
{-# INLINE findLKA #-}
findLKA :: Piece -> Square -> Int -> BBoard
findLKA Queen !ksq !psq
    | rAttacs bpsq ksq .&. bpsq == 0 = findLKA0 Bishop ksq psq
    | otherwise                      = findLKA0 Rook   ksq psq
    where !bpsq = uBit psq
findLKA pt !ksq !psq = findLKA0 pt ksq psq

findLKA0 :: Piece -> Square -> Int -> BBoard
findLKA0 pt ksq psq
    | pt == Bishop = go bAttacs
    | pt == Rook   = go rAttacs
    | otherwise    = 0	-- it will not be called with other pieces
    where go f = bb
              where !kp = f (uBit psq) ksq
                    !pk = f (uBit ksq) psq
                    !bb = kp .&. pk

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
          p1 = lsbBBoard p
          n1 = lsbBBoard n
          b1 = lsbBBoard b
          r1 = lsbBBoard r
          q1 = lsbBBoard q
          k1 = lsbBBoard k

-- Data structure to keep the status for the incremental calculation
-- of the new attacks during SEE
data Attacks = Attacks {
                   atAtt, atOcc, atBQ, atRQ, atRst :: !BBoard
               }
 
-- The new attacks are calculated once per central square with this function,
-- which is more heavy, and then updated with newAttacs incrementally, which is cheaper
theAttacs :: MyPos -> Square -> Attacks
theAttacs pos sq = axx
    where !occ = occup pos
          !b = bishops pos
          !r = rooks pos
          !q = queens pos
          !n = knights pos
          !k = kings pos
          !p = pawns pos
          !white = occ `less` black pos
          !bq  = b .|. q                -- bishops & queens
          !rq  = r .|. q                -- rooks & queens
          !rst =   nAttacs     sq .&. n
               .|. kAttacs     sq .&. k
               .|. (pAttacs White sq .&. black pos .|. pAttacs Black sq .&. white) .&. p
          !bqa = bAttacs occ sq .&. bq
          !rqa = rAttacs occ sq .&. rq
          !ats = bqa .|. rqa .|. rst    -- these are all attackers
          !axx = Attacks ats occ bq rq rst      -- this is result and state for the next step
 
newAttacs :: Square -> BBoard -> Attacks -> Attacks
newAttacs sq !moved !atts = axx
    where !mvc = complement moved
          !occ = atOcc atts .&. mvc     -- reduce occupacy
          !bq  = atBQ  atts .&. mvc     -- reduce bishops & queens
          !rq  = atRQ  atts .&. mvc     -- reduce rooks & queens
          !rst = atRst atts .&. mvc     -- reduce pawns, knights & kings
          !bqa = bAttacs occ sq .&. bq  -- new bishops & queens can arise because reduced occupacy
          !rqa = rAttacs occ sq .&. rq  -- new rooks & queens can arise because reduced occupacy
          !ats = bqa .|. rqa .|. rst    -- these are all new attackers
          !axx = Attacks ats occ bq rq rst      -- this is result and state for the next step

slideAttacs :: Square -> BBoard -> BBoard -> BBoard -> BBoard -> BBoard
slideAttacs sq b r q occ = bAttacs occ sq .&. (b .|. q)
                       .|. rAttacs occ sq .&. (r .|. q)

xrayAttacs :: MyPos -> Square -> Bool
xrayAttacs pos sq = sa1 /= sa0
    where sa1 = slideAttacs sq (bishops pos) (rooks pos) (queens pos) (occup pos)
          sa0 = slideAttacs sq (bishops pos) (rooks pos) (queens pos) 0

unimax :: Int -> [Int] -> Int
unimax = foldl' (\a g -> min g (-a))

usePosXRay :: Bool
usePosXRay = False

data SEEPars = SEEPars {
                   seeGain, seeVal :: !Int,
                   seeAtts, seeFrom, seeMovd, seeDefn, seeAgrs :: !BBoard,
                   seeAttsRec :: !Attacks
               }

-- Calculate the value of a move per SEE, given the position,
-- the source square of the first capture, the destination of the captures
-- and the value of the first captured piece
seeMoveValue :: MyPos -> Attacks -> Square -> Square -> Int -> Int
seeMoveValue !pos !attacks !sqfirstmv !sqto !gain0 = v
    where v = go sp0 [gain0]
          go :: SEEPars -> [Int] -> Int
          go !seepars acc =
             let gain'   = seeVal  seepars -     seeGain seepars
                 moved'  = seeMovd seepars .|.   seeFrom seepars
                 attacs1 = seeAtts seepars `xor` seeFrom seepars
                 (from', val') = chooseAttacker pos (attacs1 .&. seeAgrs seepars)
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
                           else if seeFrom seepars .&. mayXRay /= 0
                                   then go seepars2 acc'
                                   else go seepars1 acc'
          !mayXRay = pawns pos .|. bishops pos .|. rooks pos .|. queens pos  -- could be calc.
          posXRay = xrayAttacs pos sqto  -- only once, as it is per pos (but it's cheap anyway)
          !moved0 = uBit sqfirstmv
          attacs0 = newAttacs sqto moved0 attacks
          (!from0, !valfrom) = chooseAttacker pos (atAtt attacs0 .&. yo pos)
          sp0 = SEEPars { seeGain = gain0, seeVal = valfrom, seeAtts = atAtt attacs0,
                          seeFrom = from0, seeMovd = moved0, seeDefn = yo pos, seeAgrs = me pos,
                          seeAttsRec = attacs0 }

-- This function can produce illegal captures with the king!
genMoveCaptWL :: MyPos -> ([Move], [Move])
genMoveCaptWL !pos = (map f $ sort ws, map f $ sort ls)
    where !capts = myAttacs pos .&. yo pos
          epcs  = genEPCapts pos
          c     = moving pos
          (ws, ls) = foldr (perCaptFieldWL pos (me pos) (yoAttacs pos)) (lepcs,[]) $ bbToSquares capts
          lepcs = map (moveToLMove Pawn Pawn) epcs
          f = moveAddColor c . lmoveToMove

type LMove = Word32

-- We want to sort MVVLVA, which means first victim, then attacker
-- Victim is "negate" so that the normal sort will pick higher victims first
-- We rely here on the fact that the piece type enumeration
-- is from low to high value (i.e. pawn, knight, bishop, rook, queen, king)
-- Otherwise this will not work!
{-# INLINE moveToLMove #-}
moveToLMove :: Piece -> Piece -> Move -> LMove
moveToLMove attacker victim
    = \ (Move w) -> (vicval `unsafeShiftL` 24)
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
    where !epBB = epcas pos .&. epMask
          dst   = head $ bbToSquares epBB	-- safe because epBB /= 0
          srcBB = pAttacs (other $ moving pos) dst .&. me pos .&. pawns pos

perCaptFieldWL :: MyPos -> BBoard -> BBoard -> Square -> ([LMove], [LMove]) -> ([LMove], [LMove])
perCaptFieldWL pos mypc advdefence sq mvlst
    | Busy _ pcto <- tabla pos sq
        = if defended
             then let valto  = seeValue pcto
                      mvlst1 = foldr (perCaptWL pos myAttRec False pcto valto sq) mvlst  reAgrsqs
                  in           foldr (perCaptWL pos myAttRec True  pcto valto sq) mvlst1 prAgrsqs
             else let mvlst1 = foldr (addHanging  pos pcto sq) mvlst  reAgrsqs
                  in           foldr (addHangingP     pcto sq) mvlst1 prAgrsqs	-- for promotions
    | otherwise = error "perCaptFieldWL pattern"
    where !myAttRec = theAttacs pos sq	-- is this strictnes necessary?
          myattacs = mypc .&. atAtt myAttRec
          defended = advdefence `testBit` sq
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
perCaptWL !pos !attacks promo vict !gain0 !sq !sqfa (wsqs, lsqs)
    | promo = (map (moveToLMove Pawn vict) promos ++ wsqs, lsqs)
    | Busy _ attc <- tabla pos sqfa
        = let v0  = seeValue attc
              ss  = moveToLMove attc vict $ moveAddPiece attc $ moveFromTo sqfa sq
              adv = seeMoveValue pos attacks sqfa sq v0
              approx = approximateEasyCapts && gain0 >= v0
          in if approx || adv <= gain0
                then (ss:wsqs, lsqs)
                else (wsqs, ss:lsqs)
    | otherwise = error "perCaptWL pattern"
    where promos = map (\p -> makePromo p sqfa sq) [Queen, Rook, Bishop, Knight]

-- Captures of hanging pieces are always winning
addHanging :: MyPos -> Piece -> Square -> Square -> ([LMove], [LMove]) -> ([LMove], [LMove])
addHanging pos vict to from (wsqs, lsqs)
    | Busy _ apiece <- tabla pos from
          = ((moveToLMove apiece vict $ moveAddPiece apiece (moveFromTo from to)) : wsqs, lsqs)
    | otherwise = error "addHanging pattern"

addHangingP :: Piece -> Square -> Square -> ([LMove], [LMove]) -> ([LMove], [LMove])
addHangingP vict to from (wsqs, lsqs) = (map (moveToLMove Pawn vict) promos ++ wsqs, lsqs)
    where promos = map (\p -> makePromo p from to) [Queen, Rook, Bishop, Knight]

filtQPSEE :: MyPos -> Piece -> [(Square, Square)] -> [(Square, Square)]
filtQPSEE !pos piece = filter (quietPositiveSEE pos v0)
    where v0 = seeValue piece

quietPositiveSEE :: MyPos -> Int -> (Square, Square) -> Bool
quietPositiveSEE !pos !v0 (!sqfa, !sq) = adv <= 0
    where !allAttRec = theAttacs pos sq
          adv = seeMoveValue pos allAttRec sqfa sq v0
