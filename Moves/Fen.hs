{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
module Moves.Fen (
    posFromFen, initPos, updatePos, setPiece, testAttacksCalculation
    ) where

import Data.Array.IArray
import Data.Array.MArray
import Data.Array.Unsafe (unsafeFreeze)
import Data.Array.Base (unsafeAt, unsafeRead, unsafeWrite)
import Control.Monad (when, forM_)
import Control.Monad.ST (ST, runST)
import Data.STRef
import Data.Array.ST
import Data.Bits
import Data.Char
import Data.List (tails)
import Data.Maybe (fromJust)

import Struct.Struct
import Moves.Moves
import Moves.BitBoard
import Moves.Pattern
import Moves.ShowMe
import Eval.BasicEval
import Hash.Zobrist

startFen :: String
startFen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR/ w KQkq - 0 1"

fenToTable :: String -> MyPos
fenToTable fen = foldr setp emptyPos $ fenToAssocs fen
    where setp (sq, (c, p)) = setPiece sq c p

fenToAssocs :: String -> [(Square, (Color, Piece))]
fenToAssocs str = go 56 str []
    where go _ [] acc = acc
          go sq (c:cs) acc
              | sq < 0 = acc
              | c `elem` "PRNBQK" = go (sq+1) cs $ (sq, fcw) : acc
              | c `elem` "prnbqk" = go (sq+1) cs $ (sq, fcb) : acc
              | isDigit c = go (skip sq c) cs acc
              | otherwise = go (nextline sq) cs acc	-- treat like /
              where fcw = (White, toPiece c)
                    fcb = (Black, toPiece $ toUpper c)
          skip f c = f + fromIntegral (ord c - ord '0')
          nextline f = f - 16
          toPiece c = fromJust $ lookup c letterToPiece

letterToPiece :: [(Char, Piece)]
letterToPiece = [('P', Pawn), ('R', Rook), ('N', Knight), ('B', Bishop),
                    ('Q', Queen), ('K', King)]

initPos :: MyPos
initPos = posFromFen startFen

posFromFen :: String -> MyPos
posFromFen fen = updatePos Nothing p { epcas = x, zobkey = zk }
    where fen1:fen2:fen3:fen4:fen5:_ = fenFromString fen
          p  = fenToTable fen1
          x  = fyInit . castInit . epInit $ epcas0
          (epcas0, z) = case fen2 of
              'w':_ -> (0, 0)
              'b':_ -> (mvMask, zobMove)
              _     -> error "posFromFen: expect w or b"
          (cK, z1) = if 'K' `elem` fen3 then ((.|. caRKiw), zobCastKw) else (id, 0)
          (cQ, z2) = if 'Q' `elem` fen3 then ((.|. caRQuw), zobCastQw) else (id, 0)
          (ck, z3) = if 'k' `elem` fen3 then ((.|. caRKib), zobCastKb) else (id, 0)
          (cq, z4) = if 'q' `elem` fen3 then ((.|. caRQub), zobCastQb) else (id, 0)
          castInit = cQ . cK . cq . ck
          (epInit, ze) = case fen4 of
              f:r:_ | f `elem` "abcdefgh" && r `elem` "36"
                    -> let fn  = ord f - ord 'a'
                           ms' = case r of
                                     '3' -> 0x10000
                                     _   -> 0x10000000000
                           ms = ms' `shiftL` fn
                           zz = zobEP fn
                       in ((.|.) ms, zz)
              _     -> (id, 0)
          fyInit = set50Moves $ read fen5
          zk = zobkey p `xor` z `xor` z1 `xor` z2 `xor` z3 `xor` z4 `xor` ze

-- A primitive decomposition of the fen string
fenFromString :: String -> [String]
fenFromString fen = zipWith ($) fenfuncs fentails
    where fentails = tails $ words fen
          fenfuncs = [ getFenPos, getFenMv, getFenCast, getFenEp, getFenHalf, getFenMvNo ]
          headOrDefault a0 as = if null as then a0 else head as
          getFenPos  = headOrDefault ""
          getFenMv   = headOrDefault "w"
          getFenCast = headOrDefault "-"
          getFenEp   = headOrDefault "-"
          getFenHalf = headOrDefault "0"
          getFenMvNo = headOrDefault "0"

-- Passed pawns: only with bitboard operations
whitePassed :: BBoard -> BBoard -> BBoard
whitePassed !wp !bp = wpa
    where !bpL = bbLeft bp
          !bpR = bbRight bp
          !wb0 = bpR .|. bpL .|. bp .|. wp
          !sha = shadowDown wb0	-- erase
          !wpa = wp `less` sha

blackPassed :: BBoard -> BBoard -> BBoard
blackPassed !wp !bp = bpa
    where !wpL = bbLeft wp
          !wpR = bbRight wp
          !wb0 = wpR .|. wpL .|. wp .|. bp
          !sha = shadowUp wb0	-- erase
          !bpa = bp `less` sha

-- Set a piece on a square of the board
setPiece :: Square -> Color -> Piece -> MyPos -> MyPos
setPiece sq c f !p
    = p { black = setCond (c == Black) $ black p,
          slide = setCond (isSlide f)  $ slide p,
          kkrq  = setCond (isKkrq f)   $ kkrq p,
          diag  = setCond (isDiag f)   $ diag p,
          zobkey = nzob, mater = nmat }
    where setCond cond = if cond then (.|. bsq) else (.&. nbsq)
          !nzob = zobkey p `xor` zold `xor` znew
          !nmat = mater p - mold + mnew
          (!zold, !mold) = case tabla p sq of
                             Empty      -> (0, 0)
                             Busy co fo -> (zobPiece co fo sq, matPiece co fo)
          !znew = zobPiece c f sq
          !mnew = matPiece c f
          !bsq = uBit sq
          !nbsq = complement bsq

-- Update the rest of position representation according to the basic representation
updatePos :: Maybe BBoard -> MyPos -> MyPos
updatePos mbb !p = p {
        occup = toccup, me = tme, yo = tyo, kings  = tkings,
        pawns = tpawns, knights = tknights, queens = tqueens,
        rooks = trooks, bishops = tbishops, passed = tpassed,
        attacks = tattacks, attacked = tattacked, lazyBits = lzb, logbook = tlog
    }
    where (tattacks, tattacked, tlog) = case mbb of
              Just cha -> updateAttacks (slide p)
                              toccup twpawns tbpawns tknights tbishops trooks tqueens tkings
                              cha (attacks p) (attacked p)
              Nothing  -> recalcAttacks toccup
                              twpawns tbpawns tknights tbishops trooks tqueens tkings
          !toccup = kkrq p .|. diag p
          !tkings = kkrq p .&. diag p `less` slide p
          !twhite = toccup `less` black p
          (!tme, !tyo) | moving p == White = (twhite, black p)
                       | otherwise         = (black p, twhite)
          !tpawns   = diag p `less` (kkrq p .|. slide p)
          !tknights = kkrq p `less` (diag p .|. slide p)
          !tqueens  = slide p .&. kkrq p .&. diag p
          !trooks   = slide p .&. kkrq p `less` diag p
          !tbishops = slide p .&. diag p `less` kkrq p
          !twpawns = tpawns .&. twhite
          !tbpawns = tpawns .&. black p
          !tpassed = whitePassed twpawns tbpawns .|. blackPassed twpawns tbpawns
          -- For passed pawns calculation:
          -- Now that we know what was changed (mbb) we can check if some pawn was touched
          -- and recalculate only when true
          lzb = posLazy (moving p) tattacks toccup (black p) tpawns tknights tbishops trooks tqueens tkings

posLazy :: Color -> MaArray -> BBoard -> BBoard -> BBoard -> BBoard -> BBoard -> BBoard -> BBoard -> BBoard -> LazyBits
posLazy !co tattacks !ocp !tblack !tpawns !tknights !tbishops !trooks !tqueens !tkings
    | co == White = LazyBits {
                      -- _check = tcheck,
                      _myPAttacs = twhPAtt, _myNAttacs = twhNAtt, _myBAttacs = twhBAtt,
                      _myRAttacs = twhRAtt, _myQAttacs = twhQAtt, _myKAttacs = twhKAtt,
                      _yoPAttacs = tblPAtt, _yoNAttacs = tblNAtt, _yoBAttacs = tblBAtt,
                      _yoRAttacs = tblRAtt, _yoQAttacs = tblQAtt, _yoKAttacs = tblKAtt,
                      _myAttacs  = twhAttacs, _yoAttacs = tblAttacs
                  }
    | otherwise   = LazyBits {
                      -- _check = tcheck,
                      _myPAttacs = tblPAtt, _myNAttacs = tblNAtt, _myBAttacs = tblBAtt,
                      _myRAttacs = tblRAtt, _myQAttacs = tblQAtt, _myKAttacs = tblKAtt,
                      _yoPAttacs = twhPAtt, _yoNAttacs = twhNAtt, _yoBAttacs = twhBAtt,
                      _yoRAttacs = twhRAtt, _yoQAttacs = twhQAtt, _yoKAttacs = twhKAtt,
                      _myAttacs  = tblAttacs, _yoAttacs = twhAttacs
                  }
    where !twhPAtt = bbToSquaresBB f $ tpawns .&. white
          !twhNAtt = bbToSquaresBB f $ tknights .&. white
          !twhBAtt = bbToSquaresBB f $ tbishops .&. white
          !twhRAtt = bbToSquaresBB f $ trooks .&. white
          !twhQAtt = bbToSquaresBB f $ tqueens .&. white
          !twhKAtt = f $ firstOne $ tkings .&. white
          !tblPAtt = bbToSquaresBB f $ tpawns .&. tblack
          !tblNAtt = bbToSquaresBB f $ tknights .&. tblack
          !tblBAtt = bbToSquaresBB f $ tbishops .&. tblack
          !tblRAtt = bbToSquaresBB f $ trooks .&. tblack
          !tblQAtt = bbToSquaresBB f $ tqueens .&. tblack
          !tblKAtt = f $ firstOne $ tkings .&. tblack
          !twhAttacs = twhPAtt .|. twhNAtt .|. twhBAtt .|. twhRAtt .|. twhQAtt .|. twhKAtt
          !tblAttacs = tblPAtt .|. tblNAtt .|. tblBAtt .|. tblRAtt .|. tblQAtt .|. tblKAtt
          !white = ocp `less` tblack
          -- !whcheck = white  .&. tkings .&. tblAttacs
          -- !blcheck = tblack .&. tkings .&. twhAttacs
          -- !tcheck = whcheck .|. blcheck
          f = unsafeAt tattacks

recalcAttacks :: BBoard -> BBoard -> BBoard -> BBoard -> BBoard -> BBoard -> BBoard -> BBoard
              -> (MaArray, MaArray, [String])
recalcAttacks toccup twpawns tbpawns tknights tbishops trooks tqueens tkings
    = (rattacks, rattacked, [])
    where nlist = map (\s -> (s, nAttacs s))        $ bbToSquares tknights
          klist = map (\s -> (s, kAttacs s))        $ bbToSquares tkings
          pwist = map (\s -> (s, pAttacs White s))  $ bbToSquares twpawns
          pbist = map (\s -> (s, pAttacs Black s))  $ bbToSquares tbpawns
          blist = map (\s -> (s, bAttacs toccup s)) $ bbToSquares tbishops
          rlist = map (\s -> (s, rAttacs toccup s)) $ bbToSquares trooks
          qlist = map (\s -> (s, rAttacs toccup s
                             .|. bAttacs toccup s)) $ bbToSquares tqueens
          rattacks = zeroArray // concat [nlist, klist, pwist, pbist, blist, rlist, qlist]
          rattacked = calcAttacked rattacks
          zeroArray = listArray (0, 63) $ repeat 0

-- Recompute the whole attacked array
-- Optimisation: GHC did detect the correct strictness
calcAttacked :: MaArray -> MaArray
calcAttacked arr
    = runSTUArray $ do
        tarr <- newArray (0, 63) 0
        forM_ (assocs arr) $ \(s, bb) -> when (bb /= 0) $ do
            let sb = uBit s
            forM_ (bbToSquares bb) $ \d -> do
                v <- unsafeRead tarr d
                unsafeWrite tarr d (v .|. sb)
        return tarr

-- Update the attacks/attacked arrays
-- cha is the bitboard of added/removed pieces during the move
-- out is the array of attacks from every square
-- ins is the array of attackedBy to every square
--
-- Optimisation: there is no need currently to set explicit strictness in let or monad
-- GHC did find the strict expressions
updateAttacks :: BBoard -> BBoard -> BBoard -> BBoard -> BBoard -> BBoard -> BBoard
              -> BBoard -> BBoard -> BBoard -> MaArray -> MaArray
              -> (MaArray, MaArray, [String])
updateAttacks tslide toccup twpawns tbpawns tknights tbishops trooks tqueens tkings cha out ins
    = runST $ do
        (logger, getlog) <- loggingUtilities
        -- logger $ "Occup:\n" ++ showBB toccup
        logger $ "Move BB:\n" ++ showBB cha
        -- The new out & ins (take a copy)
        nout <- (thaw out) :: ST s (STUArray s Int BBoard)
        nins <- (thaw ins) :: ST s (STUArray s Int BBoard)
        -- These squares are blocked/deblocked by adding/removing the pieces
        let blckd = tslide .&. bbToSquaresBB (unsafeAt ins) cha
            arebb = cha .|. blckd	-- for those we need an attack recalculation
            pcfs = [
                     (twpawns,  pAttacs White),
                     (tbpawns,  pAttacs Black),
                     (tknights, nAttacs),
                     (tbishops, bAttacs toccup),
                     (trooks,   rAttacs toccup),
                     (tqueens,  qAttacs toccup),
                     (tkings,   kAttacs),
                     (complement toccup, const 0)	-- no attacks from empty leaved squares
                   ]
        logger $ "Arebb:\n" ++ showBB arebb
        -- For every piece type existing in the attack recalculation bitboard...
        -- Alternativey we could just take every square and analyse what piece is on it
        -- It should not be very much variation here I guess... But only tests can say
        forM_ pcfs $ \(bb, func) -> do
            -- and every piece of that type
            forM_ (bbToSquares $ arebb .&. bb) $ \sq -> do
                let oatc = out `unsafeAt` sq	-- old attacks from that square
                    natc = func sq		-- new attacks from that square
                unsafeWrite nout sq natc
                logger $ "New attacks from square " ++ show sq ++ ":\n" ++ showBB natc
                let sqbb = uBit sq
                    ains = natc `less` oatc	-- added attackedBy from this square
                forM_ (bbToSquares ains) $ \s -> do
                    obb <- unsafeRead nins s
                    unsafeWrite nins s (obb .|. sqbb)	-- add sq to the attackedBy of s
                    logger $ "Add " ++ show sq ++ " to attacked of " ++ show s
                let sqdd = complement sqbb
                    dins = oatc `less` natc	-- deleted attackedBy from this square
                forM_ (bbToSquares dins) $ \s -> do
                    obb <- unsafeRead nins s
                    unsafeWrite nins s (obb .&. sqdd)	-- delete sq from the attackedBy of s
                    logger $ "Remove " ++ show sq ++ " from attacked of " ++ show s
        noutf <- unsafeFreeze nout
        ninsf <- unsafeFreeze nins
        finlog <- getlog
        return (noutf, ninsf, finlog)

-- Just to test the incremental attacks update
-- Not an exhaustive test
testAttacksCalculation :: MyPos -> Maybe (BBoard, BBoard, BBoard, [(Square, BBoard)])
testAttacksCalculation p
    | myAttacs p /= myatts = Just (myAttacs p, myatts, diffs, froms)
    | otherwise            = Nothing
    where myatts = bbToSquaresBB (unsafeAt $ attacks p) $ me p
          diffs = myAttacs p `xor` myatts
          froms = map (\s -> (s, (attacked p `unsafeAt` s) .&. me p)) $ bbToSquares diffs

-- Used for debugging the attacks/attacked updates, normally disabled
logUpdateAttacks :: Bool
logUpdateAttacks = False

loggingUtilities :: forall s. ST s (String -> ST s (), ST s [String])
loggingUtilities
    | logUpdateAttacks = do
        logRef <- newSTRef []
        let logger = \s -> modifySTRef logRef $ \l -> s : l
            getlog = reverse <$> readSTRef logRef
        return (logger, getlog)
    | otherwise = do
        let logger = \_ -> return ()
            getlog = return []
        return (logger, getlog)
