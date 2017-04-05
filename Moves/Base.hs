{-# LANGUAGE TypeSynonymInstances,
             MultiParamTypeClasses,
             BangPatterns,
             RankNTypes, UndecidableInstances,
             FlexibleInstances
             #-}

module Moves.Base (
    posToState, getPos, posNewSearch,
    doRealMove, doMove, undoMove, genMoves, genTactMoves, canPruneMove,
    tacticalPos, zugZwang, isMoveLegal, isKillCand, isTKillCand,
    betaCut, doNullMove, ttRead, ttStore, curNodes, chooseMove, isTimeout, informCtx,
    mateScore, scoreDiff, qsDelta,
    draftStats,
    finNode,
    showMyPos, logMes,
    nearmate
) where

import Data.Bits
import Data.Int
import Control.Monad.State
import Control.Monad.Reader (ask)
-- import Numeric
import System.Random

import Moves.BaseTypes
import Search.AlbetaTypes
import Struct.Struct
import Struct.Context
import Struct.Status
import Hash.TransTab
import Moves.Board
import Moves.BitBoard (less)
import Eval.BasicEval
import Eval.Eval
import Moves.ShowMe
import Moves.History
import Moves.Notation

{-# INLINE nearmate #-}
nearmate :: Int -> Bool
nearmate i = i >= mateScore - 255 || i <= -mateScore + 255

-- Some options and parameters:
-- debug :: Bool
-- debug = False
scoreDiffEqual :: Int
scoreDiffEqual = 4 -- under this score difference moves are considered to be equal (choose random)

printEvalInt :: Int64
printEvalInt   = 2 `shiftL` 12 - 1	-- if /= 0: print eval info every so many nodes

mateScore :: Int
mateScore = 20000

curNodes :: Game Int64
{-# INLINE curNodes #-}
curNodes = gets (sNodes . mstats)

{-# INLINE getPos #-}
getPos :: Game MyPos
getPos = gets (head . stack)

{-# INLINE informCtx #-}
informCtx :: Comm -> Game ()
informCtx = lift . talkToContext

posToState :: MyPos -> Cache -> History -> EvalState -> MyState
posToState p c h e = MyState {
                       stack = [p''],
                       hash = c,
                       hist = h,
                       mstats = ssts0,
                       gstats = ssts0,
                       evalst = e
                   }
    where stsc = evalState (posEval p) e
          p'' = p { staticScore = stsc }

posNewSearch :: MyState -> MyState
posNewSearch p = p { hash = newGener (hash p) }

draftStats :: SStats -> Game ()
draftStats dst = do
    s <- get
    put s { mstats = addStats (mstats s) dst, gstats = addStats (gstats s) dst }

-- Loosing captures after non-captures?
loosingLast :: Bool
loosingLast = True

genMoves :: Int -> Game ([Move], [Move])
genMoves d = do
    p <- getPos
    if isCheck p $ moving p
       then return (genMoveFCheck p, [])
       else do
            h <- gets hist
            let l0 = genMoveCast p
                l1 = genMovePromo p
                (l2w, l2l) = genMoveCaptWL p
                l3' = genMoveNCapt p
                l3 = histSortMoves d h l3'
            return $! if loosingLast
                         then (l1 ++ l2w, l0 ++ l3 ++ l2l)
                         else (l1 ++ l2w ++ l2l, l0 ++ l3)

-- Generate only tactical moves, i.e. promotions, captures & check escapes
genTactMoves :: Game [Move]
genTactMoves = do
    p <- getPos
    if isCheck p $ moving p
       then return $ genMoveFCheck p
       else do
           let l1  = genMovePromo p
               l2w = fst $ genMoveCaptWL p
           return $ l1 ++ l2w

{--
checkGenMoves :: MyPos -> [Move] -> [Move]
checkGenMoves p = map $ toError . checkGenMove p
    where toError (Left str) = error str
          toError (Right m)  = m

checkGenMove :: MyPos -> Move -> Either String Move
checkGenMove p m@(Move w)
    = case tabla p f of
          Empty     -> wrong "empty src"
          Busy c pc -> if moveColor m /= c
                          then if mc == c
                                  then wrong $ "wrong move color (should be " ++ show mc ++ ")"
                                  else wrong $ "wrong pos src color (should be " ++ show mc ++ ")"
                          else if movePiece m /= pc
                                  then wrong $ "wrong move piece (should be " ++ show pc ++ ")"
                                  else Right m
    where f  = fromSquare m
          mc = moving p
          wrong mes = Left $ "checkGenMove: " ++ mes ++ " for move "
                            ++ showHex w (" in pos\n" ++ showMyPos p)
--}

-- massert :: String -> Game Bool -> Game ()
-- massert s mb = do
--     b <- mb
--     if b then return () else error s

showMyPos :: MyPos -> String
showMyPos p = showTab (black p) (slide p) (kkrq p) (diag p) ++ "================ " ++ mc ++ "\n"
    where mc = if moving p == White then "w" else "b"

{-# INLINE uBitSet #-}
uBitSet :: BBoard -> Int -> Bool
uBitSet bb sq = bb .&. (1 `unsafeShiftL` sq) /= 0

{-# INLINE uBitClear #-}
uBitClear :: BBoard -> Int -> Bool
uBitClear bb sq = bb .&. (1 `unsafeShiftL` sq) == 0

-- Move from a node to a descendent - the real move version
doRealMove :: Move -> Game DoResult
doRealMove m = do
    s  <- get
    let (pc:_) = stack s	-- we never saw an empty stack error until now
        !m1 = checkCastle (checkEnPas m pc) pc
        -- Moving a non-existent piece?
        il = occup pc `uBitClear` fromSquare m1
        -- Capturing one king?
        kc = kings pc `uBitSet` toSquare m1
        p' = doFromToMove m1 pc
        cok = checkOk p'
    -- If the move is real and one of those conditions occur,
    -- then we are really in trouble...
    if (il || kc)
       then do
           logMes $ "Illegal REAL move or position: move = " ++ show m
                    ++ ", il = " ++ show il ++ ", kc = " ++ show kc ++ "\n"
           logMes $ "Illegal position (after the move):\n" ++ showMyPos p'
           logMes $ "Stack:\n" ++ showStack 3 (stack s)
           -- After an illegal result there must be no undo!
           return Illegal
       else if not cok
               then return Illegal
               else do
                   put s { stack = p' : stack s }
                   return $ Exten 0 False

-- Move from a node to a descendent - the search version
doMove :: Move -> Bool -> Game DoResult
doMove m qs = do
    s <- get
    let (pc:_) = stack s	-- we never saw an empty stack error until now
        -- Moving a non-existent piece?
        il  = occup pc `uBitClear` fromSquare m
        -- Capturing one king?
        kc  = kings pc `uBitSet` toSquare m
        sts = evalState (posEval p) (evalst s)
        p   = doFromToMove m pc { staticScore = sts }
    if (il || kc)
       then do
           logMes $ "Illegal move or position: move = " ++ show m
                    ++ ", il = " ++ show il ++ ", kc = " ++ show kc ++ "\n"
           logMes $ "Illegal position (after the move):\n" ++ showMyPos p
           logMes $ "Stack:\n" ++ showStack 3 (stack s)
           -- After an illegal result there must be no undo!
           return Illegal
       else if not $ checkOk p
               then return Illegal
               else do
                   put s { stack = p : stack s }
                   remis <- if qs then return False else checkRemisRules p
                   if remis
                      then return $ Final 0
                      else do
                          let dext = if inCheck p then 1 else 0
                          return $! Exten dext $ moveIsCaptPromo pc m

doNullMove :: Game ()
doNullMove = do
    s <- get
    let (pc:_) = stack s	-- we never saw an empty stack error until now
        sts = evalState (posEval p) (evalst s)
        p   = reverseMoving pc { staticScore = sts }
    put s { stack = p : stack s }

checkRemisRules :: MyPos -> Game Bool
checkRemisRules p = do
    s <- get
    if remis50Moves p
       then return True
       else do	-- check repetition rule
         let revers = map zobkey $ takeWhile isReversible $ stack s
             equal  = filter (== zobkey p) revers	-- if keys are equal, pos is equal
         case equal of
            (_:_:_)    -> return True
            _          -> return False

{-# INLINE undoMove #-}
undoMove :: Game ()
undoMove = modify $ \s -> s { stack = tail $ stack s }

-- Tactical positions will be searched complete in quiescent search
-- Currently only when in in check
{-# INLINE tacticalPos #-}
tacticalPos :: MyPos -> Bool
tacticalPos = (/= 0) . check

{-# INLINE zugZwang #-}
zugZwang :: MyPos -> Bool
zugZwang p = me p `less` (kings p .|. pawns p) == 0

{-# INLINE isMoveLegal #-}
isMoveLegal :: MyPos -> Move -> Bool
isMoveLegal = legalMove

-- Why not just like isTKillCand?
-- Also: if not normal, it is useless, as now it is not recognized as legal...
{-# INLINE isKillCand #-}
isKillCand :: MyPos -> Move -> Move -> Bool
isKillCand p mm ym
    | toSquare mm == toSquare ym = False
    | otherwise                  = not $ moveIsCapture p ym

-- If not normal, it is useless, as now it is not recognized as legal...
{-# INLINE isTKillCand #-}
isTKillCand :: MyPos -> Move -> Bool
isTKillCand p mm = not $ moveIsCapture p mm

-- Static evaluation function
-- This does not detect mate or stale mate, it only returns the calculated
-- static score from a position which has already to be valid
-- Mate/stale mate has to be detected by search!
-- {-# INLINE staticVal #-}
-- staticVal :: Game Int
-- staticVal = staticScore <$> getPos

{-# INLINE finNode #-}
finNode :: String -> Int64 -> Game ()
finNode str nodes =
    when (printEvalInt /= 0 && (nodes .&. printEvalInt == 0)) $ do
        s <- get
        let (p:_) = stack s	-- we never saw an empty stack error until now
            fen = posToFen p
            -- mv = case tail $ words fen of
            --          mv':_ -> mv'
            --          _     -> error "Wrong fen in finNode"
        logMes $ str ++ " Fen: " ++ fen
        -- logMes $ "Eval info " ++ mv ++ ":"
        --               ++ concatMap (\(n, v) -> " " ++ n ++ "=" ++ show v)
        --                            (("score", staticScore p) : weightPairs (staticFeats p))

{--
materVal :: Game Int
materVal = do
    t <- getPos
    let !m = mater t
    return $! case moving t of
                   White -> m
                   _     -> -m
--}

{-# INLINE qsDelta #-}
qsDelta :: Game Int
qsDelta = do
    p <- getPos
    let !pat = yo p .&. myAttacs p
    if pat == 0
       then return 0	-- we have no captures anymore, fail low without generating moves
       else if pat .&. queens p /= 0
               then return qval
               else if pat .&. rooks p /= 0
                       then return rval
                       else return bval
    where qsDeltaMargin = matPiece White Pawn
          qval = qsDeltaMargin + matPiece White Queen
          rval = qsDeltaMargin + matPiece White Rook
          bval = qsDeltaMargin + matPiece White Bishop

{-# INLINE ttRead #-}
ttRead :: Game (Int, Int, Int, Move, Int64)
ttRead = do
    s <- get
    p <- getPos
    mhr <- liftIO $ do
        let ptr = retrieveEntry (hash s) (zobkey p)
        readCache ptr
    case mhr of
        Nothing -> return empRez
        Just t@(_, _, _, m, _) ->
            if legalMove p m then return t else return empRez	-- we should count...
    where empRez = (-1, 0, 0, Move 0, 0)

{-# INLINE ttStore #-}
ttStore :: Int -> Int -> Int -> Move -> Int64 -> Game ()
ttStore !deep !tp !sc !bestm !nds = do
    s <- get
    p <- getPos
    -- We use the type: 0 - upper limit, 1 - lower limit, 2 - exact score
    liftIO $ writeCache (hash s) (zobkey p) deep tp sc bestm nds

-- History heuristic table update when beta cut
betaCut :: Bool -> Int -> Move -> Game ()
betaCut good absdp m
    | moveIsCastle m = do
        s <- get
        liftIO $ toHist (hist s) good m absdp
    | moveIsNormal m = do
        s <- get
        t <- getPos
        case tabla t (toSquare m) of
            Empty -> liftIO $ toHist (hist s) good m absdp
            _     -> return ()
    | otherwise = return ()

-- Will not be pruned nor LMR reduced
-- Now: only for captures or promotions (but check that with LMR!!!)
moveIsCaptPromo :: MyPos -> Move -> Bool
moveIsCaptPromo p m
    | moveIsPromo m || moveIsEnPas m = True
    | otherwise                      = moveIsCapture p m

-- We will call this function before we do the move
-- This will spare a heavy operation for pruned moved
{-# INLINE canPruneMove #-}
canPruneMove :: MyPos -> Move -> Bool
canPruneMove p m
    | not (moveIsNormal m) = False
    | moveIsCapture p m    = False
    | otherwise            = not $ moveChecks p m

-- Score difference obtained by last move, from POV of the moving part
-- It considers the fact that static score is for the part which has to move
scoreDiff :: Game Int
scoreDiff = do
    s <- get
    case stack s of
        (p1:p2:_) -> return $! negate (staticScore p1 + staticScore p2)
        _         -> return 0

-- Choose between almost equal (root) moves
chooseMove :: Bool -> [(Int, [Move])] -> Game (Int, [Move])
chooseMove True pvs = return $ if null pvs then error "Empty choose!" else head pvs
chooseMove _    pvs = case pvs of
    p1 : [] -> return p1
    p1 : ps -> do
         let equal = p1 : takeWhile inrange ps
             minscore = fst p1 - scoreDiffEqual
             inrange x = fst x >= minscore
             len = length equal
         logMes $ "Choose from: " ++ show pvs
         logMes $ "Choose length: " ++ show len
         logMes $ "Choose equals: " ++ show equal
         if len == 1
            then return p1
            else do
               r <- liftIO $ getStdRandom (randomR (0, len - 1))
               return $! equal !! r
    []      -> return (0, [])	-- just for Wall

logMes :: String -> Game ()
logMes s = lift $ talkToContext . LogMes $ s

{-# INLINE isTimeout #-}
isTimeout :: Int -> Game Bool
isTimeout msx = do
    curr <- lift timeFromContext
    return $! msx < curr

showStack :: Int -> [MyPos] -> String
showStack n = concatMap showMyPos . take n

talkToContext :: Comm -> CtxIO ()
talkToContext (LogMes s)       = ctxLog LogInfo s
talkToContext (BestMv a b c d) = informGui a b c d
talkToContext (CurrMv a b)     = informGuiCM a b
talkToContext (InfoStr s)      = informGuiString s

timeFromContext :: CtxIO Int
timeFromContext = do
    ctx <- ask
    let refs = startSecond ctx
    lift $ currMilli refs
