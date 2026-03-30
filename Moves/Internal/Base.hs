{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternGuards #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

module Moves.Internal.Base (
    posToState, getPos, posNewSearch,
    doRealMove, doMove, doQSMove, doNullMove, undoMove,
    genMoves, genTactMoves, genEscapeMoves, canPruneMove,
    tacticalPos, zugZwang, isMoveLegal, isKillCand, isTKillCand,
    betaCut, ttRead, ttStore, curNodes, isTimeout, informCtx,
    mateScore, qsDelta,
    draftStats,
    finNode, countRepetitions,
    nearmate,
    getRootMoveNumber, incrementRootMoveNumber
) where

import Data.Bits
import Data.Int
import Data.List (nub, partition)
import Control.Monad.State
import Control.Monad.Reader (ask)
import Control.Monad (when)

import Moves.Internal.BaseTypes
import Search.AlbetaTypes
import Struct.Struct
import Struct.Context
import Struct.Status
import Hash.TransTab
import Moves.Core
import Eval.Core (posEval)
import Moves.ShowMe
import Moves.History
import Moves.Notation

{-# INLINE nearmate #-}
nearmate :: Int -> Bool
nearmate i = i >= mateScore - 255 || i <= -mateScore + 255

-- Some options and parameters:
printEvalInt :: Int64
printEvalInt   = 2 `shiftL` 12 - 1	-- if /= 0: print eval info every so many nodes

mateScore :: Int
mateScore = 20000

{-# INLINE curNodes #-}
curNodes :: Int64 -> Game Int64
curNodes n = (n + ) <$> gets (sNodes . mstats)

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
                       evalst = e,
                       rootmn = 1
                   }
    where stsc = posEval p e
          p'' = p { staticScore = stsc }

posNewSearch :: MyState -> MyState
posNewSearch p = p { hash = newGener (hash p), rootmn = 1 }

draftStats :: SStats -> Game ()
draftStats dst = do
    s <- get
    put s { mstats = addStats (mstats s) dst }

genMoves :: Int -> Game ([Move], [Move])
genMoves d = do
    p <- getPos
    h <- gets hist
    if inCheck p
       then do
            let escs = genMoveFCheck p
                (escc, escq) = partition (moveIsCapture p) escs
            return (escc, histSortMoves d h escq)
       else do
            let l0 = genMoveCast p
                (l1q, l1r) = genMovePromo p
                (l2w, l2l) = genMoveCaptWL p
                l3 = histSortMoves d h $ genMoveNCapt p
            -- Loosing captures after non-captures
            return (l1q ++ l2w, l1r ++ l0 ++ l3 ++ l2l)

-- Generate only tactical moves, i.e. promotions & captures
-- Needed only in QS, when we know we are not in check
-- In the frontier nodes (i.e. first level QS) we generate
-- also checking quiet moves with non-negative SEE
genTactMoves :: Bool -> Game [Move]
genTactMoves front = do
    p <- getPos
    return $ if front
                then fst (genMovePromo p) ++ fst (genMoveCaptWL p) ++ genMoveNCaptToCheck p
                else fst (genMovePromo p) ++ fst (genMoveCaptWL p)

-- Generate only escape moves: needed only in QS when we know we have to escape
genEscapeMoves :: Game [Move]
genEscapeMoves = do
    p <- getPos
    return $ genMoveFCheck p

showMyPos :: MyPos -> String
showMyPos p = showTab (black p) (slide p) (kkrq p) (diag p) ++ "================ " ++ mc ++ "\n"
    where mc = if moving p == White then "w" else "b"

{-# INLINE uBitSet #-}
uBitSet :: BBoard -> Int -> Bool
uBitSet bb sq = bb .&. uBit sq /= 0

{-# INLINE uBitClear #-}
uBitClear :: BBoard -> Int -> Bool
uBitClear bb sq = bb .&. uBit sq == 0

-- Move from a node to a descendent - the real move version
doRealMove :: Move -> Game DoResult
doRealMove m = safeStack $ \s pc -> do
    let m1 = checkCastle (checkEnPas m pc) pc
    -- If the move is real and one of those conditions occur,
    -- then we are really in trouble...
    if occup pc `uBitClear` fromSquare m1	-- Moving a non-existent piece?
       then logIllegal True False s pc m1
       else if kings pc `uBitSet` toSquare m1	-- Capturing one king?
               then logIllegal True True s pc m1
               else do
                   let p = doFromToMove m1 pc
                   if not $ checkOk p
                           then return Illegal
                           else do
                               put s { stack = p : stack s }
                               return $ Exten 0 False False

-- Move from a node to a descendent - the normal search version
doMove :: Move -> Game DoResult
doMove m = safeStack $ \s pc ->
    if occup pc `uBitClear` fromSquare m	-- Moving a non-existent piece?
       then logIllegal False False s pc m
       else if kings pc `uBitSet` toSquare m	-- Capturing one king?
               then logIllegal False True s pc m
               else do
                   let p = doFromToMove m pc { staticScore = posEval p (evalst s) }
                   if not $ checkOk p
                      then return Illegal
                      else do
                          put s { stack = p : stack s }
                          if checkRemisRules p (stack s)
                             then return Final
                             else return $ if captOrPromo pc m
                                              then Exten (exten pc p) True True
                                              else Exten (exten pc p) False (noLMR pc m)

logIllegal :: Bool -> Bool -> MyState -> MyPos -> Move -> Game DoResult
logIllegal real kingcapt s p m = do
    when real $ logMes "Illegal real move!"
    logMes $ "Illegal move " ++ show m ++ " in position:\n" ++ showMyPos p
    if kingcapt
       then logMes "King will be captured"
       else logMes "Move non-existent piece"
    logMes $ "Stack:\n" ++ showStack 3 (stack s)
    return Illegal

-- Move from a node to a descendent - the QS search version
-- Here we do only a restricted check for illegal moves
-- It does not check for remis, so it can't return Final
-- It does not check for extensions a.s.o.
doQSMove :: Move -> Game Bool
doQSMove m = safeStack $ \s pc -> do
    let sts = posEval p (evalst s)
        p   = doFromToMove m pc { staticScore = sts }
    if not $ checkOk p
       then return False
       else do
           put s { stack = p : stack s }
           return True

doNullMove :: Game ()
doNullMove = safeStack $ \s pc -> do
    let sts = posEval p (evalst s)
        p   = reverseMoving pc { staticScore = sts }
    put s { stack = p : stack s }

safeStack :: (MyState -> MyPos -> Game r) -> Game r
safeStack a = do
    s <- get
    case stack s of
        []  -> error "Position stack is empty!"
        p:_ -> a s p

checkRemisRules :: MyPos -> [MyPos] -> Bool
checkRemisRules p ps
    | remis50Moves p       = True
    | not $ isReversible p = False
    | otherwise            = not $ null $ filter (== zobkey p) $ map zobkey $ takeWhile isReversible ps
    -- Remarks:
    -- ps does not contain p
    -- if keys are equal, pos is equal

-- If we have a few repetitions in the last moves, then we will reduce moves to go
-- so the time management can allocate more time for next moves
countRepetitions :: MyState -> Int
countRepetitions s = length f6 - uniq
    where uniq = length $ nub $ map zobkey f6
          f6   = take 6 $ stack s

{-# INLINE undoMove #-}
undoMove :: Game ()
undoMove = modify $ \s -> s { stack = drop 1 $ stack s }

-- We extend when last move:
-- - gives check
-- - captures last queen
-- - captures last rook when no queens
exten :: MyPos -> MyPos -> Int
exten p1 p2 | inCheck p2     = 1
            | queens p2 /= 0 = 0
            | queens p1 /= 0 = 1
            | rooks  p2 /= 0 = 0
            | rooks  p1 /= 0 = 1
            | otherwise      = 0

-- Tactical positions will be searched complete in quiescent search
-- Currently only when in check
{-# INLINE tacticalPos #-}
tacticalPos :: MyPos -> Bool
tacticalPos p = isCheck p (moving p)

{-# INLINE zugZwang #-}
zugZwang :: MyPos -> Bool
zugZwang p = me p `less` (kings p .|. pawns p) == 0

{-# INLINE isMoveLegal #-}
isMoveLegal :: MyPos -> Move -> Bool
isMoveLegal = legalMove

-- Why not just like isTKillCand?
{-# INLINE isKillCand #-}
isKillCand :: MyPos -> Move -> Move -> Bool
isKillCand p mm ym
    | toSquare mm == toSquare ym = False
    | otherwise                  = not $ moveIsCapture p ym

{-# INLINE isTKillCand #-}
isTKillCand :: MyPos -> Move -> Bool
isTKillCand p mm = not $ moveIsCapture p mm

{-# INLINE finNode #-}
finNode :: String -> Int64 -> Game ()
finNode str nodes =
    when (printEvalInt /= 0 && (nodes .&. printEvalInt == 0)) $ do
        p <- getPos	-- we never saw an empty stack error until now
        logMes $ str ++ " Score: " ++ show (staticScore p) ++ " Fen: " ++ posToFen p

{-# INLINE getRootMoveNumber #-}
getRootMoveNumber :: Game Int
getRootMoveNumber = gets rootmn

{-# INLINE incrementRootMoveNumber #-}
incrementRootMoveNumber :: Game ()
incrementRootMoveNumber = modify $ \s -> s { rootmn = rootmn s + 1 }

-- {-# INLINE qsDelta #-}
qsDelta :: Int -> Game Bool
qsDelta !a
    | matPiece White Bishop >= a = return False
    | matPiece White Queen  <  a = return True
    | otherwise = do
        p <- getPos
        let !ua = yo p .&. myAttacs p	-- under attack!
        if ua .&. queens p /= 0	-- TODO: need to check also pawns on 7th!
           then return False
           else if matPiece White Rook < a
                   then return True
                   else if ua .&. rooks p /= 0
                           then return False
                           else return True

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
    -- Warning: depth has 6 bit in TT (so max 64)! We are currently still far from this,
    -- but by summing different paths this could happen: so limit it here
    liftIO $ writeCache (hash s) (zobkey p) (min 40 deep) tp sc bestm nds

-- History heuristic table update when beta cut
betaCut :: Int -> Move -> Game ()
betaCut absdp m
    | moveIsCastle m = do
        s <- get
        liftIO $ toHist (hist s) m absdp
    | moveIsNormal m = do
        s <- get
        t <- getPos
        case tabla t (toSquare m) of
            Empty -> liftIO $ toHist (hist s) m absdp
            _     -> return ()
    | otherwise = return ()

-- Captures & promotions
captOrPromo :: MyPos -> Move -> Bool
captOrPromo p m
    | moveIsPromo m || moveIsEnPas m = True
    | otherwise                      = moveIsCapture p m

-- Can be LMR reduced, if not captures & promotions
noLMR :: MyPos -> Move -> Bool
noLMR = movePassed

-- We will call this function before we do the move
-- This will spare a heavy operation for pruned moved
{-# INLINE canPruneMove #-}
canPruneMove :: MyPos -> Move -> Bool
canPruneMove p m
    | not (moveIsNormal m) = False
    | moveIsCapture p m    = False
    | movePassed p m       = False
    | moveChecks p m       = False
    | myQAttacs p == 0     = True	-- the rest makes sense only with own queen on board
    | otherwise            = not $ newQueenAttack p m

-- A move that initiates a new queen attack will not be pruned
newQueenAttack :: MyPos -> Move -> Bool
newQueenAttack p m
    | myAttacs  p .&. yoKAttacs p == 0 = False	-- not enough pressure
    | myQAttacs p .&. yoKAttacs p /= 0 = False	-- already attacked
    | movePiece m /= Queen             = False	-- not quite right: it could be a discovered new attack
    | otherwise                        = bAttacs (occup p) (toSquare m) .&. yoKAttacs p /= 0
                                      || rAttacs (occup p) (toSquare m) .&. yoKAttacs p /= 0

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
talkToContext (LogMes s)         = ctxLog LogInfo s
talkToContext (BestMv a b c d e) = informGuiBM a b c d e
talkToContext (CurrMv a b)       = informGuiCM a b
talkToContext (InfoStr s)        = informGuiSt s

timeFromContext :: CtxIO Int
timeFromContext = do
    ctx <- ask
    let refs = strttm ctx
    lift $ currMilli refs
