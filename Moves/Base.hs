{-# LANGUAGE TypeSynonymInstances,
             MultiParamTypeClasses,
             BangPatterns,
             RankNTypes, UndecidableInstances,
             FlexibleInstances
             #-}

module Moves.Base (
    posToState, getPos, posNewSearch,
    doRealMove, doMove, doQSMove, doNullMove, undoMove,
    genMoves, genTactMoves, genEscapeMoves, canPruneMove,
    tacticalPos, zugZwang, isMoveLegal, isKillCand, isTKillCand,
    betaCut, ttRead, ttStore, curNodes, isTimeout, informCtx,
    mateScore, scoreDiff, qsDelta,
    draftStats,
    finNode, countRepetitions,
    showMyPos, logMes,
    nearmate
) where

import Data.Bits
import Data.Int
import Data.List (nub)
import Control.Monad.State
import Control.Monad.Reader (ask)
-- import Numeric

import Moves.BaseTypes
import Search.AlbetaTypes
import Struct.Struct
import Struct.Context
import Struct.Status
import Hash.TransTab
import Moves.Board
import Moves.BitBoard (less, uBit)
import Eval.BasicEval
import Eval.Eval
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
                       evalst = e
                   }
    where stsc = posEval p e
          p'' = p { staticScore = stsc }

posNewSearch :: MyState -> MyState
posNewSearch p = p { hash = newGener (hash p) }

draftStats :: SStats -> Game ()
draftStats dst = do
    s <- get
    put s { mstats = addStats (mstats s) dst }

genMoves :: Int -> Game ([Move], [Move])
genMoves d = do
    p <- getPos
    if inCheck p
       then return (genMoveFCheck p, [])
       else do
            h <- gets hist
            let l0 = genMoveCast p
                l1 = genMovePromo p
                (l2w, l2l) = genMoveCaptWL p
                l3 = histSortMoves d h $ genMoveNCapt p
            -- Loosing captures after non-captures
            return (l1 ++ l2w, l0 ++ l3 ++ l2l)

-- Generate only tactical moves, i.e. promotions & captures
-- Needed only in QS, when we know we are not in check
-- In the frontier nodes (i.e. first level QS) we generate
-- also checking quiet moves with non-negative SEE
genTactMoves :: Bool -> Game [Move]
genTactMoves front = do
    p <- getPos
    let l1  = genMovePromo p
        l2w = fst $ genMoveCaptWL p
        l3  = genMoveNCaptToCheck p
    if front
       then return $ l1 ++ l2w ++ l3
       else return $ l1 ++ l2w

-- Generate only escape moves: needed only in QS when we know we have to escape
genEscapeMoves :: Game [Move]
genEscapeMoves = do
    p <- getPos
    return $ genMoveFCheck p

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
                   return $ Exten 0 False False

-- Move from a node to a descendent - the normal search version
doMove :: Move -> Game DoResult
doMove m = do
    s <- get
    let (pc:_) = stack s	-- we never saw an empty stack error until now
        -- Moving a non-existent piece?
        il  = occup pc `uBitClear` fromSquare m
        -- Capturing one king?
        kc  = kings pc `uBitSet` toSquare m
        sts = posEval p (evalst s)
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
                   remis <- checkRemisRules p
                   if remis
                      then return $ Final 0
                      else if captOrPromo pc m
                              then return $! Exten (exten pc p) True True
                              else return $! Exten (exten pc p) False (noLMR pc m)

-- Move from a node to a descendent - the QS search version
-- Here we do only a restricted check for illegal moves
-- It does not check for remis, so it can't return Final
-- It does not check for extensions a.s.o.
doQSMove :: Move -> Game Bool
doQSMove m = do
    s <- get
    let (pc:_) = stack s	-- we never saw an empty stack error until now
        sts = posEval p (evalst s)
        p   = doFromToMove m pc { staticScore = sts }
    if not $ checkOk p
       then return False
       else do
           put s { stack = p : stack s }
           return True

doNullMove :: Game ()
doNullMove = do
    s <- get
    let (pc:_) = stack s	-- we never saw an empty stack error until now
        sts = posEval p (evalst s)
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

-- If we have a few repetitions in the last moves, then we will reduce moves to go
-- so the time management can allocate more time for next moves
countRepetitions :: MyState -> Int
countRepetitions s = length f6 - uniq
    where uniq = length $ nub $ map zobkey f6
          f6   = take 6 $ stack s

{-# INLINE undoMove #-}
undoMove :: Game ()
undoMove = modify $ \s -> s { stack = tail $ stack s }

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

{--
-- Parameters for pawn threats
validThreat, majorThreat :: Bool
validThreat = True	-- pawn threat must be valid?
majorThreat = False	-- pawn threat: only majors?

pawnThreat :: MyPos -> MyPos -> Bool
pawnThreat p1 p2
    | npa .&. fig == 0 = False
    | validThreat      = validPawnThreat p1 p2
    | otherwise        = True
    where !npa = yoPAttacs p2 `less` myPAttacs p1
          !fig | majorThreat = me p2 .&. (queens p2 .|. rooks p2)
               | otherwise   = me p2 `less` pawns p2

-- Valid pawn threat is when the pawn is defended or not attacked
validPawnThreat :: MyPos -> MyPos -> Bool
validPawnThreat p1 p2 = mvpaw .&. yoAttacs p2 /= 0 || mvpaw .&. myAttacs p2 == 0
    where !mvpaw = yo p2 `less` me p1
--}

-- Tactical positions will be searched complete in quiescent search
-- Currently only when in check
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
        s <- get
        let (p:_) = stack s	-- we never saw an empty stack error until now
            fen = posToFen p
        logMes $ str ++ " Score: " ++ show (staticScore p) ++ " Fen: " ++ fen

-- {-# INLINE qsDelta #-}
qsDelta :: Int -> Game Bool
qsDelta !a = do
    p <- getPos
    if matPiece White Bishop >= a
       then return False
       else if matPiece White Queen < a
               then return True
               else do
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
    | otherwise            = not $ moveChecks p m

-- Score difference obtained by last move, from POV of the moving part
-- It considers the fact that static score is for the part which has to move
scoreDiff :: Game Int
scoreDiff = do
    s <- get
    case stack s of
        (p1:p2:_) -> return $! negate (staticScore p1 + staticScore p2)
        _         -> return 0

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
