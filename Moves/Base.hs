{-# LANGUAGE TypeSynonymInstances,
             MultiParamTypeClasses,
             BangPatterns,
             RankNTypes, UndecidableInstances,
             FlexibleInstances
             #-}

module Moves.Base (
    posToState, getPos, posNewSearch,
    doRealMove, doMove, undoMove, genMoves, genTactMoves, canPruneMove,
    useHash, pruneVals,
    staticVal, materVal, tacticalPos, isMoveLegal, isKillCand, isTKillCand, okInSequence,
    betaCut, doNullMove, ttRead, ttStore, curNodes, chooseMove, isTimeout, informCtx,
    mateScore, scoreDiff,
    finNode,
    showMyPos, logMes,
    nearmate
) where

import Data.Bits
import Data.List
import Control.Monad.State
import Control.Monad.Reader (ask)
import Data.Ord (comparing)
-- import Numeric
import System.Random

import Moves.BaseTypes
import Search.AlbetaTypes
import Struct.Struct
import Struct.Context
import Struct.Status
import Hash.TransTab
import Moves.Board
import Eval.Eval
import Moves.ShowMe
import Moves.History
import Moves.Notation

{-# INLINE nearmate #-}
nearmate :: Int -> Bool
nearmate i = i >= mateScore - 255 || i <= -mateScore + 255

-- Some options and parameters:
-- debug, useHash :: Bool
-- debug       = False
useHash :: Bool
useHash = True

scoreDiffEqual, printEvalInt :: Int
scoreDiffEqual = 4 -- under this score difference moves are considered to be equal (choose random)
printEvalInt   = 2 `shiftL` 12 - 1	-- if /= 0: print eval info every so many nodes

mateScore :: Int
mateScore = 20000

curNodes :: Game Int
{-# INLINE curNodes #-}
curNodes = gets (nodes . stats)

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
                       stats = stats0,
                       evalst = e
                   }
    where (stsc, ksd) = posEval p e
          p'' = p { staticScore = stsc, kSafeDiff = ksd }
          stats0 = Stats { nodes = 0, maxmvs = 0 }

posNewSearch :: MyState -> MyState
posNewSearch p = p { hash = newGener (hash p) }

-- Loosing captures after non-captures?
loosingLast :: Bool
loosingLast = True

genMoves :: Game ([Move], [Move])
genMoves = do
    p <- getPos
    if isCheck p $ moving p
       then return (genMoveFCheck p, [])
       else do
            let l0 = genMoveCast p
                l1 = genMovePromo p
                (l2w, l2l) = genMoveCaptWL p
                l3' = genMoveNCapt p
            l3 <- sortMovesFromHist l3'
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
           -- return $ checkGenMoves p mvs
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

sortMovesFromHist :: [Move] -> Game [Move]
sortMovesFromHist mvs = do
    s <- get
    mvsc <- liftIO $ mapM (\m -> valHist (hist s) m) mvs
    let (posi, zero) = partition ((/=0) . snd) $ zip mvs mvsc
    -- return $! map fst $ sortBy (comparing snd) posi ++ zero
    return $ map fst $ sortBy (comparing snd) posi ++ zero	-- does it differ?

-- massert :: String -> Game Bool -> Game ()
-- massert s mb = do
--     b <- mb
--     if b then return () else error s

{-# INLINE statNodes #-}
statNodes :: Game ()
statNodes = do
    s <- get
    let st = stats s
        !n = nodes st + 1
        !s1 = s { stats = st { nodes = n } }
    put s1

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
    -- logMes $ "** doMove " ++ show m
    statNodes   -- when counting all visited nodes
    s  <- get
    let (pc:_) = stack s	-- we never saw an empty stack error until now
        -- Moving a non-existent piece?
        il = occup pc `uBitClear` fromSquare m
        -- Capturing one king?
        kc = kings pc `uBitSet` toSquare m
        p' = doFromToMove m pc
        cok = checkOk p'
    if (il || kc)
       then do
           logMes $ "Illegal move or position: move = " ++ show m
                    ++ ", il = " ++ show il ++ ", kc = " ++ show kc ++ "\n"
           logMes $ "Illegal position (after the move):\n" ++ showMyPos p'
           logMes $ "Stack:\n" ++ showStack 3 (stack s)
           -- After an illegal result there must be no undo!
           return Illegal
       else if not cok
               then return Illegal
               else do
                   -- bigCheckPos "doMove" pc (Just m) p'
                   let (sts, ksd) = posEval p' (evalst s)
                       p = p' { staticScore = sts, kSafeDiff = ksd }
                   put s { stack = p : stack s }
                   remis <- if qs then return False else checkRemisRules p'
                   if remis
                      then return $ Final 0
                      else do
                          let dext = if inCheck p then 1 else 0
                          return $! Exten dext $ moveIsCaptPromo pc m

doNullMove :: Game ()
doNullMove = do
    -- logMes "** doMove null"
    s <- get
    let p0 = if null (stack s) then error "doNullMove" else head $ stack s
        p' = reverseMoving p0
        (sts, ksd) = posEval p' (evalst s)
        p = p' { staticScore = sts, kSafeDiff = ksd }
    -- bigCheckPos "doNullMove" p0 Nothing p'
    put s { stack = p : stack s }

{-- Activae when used:
bigCheckPos :: String -> MyPos -> Maybe Move -> MyPos -> Game ()
bigCheckPos loc pin mmv pou = do
    let fpou = posToFen pou
        p    = posFromFen fpou
    -- when (pou { staticScore = 0, staticFeats = [] } /= p { staticScore = 0, staticFeats = [] }) $ do
    when (zobkey pou /= zobkey p) $ do
        let fpin = posToFen pin
        logMes $ "Wrong pos in " ++ loc
        logMes $ "Input:  " ++ fpin
        case mmv of
            Just mv -> logMes $ "Move:   " ++ toNiceNotation pin mv
            Nothing -> logMes $ "Move:   null move"
        logMes $ "Output: " ++ fpou
        logMes $ "Outget: " ++ fpou
        logMes $ "MyPos pou: " ++ show pou
        logMes $ "MyPos p:   " ++ show p
--}

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
tacticalPos :: Game Bool
tacticalPos = do
    t <- getPos
    return $! check t /= 0

{-# INLINE isMoveLegal #-}
isMoveLegal :: Move -> Game Bool
isMoveLegal m = do
    t <- getPos
    return $! legalMove t m

-- Why not just like isTKillCand?
-- Also: if not normal, it is useless, as now it is not recognized as legal...
isKillCand :: Move -> Move -> Game Bool
isKillCand mm ym
    | toSquare mm == toSquare ym = return False
    | otherwise = do
        t <- getPos
        return $! not $ moveIsCapture t ym

-- If not normal, it is useless, as now it is not recognized as legal...
isTKillCand :: Move -> Game Bool
isTKillCand mm = do
    t <- getPos
    return $! not $ moveIsCapture t mm

okInSequence :: Move -> Move -> Game Bool
okInSequence m1 m2 = do
    t <- getPos
    return $! alternateMoves t m1 m2

-- Static evaluation function
-- This does not detect mate or stale mate, it only returns the calculated
-- static score from a position which has already to be valid
-- Mate/stale mate has to be detected by search!
{-# INLINE staticVal #-}
staticVal :: Game Int
staticVal = staticScore <$> getPos

{-# INLINE pruneVals #-}
pruneVals :: Game (Int, Int)
pruneVals = ((,) <$> staticScore <*> kSafeDiff) <$> getPos

{-# INLINE finNode #-}
finNode :: String -> Bool -> Game ()
finNode str force = do
    s <- get
    when (printEvalInt /= 0 && (force || nodes (stats s) .&. printEvalInt == 0)) $ do
        let (p:_) = stack s	-- we never saw an empty stack error until now
            fen = posToFen p
            -- mv = case tail $ words fen of
            --          mv':_ -> mv'
            --          _     -> error "Wrong fen in finNode"
        logMes $ str ++ " Fen: " ++ fen
        -- logMes $ "Eval info " ++ mv ++ ":"
        --               ++ concatMap (\(n, v) -> " " ++ n ++ "=" ++ show v)
        --                            (("score", staticScore p) : weightPairs (staticFeats p))

materVal :: Game Int
materVal = do
    t <- getPos
    let !m = mater t
    return $! case moving t of
                   White -> m
                   _     -> -m

{-# INLINE ttRead #-}
ttRead :: Game (Int, Int, Int, Move, Int)
ttRead = if not useHash then return empRez else do
    -- when debug $ lift $ ctxLog "Debug" $ "--> ttRead "
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
ttStore :: Int -> Int -> Int -> Move -> Int -> Game ()
ttStore !deep !tp !sc !bestm !nds = if not useHash then return () else do
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
canPruneMove :: Move -> Game Bool
canPruneMove m
    | not (moveIsNormal m) = return False
    | otherwise = do
        p <- getPos
        if moveIsCapture p m
           then return False
           else return $! not $ moveChecks p m

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
