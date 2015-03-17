{-# LANGUAGE TypeSynonymInstances,
             MultiParamTypeClasses,
             BangPatterns,
             PatternGuards,
             RankNTypes, UndecidableInstances,
             FlexibleInstances
             #-}

module Moves.Base (
    posToState, getPos, posNewSearch,
    doMove, undoMove, genMoves, genTactMoves,
    useHash,
    staticVal, materVal, tacticalPos, isMoveLegal, isKillCand, okInSequence,
    betaCut, doNullMove, ttRead, ttStore, curNodes, chooseMove, isTimeout, informCtx,
    mateScore,
    finNode,
    showMyPos, logMes,
    nearmate	-- , special
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

depthForMovesSortPv, depthForMovesSort, scoreDiffEqual, printEvalInt :: Int
depthForMovesSortPv = 1	-- use history for sorting moves when pv or cut nodes
depthForMovesSort   = 1	-- use history for sorting moves
scoreDiffEqual      = 4 -- under this score difference moves are considered to be equal (choose random)
printEvalInt        = 2 `shiftL` 12 - 1	-- if /= 0: print eval info every so many nodes

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
    where (stsc, feats) = evalState (posEval p) e
          p'' = p { staticScore = stsc, staticFeats = feats }
          stats0 = Stats { nodes = 0, maxmvs = 0 }

posNewSearch :: MyState -> MyState
posNewSearch p = p { hash = newGener (hash p) }

-- debugGen :: Bool
-- debugGen = False

loosingLast :: Bool
loosingLast = False

genMoves :: Int -> Int -> Bool -> Game ([Move], [Move])
genMoves depth _ pv = do	-- absdp not used
    p <- getPos
    let lc = genMoveFCheck p
    if inCheck p
       then return (lc, [])
       else do
            let l0  = genMoveCast p
                l1  = genMoveTransf p
                (l2w, l2l) = genMoveCaptWL p
                l3' = genMoveNCapt p
            l3 <- if pv && depth >= depthForMovesSortPv
                     || not pv && depth >= depthForMovesSort
                     -- then sortMovesFromHash l3'
                     then sortMovesFromHist l3'
                     else return l3'
            return $! if loosingLast
                         -- then (checkGenMoves p $ l1 ++ l2w, checkGenMoves p $ l0 ++ l3 ++ l2l)
                         -- else (checkGenMoves p $ l1 ++ l2w ++ l2l, checkGenMoves p $ l0 ++ l3)
                         then (l1 ++ l2w, l0 ++ l3 ++ l2l)
                         else (l1 ++ l2w ++ l2l, l0 ++ l3)

-- Generate only tactical moves, i.e. promotions, captures & check escapes
genTactMoves :: Game [Move]
genTactMoves = do
    p <- getPos
    let l1 = genMoveTransf p
        (l2w, _) = genMoveCaptWL p
        lc = genMoveFCheck p
        !mvs | inCheck p = lc
             | otherwise = l1 ++ l2w
    -- return $ checkGenMoves p mvs
    return mvs

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
    return $! map fst $ sortBy (comparing snd) posi ++ zero

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

-- move from a node to a descendent
doMove :: Bool -> Move -> Bool -> Game DoResult
doMove real m qs = do
    -- logMes $ "** doMove " ++ show m
    statNodes   -- when counting all visited nodes
    s <- get
    -- let pc = if null (stack s) then error "doMove" else head $ stack s
    let (pc:_) = stack s	-- we never saw an empty stack error until now
        !m1 = if real then checkCastle (checkEnPas m pc) pc else m
        -- Moving a non-existent piece?
        il = occup pc `uBitClear` fromSquare m1
        -- Capturing one king?
        kc = kings pc `uBitSet` toSquare m1
        p' = doFromToMove m1 pc
        kok = kingsOk p'	-- actually not needed if kc is always checked...
    -- If the move is real and one of those conditions occur,
    -- then we are really in trouble...
    if not real && (il || kc || not kok)
        then do
            logMes $ "Illegal move or position: move = " ++ show m
                     ++ ", il = " ++ show il ++ ", kc = " ++ show kc ++ "\n"
            unless kok $ logMes $ "Illegal position (after the move):\n" ++ showMyPos p'
            logMes $ "Stack:\n" ++ showStack 3 (stack s)
            -- After an illegal result there must be no undo!
            return dorIllegal
        else if not (checkOk p')
                then return dorIllegal
                else do
                    -- bigCheckPos "doMove" pc (Just m) p'
                    let (sts, feats) | real      = (0, [])
                                     | otherwise = evalState (posEval p') (evalst s)
                        p = p' { staticScore = sts, staticFeats = feats }
                    put s { stack = p : stack s }
                    if qs
                       then return dorNormal	-- in QS we dont need extensions & co
                       else if checkRemisRules p' (stack s)
                               then return dorRepet
                               else do
                                   let !inc = inCheck pc
                                       !spc = moveIsSpecial pc m1
                                       !chk = inCheck p
                                       -- we still have to set the treat!
                                   return $ setDoRExten chk $ setDoRInCheck inc
                                          $ setDoRChecking chk $ setDoRSpecial spc dorNormal

checkRemisRules :: MyPos -> [MyPos] -> Bool
checkRemisRules p stk
    | remis50Moves p   = True
    | (_:_:_) <- equal = True
    | otherwise        = False
    where revers = map zobkey $ takeWhile isReversible stk
          equal  = filter (== zobkey p) revers	-- if keys are equal, pos is repeated

doNullMove :: Game ()
doNullMove = do
    -- logMes "** doMove null"
    s <- get
    let !p0 = if null (stack s) then error "doNullMove" else head $ stack s
        !p' = reverseMoving p0
        (!sts, feats) = evalState (posEval p') (evalst s)
        !p = p' { staticScore = sts, staticFeats = feats }
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

{-# INLINE undoMove #-}
undoMove :: Game ()
undoMove = do
    -- logMes "** undoMove"
    modify $ \s -> s { stack = tail $ stack s }

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

-- Should be: not $ moveIsSpecial ...
isKillCand :: Move -> Move -> Game Bool
isKillCand mm ym
    | toSquare mm == toSquare ym = return False
    | otherwise = do
        t <- getPos
        return $! not $ moveIsCapture t ym

okInSequence :: Move -> Move -> Game Bool
okInSequence m1 m2 = do
    t <- getPos
    return $! alternateMoves t m1 m2

-- Static evaluation function
{-# INLINE staticVal #-}
staticVal :: Game Int
staticVal = getPos >>= return . staticScore
{--
    s <- get
    t <- getPos
    let !c = moving t
        !stSc = if not (checkOk t && kingsOk t)
                   then error $ "Wrong position, pos stack:\n" ++ concatMap showMyPos (stack s)
                   else staticScore t
        !stSc1 | check t /= 0 = if hasMoves t c then stSc else -mateScore
               | otherwise    = stSc
    return stSc1
--}

{-# INLINE finNode #-}
finNode :: String -> Bool -> Game ()
finNode str force = do
    s <- get
    when (printEvalInt /= 0 && (force || nodes (stats s) .&. printEvalInt == 0)) $ do
        let (p:_) = stack s	-- we never saw an empty stack error until now
            fen = posToFen p
            mv = case tail $ words fen of
                     mv':_ -> mv'
                     _     -> error "Wrong fen in finNode"
        logMes $ str ++ " Fen: " ++ fen
        logMes $ "Eval info " ++ mv ++ ":"
                      ++ concatMap (\(n, v) -> " " ++ n ++ "=" ++ show v)
                                   (("score", staticScore p) : weightPairs (staticFeats p))

materVal :: Game Int
materVal = do
    t <- getPos
    let !m = mater t
    return $! case moving t of
                   White -> m
                   _     -> -m

-- quiet :: MyPos -> Bool
-- quiet p = at .&. ta == 0
--     where (!at, !ta) = if moving p == White then (whAttacs p, black p) else (blAttacs p, white p)

-- Fixme!! We have big problems with hash store/retrieval: many wrong scores (and perhaps hash moves)
-- come from there!!

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
    -- when (sc `mod` 4 /= 0 && tp == 2) $ liftIO $ do
    --     putStrLn $ "info string In ttStore: tp = " ++ show tp ++ " sc = " ++ show sc
    --         ++ " best = " ++ show best ++ " nodes = " ++ show nodes
        -- putStrLn $ "info string score in position: " ++ show (staticScore p)
    -- We use the type: 0 - upper limit, 1 - lower limit, 2 - exact score
    liftIO $ writeCache (hash s) (zobkey p) deep tp sc bestm nds
    -- when debug $ lift $ ctxLog "Debug" $ "*** ttStore (deep/tp/sc/mv) " ++ show deep
    --      ++ " / " ++ show tp ++ " / " ++ show sc ++ " / " ++ show best
    --      ++ " status: " ++ show st ++ " (" ++ show (zobkey p) ++ ")"
    -- return ()

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
moveIsSpecial :: MyPos -> Move -> Bool
moveIsSpecial p m
    | moveIsPromo m || moveIsEnPas m = True
    | otherwise                      = moveIsCapture p m

{--
showChoose :: [] -> Game ()
showChoose pvs = do
    mapM_ (\(i, (s, pv)) -> lift $ ctxLog "Info"
                                 $ "choose pv " ++ show i ++ " score " ++ show s ++ ": " ++ show pv)
                 $ zip [1..] pvs
    return $ if null pvs then error "showChoose" else head pvs
--}

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
