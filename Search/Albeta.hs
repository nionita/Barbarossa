{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE CPP #-}

module Search.Albeta (
    alphaBeta, logmes
) where

import Control.Monad
import Control.Monad.State hiding (gets, modify)
import Data.Bits ((.&.))
import Data.List (delete, sortBy)
import Data.Ord (comparing)
import Data.Array.Base
-- import Data.Array.Unboxed
import Data.Maybe (fromMaybe)
-- import Control.Applicative ((<$>))

import Search.CStateMonad
import Search.AlbetaTypes
import Struct.Struct
import Moves.BaseTypes
import Moves.Base

-- debug :: Bool
-- debug = False

-- To generate info for tree vizualization
viztree :: Bool
#ifdef VIZTREE
viztree = True
#else
viztree = False
#endif

-- Parameter for aspiration
useAspirWin :: Bool
useAspirWin = False
-- aspIncr :: UArray Int Int
-- aspIncr = array (1, 3) [ (1, 128), (2, 32), (3, 8) ]
-- aspTries = 3
-- Aspiration parameter optimization - 300 games:
-- First digit: tries, second: version (see below)
-- a21 = 64, 8		-> elo  -8 +- 59
-- a22 = 64, 16		-> elo  -2 +- 58
-- a23 = 128, 16	-> elo -32 +- 60
-- a31 = 64, 16, 4	-> elo -10 +- 57
-- a32 = 128, 32, 8	-> elo +53 +- 60 --> this is it
-- a33 = 100, 20, 4	-> elo   0 +- 58

-- Some fix search parameter
scoreGrain, depthForCM, minToStore, minToRetr, maxDepthExt, negHistMNo :: Int
useNegHist, useTTinPv :: Bool
scoreGrain  = 4	-- score granularity
depthForCM  = 7 -- from this depth inform current move
minToStore  = 1 -- minimum remaining depth to store the position in hash
minToRetr   = 1 -- minimum remaining depth to retrieve
maxDepthExt = 3 -- maximum depth extension
useNegHist  = False	-- when not cutting - negative history
negHistMNo  = 1		-- how many moves get negative history
useTTinPv   = False	-- retrieve from TT in PV?

-- Parameters for late move reduction:
lmrActive :: Bool
lmrActive   = True

lmrMaxDepth, lmrMaxWidth :: Int
lmrMaxDepth = 15
lmrMaxWidth = 63
lmrPv, lmrRest :: Double
lmrPv     = 13
lmrRest   = 8
-- LMR parameter optimisation (lmrPv, lmrRest):
-- lm1 = 2, 1	-> elo -127 +- 58
-- lm2 = 3, 2	-> elo  -14 +- 52
-- lm3 = 5, 3	-> elo   17 +- 55
-- lm4 = 8, 5	-> elo   32 +- 53
-- lm5 = 13, 8	-> elo   92 +- 54 --> this is it
lmrReducePv, lmrReduceArr :: UArray (Int, Int) Int
lmrReducePv  = array ((1, 1), (lmrMaxDepth, lmrMaxWidth))
    [((i, j), ceiling $ logrd i j lmrPv) | i <- [1..lmrMaxDepth], j <- [1..lmrMaxWidth]]
lmrReduceArr = array ((1, 1), (lmrMaxDepth, lmrMaxWidth))
    [((i, j), ceiling $ logrd i j lmrRest) | i <- [1..lmrMaxDepth], j <- [1..lmrMaxWidth]]

logrd :: Int -> Int -> Double -> Double
logrd i j f = log (fromIntegral i) * log (fromIntegral j) / f

-- Parameters for futility pruning:
futilActive :: Bool
futilActive = True

maxFutilDepth :: Int
maxFutilDepth = 3

-- This is a linear formula for futility margin
-- Should apply from 1 to maxFutilDepth (checked elsewehere)
-- Optimisation for futilMs:
-- A:  25
-- B:  50
-- C:  75 == n1ns
-- D: 100
-- E: 125
futilMs, futilMv :: Int
futilMs = 275	-- margin for depth 1
futilMv = 150	-- suplementary margin for every further depth
futilMargins :: Int -> Int
futilMargins d = futilMs - futilMv + d*futilMv

-- Parameters for quiescent search:
qsBetaCut, qsDeltaCut :: Bool
qsBetaCut  = True	-- use beta cut in QS?
qsDeltaCut = True	-- use delta prune in QS?
qsMaxChess :: Int
qsMaxChess = 2		-- max number of chess for a quiet search path

-- Parameters for null move pruning
nulActivate :: Bool
nulActivate = True		-- activate null move reduction
nulRedux, nulMoves :: Int
nulRedux    = 2 -- depth reduction for null move
nulMoves    = 2	-- how many null moves in sequence are allowed (one or two)
nulMargin, nulSubmrg, nulTrig :: Int
nulMargin   = 1		-- margin to search the null move (over beta) (in scoreGrain units!)
nulSubmrg   = 2		-- improved margin (in scoreGrain units!)
nulTrig     = -15	-- static margin to beta, to trigger null move (in scoreGrain units!)
nulSubAct :: Bool
nulSubAct   = True

-- Parameters for internal iterative deepening
useIID :: Bool
useIID      = False

minIIDPV, minIIDCut, maxIIDDepth :: Int
minIIDPV    = 5
minIIDCut   = 7
maxIIDDepth = 3

iidNewDepth :: Int -> Int
iidNewDepth = subtract 1
-- iidNewDepth = `shiftR` 1	-- i.e. div 2

-- Parameter for quiescenst search
inEndlessCheck, qsDelta :: Int
inEndlessCheck = -scoreGrain	-- there is a risk to be left in check
qsDelta     = 1100

type Search a = CState PVState Game a

alpha0, beta0 :: Int
alpha0 = minBound + 2000
beta0  = maxBound - 2000

data Pvsl = Pvsl {
        pvPath :: Path,		-- pv path
        pvNodes :: !Int,	-- number of nodes in the current search
        pvGood  :: !Bool	-- beta cut or alpha improvement
    } deriving Show

data Killer = NoKiller | OneKiller !Move | TwoKillers !Move !Move
                  deriving Show

-- Read only parameters of the search, so that we can change them programatically
data PVReadOnly
    = PVReadOnly {
          draft  :: !Int,	-- root search depth
          albest :: !Bool,	-- always choose the best move (i.e. first)
          timeli :: !Bool,	-- do we have time limit?
          abmili :: !Int	-- abort when after this milisecond
    } deriving Show

data PVState
    = PVState {
          ronly :: !PVReadOnly,	-- read only parameters
          absdp :: !Int,	-- absolute depth (root = 0)
          usedext :: !Int,	-- used extension
          abort :: !Bool,	-- search aborted (time)
          short :: !Bool,	-- for check path (shorter than depth in pv)
          stats :: !SStats	-- search statistics
      } deriving Show

-- This is a state which reflects the status of alpha beta in a node while going through the edges
data NodeState
    = NSt {
          crtnt :: !NodeType,	-- parent node type (actually expected)
          nxtnt :: !NodeType,	-- expected child node type
          forpv :: !Bool,	-- still searching for PV?
          cursc :: !Path,	-- current alpha value (now plus path & depth)
          movno :: !Int,	-- current move number
          pvsl  :: [Pvsl],	-- principal variation list (at root) with node statistics
          killer :: !Killer,	-- the current killer moves
          pvcont :: Seq Move	-- a pv continuation from the previous iteration, if available
      } deriving Show

data SStats = SStats {
        sNodes, sNodesQS, sRetr, sRSuc :: !Int
    } deriving Show

-- The node type is the expected node type of the new (child) node, which
-- is obtained by making one move from the current (parent) node
data NodeType = PVNode | CutNode | AllNode deriving (Eq, Show)

deepNodeType :: NodeType -> NodeType
deepNodeType PVNode  = PVNode
deepNodeType CutNode = AllNode
deepNodeType AllNode = CutNode

nextNodeType :: NodeType -> NodeType
nextNodeType PVNode = CutNode
nextNodeType t      = t

newtype Alt e = Alt { unalt :: [e] } deriving Show
newtype Seq e = Seq { unseq :: [e] } deriving Show

-- firstMove :: Seq Move -> Move
-- firstMove = head . unseq

data Path
    = Path {
         pathScore :: !Int,
         pathDepth :: !Int,
         pathMoves :: Seq Move,
         pathOrig  :: String
      } deriving Show

staleMate, matedPath :: Path
staleMate = Path { pathScore = 0, pathDepth = 0, pathMoves = Seq [], pathOrig = "stale mate" }
matedPath = Path { pathScore = -mateScore, pathDepth = 0, pathMoves = Seq [], pathOrig = "mated" }

-- Making a path from a plain score:
pathFromScore :: String -> Int -> Path
pathFromScore ori s = Path { pathScore = s, pathDepth = 0, pathMoves = Seq [], pathOrig = ori }

-- Add a move to a path:
addToPath :: Move -> Path -> Path
addToPath e p = p { pathDepth = pathDepth p + 1, pathMoves = Seq $ e : unseq (pathMoves p) }

-- Take only the score from a path (to another), rest empty
onlyScore :: Path -> Path
onlyScore p = Path { pathScore = pathScore p, pathDepth = 0, pathMoves = Seq [],
                  pathOrig = "onlyScore from " ++ pathOrig p }

-- Take all from the first path, except the score, which comes from the second (for fail hard)
combinePath :: Path -> Path -> Path
combinePath p1 p2 = p1 { pathScore = pathScore p2,
                         pathOrig = "(" ++ pathOrig p1 ++ ") <+> (" ++ pathOrig p2 ++ ")" }

-- When we negate the path, we empty the moves, so the new path is adequate
-- for the adverse part; then we can use always bestPath to get the path with longer sequence
negatePath :: Path -> Path
negatePath p = p { pathScore = - pathScore p, pathDepth = 0, pathMoves = Seq [],
                   pathOrig = "negatePath (" ++ pathOrig p ++ ")" }

-- This should be used only when scores are equal
-- Then it takes the longer path
bestPath :: Path -> Path -> Path
bestPath a b
    | a == b    = if pathDepth b > pathDepth a then b else a
    | otherwise = error $ "bestPath on unequal scores: " ++ show a ++ " versus " ++ show b

(-:) :: Path -> Int -> Path
p -: s = p { pathScore = pathScore p - s, pathOrig = pathOrig p ++ " <-:> " ++ show s }

pnearmate :: Path -> Bool
pnearmate = nearmate . pathScore

pnextlev :: Path -> Path
pnextlev p = p { pathScore = - pathScore p }

-- If we compare depths when equal scores, then nothing works anymore!
instance Eq Path where
    -- p1 == p2 = pathScore p1 == pathScore p2 && pathDepth p1 == pathDepth p2
    p1 == p2 = pathScore p1 == pathScore p2

instance Ord Path where
    compare = comparing pathScore

instance Bounded Path where
    minBound = Path { pathScore = minBound, pathDepth = 0, pathMoves = Seq [], pathOrig = "" }
    maxBound = Path { pathScore = maxBound, pathDepth = 0, pathMoves = Seq [], pathOrig = "" }

noMove :: Alt Move -> Bool
noMove (Alt es) = null es

nullSeq :: Seq Move -> Bool
nullSeq (Seq es) = null es

emptySeq :: Seq Move
emptySeq = Seq []

tailSeq :: Seq Move -> Seq Move
tailSeq es
    | nullSeq es = emptySeq
    | otherwise  = Seq $ tail $ unseq es

pvsInit :: PVState
pvsInit = PVState { ronly = pvro00, absdp = 0, usedext = 0,
                    abort = False, short = False, stats = stt0 }
nst0 :: NodeState
nst0 = NSt { crtnt = PVNode, nxtnt = PVNode, forpv = True, cursc = pathFromScore "Zero" 0,
             movno = 1, killer = NoKiller, pvsl = [], pvcont = emptySeq }

stt0 :: SStats
stt0 = SStats { sNodes = 0, sNodesQS = 0, sRetr = 0, sRSuc = 0 }

pvro00 :: PVReadOnly
pvro00 = PVReadOnly { draft = 0, albest = False, timeli = False, abmili = 0 }

alphaBeta :: ABControl -> Game (Int, [Move], [Move])
alphaBeta abc = {-# SCC "alphaBeta" #-} do
    let !d = maxdepth abc
        rmvs = Alt $ rootmvs abc
        lpv  = Seq $ lastpv abc
        searchReduced a b = pvRootSearch a      b     d lpv rmvs True
        -- We have lastpath as a parameter here (can change after fail low or high)
        searchFull    lp  = pvRootSearch alpha0 beta0 d lp  rmvs False
        pvro = PVReadOnly { draft = d, albest = best abc,
                            timeli = stoptime abc /= 0, abmili = stoptime abc }
        pvs0 = pvsInit { ronly = pvro } :: PVState
    r <- if useAspirWin
         then case lastscore abc of
             Just sp -> do
                let !alpha1 = sp - window abc
                    !beta1  = sp + window abc
                -- informStr $ "+++ Aspi search with d = " ++ show d
                --                ++ " alpha = " ++ show alpha1
                --                ++ " beta = " ++ show beta1
                -- aspirWin alpha1 beta1 d lpv rmvs aspTries
                r1@((s1, es1, _), pvsf)
                    <- {-# SCC "alphaBetaSearchReduced" #-}
                         runCState (searchReduced alpha1 beta1) pvs0
                if abort pvsf || (s1 > alpha1 && s1 < beta1 && not (nullSeq es1))
                    then return r1
                    else {-# SCC "alphaBetaSearchFullRe" #-} if nullSeq es1
                        then runCState (searchFull lpv) pvs0
                        else runCState (searchFull es1) pvs0
             Nothing -> {-# SCC "alphaBetaSearchFullIn" #-} runCState (searchFull lpv) pvs0
         else {-# SCC "alphaBetaSearchFull" #-} runCState (searchFull lpv) pvs0
    -- when aborted, return the last found good move
    -- we have to trust that abort is never done in draft 1!
    -- if abort (snd r)
    --    then return (fromMaybe 0 $ lastscore abc, lastpv abc, [])
    --    else return $! case fst r of (s, Seq path, Alt rmvs') -> (s, path, rmvs')
    case fst r of
        (s, Seq path, Alt rmvs') -> if null path
           then return (fromMaybe 0 $ lastscore abc, lastpv abc, [])
           else return (s, path, rmvs')

{--
aspirWin :: Int -> Int -> Int -> Seq Move -> Alt Move -> Int -> m (Int, Seq Move, Alt Move)
aspirWin _ _ d lpv rmvs 0 = liftM fst $ runCState (pvRootSearch alpha0 beta0 d lpv rmvs True) pvsInit
aspirWin a b d lpv rmvs t = do
    r@(s, p, ms) <- liftM fst $ runCState (pvRootSearch a b d lpv rmvs True) pvsInit
    if s <= a
       then aspirWin (a - incr) b d lpv rmvs (t-1)
       else if s >= b
            then aspirWin a (b + incr) d p ms (t-1)
            else if nullSeq p
                    then aspirWin (a - incr) (b + incr) d lpv rmvs (t-1)
                    else return r
    where incr = aspIncr!t
--}

-- Root PV Search
pvRootSearch :: Int -> Int -> Int -> Seq Move -> Alt Move -> Bool
             -> Search (Int, Seq Move, Alt Move)
pvRootSearch a b d lastpath rmvs aspir = do
    viztreeNew d
    edges <- if null (unalt rmvs)	-- only when d==1, but we could have lastpath from the previous real move
                then do
                    let a' = pathFromScore "a" a
                        b' = pathFromScore "b" b
                    genAndSort nst0 { pvcont = lastpath } a' b' d -- this will never really do IID as d==1
                else case lastpath of
                         Seq []    -> return rmvs	-- probably this never happens... - check to simplify!
                         Seq (e:_) -> return $ Alt $ e : delete e (unalt rmvs)
    -- pvcont is the pv continuation from the last iteration
    let !nsti = nst0 { cursc = pathFromScore "Alpha" a, pvcont = tailSeq lastpath }	-- strict?
    nstf <- pvLoop (pvInnerRoot (pathFromScore "Beta" b) d) nsti edges
    reportStats
    let failedlow = (a, emptySeq, edges)	-- just to permit aspiration to retry
        sc | d > 1            = pathScore (cursc nstf)
           | null (pvsl nstf) = a
           | otherwise        = pathScore $ pvPath $ head $ pvsl nstf
    -- lift $ informStr $ "pvRootSearch: cursc = " ++ show (cursc nstf) ++ ", a = " ++ show a
    -- Root is pv node, cannot fail low, except when aspiration fails!
    if sc <= a	-- failed low
         then do
           unless aspir $ lift $ informStr "Failed low at root??"
           return failedlow	-- should we alter somehow the moves order in this case?
         else do
            -- lift $ mapM_ (\m -> informStr $ "Root move: " ++ show m) (pvsl nstf)
            albest' <- gets (albest . ronly)
            abrt <- gets abort
            (s, p) <- if sc >= b || abrt
                         then return (sc, unseq $ pathMoves (cursc nstf))
                         else lift $ chooseMove albest'
                                   $ sortBy (comparing fstdesc)
                                   $ map pvslToPair
                                   $ filter pvGood $ pvsl nstf
            when (d < depthForCM) $ informBest s d p
            let (best':_) = p
                allrmvs = if sc >= b then unalt edges else map pvslToMove (pvsl nstf)
                xrmvs = Alt $ best' : delete best' allrmvs	-- best on top
            return (s, Seq p, xrmvs)
    where fstdesc (a', _) = -a'

pvslToPair :: Pvsl -> (Int, [Move])
pvslToPair (Pvsl { pvPath = p }) = (score, pv)
    where pv = unseq $ pathMoves p
          de = pathDepth p
          sc = pathScore p
          score = scoreToExtern sc de

pvslToMove :: Pvsl -> Move
pvslToMove (Pvsl { pvPath = Path { pathMoves = Seq (m:_)}}) = m
pvslToMove _ = undefined	-- just for Wall

-- The internal score is for weird for found mates (always mate)
-- Turn it to nicer score by considering path lenght to mate
scoreToExtern :: Int -> Int -> Int
scoreToExtern sc de
    | nearmate sc = if sc > 0 then sc - de else sc + de
    | otherwise   = sc

legalResult :: DoResult -> Bool
legalResult Illegal = False
legalResult _       = True

-- This is the inner loop of the PV search of the root, executed at root once per possible move
-- See the parameter
-- Returns: ...flag if it was a beta cut and new status
pvInnerRoot :: Path 	-- current beta
            -> Int	-- current search depth
            -> NodeState 	-- node status
            -> Move	-- move to search
            -> Search (Bool, NodeState)
pvInnerRoot b d nst e = do
    abrt <- timeToAbort
    if abrt
       then return (True, nst)
       else do
         old <- get
         when (draft (ronly old) >= depthForCM) $ lift $ informCM e $ movno nst
         pindent $ "-> " ++ show e
         -- lift $ logmes $ "Search root move " ++ show e ++ " a = " ++ show a ++ " b = " ++ show b
         -- do the move
         exd <- {-# SCC "newNode" #-} lift $ doMove False e False
         if legalResult exd
            then do
                nn <- newNode
                viztreeDown nn e
                modify $ \s -> s { absdp = absdp s + 1 }
                s <- case exd of
                         Exten exd' -> pvInnerRootExten b d (special e) exd' nst
                         Final sco  -> do
                             viztreeScore $ "Final: " ++ show sco
                             return $! pathFromScore "Final" (-sco)
                -- undo the move
                lift undoMove
                viztreeUp nn e (pathScore s)
                modify $ \s' -> s' { absdp = absdp old, usedext = usedext old }
                s' <- checkPath nst d "cpl 1" $ addToPath e s
                pindent $ "<- " ++ show e ++ " (" ++ show s' ++ ")"
                checkFailOrPVRoot (stats old) b d e s' nst
            else return (False, nst)

pvInnerRootExten :: Path -> Int -> Bool -> Int -> NodeState -> Search Path
pvInnerRootExten b d spec !exd nst = {-# SCC "pvInnerRootExten" #-} do
    pindent $ "depth = " ++ show d
    old <- get
    exd' <- reserveExtension (usedext old) exd
    let !inPv = nxtnt nst == PVNode
        !a  = cursc nst
        !pvs = forpv nst
        !d1 = d + exd' - 1	-- this is the normal (unreduced) depth for the next search
    d' <- reduceLmr d1 inPv False spec exd (movno nst) pvs
    pindent $ "depth " ++ show d ++ " nt " ++ show (nxtnt nst)
              ++ " exd' = " ++ show exd'
              ++ " mvn " ++ show (movno nst) ++ " next depth " ++ show d'
              ++ " forpv " ++ show (forpv nst)
    let nega = negatePath a
        negb = negatePath b
    if pvs	-- search of principal variation
       then {-# SCC "forpvSearchRoot" #-} do
           viztreeABD (pathScore negb) (pathScore nega) d'
           fmap pnextlev (pvSearch nst negb nega d' nulMoves) >>= checkPath nst d' "cpl 11"
       else {-# SCC "nullWindowRoot" #-} do
           let aGrain = nega -: scoreGrain
           -- no futility pruning for root moves!
           -- Here we expect to fail low
           viztreeABD (pathScore aGrain) (pathScore nega) d'
           !s1 <- fmap pnextlev (pvZeroW nst nega d' nulMoves) >>= checkPath nst d' "cpl 2"
           abrt <- gets abort
           if abrt || s1 <= a -- we failed low as expected
              then return s1
              else {-# SCC "nullWinResRoot" #-} do
                 -- Here we didn't fail low and need re-search
                 -- Two re-searches must be considered: a: full depth, b: full window
                 pindent $ "Research! (" ++ show s1 ++ ")"
                 viztreeReSe
                 -- let pvc = if pathDepth s1 > 0 then pathMoves s1 else (pvcont nst)
                 let pvc = if nullSeq (pathMoves s1) then pvcont nst else pathMoves s1
                 if d' < d1	-- did we search with reduced depth?
                    then do	-- yes: re-search with normal depth
                      viztreeABD (pathScore aGrain) (pathScore nega) d1
                      !s2 <- fmap pnextlev (pvZeroW nst { pvcont = pvc } nega d1 nulMoves)
                             >>= checkPath nst d1 "cpl 12"
                      abrt1 <- gets abort
                      if abrt1 || s2 <= a -- now we failed low as expected
                         then return s2
                         else do
                             -- we must search full window
                             viztreeReSe
                             viztreeABD (pathScore negb) (pathScore nega) d1
                             let nst' = nst { nxtnt = PVNode, forpv = True, pvcont = pvc' }
                                 -- pvc' = if pathDepth s2 > 0 then pathMoves s2 else pvc
                                 pvc' = if nullSeq (pathMoves s2) then pvc else pathMoves s2
                             fmap pnextlev (pvSearch nst' negb nega d1 0) >>= checkPath nst d1 "cpl 12a"
                    else {-# SCC "nullWinResRootSD" #-} do
                        -- Depth was not reduced, so re-search full window
                        viztreeABD (pathScore negb) (pathScore nega) d1
                        let nst' = nst { nxtnt = PVNode, forpv = True, pvcont = pvc }
                        fmap pnextlev (pvSearch nst' negb nega d1 0) >>= checkPath nst d1 "cpl 13"

checkFailOrPVRoot :: SStats -> Path -> Int -> Move -> Path
                  -> NodeState -> Search (Bool, NodeState)
checkFailOrPVRoot xstats b d e s nst = {-# SCC "checkFailOrPVRoot" #-} do
    abrt <- timeToAbort
    if abrt
       then return (True, nst)
       else do
         sst <- get
         let !mn     = movno nst
             !a      = cursc nst
             -- !np     = pathMoves s
             !nodes0 = sNodes xstats + sRSuc xstats
             !nodes1 = sNodes (stats sst) + sRSuc (stats sst)
             !nodes' = nodes1 - nodes0
             pvg    = Pvsl s nodes' True	-- the good
             pvb    = Pvsl s nodes' False	-- the bad
             -- xpvslg = insertToPvs d pvg (pvsl nst)	-- the good
             -- xpvslb = insertToPvs d pvb (pvsl nst)	-- the bad
             de = max d $ pathDepth s
         if d == 1	-- FIXME: for draft 1 we search all root moves exact (but it seems this does not work!)
            then {-# SCC "allExactRoot" #-} do
                 let typ = 2
                 when (de >= minToStore) $ lift $ {-# SCC "hashStore" #-} ttStore de typ (pathScore s) e nodes'
                 let nst1 = if s > a	-- we should probably even go with PVNode for all root moves here
                               -- then nst { cursc = s, nxtnt = nextNodeType (nxtnt nst), forpv = False }
                               then nst { nxtnt = nextNodeType (nxtnt nst) }
                               else nst
                 xpvslg <- insertToPvs d pvg (pvsl nst)	-- the good
                 return (False, nst1 {movno = mn + 1, pvsl = xpvslg, pvcont = emptySeq})
            else if s <= a
                    then {-# SCC "scoreWorseAtRoot" #-}	-- do	-- failed low
                      -- when in a cut node and the move dissapointed - negative history
                      -- when (useNegHist && forpv nst && a == b - 1 && mn <= negHistMNo) -- Check this!
                      --      $ lift $ betaCut False d (absdp sst) e
                      if forpv nst
                         then return (True, nst { cursc = s })	-- i.e we failed low in aspiration with 1st move
                         else do				-- is (above) this still ok so?
                           kill1 <- newKiller d s nst
                           xpvslb <- insertToPvs d pvb (pvsl nst)	-- the bad
                           -- should we set here cursc on combinePath s a if s == a, so that we have always some sequence?
                           -- And why we don't change the node types?
                           let nst1 = nst { movno = mn + 1, pvsl = xpvslb, killer = kill1, pvcont = emptySeq }
                           return (False, nst1)
                    else if s >= b
                      then {-# SCC "scoreBetaCutRoot" #-} do
                        -- what when a root move fails high? We are in aspiration
                        let typ = 1	-- best move is e and is beta cut (score is lower limit)
                        when (de >= minToStore) $ lift $ {-# SCC "hashStore" #-} ttStore de typ (pathScore b) e nodes'
                        lift $ betaCut True d (absdp sst) e
                        xpvslg <- insertToPvs d pvg (pvsl nst)	-- the good
                        !csc <- checkPath nst d "cpl 3" $ if s > b then combinePath s b else bestPath s b
                        pindent $ "beta cut: " ++ show csc
                        let nst1 = nst { cursc = csc, pvsl = xpvslg, pvcont = emptySeq }
                        -- lift $ logmes $ "Root move " ++ show e ++ " failed high: " ++ show s
                        -- lift $ informStr $ "Cut (" ++ show b ++ "): " ++ show np
                        return (True, nst1)
                      else {-# SCC "scoreBetterAtRoot" #-} do	-- means: > a && < b
                        let sc = pathScore s
                            pa = unseq $ pathMoves s
                            -- le = pathDepth s
                        informBest (scoreToExtern sc de) (draft $ ronly sst) pa
                        let typ = 2	-- best move so far (score is exact)
                        when (de >= minToStore) $ lift $ {-# SCC "hashStore" #-} ttStore de typ sc e nodes'
                        xpvslg <- insertToPvs d pvg (pvsl nst)	-- the good
                        let nst1 = nst { cursc = s, nxtnt = nextNodeType (nxtnt nst),
                                         forpv = False, movno = mn + 1,
                                         pvsl = xpvslg, pvcont = emptySeq }
                        -- lift $ logmes $ "Root move " ++ show e ++ " improves alpha: " ++ show s
                        -- lift $ informStr $ "Better (" ++ show s ++ "):" ++ show np
                        return (False, nst1)

-- FIXME! This can be pure
insertToPvs :: Int -> Pvsl -> [Pvsl] -> Search [Pvsl]
insertToPvs _ p [] = return [p]
insertToPvs d p ps@(q:qs)
    | d == 1 && (betters || equals) = return $ p : ps
    | pmate && not qmate            = return $ p : ps
    | not pmate && qmate            = do ir <- insertToPvs d p qs
                                         return $ q : ir
    | pmate && betters              = return $ p : ps
    | bettern || equaln && betters  = return $ p : ps
    | otherwise                    = do ir <- insertToPvs d p qs
                                        return $ q : ir
    where betters = pvPath p >  pvPath q
          equals  = pvPath p == pvPath q
          equaln  = pvNodes p == pvNodes q
          bettern = pvNodes p > pvNodes q
          pmate   = pnearmate $ pvPath p
          qmate   = pnearmate $ pvPath q

{-# INLINE mustQSearch #-}
mustQSearch :: Int -> Int -> Search (Int, Int)
mustQSearch !a !b = do
    nodes0 <- gets (sNodes . stats)
    v <- pvQSearch a b 0
    nodes1 <- gets (sNodes . stats)
    let deltan = nodes1 - nodes0
    return (v, deltan)

-- PV Search
pvSearch :: NodeState -> Path -> Path -> Int -> Int -> Search Path
pvSearch _ !a !b !d _ | d <= 0 = do
    (v, ns) <- if minToRetr == 0
                  then do
                    (hdeep, tp, hscore, _, nodes')
                        <- {-# SCC "hashRetrieveScore" #-} reTrieve >> lift ttRead
                    let sca = pathScore a
                    if hdeep >= 0 && (tp == 2 || tp == 1 && hscore >  sca || tp == 0 && hscore <= sca)
                       then {-# SCC "hashRetrieveScoreOk" #-} reSucc nodes' >> return (hscore, 0)
                       else mustQSearch (pathScore a) (pathScore b)
                  else mustQSearch (pathScore a) (pathScore b)
    when (minToStore == 0 && ns > 0)
        $ lift $ {-# SCC "hashStore" #-} ttStore 0 2 v (Move 0) ns
    let !esc = pathFromScore ("pvQSearch 1:" ++ show v) v
    pindent $ "<> " ++ show esc
    return esc
pvSearch nst !a !b !d lastnull = do
    pindent $ "=> " ++ show a ++ ", " ++ show b
    nmhigh <- nullEdgeFailsHigh nst b d lastnull
    abrt <- gets abort
    if abrt || nmhigh
       then do
         let !s = onlyScore b
         pindent $ "<= " ++ show s
         viztreeScore $ "nmhigh: " ++ show (pathScore s)
         return s
       else do
         edges <- genAndSort nst a b d
         if noMove edges
            then do
              v <- lift staticVal
              viztreeScore $ "noMove: " ++ show v
              let !s = pathFromScore ("static: " ++ show v) v
              pindent $ "<= " ++ show s
              return s	-- shouldn't we ttStore this?
            else do
              nodes0 <- gets (sNodes . stats)
              -- futility pruning?
              prune <- if not futilActive || nxtnt nst == PVNode
                          then return False
                          else isPruneFutil d a
              -- Loop thru the moves
              let !nsti = nst0 { crtnt = nxtnt nst, nxtnt = deepNodeType (nxtnt nst),
                                 cursc = a, pvcont = tailSeq (pvcont nst) }
              nstf <- pvSLoop b d prune nsti edges
              let s = cursc nstf
              pindent $ "<= " ++ show s
              -- After pvSLoop ... we expect always that s >= a - this must be checked if it is so
              -- then it makes sense below to take bestPath when failed low (s == a)
              abrt' <- gets abort
              if abrt' || s > a
                 then checkPath nst d "cpl 6b" s
                 else do
                     -- here we failed low
                     let de = max d $ pathDepth s
                         es = unalt edges
                     when (de >= minToStore) $ do
                         nodes1 <- gets (sNodes . stats)
                         let typ = 0
                             !deltan = nodes1 - nodes0
                         -- store as upper score, and as move, the first one generated
                         lift $ {-# SCC "hashStore" #-}
                                ttStore de typ (pathScore a) (head es) deltan	-- should be d or de?
                     if movno nstf > 1
                         then checkPath nst d "cpl 6a" $! bestPath s a
                         else do
                             chk <- lift tacticalPos
                             let s' = if chk then matedPath else staleMate
                             return $! trimaxPath a b s'	-- shouldn't we ttStore this?

-- PV Zero Window
pvZeroW :: NodeState -> Path -> Int -> Int -> Search Path
pvZeroW _ !b !d _ | d <= 0 = do
    (v, ns) <- if minToRetr == 0
                  then do
                    (hdeep, tp, hscore, _, nodes')
                        <- {-# SCC "hashRetrieveScore" #-} reTrieve >> lift ttRead
                    let scb = pathScore b
                    if hdeep >= 0 && (tp == 2 || tp == 1 && hscore >= scb || tp == 0 && hscore < scb)
                       then {-# SCC "hashRetrieveScoreOk" #-} reSucc nodes' >> return (hscore, 0)
                       else mustQSearch (pathScore bGrain) (pathScore b)
                  else mustQSearch (pathScore bGrain) (pathScore b)
    when (minToStore == 0 && ns > 0)
        $ lift $ {-# SCC "hashStore" #-} ttStore 0 2 v (Move 0) ns
    let !esc = pathFromScore ("pvQSearch 21:" ++ show v) v
    pindent $ "<> " ++ show esc
    return esc
    where bGrain = b -: scoreGrain
pvZeroW nst b !d lastnull = do
    pindent $ ":> " ++ show b
    nmhigh <- nullEdgeFailsHigh nst b d lastnull
    abrt <- gets abort
    if abrt || nmhigh
       then do
         let !s = onlyScore b
         pindent $ "<= " ++ show s
         viztreeScore $ "nmhigh: " ++ show (pathScore s)
         return s
       else do
         edges <- genAndSort nst bGrain b d
         if noMove edges
            then do
              v <- lift staticVal
              viztreeScore $ "noMove: " ++ show v
              let !s = pathFromScore ("static: " ++ show v) v
              pindent $ "<= " ++ show s
              return s	-- shouldn't we ttStore this?
            else do
              nodes0 <- gets (sNodes . stats)
              -- futility pruning?
              prune <- if not futilActive	-- || nxtnt nst == PVNode	-- can't be PVNode
                          then return False
                          else isPruneFutil d bGrain	-- was a
              -- Loop thru the moves
              let !nsti = nst0 { crtnt = nxtnt nst, nxtnt = deepNodeType (nxtnt nst),
                                 cursc = bGrain, pvcont = tailSeq (pvcont nst) }
              nstf <- pvZLoop b d prune nsti edges
              let s = cursc nstf
              -- Here we expect bGrain <= s < b -- this must be checked
              pindent $ "<: " ++ show s
              let de = max d $ pathDepth s
                  es = unalt edges
              when (de >= minToStore && s < b) $ do	-- why s < b ?
                  nodes1 <- gets (sNodes . stats)
                  let typ = 0
                      !deltan = nodes1 - nodes0
                  -- store as upper score, and as move the first one (generated)
                  lift $ {-# SCC "hashStore" #-}
                         ttStore de typ (pathScore b) (head es) deltan
              if s > bGrain || movno nstf > 1
                 then return s
                 else do	-- here: store exact mate or stalemate score!
                     chk <- lift tacticalPos
                     let s' = if chk then matedPath else staleMate
                     return $! trimaxPath bGrain b s'
    where bGrain = b -: scoreGrain

nullEdgeFailsHigh :: NodeState -> Path -> Int -> Int -> Search Bool
nullEdgeFailsHigh nst b d lastnull
    | not nulActivate || lastnull < 1 || nxtnt nst == PVNode || d1 <= 0 = return False
    | otherwise = do
         tact <- lift tacticalPos
         if tact
            then return False
            else do
               v <- lift staticVal
               if v < pathScore b + nulTrig * scoreGrain
                  then return False
                  else do
                      lift doNullMove	-- do null move
                      nn <- newNode
                      viztreeDown0 nn
                      viztreeABD (pathScore negnmb) (pathScore negnma) d1
                      val <- liftM pnextlev $ pvSearch nst { pvcont = emptySeq } negnmb negnma d1 lastnull1
                      lift undoMove	-- undo null move
                      viztreeUp0 nn (pathScore val)
                      return $! val >= nmb
    where d1  = d - (1 + nulRedux)
          nmb = if nulSubAct then b -: (nulSubmrg * scoreGrain) else b
          nma = nmb -: (nulMargin * scoreGrain)
          negnmb = negatePath nmb
          negnma = negatePath nma
          lastnull1 = lastnull - 1

pvSLoop :: Path -> Int -> Bool -> NodeState -> Alt Move -> Search NodeState
pvSLoop b d p = go
    where go !s (Alt []) = return s
          go !s (Alt (e:es)) = do
              (!cut, !s') <- pvInnerLoop b d p s e
              if cut then return s'
                     else go s' $ Alt es

pvZLoop :: Path -> Int -> Bool -> NodeState -> Alt Move -> Search NodeState
pvZLoop b d p = go
    where go !s (Alt []) = return s
          go !s (Alt (e:es)) = do
              (!cut, !s') <- pvInnerLoopZ b d p s e
              if cut then return s'
                     else go s' $ Alt es

-- This is the inner loop of the PV search, executed at every level (except root) once per possible move
-- See the parameter
-- Returns: flag if it was a beta cut and new status
pvInnerLoop :: Path 	-- current beta
            -> Int	-- current search depth
            -> Bool	-- prune?
            -> NodeState 	-- node status
            -> Move	-- move to search
            -> Search (Bool, NodeState)
pvInnerLoop b d prune nst e = do
    abrt <- timeToAbort
    if abrt
       then return (True, nst)
       else do
         old <- get
         pindent $ "-> " ++ show e
         exd <- {-# SCC "newNode" #-} lift $ doMove False e False	-- do the move
         if legalResult exd
            then do
                nn <- newNode
                viztreeDown nn e
                modify $ \s -> s { absdp = absdp s + 1 }
                s <- case exd of
                         Exten exd' -> do
                           let speci = special e
                           if prune && exd' == 0 && not speci -- don't prune special or extended
                              then return $! onlyScore $! cursc nst	-- prune, return a
                              else pvInnerLoopExten b d speci exd' nst
                         Final sco  -> do
                             viztreeScore $ "Final: " ++ show sco
                             return $! pathFromScore "Final" (-sco)
                lift undoMove	-- undo the move
                viztreeUp nn e (pathScore s)
                modify $ \s' -> s' { absdp = absdp old, usedext = usedext old }
                s' <- checkPath nst d "cpl 8" $ addToPath e s
                pindent $ "<- " ++ show e ++ " (" ++ show s' ++ ")"
                checkFailOrPVLoop (stats old) b d e s' nst
            else return (False, nst)

-- This part for the zero window search
pvInnerLoopZ :: Path 	-- current beta
            -> Int	-- current search depth
            -> Bool	-- prune?
            -> NodeState 	-- node status
            -> Move	-- move to search
            -> Search (Bool, NodeState)
pvInnerLoopZ b d prune nst e = do
    abrt <- timeToAbort
    if abrt
       then return (True, nst)
       else do
         old <- get
         pindent $ "-> " ++ show e
         exd <- {-# SCC "newNode" #-} lift $ doMove False e False	-- do the move
         if legalResult exd
            then do
                nn <- newNode
                viztreeDown nn e
                modify $ \s -> s { absdp = absdp s + 1 }
                s <- case exd of
                         Exten exd' -> do
                           let speci = special e
                           if prune && exd' == 0 && not speci -- don't prune special or extended
                              then return $! onlyScore $! cursc nst	-- prune, return a
                              else pvInnerLoopExtenZ b d speci exd' nst
                         Final sco  -> do
                             viztreeScore $ "Final: " ++ show sco
                             return $! pathFromScore "Final" (-sco)
                lift undoMove	-- undo the move
                viztreeUp nn e (pathScore s)
                modify $ \s' -> s' { absdp = absdp old, usedext = usedext old }
                s' <- checkPath nst d "cpl 8" $ addToPath e s
                pindent $ "<- " ++ show e ++ " (" ++ show s' ++ ")"
                checkFailOrPVLoopZ (stats old) b d e s' nst
            else return (False, nst)

reserveExtension :: Int -> Int -> Search Int
reserveExtension !uex !exd
    | uex >= maxDepthExt || exd == 0 = return 0
    | otherwise = do
        modify $ \s -> s { usedext = usedext s + exd }
        return exd

pvInnerLoopExten :: Path -> Int -> Bool -> Int -> NodeState
                 -> Search Path
pvInnerLoopExten b d spec !exd nst = do
    old <- get
    exd' <- reserveExtension (usedext old) exd
    -- late move reduction
    let !inPv = nxtnt nst == PVNode
        pvs = forpv nst
        a = cursc nst
        !d1 = d + exd' - 1	-- this is the normal (unreduced) depth for next search
    d' <- reduceLmr d1 inPv (pnearmate a) spec exd (movno nst) pvs
    pindent $ "depth " ++ show d ++ " nt " ++ show (nxtnt nst)
              ++ " exd' = " ++ show exd'
              ++ " mvn " ++ show (movno nst) ++ " next depth " ++ show d'
              ++ " forpv " ++ show (forpv nst)
    (hdeep, tp, hscore, e', nodes')
        <- if (useTTinPv || not inPv) && d' >= minToRetr
              then {-# SCC "hashRetrieveScore" #-} reTrieve >> lift ttRead
              else return (-1, 0, 0, undefined, 0)
    -- TT score is for the opponent (we just made our move),
    -- so we have to invert the score and the inequality (tp: 2->2, 1->0, 0->1)
    let asco = pathScore a
        !hsco = - hscore		
        !tp'  = if tp == 2 then 2 else 1-tp
    -- This logic could be done depending on node type?
    if hdeep >= d' && (tp' == 2 || tp' == 1 && hsco > asco || tp' == 0 && hsco <= asco)
       then {-# SCC "hashRetrieveScoreOk" #-} do
           let ttpath = Path { pathScore = hsco, pathDepth = hdeep, pathMoves = Seq [e'], pathOrig = "TT" }
           reSucc nodes' >> return ttpath
       else do
          let nega = negatePath a
              negb = negatePath b
          if pvs
             then do
                viztreeABD (pathScore negb) (pathScore nega) d'
                fmap pnextlev (pvSearch nst negb nega d' nulMoves) >>= checkPath nst d' "cpl 14"
             else do
                let aGrain = nega -: scoreGrain
                    pvpath_ = pvcont nst
                    pvpath' = if nullSeq pvpath_ && hdeep > 0 then Seq [e'] else pvpath_
                -- Here we expect to fail low
                viztreeABD (pathScore aGrain) (pathScore nega) d'
                !s1 <- fmap pnextlev (pvZeroW nst { pvcont = pvpath' } nega d' nulMoves)
                       >>= checkPath nst d' "cpl 9"
                abrt <- gets abort
                if abrt || s1 <= a
                   then return s1	-- failed low (as expected) or aborted
                   else do
                     -- we didn't fail low and need re-search, 2 kinds: full depth, full window
                     pindent $ "Research! (" ++ show s1 ++ ")"
                     viztreeReSe
                     -- let pvc = if pathDepth s1 > 0 then pathMoves s1 else pvpath'
                     let pvc = if nullSeq (pathMoves s1) then pvpath' else pathMoves s1
                     if d' < d1	-- did we search with reduced depth?
                        then do	-- yes: re-search with with normal depth
                            viztreeABD (pathScore aGrain) (pathScore nega) d1
                            !s2 <- fmap pnextlev (pvZeroW nst { pvcont = pvc } nega d1 nulMoves)
                                     >>= checkPath nst d1 "cpl 9a"
                            abrt <- gets abort
                            if abrt || s2 <= a
                               then return s2	-- failed low (as expected) or aborted
                               else do
                                   viztreeReSe
                                   viztreeABD (pathScore negb) (pathScore nega) d1
                                   let nst' = if crtnt nst == PVNode
                                                 then nst { nxtnt = PVNode, forpv = True, pvcont = pvc' }
                                                 else nst { forpv = True, pvcont = pvc' }
                                       -- pvc' = if pathDepth s2 > 0 then pathMoves s2 else pvc
                                       pvc' = if nullSeq (pathMoves s2) then pvc else pathMoves s2
                                   fmap pnextlev (pvSearch nst' negb nega d1 0)
                                       >>= checkPath nst d1 "cpl 15"
                        else do
                           -- was not reduced, try full window
                           viztreeABD (pathScore negb) (pathScore nega) d1
                           let nst' = if crtnt nst == PVNode
                                         then nst { nxtnt = PVNode, forpv = True, pvcont = pvc }
                                         else nst { forpv = True, pvcont = pvc }
                           fmap pnextlev (pvSearch nst' negb nega d1 0) >>= checkPath nst d1 "cpl 16"

-- For zero window
pvInnerLoopExtenZ :: Path -> Int -> Bool -> Int -> NodeState
                 -> Search Path
pvInnerLoopExtenZ b d spec !exd nst = do
    old <- get
    exd' <- reserveExtension (usedext old) exd
    -- late move reduction
    let !d1 = d + exd' - 1	-- this is the normal (unreduced) depth for next search
    d' <- reduceLmr d1 False (pnearmate b) spec exd (movno nst) False
    pindent $ "depth " ++ show d ++ " nt " ++ show (nxtnt nst)
              ++ " exd' = " ++ show exd'
              ++ " mvn " ++ show (movno nst) ++ " next depth " ++ show d'
              ++ " forpv " ++ show False
    (hdeep, tp, hscore, e', nodes')
        <- if d' >= minToRetr
              then {-# SCC "hashRetrieveScore" #-} reTrieve >> lift ttRead
              else return (-1, 0, 0, undefined, 0)
    -- Score and inequality must be inverted
    let bsco = pathScore b
        !hsco = - hscore
        !tp' = if tp == 2 then 2 else 1-tp
    if hdeep >= d' && (tp' == 2 || tp' == 1 && hsco >= bsco || tp' == 0 && hsco < bsco)
       then {-# SCC "hashRetrieveScoreOk" #-} do
           let ttpath = Path { pathScore = hsco, pathDepth = hdeep, pathMoves = Seq [e'], pathOrig = "TT" }
           reSucc nodes' >> return ttpath	-- !!!
       else do
          -- Very probable we don't have pvpath, so don't bother - why not?
          let pvpath_ = pvcont nst
          let pvpath' = if nullSeq pvpath_ && hdeep > 0 then Seq [e'] else pvpath_
          -- Here we expect to fail low
          viztreeABD (pathScore negb) (pathScore onemB) d'
          fmap pnextlev (pvZeroW nst { pvcont = pvpath' } onemB d' nulMoves) >>= checkPath nst d' "cpl 9"
    where onemB = negatePath $ b -: scoreGrain
          negb = negatePath b

checkFailOrPVLoop :: SStats -> Path -> Int -> Move -> Path
                  -> NodeState -> Search (Bool, NodeState)
checkFailOrPVLoop xstats b d e s nst = do
    sst <- get
    let mn = movno nst
    if s <= cursc nst
       then do
            -- when in a cut node and the move dissapointed - negative history
            !kill1 <- newKiller d s nst
            let !nst1 = nst { movno = mn+1, killer = kill1, pvcont = emptySeq }
            return (False, nst1)
       else do
         let nodes0 = sNodes xstats
             nodes1 = sNodes $ stats sst
             nodes' = nodes1 - nodes0
             !de = max d $ pathDepth s
         if s >= b
            then do
              let typ = 1	-- best move is e and is beta cut (score is lower limit)
              when (de >= minToStore) $
                  lift $ {-# SCC "hashStore" #-} ttStore de typ (pathScore b) e nodes'
              lift $ betaCut True d (absdp sst) e -- anounce a beta move (for example, update history)
              -- when debug $ logmes $ "<-- pvInner: beta cut: " ++ show s ++ ", return " ++ show b
              !csc <- checkPath nst d "cpl 10" $ if s > b then combinePath s b else bestPath s b
              pindent $ "beta cut: " ++ show csc
              let !nst1 = nst { cursc = csc, pvcont = emptySeq }
              -- lift $ informStr $ "Cut (" ++ show b ++ "): " ++ show np
              return (True, nst1)
            else do	-- means: > a && < b
              let typ = 2	-- score is exact
              when (nxtnt nst == PVNode || de >= minToStore) $	-- why this || with node type?
                  lift $ {-# SCC "hashStore" #-} ttStore de typ (pathScore s) e nodes'
              -- when debug $ logmes $ "<-- pvInner - new a: " ++ show s
              let !nst1 = nst { cursc = s, nxtnt = nextNodeType (nxtnt nst),
                               forpv = False, movno = mn+1, pvcont = emptySeq }
              -- lift $ informStr $ "Better (" ++ show s ++ "): " ++ show np
              return (False, nst1)

-- For zero window
checkFailOrPVLoopZ :: SStats -> Path -> Int -> Move -> Path
                  -> NodeState -> Search (Bool, NodeState)
checkFailOrPVLoopZ xstats b d e s nst = do
    sst <- get
    let mn = movno nst
        -- a  = cursc nst
    if s <= cursc nst	-- see below by "else"
       then do
            -- when in a cut node and the move dissapointed - negative history - ???
            when (useNegHist && mn <= negHistMNo)
                 $ lift $ betaCut False d (absdp sst) e
            !kill1 <- newKiller d s nst
            let !nst1 = nst { movno = mn+1, killer = kill1, pvcont = emptySeq }
            return (False, nst1)
       else do	-- here is s >= b: why cursc nst and now b???
         let nodes0 = sNodes xstats
             nodes1 = sNodes $ stats sst
             nodes' = nodes1 - nodes0
             !de = max d $ pathDepth s
         let typ = 1	-- best move is e and is beta cut (score is lower limit)
         when (de >= minToStore) $ lift $ {-# SCC "hashStore" #-} ttStore de typ (pathScore b) e nodes'
         lift $ betaCut True d (absdp sst) e -- anounce a beta move (for example, update history)
         -- when debug $ logmes $ "<-- pvInner: beta cut: " ++ show s ++ ", return " ++ show b
         !csc <- checkPath nst d "cpl 10" $ if s > b then combinePath s b else bestPath s b
         pindent $ "beta cut: " ++ show csc
         let !nst1 = nst { cursc = csc, pvcont = emptySeq }
         -- lift $ informStr $ "Cut (" ++ show b ++ "): " ++ show np
         return (True, nst1)

newKiller :: Int -> Path -> NodeState -> Search Killer
newKiller d s nst
    | d >= 2, (mm:km:_) <- unseq $ pathMoves s = do
        !iskm <- lift $ isKillCand mm km
        if iskm then return $! pushKiller km (killer nst)
                else return $ killer nst
    | otherwise = return $ killer nst

-- We don't sort the moves here, they have to come sorted from genMoves
-- But we consider the best moves first (from previous iteration, TT or IID)
-- and the killers
genAndSort :: NodeState -> Path -> Path -> Int -> Search (Alt Move)
genAndSort nst a b d = do
    let path' = unseq $ pvcont nst
    path <- if not $ null path'
               then return path'
               else do
                   me <- bestMoveFromHash
                   case me of
                       Nothing -> if useIID
                           then do
                               mmv <- bestMoveFromIID nst a b d	-- it will do nothing for AllNode
                               case mmv of
                                   Just e' -> return [e']
                                   Nothing -> return []
                           else return []
                       Just e  -> do
                           le <- lift $ isMoveLegal e
                           if le
                              then return [e]
                              else if useIID
                                  then do
                                      mmv <- bestMoveFromIID nst a b d	-- it will do nothing for AllNode
                                      case mmv of
                                          Just e' -> return [e']
                                          Nothing -> return []
                                  else return []
    adp <- gets absdp
    esp <- lift $ genMoves d adp (crtnt nst /= AllNode)
    kl  <- lift $ filterM isMoveLegal $ killerToList (killer nst)
    let es = bestFirst path kl esp
    return $ Alt es
    -- where pv' = pv || not (nullSeq lastpath)	-- why this? We would sort most of the time...

-- Late Move Reduction
-- This part (including lmrIndex) seems well optimized
{-# INLINE reduceLmr #-}
reduceLmr :: Int -> Bool -> Bool -> Bool -> Int -> Int -> Bool -> Search Int
reduceLmr d inPv nearmatea spec exd w pvs
    = if not lmrActive || d < lmrMinDRed || inPv || spec || exd > 0 || nearmatea
         then return d
         else do
             !tact <- lift tacticalPos
             let !rd = reduceDepth d w pvs
             return $! if tact then d else rd
    where lmrMinDRed = 2 :: Int		-- minimum reduced depth

{-# INLINE reduceDepth #-}
reduceDepth :: Int -> Int -> Bool -> Int
reduceDepth !d !w !pvs = m0n
    where nd = d - k
          !m0n = max 0 nd
          k  = if pvs then lmrReducePv  `unsafeAt` lmrIndex d w
                      else lmrReduceArr `unsafeAt` lmrIndex d w

-- Here we know the index is correct, but unsafeIndex (from Data.Ix)
-- is unfortunately not exported...
-- The trick: define an UnsafeIx class to calculate direct unsafeIndex
lmrIndex :: Int -> Int -> Int
lmrIndex d w = unsafeIndex ((1, 1), (lmrMaxDepth, lmrMaxWidth)) (d1, w1)
    where d1 = min lmrMaxDepth $ max 1 d
          w1 = min lmrMaxWidth $ max 1 w

-- The UnsafeIx inspired from GHC.Arr (class Ix)
class Ord a => UnsafeIx a where
    unsafeIndex :: (a, a) -> a -> Int
    unsafeRangeSize :: (a, a) -> Int
    unsafeRangeSize b@(_, h) = unsafeIndex b h + 1

instance UnsafeIx Int where
    {-# INLINE unsafeIndex #-}
    unsafeIndex (m, _) i = i - m

instance (UnsafeIx a, UnsafeIx b) => UnsafeIx (a, b) where -- as derived
    {-# SPECIALISE instance UnsafeIx (Int,Int) #-}
    {-# INLINE unsafeIndex #-}
    unsafeIndex ((l1,l2),(u1,u2)) (i1,i2) = unsafeIndex (l1,u1) i1 * unsafeRangeSize (l2,u2) + unsafeIndex (l2,u2) i2

-- This is a kind of monadic fold optimized for (beta) cut
-- {-# INLINE pvLoop #-}
pvLoop :: Monad m => (s -> e -> m (Bool, s)) -> s -> Alt e -> m s
pvLoop _ s (Alt [])     = return s
pvLoop f s (Alt (e:es)) = do
    (cut, s') <- f s e
    if cut then return s'
           else pvLoop f s' $ Alt es

isPruneFutil :: Int -> Path -> Search Bool
isPruneFutil d a
    | d <= 0 || d > maxFutilDepth || nearmate (pathScore a) = return False
    | otherwise = do
        tact <- lift tacticalPos
        if tact then return False else do
            -- let !margin = futilMargins ! d
            v <- lift staticVal	-- E1
            -- v <- lift materVal	-- can we do here direct static evaluation?
            -- v <- pvQSearch a' b' 0	-- E2
            let margin = futilMargins d
                a' = pathScore a
            if v + margin <= a'
               then return True
               else return False

{-# INLINE checkPath #-}
-- checkPath _ _ _ s = return s
checkPath :: NodeState -> Int -> String -> Path -> Search Path
checkPath nst d mes s = do
    when (nxtnt nst == PVNode) $ do
        iss  <- gets short
        unless iss $ do
            abrt <- gets abort
            when (not abrt && length (unseq $ pathMoves s) < d) $ do
                lift $ informStr $ "Short - " ++ mes ++ " : d = " ++ show d
                                     ++ ", path = " ++ show s
                modify $ \s' -> s' { short = True }
    return s

trimaxPath :: Path -> Path -> Path -> Path
trimaxPath a b x
    | x < a     = a
    | x > b     = b
    | otherwise = x

trimax :: Int -> Int -> Int -> Int
trimax a b x
    | x < a     = a
    | x > b     = b
    | otherwise = x

-- PV Quiescent Search
pvQSearch :: Int -> Int -> Int -> Search Int
pvQSearch !a !b c = do				   -- to avoid endless loops
    -- qindent $ "=> " ++ show a ++ ", " ++ show b
    !stp <- lift staticVal				-- until we can recognize repetition
    viztreeScore $ "Static: " ++ show stp
    !tact <- lift tacticalPos
    if tact
       then do
           (es1, es2) <- lift $ genMoves 0 0 False
           let edges = Alt $ es1 ++ es2
           if noMove edges
              -- then qindent ("<= " ++ show stp) >> return stp
              then do
                  lift $ finNode "MATE"
                  return $! trimax a b stp
              else if c >= qsMaxChess
                      -- then qindent ("<= -1") >> return inEndlessCheck
                      then do
                          viztreeScore $ "endless check: " ++ show inEndlessCheck
                          lift $ finNode "ENDL"
                          return $! trimax a b inEndlessCheck
                      {--
                      else if stp >= b
                              then return b
                              else do
                                  -- for check extensions in case of very few moves (1 or 2):
                                  -- if 1 move: search even deeper
                                  -- if 2 moves: same depth
                                  -- if 3 or more: no extension
                                  let !esc = lenmax3 $ unalt edges
                                      !nc = c + esc - 2
                                  if stp > a
                                     then pvQLoop b nc stp edges
                                     else pvQLoop b nc a   edges
                                  -- qindent $ "<= " ++ show s
                      --}
                      else do
                          -- for check extensions in case of very few moves (1 or 2):
                          -- if 1 move: search even deeper
                          -- if 2 moves: same depth
                          -- if 3 or more: no extension
                          let !esc = lenmax3 $ unalt edges
                              !nc = c + esc - 2
                          pvQLoop b nc a edges
                          -- qindent $ "<= " ++ show s
       else if qsBetaCut && stp >= b
               -- then qindent ("<= " ++ show b) >> return b
               then do
                   lift $ finNode "BETA"
                   return b
               else if qsDeltaCut && stp + qsDelta < a
                      -- then qindent ("<= " ++ show a) >> return a
                      then do
                          lift $ finNode "DELT"
                          return a
                      else do
                          edges <- liftM Alt $ lift genTactMoves
                          if noMove edges
                             -- then qindent ("<= " ++ show stp) >> return stp
                             then do
                                 lift $ finNode "NOCA"
                                 return $! trimax a b stp
                             else if stp > a
                                     then pvQLoop b c stp edges
                                     else pvQLoop b c a   edges
                                  -- qindent $ "<= " ++ show s
    where lenmax3 = lenmax3' 0
          lenmax3' !n _ | n == 3 = 3
          lenmax3' !n []         = n
          lenmax3' !n (_:as)     = lenmax3' (n+1) as

pvQLoop :: Int -> Int -> Int -> Alt Move -> Search Int
pvQLoop b c = go
    where go !s (Alt [])     = return s
          go !s (Alt (e:es)) = do
              (!cut, !s') <- pvQInnerLoop b c s e
              if cut then return s'
                     else go s' $ Alt es

pvQInnerLoop :: Int -> Int -> Int -> Move -> Search (Bool, Int)
pvQInnerLoop !b c !a e = do
    abrt <- timeToAbort
    if abrt
       then return (True, b)	-- it doesn't matter which score we return
       else do
         -- here: delta pruning: captured piece + 200 > a? then go on, else return
         -- qindent $ "-> " ++ show e
         r <- {-# SCC "newNodeQS" #-} lift $ doMove False e True
         if legalResult r
            then do
                nn <- newNodeQS
                viztreeDown nn e
                !sc <- case r of
                           Final sc -> do
                               viztreeScore $ "Final: " ++ show sc
                               return (-sc)
                           _        -> do
                             modify $ \s -> s { absdp = absdp s + 1 }
                             !s <- pvQSearch (-b) (-a) c
                             modify $ \s -> s { absdp = absdp s - 1 }	-- don't care about usedext here
                             return (-s)
                lift undoMove
                viztreeUp nn e sc
                -- qindent $ "<- " ++ show e ++ " (" ++ show s ++ ")"
                if sc >= b
                   then return (True, b)
                   else do
                       !abrt' <- gets abort
                       if sc > a
                          then return (abrt', sc)
                          else return (abrt', a)
            else return (False, a)

bestMoveFromHash :: Search (Maybe Move)
bestMoveFromHash = do
    reTrieve
    (hdeep, tp, _, e, _) <- {-# SCC "hashRetrieveMove" #-} lift ttRead
    when (hdeep > 0) $ reSucc 1		-- here we save just move generation
    -- return $! if hdeep > 0 && tp > 0 then {-# SCC "hashRetrieveMoveOk" #-} Seq [e] else emptySeq
    return $! if hdeep > 0 then {-# SCC "hashRetrieveMoveOk" #-} Just e else Nothing	-- upper score has correct move

{-# INLINE bestMoveFromIID #-}
bestMoveFromIID :: NodeState -> Path -> Path -> Int -> Search (Maybe Move)
bestMoveFromIID nst a b d
    | nt == PVNode  && d >= minIIDPV ||
      nt == CutNode && d >= minIIDCut
                = {-# SCC "iidExecutedYes" #-} do
                   s <- pvSearch nst a b d' 0
                   return $! shead $ pathMoves s
                -- Here we come from a search (pv or zero), and end up in a new search
                -- with a smaller depth, but this could be an expensive loop!
                -- All this time the moves are generated anew!!
    | otherwise = {-# SCC "iidExecutedNo"  #-} return Nothing
    where d' = min maxIIDDepth (iidNewDepth d)
          nt = nxtnt nst
          shead (Seq (e:_)) = Just e
          shead _           = Nothing

{-# INLINE timeToAbort #-}
timeToAbort :: Search Bool
timeToAbort = do
    s <- get
    let ro = ronly s
    if draft ro > 1 && timeli ro
       then if timeNodes .&. sNodes (stats s) /= 0
               then return False
               else do
                   !abrt <- lift $ isTimeout $ abmili ro
                   when abrt $ do
                       lift $ informStr "Albeta: search abort!"
                       put s { abort = True }
                   return abrt
       else return False
    where timeNodes = 4 * 1024 - 1	-- check time every so many nodes

{-# INLINE reportStats #-}
reportStats :: Search ()
reportStats = do
    s <- get
    let !xst = stats s
    lift $ logmes $ "Search statistics after draft " ++ show (draft $ ronly s) ++ ":"
    lift $ logmes $ "Nodes: " ++ show (sNodes xst) ++ ", in QS: " ++ show (sNodesQS xst)
             ++ ", retrieve: " ++ show (sRetr xst) ++ ", succes: " ++ show (sRSuc xst)

-- Functions to keep statistics
modStat :: (SStats -> SStats) -> Search ()
modStat f = modify $ \s -> case f (stats s) of st -> s { stats = st }

modRetStat :: (SStats -> SStats) -> (SStats -> Int) -> Search Int
modRetStat f g = do
    s <- get
    let ss = f $ stats s
    put s { stats = ss }
    return $! g ss

incNodes, incNodesQS :: SStats -> SStats
incNodes   s = case sNodes s + 1 of n1 -> s { sNodes = n1 }
incNodesQS s = case sNodes s + 1 of
                 n1 -> case sNodesQS s + 1 of n2 -> s { sNodes = n1, sNodesQS = n2 }

incReTrieve :: SStats -> SStats
incReTrieve s = case sRetr s + 1 of n1 -> s { sRetr = n1 }

addReSucc :: Int -> SStats -> SStats
addReSucc n s = case sRSuc s + n of n1 -> s { sRSuc = n1 }

newNode :: Search Int
newNode   = modRetStat incNodes sNodes

newNodeQS :: Search Int
newNodeQS = modRetStat incNodesQS sNodes

reTrieve :: Search ()
reTrieve  = modStat incReTrieve

reSucc :: Int -> Search ()
reSucc n  = modStat (addReSucc n)

indentActive :: String -> Search ()
indentActive s = do
    ad <- gets absdp
    lift $ informStr $ replicate ad ' ' ++ s

indentPassive :: String -> Search ()
indentPassive _ = return ()

pindent, qindent :: String -> Search ()
pindent = indentPassive
qindent = indentPassive

viztreeDown :: Int -> Move -> Search ()
viztreeDown n e = when viztree $ lift $ logmes $ "***DOWN " ++ show n ++ " " ++ show e

viztreeDown0 :: Int -> Search ()
viztreeDown0 n = when viztree $ lift $ logmes $ "***DOWN " ++ show n ++ " null"

viztreeUp :: Int -> Move -> Int -> Search ()
viztreeUp n e s = when viztree $ lift $ logmes $ "***UP " ++ show n ++ " " ++ show e ++ " " ++ show s

viztreeUp0 :: Int -> Int -> Search ()
viztreeUp0 n s = when viztree $ lift $ logmes $ "***UP " ++ show n ++ " null " ++ show s

viztreeNew :: Int -> Search ()
viztreeNew d = when viztree $ lift $ logmes $ "***NEW " ++ show d

viztreeABD :: Int -> Int -> Int -> Search ()
viztreeABD a b d = when viztree $ lift $ logmes $ "***ABD " ++ show a ++ " " ++ show b ++ " " ++ show d

viztreeReSe :: Search ()
viztreeReSe = when viztree $ lift $ logmes "***RESE"

viztreeScore :: String -> Search ()
viztreeScore s = when viztree $ lift $ logmes $ "***SCO " ++ s

bestFirst :: Eq e => [e] -> [e] -> ([e], [e]) -> [e]
bestFirst path kl (es1, es2)
    | null path = es1 ++ kl ++ delall es2 kl
    | otherwise = e : delete e es1 ++ kl ++ delall es2 (e : kl)
    where delall = foldr delete
          (e:_)  = path

pushKiller :: Move -> Killer -> Killer
pushKiller !e NoKiller = OneKiller e
pushKiller !e ok@(OneKiller e1)
    | e == e1   = ok
    | otherwise = TwoKillers e e1
pushKiller !e tk@(TwoKillers e1 e2)
    | e == e1 || e == e2 = tk
    | otherwise          = TwoKillers e e1

killerToList :: Killer -> [Move]
killerToList  NoKiller          = []
killerToList (OneKiller e)      = [e]
killerToList (TwoKillers e1 e2) = [e1, e2]

--- Communication to the outside - some convenience functions ---

informBM :: Int -> Int -> Int -> [Move] -> Game ()
informBM a b c d = informCtx (BestMv a b c d)

informCM :: Move -> Int -> Game ()
informCM a b = informCtx (CurrMv a b)

informStr :: String -> Game ()
informStr s = informCtx (InfoStr s)

logmes :: String -> Game ()
logmes s = informCtx (LogMes s)

informBest :: Int -> Int -> [Move] -> Search ()
informBest s d es = do
    n <- lift curNodes
    lift $ informBM s d n es
