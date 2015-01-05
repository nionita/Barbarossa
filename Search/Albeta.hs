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
-- import Data.Array.Base
import Data.Maybe (fromMaybe)

import Search.CStateMonad
import Search.AlbetaTypes
import Struct.Struct
import Moves.BaseTypes
import Moves.Base

-- debug :: Bool
-- debug = False
absurd :: String -> Game ()
absurd s = logmes $ "o/\\o: " ++ s	-- used for messages when assertions fail

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
lmrActive, lmrDebug :: Bool
lmrActive   = True
lmrDebug    = False

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
useIID      = True

minIIDPV, minIIDCut, minIIDCutNK, maxIIDDepth :: Int
minIIDPV    = 5
minIIDCutNK = 6
minIIDCut   = 8
maxIIDDepth = 2

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
                  deriving (Eq, Show)

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
          ronly   :: PVReadOnly,	-- read only parameters
          stats   :: SStats,	-- search statistics
          absdp   :: !Int,	-- absolute depth (root = 0)
          usedext :: !Int,	-- used extension
          abort   :: !Bool	-- search aborted (time)
      } deriving Show

-- This is a state which reflects the status of alpha beta in a node while going through the edges
data NodeState
    = NSt {
          crtnt  :: !NodeType,	-- parent node type (actually expected)
          nxtnt  :: !NodeType,	-- expected child node type
          cursc  :: !Path,	-- current alpha value (now plus path & depth)
          movno  :: !Int,	-- current move number
          killer :: !Killer,	-- the current killer moves
          pvsl   :: [Pvsl],	-- principal variation list (at root) with node statistics
          pvcont :: Seq Move	-- a pv continuation from the previous iteration, if available
      } deriving Show

data SStats = SStats {
        sNodes, sNodesQS, sRetr, sRSuc, sBeta, sBMNo, sRedu, sReMi, sReBe, sReSe, sReNo :: !Int
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
pvsInit = PVState { ronly = pvro00, absdp = 0, usedext = 0, abort = False, stats = stt0 }
nst0 :: NodeState
nst0 = NSt { crtnt = PVNode, nxtnt = PVNode, cursc = pathFromScore "Zero" 0,
             movno = 1, killer = NoKiller, pvsl = [], pvcont = emptySeq }

stt0 :: SStats
stt0 = SStats { sNodes = 0, sNodesQS = 0, sRetr = 0, sRSuc = 0,
                sBeta = 0, sBMNo = 0, sRedu = 0, sReMi = 0, sReBe = 0, sReSe = 0, sReNo = 0 }

pvro00 :: PVReadOnly
pvro00 = PVReadOnly { draft = 0, albest = False, timeli = False, abmili = 0 }

alphaBeta :: ABControl -> Game (Int, [Move], [Move], Bool)
alphaBeta abc =  do
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
                    <- 
                         runCState (searchReduced alpha1 beta1) pvs0
                if abort pvsf || (s1 > alpha1 && s1 < beta1 && not (nullSeq es1))
                    then return r1
                    else  if nullSeq es1
                        then runCState (searchFull lpv) pvs0
                        else runCState (searchFull es1) pvs0
             Nothing ->  runCState (searchFull lpv) pvs0
         else  runCState (searchFull lpv) pvs0
    let timint = abort (snd r)
    -- when aborted, return the last found good move
    -- we have to trust that abort is never done in draft 1!
    -- if abort (snd r)
    --    then return (fromMaybe 0 $ lastscore abc, lastpv abc, [])
    --    else return $! case fst r of (s, Seq path, Alt rmvs') -> (s, path, rmvs')
    case fst r of
        (s, Seq path, Alt rmvs') -> if null path
           then return (fromMaybe 0 $ lastscore abc, lastpv abc, [], timint)
           else return (s, path, rmvs', timint)

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
                    genAndSort nst0 { pvcont = lastpath } a' b' d	-- this will never really do IID as d==1
                else case lastpath of
                         Seq []    -> return rmvs	-- probably this never happens... - check to simplify!
                         Seq (e:_) -> return $ Alt $ e : delete e (unalt rmvs)
    -- pvcont is the pv continuation from the last iteration
    let !nsti = nst0 { cursc = pathFromScore "Alpha" a, pvcont = tailSeq lastpath }	-- strict?
    nstf <- pvLoop (pvInnerRoot (pathFromScore "Beta" b) d) nsti edges
    reportStats
    let failedlow = (a, emptySeq, edges)	-- just to permit aspiration to retry
        sc | d > 1            = pathScore (cursc nstf)
           | p:_ <- pvsl nstf = pathScore $ pvPath p
           | otherwise        = a
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
         -- do the move
         exd <-  lift $ doMove False e False
         if legalResult exd
            then do
                nn <- newNode
                viztreeDown nn e
                modify $ \s -> s { absdp = absdp s + 1 }
                -- (s, def) <- case exd of
                s <- case exd of
                         Exten exd' _ -> pvInnerRootExten b d exd' nst
                         Final sco    -> do
                             viztreeScore $ "Final: " ++ show sco
                             -- return (pathFromScore "Final" (-sco), d)
                             return $! pathFromScore "Final" (-sco)
                         Illegal -> error "Cannot be illegal here"
                -- undo the move
                lift undoMove
                viztreeUp nn e (pathScore s)
                modify $ \s' -> s' { absdp = absdp old, usedext = usedext old }
                let s' = addToPath e s
                pindent $ "<- " ++ show e ++ " (" ++ show s' ++ ")"
                checkFailOrPVRoot (stats old) b d e s' nst
            else return (False, nst)

-- pvInnerRootExten :: Path -> Int -> Bool -> Int -> NodeState -> Search (Path, Int)
pvInnerRootExten :: Path -> Int -> Int -> NodeState -> Search Path
pvInnerRootExten b d !exd nst =  do
    pindent $ "depth = " ++ show d
    old <- get
    exd' <- reserveExtension (usedext old) exd
    let !inPv = nxtnt nst == PVNode
        !a  = cursc nst
        !d1 = d + exd' - 1	-- this is the normal (unreduced) depth for the next search
    pindent $ "depth " ++ show d ++ " nt " ++ show (nxtnt nst)
              ++ " exd' = " ++ show exd'
              ++ " mvn " ++ show (movno nst) ++ " next depth " ++ show d1
    let nega = negatePath a
        negb = negatePath b
    if inPv	-- search of principal variation
       then do
           viztreeABD (pathScore negb) (pathScore nega) d1
           fmap pnextlev (pvSearch nst negb nega d1)
       else do
           let aGrain = nega -: scoreGrain
           -- no futility pruning & no LMR for root moves!
           -- Here we expect to fail low
           viztreeABD (pathScore aGrain) (pathScore nega) d1
           !s1 <- fmap pnextlev (pvZeroW nst nega d1 nulMoves True)
           abrt <- gets abort
           if abrt || s1 <= a -- we failed low as expected
              then return s1
              else  do
                 -- Here we didn't fail low and need re-search
                 -- As we don't reduce (beeing in a PV node), re-search is full window
                 pindent $ "Research! (" ++ show s1 ++ ")"
                 viztreeReSe
                 viztreeABD (pathScore negb) (pathScore nega) d1
                 let pvc  = if nullSeq (pathMoves s1) then pvcont nst else pathMoves s1
                     nst' = nst { nxtnt = PVNode, pvcont = pvc }
                 fmap pnextlev (pvSearch nst' negb nega d1)

checkFailOrPVRoot :: SStats -> Path -> Int -> Move -> Path
                  -> NodeState -> Search (Bool, NodeState)
checkFailOrPVRoot xstats b d e s nst =  do
    abrt <- timeToAbort
    if abrt
       then return (True, nst)
       else do
         sst <- get
         let !mn     = movno nst
             !a      = cursc nst
             !nodes0 = sNodes xstats + sRSuc xstats
             !nodes1 = sNodes (stats sst) + sRSuc (stats sst)
             !nodes' = nodes1 - nodes0
             pvg    = Pvsl s nodes' True	-- the good
             pvb    = Pvsl s nodes' False	-- the bad
             de = max d $ pathDepth s
         if d == 1
            then  do
                 let typ = 2
                 when (de >= minToStore) $ lift $  ttStore de typ (pathScore s) e nodes'
                 let xpvslg = if s > a
                                 then insertToPvs d pvg (pvsl nst)	-- the good
                                 else insertToPvs d pvb (pvsl nst)	-- the bad (when aspiration)
                 return (False, nst {movno = mn + 1, pvsl = xpvslg, pvcont = emptySeq})
            else if s <= a
                    then 	-- do	-- failed low
                      -- when in a cut node and the move dissapointed - negative history
                      -- when (useNegHist && forpv nst && a == b - 1 && mn <= negHistMNo) -- Check this!
                      --      $ lift $ betaCut False (absdp sst) e
                      if nxtnt nst == PVNode
                         then return (True, nst { cursc = s })	-- i.e we failed low in aspiration with 1st move
                         else do
                           kill1 <- newKiller d s nst
                           let xpvslb = insertToPvs d pvb (pvsl nst)	-- the bad
                               nst1 = nst { movno = mn + 1, pvsl = xpvslb, killer = kill1, pvcont = emptySeq }
                           return (False, nst1)
                    else if s >= b
                      then do
                        -- what when a root move fails high? We are in aspiration
                        lift $ do
                            when (de >= minToStore) $ do
                                let typ = 1	-- beta cut (score is lower limit) with move e
                                ttStore de typ (pathScore b) e nodes'
                            betaCut True (absdp sst) e
                        let xpvslg = insertToPvs d pvg (pvsl nst)	-- the good
                            !csc = if s > b then combinePath s b else bestPath s b
                        pindent $ "beta cut: " ++ show csc
                        let nst1 = nst { cursc = csc, pvsl = xpvslg, pvcont = emptySeq }
                        return (True, nst1)
                      else do	-- means: > a && < b
                        let sc = pathScore s
                            pa = unseq $ pathMoves s
                        informBest (scoreToExtern sc de) (draft $ ronly sst) pa
                        when (de >= minToStore) $ lift $ do
                            let typ = 2	-- best move so far (score is exact)
                            ttStore de typ sc e nodes'
                        let xpvslg = insertToPvs d pvg (pvsl nst)	-- the good
                            nst1 = nst { cursc = s, nxtnt = nextNodeType (nxtnt nst),
                                         movno = mn + 1, pvsl = xpvslg, pvcont = emptySeq }
                        return (False, nst1)

insertToPvs :: Int -> Pvsl -> [Pvsl] -> [Pvsl]
insertToPvs _ p [] = [p]
insertToPvs d p ps@(q:qs)
    | d == 1 && (betters || equals) = p : ps
    | pmate && not qmate            = p : ps
    | not pmate && qmate            = q : insertToPvs d p qs
    | pmate && betters              = p : ps
    | bettern || equaln && betters  = p : ps
    | otherwise                     = q : insertToPvs d p qs
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
pvSearch :: NodeState -> Path -> Path -> Int -> Search Path
pvSearch _ !a !b !d | d <= 0 = do
    -- Now that we moved the ttRead call under pvSearch we are not prepared
    -- to handle correctly the case minToRetr = 0
    (v, ns) <- mustQSearch (pathScore a) (pathScore b)
    when (minToStore == 0)
        $ lift $  ttStore 0 2 v (Move 0) ns
    let !esc = pathFromScore ("pvQSearch 1:" ++ show v) v
    pindent $ "<> " ++ show esc
    return esc
pvSearch nst !a !b !d = do
    pindent $ "=> " ++ show a ++ ", " ++ show b
    let !inPv = nxtnt nst == PVNode
    -- Here we are always in PV:
    when (not inPv) $ lift $ absurd $ "pvSearch not inPv, nst = " ++ show nst
    -- Check first for a TT entry of the position to search
    (hdeep, tp, hsc, e', nodes')
        <- if d >= minToRetr
              then reTrieve >> lift ttRead
              else return (-1, 0, 0, undefined, 0)
    -- tp == 1 => score >= hsc, so if hsc >= asco then we improved,
    --    but can we use hsc in PV? This score is not exact!
    --    Idea: return only if better than beta, else search for exact score
    -- tp == 0 => score <= hsc, so if hsc <= asco then we fail low and
    --    can terminate the search
    if useTTinPv && hdeep >= d && (
            tp == 2				-- exact score: always good
         || tp == 1 && hsc >= pathScore b	-- we will fail high
         || tp == 0 && hsc <= pathScore a	-- we will fail low
       )
       then  do
           let ttpath = Path { pathScore = hsc, pathDepth = hdeep,
                               pathMoves = Seq [e'], pathOrig = "TT" }
           reSucc nodes' >> return ttpath
       else do
           -- Use the found TT move as best move
           let nst' = if hdeep > 0 && (tp /= 0 || nullSeq (pvcont nst))
                         then nst { pvcont = Seq [e'] }
                         else nst
           edges <- genAndSort nst' a b d
           if noMove edges
              then do
                v <- lift staticVal
                viztreeScore $ "noMove: " ++ show v
                let !s = pathFromScore ("static: " ++ show v) v
                pindent $ "<= " ++ show s
                return s	-- shouldn't we ttStore this?
              else do
                nodes0 <- gets (sNodes . stats)
                -- Loop thru the moves
                let !nsti = nst0 { crtnt = PVNode, nxtnt = PVNode,
                                   cursc = a, pvcont = tailSeq (pvcont nst') }
                nstf <- pvSLoop b d False nsti edges
                let s = cursc nstf
                pindent $ "<= " ++ show s
                -- After pvSLoop ... we expect always that s >= a - this must be checked if it is so
                -- then it makes sense below to take bestPath when failed low (s == a)
                abrt' <- gets abort
                if abrt' || s > a
                   then return s
                   else do
                       -- here we failed low
                       let de = max d $ pathDepth s
                           es = unalt edges
                       when (de >= minToStore && not (null es)) $ do	-- what if es is null?
                           nodes1 <- gets (sNodes . stats)
                           -- store as upper score, and as move, the first one generated
                           lift $ do
                               let typ = 0
                                   !deltan = nodes1 - nodes0
                               ttStore de typ (pathScore a) (head es) deltan	-- should be d or de?
                       if movno nstf > 1
                           then return $! bestPath s a
                           else do
                               chk <- lift tacticalPos
                               let s' = if chk then matedPath else staleMate
                               return $! trimaxPath a b s'	-- shouldn't we ttStore this?

-- PV Zero Window
pvZeroW :: NodeState -> Path -> Int -> Int -> Bool -> Search Path
pvZeroW !_ !b !d !_ _ | d <= 0 = do
    -- Now that we moved the ttRead call under pvZeroW we are not prepared
    -- to handle correctly the case minToRetr = 0
    (v, ns) <- mustQSearch (pathScore bGrain) (pathScore b)
    when (minToStore == 0)
        $ lift $  ttStore 0 2 v (Move 0) ns
    let !esc = pathFromScore ("pvQSearch 21:" ++ show v) v
    pindent $ "<> " ++ show esc
    return esc
    where bGrain = b -: scoreGrain
pvZeroW !nst !b !d !lastnull redu = do
    pindent $ ":> " ++ show b
    -- Check if we have it in TT
    (hdeep, tp, hsc, e', nodes')
        <- if d >= minToRetr
              then reTrieve >> lift ttRead
              else return (-1, 0, 0, undefined, 0)
    let bsco = pathScore b
    if hdeep >= d && (tp == 2 || tp == 1 && hsc >= bsco || tp == 0 && hsc < bsco)
       then  do
           let ttpath = Path { pathScore = hsc, pathDepth = hdeep, pathMoves = Seq [e'], pathOrig = "TT" }
           reSucc nodes' >> return ttpath
       else do
           nmhigh <- nullEdgeFailsHigh nst b d lastnull
           abrt <- gets abort
           if abrt || nmhigh
              then do
                let !s = onlyScore b
                pindent $ "<= " ++ show s
                viztreeScore $ "nmhigh: " ++ show (pathScore s)
                return s
              else do
                -- Use the TT move as best move
                let nst' = if hdeep > 0 && (tp /= 0 || nullSeq (pvcont nst))
                              then nst { pvcont = Seq [e'] }
                              else nst
                edges <- genAndSort nst' bGrain b d
                if noMove edges
                   then do
                     v <- lift staticVal
                     viztreeScore $ "noMove: " ++ show v
                     let !s = pathFromScore ("static: " ++ show v) v
                     pindent $ "<= " ++ show s
                     return s	-- shouldn't we ttStore this?
                   else do
                     !nodes0 <- gets (sNodes . stats)
                     -- futility pruning?
                     prune <- if not futilActive
                                 then return False
                                 else isPruneFutil d bGrain	-- was a
                     -- Loop thru the moves
                     let !nsti = nst0 { crtnt = nxtnt nst', nxtnt = deepNodeType (nxtnt nst'),
                                        cursc = bGrain, pvcont = tailSeq (pvcont nst') }
                     nstf <- pvZLoop b d prune redu nsti edges
                     let s = cursc nstf
                     -- Here we expect bGrain <= s < b -- this must be checked
                     pindent $ "<: " ++ show s
                     let !de = max d $ pathDepth s
                         es = unalt edges
                     when (de >= minToStore && s < b && not (null es)) $ do	-- we failed low
                         !nodes1 <- gets (sNodes . stats)	-- what if es is null?
                         -- store as upper score, and as move the first one (generated)
                         lift $ do
                             let typ = 0
                                 !deltan = nodes1 - nodes0
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
    -- | not nulActivate || lastnull < 1 || d1 <= 0 = return False
    | not nulActivate || lastnull < 1 = return False	-- go smooth into QS
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
                      val <- fmap pnextlev $ pvZeroW nst { pvcont = emptySeq } negnma d1 lastnull1 True
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

pvZLoop :: Path -> Int -> Bool -> Bool -> NodeState -> Alt Move -> Search NodeState
pvZLoop b d p redu = go
    where go !s (Alt []) = return s
          go !s (Alt (e:es)) = do
              (!cut, !s') <- pvInnerLoopZ b d p s e redu
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
         exd <-  lift $ doMove False e False	-- do the move
         if legalResult exd
            then do
                nn <- newNode
                viztreeDown nn e
                modify $ \s -> s { absdp = absdp s + 1 }
                s <- case exd of
                         Exten exd' spc -> do
                           if prune && exd' == 0 && not spc -- don't prune special or extended
                              then return $! onlyScore $! cursc nst	-- prune, return a
                              else pvInnerLoopExten b d exd' nst
                         Final sco -> do
                             viztreeScore $ "Final: " ++ show sco
                             return $! pathFromScore "Final" (-sco)
                         Illegal -> error "Cannot be illegal here"
                lift undoMove	-- undo the move
                viztreeUp nn e (pathScore s)
                modify $ \s' -> s' { absdp = absdp old, usedext = usedext old }
                let s' = addToPath e s
                pindent $ "<- " ++ show e ++ " (" ++ show s' ++ ")"
                checkFailOrPVLoop (stats old) b d e s' nst
            else return (False, nst)

-- This part for the zero window search
pvInnerLoopZ :: Path 	-- current beta
            -> Int	-- current search depth
            -> Bool	-- prune?
            -> NodeState 	-- node status
            -> Move	-- move to search
            -> Bool	-- reduce in LMR?
            -> Search (Bool, NodeState)
pvInnerLoopZ b d prune nst e redu = do
    abrt <- timeToAbort
    if abrt
       then return (True, nst)
       else do
         old <- get
         pindent $ "-> " ++ show e
         exd <-  lift $ doMove False e False	-- do the move
         if legalResult exd
            then do
                nn <- newNode
                viztreeDown nn e
                modify $ \s -> s { absdp = absdp s + 1 }
                s <- case exd of
                         Exten exd' spc -> do
                           if prune && exd' == 0 && not spc -- don't prune special or extended
                              then return $! onlyScore $! cursc nst	-- prune, return a
                              else pvInnerLoopExtenZ b d spc exd' nst redu
                         Final sco -> do
                             viztreeScore $ "Final: " ++ show sco
                             return $! pathFromScore "Final" (-sco)
                         Illegal -> error "Cannot be illegal here"
                lift undoMove	-- undo the move
                viztreeUp nn e (pathScore s)
                modify $ \s' -> s' { absdp = absdp old, usedext = usedext old }
                let s' = addToPath e s
                pindent $ "<- " ++ show e ++ " (" ++ show s' ++ ")"
                checkFailOrPVLoopZ (stats old) b d e s' nst
            else return (False, nst)

reserveExtension :: Int -> Int -> Search Int
reserveExtension !uex !exd
    | uex >= maxDepthExt || exd == 0 = return 0
    | otherwise = do
        modify $ \s -> s { usedext = usedext s + exd }
        return exd

pvInnerLoopExten :: Path -> Int -> Int -> NodeState -> Search Path
pvInnerLoopExten b d !exd nst = do
    old <- get
    exd' <- reserveExtension (usedext old) exd
    -- late move reduction
    let !inPv = nxtnt nst == PVNode
        a = cursc nst
        !d1 = d + exd' - 1	-- this is the normal (unreduced) depth for next search
    pindent $ "depth " ++ show d ++ " nt " ++ show (nxtnt nst)
           ++ " exd' = " ++ show exd' ++ " mvn " ++ show (movno nst) ++ " next depth " ++ show d1
    let nega = negatePath a
        negb = negatePath b
    if inPv
       then do
          viztreeABD (pathScore negb) (pathScore nega) d1
          fmap pnextlev (pvSearch nst negb nega d1)
       else do
          let aGrain = nega -: scoreGrain
          -- Here we must be in a Cut node (will fail low)
          -- and we should have: crtnt = PVNode, nxtnt = CutNode
          viztreeABD (pathScore aGrain) (pathScore nega) d1
          !s1 <- fmap pnextlev (pvZeroW nst nega d1 nulMoves True)
          abrt <- gets abort
          if abrt || s1 <= a
             then return s1	-- failed low (as expected) or aborted
             else do
               -- we didn't fail low and need re-search: full window
               pindent $ "Research! (" ++ show s1 ++ ")"
               viztreeReSe
               viztreeABD (pathScore negb) (pathScore nega) d1
               let nst1 = if nullSeq (pathMoves s1)
                             then nst
                             else nst { pvcont = pathMoves s1 }
                   pvc  = pvcont nst1
                   nst2 = nst { nxtnt = PVNode, pvcont = pvc }
               fmap pnextlev (pvSearch nst2 negb nega d1)

-- For zero window
pvInnerLoopExtenZ :: Path -> Int -> Bool -> Int -> NodeState -> Bool -> Search Path
pvInnerLoopExtenZ b d spec !exd nst redu = do
    old  <- get
    exd' <- reserveExtension (usedext old) exd
    -- late move reduction
    let !d1 = d + exd' - 1	-- this is the normal (unreduced) depth for next search
        -- !d' = if redu && crtnt nst == AllNode
        !d' = if redu
                 then reduceLmr d1 (pnearmate b) spec exd (movno nst)
                 else d1
    pindent $ "depth " ++ show d ++ " nt " ++ show (nxtnt nst)
              ++ " exd' = " ++ show exd' ++ " mvn " ++ show (movno nst) ++ " next depth " ++ show d'
    let onemB = negatePath $ b -: scoreGrain
        negb  = negatePath b
    viztreeABD (pathScore negb) (pathScore onemB) d'
    if not redu || d' == d1
       then fmap pnextlev (pvZeroW nst onemB d' nulMoves redu)
       else do
           incRedu
           nds0 <- gets $ sNodes . stats
           !sr <- fmap pnextlev (pvZeroW nst onemB d' nulMoves True)
           nds1 <- gets $ sNodes . stats
           let nodre = nds1 - nds0
           !s1 <- if lmrDebug then fmap pnextlev (pvZeroW nst onemB d1 nulMoves False) else return sr
           nds2 <- gets $ sNodes . stats
           let nodnr = nds2 - nds1
           incReBe (nodnr - nodre)	-- so many nodes we spare by reducing
           when (sr < b && s1 >= b) $ do
               incReMi	-- LMR missed the point
               lift $ finNode "LMRM" True
           if sr < b	-- || d' >= d1
              then return sr	-- failed low (as expected), or not reduced
              else do
                -- was reduced and didn't fail low: re-search with full depth
                pindent $ "Research! (" ++ show sr ++ ")"
                viztreeReSe
                -- sts <- get
                -- let nds1 = sNodes $ stats sts
                incReSe nodre	-- so many nodes we wasted by reducing this time
                let nst1 = if nullSeq (pathMoves sr)
                              then nst
                              else nst { pvcont = pathMoves sr }
                viztreeABD (pathScore negb) (pathScore onemB) d1
                fmap pnextlev (pvZeroW nst1 onemB d1 nulMoves True)

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
              lift $ do
                  when (de >= minToStore) $ do
                      let typ = 1	-- best move is e and is beta cut (score is lower limit)
                      ttStore de typ (pathScore b) e nodes'
                  betaCut True (absdp sst) e -- anounce a beta move (for example, update history)
              incBeta mn
              -- when debug $ logmes $ "<-- pvInner: beta cut: " ++ show s ++ ", return " ++ show b
              let !csc = if s > b then combinePath s b else bestPath s b
              pindent $ "beta cut: " ++ show csc
              let !nst1 = nst { cursc = csc, pvcont = emptySeq }
              return (True, nst1)
            else do	-- means: > a && < b
              -- when (nxtnt nst == PVNode || de >= minToStore) $	-- why this || with node type?
              when (de >= minToStore) $	lift $ do
                  let typ = 2	-- score is exact
                  ttStore de typ (pathScore s) e nodes'
              let !nst1 = nst { cursc = s, nxtnt = nextNodeType (nxtnt nst),
                                movno = mn+1, pvcont = emptySeq }
              return (False, nst1)

-- For zero window
checkFailOrPVLoopZ :: SStats -> Path -> Int -> Move -> Path
                  -> NodeState -> Search (Bool, NodeState)
checkFailOrPVLoopZ xstats b d e s nst = do
    sst <- get
    let mn = movno nst
    if s < b	-- failed low
       then do
            -- when in a cut node and the move dissapointed - negative history - ???
            when (useNegHist && mn <= negHistMNo)
                 $ lift $ betaCut False (absdp sst) e
            !kill1 <- newKiller d s nst
            let !nst1 = nst { movno = mn+1, killer = kill1, pvcont = emptySeq }
            return (False, nst1)
       else do	-- here is s >= b: failed high
         let nodes0 = sNodes xstats
             nodes1 = sNodes $ stats sst
             nodes' = nodes1 - nodes0
             !de = max d $ pathDepth s
         lift $ do
             when (de >= minToStore) $ do
                 let typ = 1	-- best move is e and is beta cut (score is lower limit)
                 ttStore de typ (pathScore b) e nodes'
             betaCut True (absdp sst) e -- anounce a beta move (for example, update history)
         incBeta mn
         let !csc = if s > b then combinePath s b else bestPath s b
         pindent $ "beta cut: " ++ show csc
         let !nst1 = nst { cursc = csc, pvcont = emptySeq }
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
    {--
    path <- if useIID
               then bestMoveFromIID nst a b d	-- it will do nothing for AllNode
               else return []
    --}
    let path' = unseq $ pvcont nst
    path <- if null path' && useIID
               then bestMoveFromIID nst a b d	-- it will do nothing for AllNode
               else return path'		-- if not null
    adp <- gets absdp
    esp <- lift $ genMoves d adp True
    kl  <- lift $ filterM isMoveLegal $ killerToList (killer nst)
    let es = bestFirst path kl esp
    return $ Alt es

-- Late Move Reduction
{-# INLINE reduceLmr #-}
reduceLmr :: Int -> Bool -> Bool -> Int -> Int -> Int
reduceLmr !d nearmatea !spec !exd !w
    | not lmrActive || spec || exd > 0
        || d <= 1 || w <= lmrMvs1 || nearmatea = d
    | d <= 2 || w <= lmrMvs2 = d - 1
    | d <= 3 || w <= lmrMvs3 = d - 2
    | d <= 4 || w <= lmrMvs4 = d - 3
    | otherwise              = d - 4
    where lmrMvs1  =  5	-- unreduced quiet moves
          lmrMvs2  =  9	-- reduced by max 1 (2xprev-1)
          lmrMvs3  = 17	-- reduced by max 2
          lmrMvs4  = 33	-- reduced by max 3

{--
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
--}

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
              then do
                  lift $ finNode "MATE" False
                  return $! trimax a b stp
              else if c >= qsMaxChess
                      then do
                          viztreeScore $ "endless check: " ++ show inEndlessCheck
                          lift $ finNode "ENDL" False
                          return $! trimax a b inEndlessCheck
                      else do
                          -- for check extensions in case of very few moves (1 or 2):
                          -- if 1 move: search even deeper
                          -- if 2 moves: same depth
                          -- if 3 or more: no extension
                          let !esc = lenmax3 $ unalt edges
                              !nc = c + esc - 2
                          pvQLoop b nc a edges
       else if qsBetaCut && stp >= b
               then do
                   lift $ finNode "BETA" False
                   return b
               else if qsDeltaCut && stp + qsDelta < a
                      then do
                          lift $ finNode "DELT" False
                          return a
                      else do
                          edges <- liftM Alt $ lift genTactMoves
                          if noMove edges
                             then do
                                 lift $ finNode "NOCA" False
                                 return $! trimax a b stp
                             else if stp > a
                                     then pvQLoop b c stp edges
                                     else pvQLoop b c a   edges
    where lenmax3 = go 0
              where go n _ | n == 3 = 3
                    go n []         = n
                    go n (_:as)     = go (n+1) as

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
         r <-  lift $ doMove False e True
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
                             !sc <- pvQSearch (-b) (-a) c
                             modify $ \s -> s { absdp = absdp s - 1 }	-- no usedext here
                             return (-sc)
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

{-# INLINE bestMoveFromIID #-}
bestMoveFromIID :: NodeState -> Path -> Path -> Int -> Search [Move]
bestMoveFromIID nst a b d
    | nt == PVNode  && d >= minIIDPV
          = do s <- pvSearch nst a b d'
               return $! unseq $ pathMoves s
    | nt == CutNode && (d >= minIIDCut || (d >= minIIDCutNK && killer nst == NoKiller))
          = do s <- pvZeroW nst b d' nulMoves False
               return $! unseq $ pathMoves s
    | otherwise =  return []
    where d' = min maxIIDDepth (iidNewDepth d)
          nt = nxtnt nst

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
    lift $ do
       let !xst = stats s
       logmes $ "Search statistics after draft " ++ show (draft $ ronly s) ++ ":"
       logmes $ "Nodes: " ++ show (sNodes xst) ++ ", in QS: " ++ show (sNodesQS xst)
                  ++ ", retrieve: " ++ show (sRetr xst) ++ ", succes: " ++ show (sRSuc xst)
       let r = fromIntegral (sBMNo xst) / fromIntegral (sBeta xst) :: Double
       logmes $ "Beta cuts: " ++ show (sBeta xst) ++ ", beta factor: " ++ show r
       if lmrDebug
          then logmes $ "Reduced: " ++ show (sRedu xst) ++ ", Re-benefits: " ++ show (sReBe xst)
                 ++ ", ReSearchs: " ++ show (sReSe xst) ++ ", Re-waste: " ++ show (sReNo xst)
                 ++ ", missed: " ++ show (sReMi xst) ++ ", net benefit: "
                 ++ show (sReBe xst - sReNo xst)
          else logmes $ "Reduced: " ++ show (sRedu xst) ++ ", ReSearchs: " ++ show (sReSe xst)

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

incBeta :: Int -> Search ()
incBeta n = modStat $ \s -> s { sBeta = sBeta s + 1, sBMNo = sBMNo s + n }

incReSe :: Int -> Search ()
incReSe n = modStat $ \s -> s { sReSe = sReSe s + 1, sReNo = sReNo s + n }

incRedu :: Search ()
incRedu = modStat $ \s -> s { sRedu = sRedu s + 1 }

incReBe :: Int -> Search ()
incReBe n = modStat $ \s -> s { sReBe = sReBe s + n }

incReMi :: Search ()
incReMi = modStat $ \s -> s { sReMi = sReMi s + 1 }

pindent :: String -> Search ()
pindent = indentPassive

-- activate when used
-- qindent :: String -> Search ()
-- qindent = indentPassive

-- Uncomment this when using it above (pindent, qindent):
{--
indentActive :: String -> Search ()
indentActive s = do
    ad <- gets absdp
    lift $ informStr $ replicate ad ' ' ++ s
--}

indentPassive :: String -> Search ()
indentPassive _ = return ()

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
