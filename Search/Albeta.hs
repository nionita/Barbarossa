{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE BangPatterns #-}

module Search.Albeta (
    alphaBeta, logmes
) where

import Control.Monad
import Control.Monad.State hiding (gets, modify)
import Data.Array.Base (unsafeAt)
import Data.Array.Unboxed
import Data.Bits
import Data.Int
import Data.List (delete)
import Data.Maybe (fromMaybe)

import Search.CStateMonad
import Search.AlbetaTypes
import Struct.Struct
import Moves.BaseTypes
import Moves.Base
import Moves.Fen (initPos)

absurd :: String -> Game ()
absurd s = logmes $ "Absurd: " ++ s	-- used for messages when assertions fail

collectFens :: Bool
collectFens = True

-- Parameter for aspiration
useAspirWin :: Bool
useAspirWin = False

-- Some fix search parameter
scoreGrain, depthForCM, maxDepthExt, minPvDepth :: Int
useTTinPv :: Bool
scoreGrain  = 4	-- score granularity
depthForCM  = 7 -- from this depth inform current move
maxDepthExt = 3 -- maximum depth extension
useTTinPv   = False	-- retrieve from TT in PV?
minPvDepth  = 2		-- from this depth we use alpha beta search

-- Parameters for late move reduction:
lmrInitLv, lmrInitLim, lmrLevMin, lmrLevMax :: Int
lmrInitLv   = 7
lmrInitLim  = 8500
lmrLevMin   = 1
lmrLevMax   = 16

-- The late move reduction is variable and regulated by the number of re-searches
-- Lower levels (towards 0) means less reductions, higher - more
lmrArr :: UArray (Int, Int) Int
lmrArr = array ((lmrLevMin, 0), (lmrLevMax, 255))
               [ ((l, i), varImp (fromIntegral (1+lmrLevMax-l)) (fromIntegral i))
                    | l <- [lmrLevMin..lmrLevMax], i <- [0..255]]

-- Here b == 1 is not good, too sharp
varImp :: Double -> Double -> Int
varImp lev w = round $ go 0 lev w
    where go :: Double -> Double -> Double -> Double
          go !lv !b !i | i <= b    = lv
                       | otherwise = go (lv+1) (b*1.2) (i-b)

-- Parameters for futility pruning:
maxFutilDepth :: Int
maxFutilDepth = 3

-- Futility margins
futilMargins :: Int -> Int -> Int
futilMargins 1 s = s
futilMargins d s = s `unsafeShiftL` (d-1)

-- Score statistics parameters for variable futility
futIniVal, futMinVal, futDecayB, futDecayW :: Int
futIniVal = 100
futMinVal = 30
futDecayB = 13
futDecayW = (1 `unsafeShiftL` futDecayB) - 1

-- Parameters for quiescent search:
qsMaxChess :: Int
qsMaxChess = 2		-- max number of chess for a quiet search path

-- Parameters for null move pruning
nulMargin, nulSubmrg, nulTrig :: Int
nulMargin = 1		-- margin to search the null move (over beta) (in scoreGrain units!)
nulSubmrg = 2		-- improved margin (in scoreGrain units!)
nulTrig   = -15	-- static margin to beta, to trigger null move (in scoreGrain units!)
nulSubAct :: Bool
nulSubAct = True

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

-- Parameter for quiescenst search
inEndlessCheck, qsDeltaMargin :: Int
inEndlessCheck = -scoreGrain	-- there is a risk to be left in check
qsDeltaMargin  = 100

type Search a = CState PVState Game a

alpha0, beta0 :: Int
alpha0 = minBound + 2000
beta0  = maxBound - 2000

data Pvsl = Pvsl {
        pvPath :: Path,		-- pv path
        pvNodes :: !Int64	-- number of nodes in the current search
    } deriving Show

data Killer = NoKiller | OneKiller !Move | TwoKillers !Move !Move deriving (Eq, Show)

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
          abort   :: !Bool,	-- search aborted (time)
          futme   :: !Int,	-- variable futility score - me
          futyo   :: !Int,	-- variable futility score - you
          lmrhi   :: !Int,	-- upper limit of nodes to raise the lmr level
          lmrlv   :: !Int,	-- LMR level
          lmrrs   :: !Int	-- counter for nodes/re-searches, to adapt the LMR level
      } deriving Show

-- This is a state which reflects the status of alpha beta in a node while going through the edges
data NodeState
    = NSt {
          crtnt  :: !NodeType,	-- parent node type (actually expected)
          nxtnt  :: !NodeType,	-- expected child node type
          movno  :: !Int,	-- current move number
          spcno  :: !Int,	-- last move number of a special move
          albe   :: !Bool,	-- in alpha/beta search (for small depths)
          rbmch  :: !Int,	-- number of changes in root best move
          cursc  :: Path,	-- current alpha value (now plus path & depth)
          killer :: Killer,	-- the current killer moves
          cpos   :: MyPos,	-- current position for this node
          pvsl   :: [Pvsl]	-- principal variation list (at root) with node statistics
      } deriving Show

deepNSt :: NodeState -> NodeState
deepNSt nst = nst { crtnt = nxtnt nst, nxtnt = deepNodeType (nxtnt nst) }

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

pnearmate :: Path -> Bool
pnearmate = nearmate . pathScore

pnextlev :: Path -> Path
pnextlev p = p { pathScore = - pathScore p, pathOrig = "pnextlev (" ++ pathOrig p ++ ")" }

noMove :: Alt Move -> Bool
noMove (Alt es) = null es

nullSeq :: Seq Move -> Bool
nullSeq (Seq es) = null es

emptySeq :: Seq Move
emptySeq = Seq []

pvsInit :: PVState
pvsInit = PVState { ronly = pvro00, stats = ssts0, absdp = 0, usedext = 0, abort = False,
                    futme = futIniVal, futyo = futIniVal,
                    lmrhi = lmrInitLim, lmrlv = lmrInitLv, lmrrs = 0 }
nst0 :: NodeState
nst0 = NSt { crtnt = PVNode, nxtnt = PVNode, cursc = pathFromScore "Zero" 0, rbmch = -1,
             movno = 1, spcno = 1, killer = NoKiller, albe = False, cpos = initPos, pvsl = [] }
             -- we start with spcno = 1 as we consider the first move as special
             -- to avoid in any way reducing the tt move

resetStats :: Search SStats
resetStats = do
    st <- get
    put st { stats = ssts0 }
    return $ stats st

resetNSt :: Path -> Killer -> NodeState -> NodeState
resetNSt !sc !kill nst = nst { cursc = sc, movno = 1, spcno = 1, killer = kill }

pvro00 :: PVReadOnly
pvro00 = PVReadOnly { draft = 0, albest = False, timeli = False, abmili = 0 }

alphaBeta :: ABControl -> Game (Int, [Move], [Move], Bool, Int)
alphaBeta abc = do
    let !d = maxdepth abc
        rmvs = Alt $ rootmvs abc
        lpv  = Seq $ lastpv abc
        searchReduced a b = pvRootSearch a      b     d lpv rmvs True
        -- We have lastpath as a parameter here (can change after fail low or high)
        searchFull    lp  = pvRootSearch alpha0 beta0 d lp  rmvs False
        pvro = PVReadOnly { draft = d, albest = best abc,
                            timeli = stoptime abc /= 0, abmili = stoptime abc }
        pvs0 = pvsInit { ronly = pvro }	-- :: PVState
    r <- if useAspirWin
         then case lastscore abc of
             Just sp -> do
                let !alpha1 = sp - window abc
                    !beta1  = sp + window abc
                -- informStr $ "+++ Aspi search with d = " ++ show d
                --                ++ " alpha = " ++ show alpha1
                --                ++ " beta = " ++ show beta1
                -- aspirWin alpha1 beta1 d lpv rmvs aspTries
                r1@((s1, es1, _, _), pvsf)
                    <- runCState (searchReduced alpha1 beta1) pvs0
                if abort pvsf || (s1 > alpha1 && s1 < beta1 && not (nullSeq es1))
                    then return r1
                    else if nullSeq es1
                        then runCState (searchFull lpv) pvs0
                        else runCState (searchFull es1) pvs0
             Nothing -> runCState (searchFull lpv) pvs0
         else runCState (searchFull lpv) pvs0
    let timint = abort (snd r)
    -- when aborted, return the last found good move
    -- we have to trust that abort is never done in draft 1!
    case fst r of
        (s, Seq path, Alt rmvs', ch) -> if null path
           then return (fromMaybe 0 $ lastscore abc, lastpv abc, [], timint, 0)
           else return (s, path, rmvs', timint, ch)

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
             -> Search (Int, Seq Move, Alt Move, Int)
pvRootSearch a b d lastpath rmvs aspir = do
    pos <- lift getPos
    edges <- if null (unalt rmvs)	-- only when d==1, but we could have lastpath from the previous real move
                then genAndSort nst0 { cpos = pos } Nothing a b d	-- no IID here as d==1
                else case lastpath of
                         Seq []    -> return rmvs	-- does this happens? - check to simplify!
                         Seq (e:_) -> return $ Alt $ e : delete e (unalt rmvs)
    let !nsti = nst0 { cursc = pathFromScore "Alpha" a, cpos = pos }
    nstf <- pvLoop (pvInnerRoot b d) nsti edges
    abrt <- gets abort
    reportStats
    let (sc, pm) | d > 1             = (pathScore (cursc nstf), pathMoves (cursc nstf))
                 | ms:_ <- pvsl nstf = (pathScore $ pvPath ms,  pathMoves (pvPath ms))
                 | otherwise         = (a, emptySeq)
        p = unseq pm
    -- Root is pv node, cannot fail low, except when aspiration fails!
    if sc <= a	-- failed low or timeout when searching PV
         then do
           unless (abrt || aspir) $ lift $ informStr "Failed low at root??"
           return (a, emptySeq, edges, rbmch nstf)	-- just to permit aspiration to retry
         else do
            -- lift $ mapM_ (\m -> informStr $ "Root move: " ++ show m) (pvsl nstf)
            -- when (d < depthForCM) $ informPV sc d p
            let (best':_) = p
                allrmvs = if sc >= b then unalt edges else map pvslToMove (pvsl nstf)
                xrmvs = Alt $ best' : delete best' allrmvs	-- best on top
            return (sc, Seq p, xrmvs, rbmch nstf)

pvslToMove :: Pvsl -> Move
pvslToMove (Pvsl { pvPath = Path { pathMoves = Seq (m:_)}}) = m
pvslToMove _ = undefined	-- just for Wall

legalResult :: DoResult -> Bool
legalResult Illegal = False
legalResult _       = True

-- This is the inner loop of the PV search of the root, executed at root once per possible move
-- See the parameter
-- Returns: ...flag if it was a beta cut and new status
pvInnerRoot :: Int 	-- current beta
            -> Int	-- current search depth
            -> NodeState 	-- node status
            -> Move	-- move to search
            -> Search (Bool, NodeState)
pvInnerRoot b d nst e = timeToAbort (True, nst) $ do
         -- do the move
         exd <- lift $ doMove e
         if legalResult exd
            then do
                old <- get
                when (draft (ronly old) >= depthForCM) $ lift $ informCM e $ movno nst
                newNode d
                modify $ \s -> s { absdp = absdp s + 1 }
                s <- case exd of
                         Exten exd' spc -> do
                             when (exd' == 0 && not spc) $ do
                                 sdiff <- lift scoreDiff
                                 updateFutil sdiff	-- e
                             xchangeFutil
                             s <- pvInnerRootExten b d exd' (deepNSt nst)
                             xchangeFutil
                             return s
                         Final sco -> return $! pathFromScore "Final" (-sco)
                         Illegal   -> error "Cannot be illegal here"
                -- undo the move if it was legal
                lift undoMove
                modify $ \s' -> s' { absdp = absdp old, usedext = usedext old }
                let s' = addToPath e s
                checkFailOrPVRoot (stats old) b d e s' nst
            else return (False, nst)

pvInnerRootExten :: Int -> Int -> Int -> NodeState -> Search Path
pvInnerRootExten b d !exd nst = do
    old <- get
    exd' <- reserveExtension (usedext old) exd
    let !inPv = crtnt nst == PVNode
        !d1   = d + exd' - 1	-- this is the normal (unreduced) depth for the next search
        a     = pathScore $ cursc nst
    if inPv || d <= minPvDepth	-- search of principal variation
       then do
           -- Set albe only when not in PV and not already set (to spare a copy)
           let nst' = if not (inPv || albe nst) then nst { albe = True } else nst
           pnextlev <$> pvSearch nst' (-b) (-a) d1
       else do
           -- no futility pruning & no LMR for root moves!
           -- Here we expect to fail low
           s1 <- pnextlev <$> pvZeroW nst (-a) d1 True
           whenAbort s1 $ do
               checkFailHard "pvZeroW" a b (pathScore s1)
               if pathScore s1 <= a -- we failed low as expected
                  then return s1
                  else do
                      -- Here we didn't fail low and need re-search
                      -- As we don't reduce (beeing in a PV node), re-search is full window
                      lift $ logmes $ "Research: a = " ++ show a ++ ", s1 = " ++ show s1
                      let nst' = nst { crtnt = PVNode, nxtnt = PVNode }
                      pnextlev <$> pvSearch nst' (-b) (-a) d1

checkFailOrPVRoot :: SStats -> Int -> Int -> Move -> Path -> NodeState -> Search (Bool, NodeState)
checkFailOrPVRoot xstats b d e s nst = whenAbort (True, nst) $ do
         sst <- get
         let mn = movno nst
             a  = pathScore $ cursc nst
             !nodes0 = sNodes xstats + sRSuc xstats
             !nodes1 = sNodes (stats sst) + sRSuc (stats sst)
             !nodes' = nodes1 - nodes0
             pvg     = Pvsl s nodes'	-- the good
             de = max d $ pathDepth s
         if d == 1	-- we shouln't get abort here...
            then do
                 let typ = 2
                 lift $ ttStore de typ (pathScore s) e nodes'
                 let xpvslg = insertToPvs d pvg (pvsl nst)	-- the good
                 -- Do not count the changes in draft 1 (they were wrong anyway,
                 -- as we do not update cursc here and search all root moves)
                 --    rch | pathScore s > a = rbmch nst + 1
                 --        | otherwise       = rbmch nst
                 return (False, nst {movno = mn + 1, pvsl = xpvslg })
            else if pathScore s <= a
                    then do	-- failed low
                        let xpvslb = insertToPvs d pvg (pvsl nst)
                            nst1   = nst { movno = mn + 1, pvsl = xpvslb, killer = newKiller d s nst }
                        return (False, nst1)
                    else if pathScore s >= b
                            then do
                              -- what when a root move fails high? We are in aspiration
                              lift $ do
                                  let typ = 1	-- beta cut (score is lower limit) with move e
                                  ttStore de typ b e nodes'
                                  betaCut True (absdp sst) e
                              let xpvslg = insertToPvs d pvg (pvsl nst)	-- the good
                                  csc = s { pathScore = b }
                                  nst1 = nst { cursc = csc, pvsl = xpvslg, rbmch = rbmch nst + 1 }
                              return (True, nst1)
                            else do	-- means: > a && < b
                              let sc = pathScore s
                                  pa = unseq $ pathMoves s
                              informPV sc (draft $ ronly sst) pa
                              lift $ do
                                  let typ = 2	-- best move so far (score is exact)
                                  ttStore de typ sc e nodes'
                                  betaCut True (absdp sst) e	-- not really cut, but good move
                              let xpvslg = insertToPvs d pvg (pvsl nst)	-- the good
                                  nst1 = nst { cursc = s, nxtnt = nextNodeType (nxtnt nst),
                                               movno = mn + 1, pvsl = xpvslg, rbmch = rbmch nst + 1 }
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
    where betters = pathScore (pvPath p) >  pathScore (pvPath q)
          equals  = pathScore (pvPath p) == pathScore (pvPath q)
          equaln  = pvNodes p == pvNodes q
          bettern = pvNodes p > pvNodes q
          pmate   = pnearmate $ pvPath p
          qmate   = pnearmate $ pvPath q

checkFailHard :: String -> Int -> Int -> Int -> Search ()
checkFailHard s a b c =
    when (c < a || c > b) $ lift
        $ absurd $ "Fail not hard (" ++ s ++ "): " ++ show a ++ " / " ++ show c ++ " / " ++ show b

-- PV Search
pvSearch :: NodeState -> Int -> Int -> Int -> Search Path
pvSearch _ !a !b !d | d <= 0 = do
    v <- pvQSearch a b 0
    checkFailHard "QS" a b v
    return $ pathFromScore ("pvQSearch 1:" ++ show v) v	-- ok: fail hard in QS
pvSearch nst !a !b !d = do
    let !inPv = crtnt nst == PVNode
        ab    = albe nst
    -- Here we are always in PV if enough depth:
    when (not $ inPv || ab) $ lift $ absurd $ "pvSearch: not inPv, not ab, nst = " ++ show nst
    -- Check first for a TT entry of the position to search
    (hdeep, tp, hsc, e, nodes') <- reTrieve >> lift ttRead
    -- tp == 1 => score >= hsc, so if hsc >= asco then we improved,
    --    but can we use hsc in PV? This score is not exact!
    --    Idea: return only if better than beta, else search for exact score
    -- tp == 0 => score <= hsc, so if hsc <= asco then we fail low and
    --    can terminate the search
    if (useTTinPv || ab) && hdeep >= d && (
            tp == 2		-- exact score: always good
         || tp == 1 && hsc >= b	-- we will fail high
         || tp == 0 && hsc <= a	-- we will fail low
       )
       then do
           let ttpath = Path { pathScore = trimax a b hsc, pathDepth = hdeep,
                               pathMoves = Seq [e], pathOrig = "TT" }
           -- we will treat the beta cut here too, if it happens
           when (tp == 1 || tp == 2 && hsc > a) $ do
               adp <- gets absdp
               lift $ betaCut True adp e
           reSucc nodes' >> return ttpath
       else do
           when (hdeep < 0) reFail
           -- Here: when ab we should do null move search
           pos <- lift getPos
           -- Use the found TT move as best move
           let mttmv = if hdeep > 0 then Just e else Nothing
               nst'  = nst { cpos = pos }
           edges <- genAndSort nst' mttmv a b d
           if noMove edges
              then return $ trimaxPath a b $ if tacticalPos pos then matedPath else staleMate
              else do
                nodes0 <- gets (sNodes . stats)
                -- futility pruning:
                prune <- isPruneFutil d a True (staticScore pos)
                -- Loop thru the moves
                let !nsti = resetNSt (pathFromScore "low limit" a) NoKiller nst'
                nstf <- pvSLoop b d prune nsti edges
                let s = cursc nstf
                whenAbort s $ do
                    if pathScore s > a
                       then checkFailHard "pvSLoop improve" a b (pathScore s) >> return s
                       else if movno nstf > 1
                               then do
                                   -- here we failed low
                                   let de = max d $ pathDepth s
                                   nodes1 <- gets (sNodes . stats)
                                   -- store as upper score, and as move, the first one generated
                                   lift $ do
                                       let typ = 0
                                           !deltan = nodes1 - nodes0
                                           mv = head $ unalt edges	-- not null - we are on "else" of noMove
                                       ttStore de typ a mv deltan
                                   checkFailHard "pvSLoop low" a b (pathScore s)
                                   return s
                               else return $ trimaxPath a b $ if tacticalPos pos then matedPath else staleMate

-- PV Zero Window
pvZeroW :: NodeState -> Int -> Int -> Bool -> Search Path
pvZeroW !_ !b !d _ | d <= 0 = do
    v <- pvQSearch bGrain b 0
    checkFailHard "QS" bGrain b v
    return $ pathFromScore ("pvQSearch 21:" ++ show v) v
    where !bGrain = b - scoreGrain
pvZeroW !nst !b !d redu = do
    -- Check if we have it in TT
    (hdeep, tp, hsc, e, nodes') <- reTrieve >> lift ttRead
    if hdeep >= d && (tp == 2 || tp == 1 && hsc >= b || tp == 0 && hsc < b)
       then do
           let ttpath = Path { pathScore = trimax bGrain b hsc, pathDepth = hdeep,
                               pathMoves = Seq [e], pathOrig = "TT" }
           -- we will treat the beta cut here too, if it happens
           when (tp == 1 || tp == 2 && hsc >= b) $ do
               adp <- gets absdp
               lift $ betaCut True adp e
           reSucc nodes' >> return ttpath
       else do
           when (hdeep < 0) reFail
           pos <- lift getPos
           nmhigh <- nullMoveFailsHigh pos nst b d
           whenAbort (pathFromScore "Aborted" b) $ do
               case nmhigh of
                 NullMoveHigh -> return $ pathFromScore "NullMoveHigh" b
                 _ -> do
                   -- Use the TT move as best move
                   let mttmv = if hdeep > 0 then Just e else Nothing
                       nst' = nst { cpos = pos }
                   edges <- genAndSort nst' mttmv bGrain b d
                   if noMove edges
                      then return $ trimaxPath bGrain b $ if tacticalPos pos then matedPath else staleMate
                      else do
                        !nodes0 <- gets (sNodes . stats)
                        -- futility pruning:
                        prune <- isPruneFutil d bGrain False (staticScore pos)
                        -- Loop thru the moves
                        let kill1 = case nmhigh of
                                        NullMoveThreat s -> newTKiller pos d s
                                        _                -> NoKiller
                            !nsti = resetNSt (pathFromScore "low limit" bGrain) kill1 nst'
                        nstf <- pvZLoop b d prune redu nsti edges
                        let s = cursc nstf
                        whenAbort s $ do
                            checkFailHard "pvZLoop" bGrain b (pathScore s)
                            -- Here we expect bGrain <= s < b -- this must be checked
                            if pathScore s >= b
                               then return s	-- failed high
                               else if movno nstf > 1 -- we failed low
                                       then do
                                           let !de = max d $ pathDepth s
                                           !nodes1 <- gets (sNodes . stats)
                                           -- store as upper score, and as move the first one (generated)
                                           lift $ do
                                               let typ = 0
                                                   !deltan = nodes1 - nodes0
                                                   mv = head $ unalt edges	-- not null, we are in "else" of noMove
                                               ttStore de typ bGrain mv deltan
                                           return s
                                       else return $ trimaxPath bGrain b $ if tacticalPos pos then matedPath else staleMate
    where !bGrain = b - scoreGrain

data NullMoveResult = NoNullMove | NullMoveHigh | NullMoveLow | NullMoveThreat Path

nullMoveFailsHigh :: MyPos -> NodeState -> Int -> Int -> Search NullMoveResult
nullMoveFailsHigh pos nst b d
    | d < 2 || tacticalPos pos || zugZwang pos		-- no null move at d < 2
      || crtnt nst == AllNode = return NoNullMove	-- no null move in all nodes
    | otherwise = do
        let v = staticScore pos
        if v < b + nulTrig * scoreGrain
           then return NoNullMove
           else do
               when nulDebug $ incReBe 1
               lift doNullMove	-- do null move
               newNode d
               xchangeFutil
               let nst' = deepNSt nst
               val <- if v > b + bigDiff
                         then fmap pnextlev $ pvZeroW nst' (-nma) d2 True
                         else fmap pnextlev $ pvZeroW nst' (-nma) d1 True
               lift undoMove	-- undo null move
               xchangeFutil
               if pathScore val >= nmb
                  then return NullMoveHigh
                  else do
                       when nulDebug $ do
                           incReMi
                           when (not $ nullSeq (pathMoves val)) $ lift $ do
                               when collectFens $ finNode "NMLO" 0
                               logmes $ "fail low path: " ++ show val
                       if nullSeq (pathMoves val)
                          then return $ NullMoveLow
                          else return $ NullMoveThreat val
    where d1  = nmDArr1 `unsafeAt` d	-- here we have always d >= 1
          d2  = nmDArr2 `unsafeAt` d	-- this is for bigger differences
          nmb = if nulSubAct then b - (nulSubmrg * scoreGrain) else b
          nma = nmb - (nulMargin * scoreGrain)
          bigDiff = 500	-- if we are very far ahead

-- This is now more than reduction 3 for depth over 9
nmDArr1, nmDArr2 :: UArray Int Int
------------------------------0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16  17  18  19  20
nmDArr1 = listArray (0, 20) [ 0, 0, 0, 0, 0, 1, 2, 3, 4, 4, 5, 6, 7, 7, 8, 9, 9, 10, 11, 11, 12 ]
nmDArr2 = listArray (0, 20) [ 0, 0, 0, 0, 0, 0, 1, 2, 3, 3, 4, 5, 6, 6, 7, 8, 8,  9, 10, 10, 11 ]

pvSLoop :: Int -> Int -> Bool -> NodeState -> Alt Move -> Search NodeState
pvSLoop b d p = go
    where go !s (Alt []) = return s
          go !s (Alt (e:es)) = do
              (!cut, !s') <- pvInnerLoop b d p s e
              if cut then return s'
                     else go s' $ Alt es

pvZLoop :: Int -> Int -> Bool -> Bool -> NodeState -> Alt Move -> Search NodeState
pvZLoop b d p redu = go
    where go !s (Alt []) = return s
          go !s (Alt (e:es)) = do
              (!cut, !s') <- pvInnerLoopZ b d p s e redu
              if cut then return s'
                     else go s' $ Alt es

-- This is the inner loop of the PV search, executed at every level (except root) once per possible move
-- See the parameter
-- Returns: flag if it was a beta cut and new status
pvInnerLoop :: Int 	-- current beta
            -> Int	-- current search depth
            -> Bool	-- prune?
            -> NodeState 	-- node status
            -> Move	-- move to search
            -> Search (Bool, NodeState)
pvInnerLoop b d prune nst e = timeToAbort (True, nst) $ do
         -- What about TT & killer moves???
         if prune && movno nst > 1 && canPruneMove (cpos nst) e
            then do
                let !nst1 = nst { movno = movno nst + 1 }
                return (False, nst1)
            else do
                old <- get
                exd <- lift $ doMove e	-- do the move
                if legalResult exd
                   then do
                       newNode d
                       modify $ \s -> s { absdp = absdp s + 1 }
                       s <- case exd of
                           Exten exd' spc -> do
                               when (exd' == 0 && not spc) $ do	-- not quite ok here
                                   sdiff <- lift scoreDiff	-- cause spc has a slighty
                                   updateFutil sdiff	-- e	-- different meaning...
                               xchangeFutil
                               s <- pvInnerLoopExten b d exd' (deepNSt nst)
                               xchangeFutil
                               return s
                           Final sco -> return $! pathFromScore "Final" (-sco)
                           Illegal   -> error "Cannot be illegal here"
                       lift undoMove	-- undo the move
                       modify $ \s' -> s' { absdp = absdp old, usedext = usedext old }
                       let s' = addToPath e s
                       checkFailOrPVLoop (stats old) b d e s' nst
                   else return (False, nst)

-- This part for the zero window search
pvInnerLoopZ :: Int 	-- current beta
            -> Int	-- current search depth
            -> Bool	-- prune?
            -> NodeState 	-- node status
            -> Move	-- move to search
            -> Bool	-- reduce in LMR?
            -> Search (Bool, NodeState)
pvInnerLoopZ b d prune nst e redu = timeToAbort (True, nst) $ do
         -- What about TT & killer moves???
         if prune && canPruneMove (cpos nst) e
            then do
                let !nst1 = nst { movno = movno nst + 1 }
                return (False, nst1)
            else do
                old <- get
                exd <- lift $ doMove e	-- do the move
                -- even the legality could be checked before, maybe much cheaper
                if legalResult exd
                   then do
                       newNode d
                       modify $ \s -> s { absdp = absdp s + 1 }
                       s <- case exd of
                         Exten exd' spc -> do
                             if spc
                                then do
                                    xchangeFutil
                                    s <- pvInnerLoopExtenZ b d spc exd' (deepNSt $ resetSpc nst) redu
                                    xchangeFutil
                                    return s
                                else do
                                    when (exd' == 0) $ do
                                        sdiff <- lift scoreDiff
                                        updateFutil sdiff	-- e
                                    xchangeFutil
                                    s <- pvInnerLoopExtenZ b d spc exd' (deepNSt nst) redu
                                    xchangeFutil
                                    return s
                         Final sco -> return $! pathFromScore "Final" (-sco)
                         Illegal   -> error "Cannot be illegal here"
                       lift undoMove	-- undo the move
                       modify $ \s' -> s' { absdp = absdp old, usedext = usedext old }
                       let s' = addToPath e s
                       checkFailOrPVLoopZ (stats old) b d e s' nst
                   else return (False, nst)

resetSpc :: NodeState -> NodeState
resetSpc nst = nst { spcno = movno nst }

reserveExtension :: Int -> Int -> Search Int
reserveExtension !uex !exd
    | exd == 0 || uex >= maxDepthExt = return 0
    | otherwise = do
        modify $ \s -> s { usedext = usedext s + exd }
        return exd

pvInnerLoopExten :: Int -> Int -> Int -> NodeState -> Search Path
pvInnerLoopExten b d !exd nst = do
    old <- get
    exd' <- reserveExtension (usedext old) exd
    let !inPv = crtnt nst == PVNode
        !d1   = d + exd' - 1	-- this is the normal (unreduced) depth for next search
        a     = pathScore $ cursc nst
    if inPv || d <= minPvDepth
       then do
          -- Set albe only when not in PV and not already set (to spare a copy)
          let nst' = if not (inPv || albe nst) then nst { albe = True } else nst
          pnextlev <$> pvSearch nst' (-b) (-a) d1
       else do
          -- Here we must be in a Cut node (will fail low)
          -- and we should have: crtnt = CutNode, nxtnt = AllNode
          s1 <- pnextlev <$> pvZeroW nst (-a) d1 True
          whenAbort s1 $ do
              if pathScore s1 <= a
                 then return s1	-- failed low (as expected) or aborted
                 else do
                     -- we didn't fail low and need re-search: full window
                     let nst1 = nst { crtnt = PVNode, nxtnt = PVNode }
                     pnextlev <$> pvSearch nst1 (-b) (-a) d1

-- For zero window
pvInnerLoopExtenZ :: Int -> Int -> Bool -> Int -> NodeState -> Bool -> Search Path
pvInnerLoopExtenZ b d spec !exd nst redu = do
    old  <- get
    exd' <- reserveExtension (usedext old) exd
    -- late move reduction
    let !d1 = d + exd' - 1	-- this is the normal (unreduced) depth for next search
        !d' = if redu
                 then reduceLmr (nearmate b) spec d1 (lmrlv old) (movno nst - spcno nst)
                 else d1
    let !onemB = scoreGrain - b
    if not redu || d' == d1
       then do
           moreLMR True 1	-- more LMR
           pnextlev <$> pvZeroW nst onemB d' redu
       else do
           incRedu
           nds0 <- gets $ sNodes . stats
           !sr <- pnextlev <$> pvZeroW nst onemB d' True
           nds1 <- gets $ sNodes . stats
           let nodre = nds1 - nds0
           !s1 <- if lmrDebug
                     then pnextlev <$> pvZeroW nst onemB d1 False
                     else return sr
           nds2 <- gets $ sNodes . stats
           let nodnr = nds2 - nds1
           when lmrDebug $ do
               incReBe (nodnr - nodre)	-- so many nodes we spare by reducing
               when (pathScore sr < b && pathScore s1 >= b) $ do
                   incReMi	-- LMR missed the point
                   when collectFens $ lift $ finNode "LMRM" 0
           whenAbort sr $ do
               if pathScore sr < b
                  then do
                    moreLMR True 1	-- more LMR
                    return sr		-- failed low (as expected)
                  else do
                    -- was reduced and didn't fail low: re-search with full depth
                    incReSe nodre	-- so many nodes we wasted by reducing this time
                    moreLMR False d'	-- less LMR
                    -- Now we expect to fail high, i.e. exchange the crt/nxt node type
                    let nst1 = nst { crtnt = nxtnt nst, nxtnt = crtnt nst }
                    sf <- pnextlev <$> pvZeroW nst1 onemB d1 True
                    whenAbort sf $ do
                        when (pathScore sf >= b) $ moreLMR False d1
                        return sf

checkFailOrPVLoop :: SStats -> Int -> Int -> Move -> Path
                  -> NodeState -> Search (Bool, NodeState)
checkFailOrPVLoop xstats b d e s nst = whenAbort (True, nst) $ do
    sst <- get
    let mn = movno nst
    if pathScore s <= pathScore (cursc nst)
       then do
           let nst1 = nst { movno = mn+1, killer = newKiller d s nst }
           return (False, nst1)
       else do
           let nodes0 = sNodes xstats
               nodes1 = sNodes $ stats sst
               !nodes' = nodes1 - nodes0
               !de = max d $ pathDepth s
           if pathScore s >= b
              then do
                lift $ do
                    let typ = 1	-- best move is e and is beta cut (score is lower limit)
                    ttStore de typ b e nodes'
                    betaCut True (absdp sst) e -- anounce a beta move (for example, update history)
                incBeta mn
                let csc = s { pathScore = b }
                    nst1 = nst { cursc = csc }
                return (True, nst1)
              else do	-- means: > a && < b
                  lift $ do
                      let typ = 2	-- score is exact
                      ttStore de typ (pathScore s) e nodes'
                      betaCut True (absdp sst) e -- not really a cut, but good move here
                  let nst1 = nst { cursc = s, nxtnt = nextNodeType (nxtnt nst), movno = mn+1 }
                  return (False, nst1)

-- For zero window
checkFailOrPVLoopZ :: SStats -> Int -> Int -> Move -> Path
                  -> NodeState -> Search (Bool, NodeState)
checkFailOrPVLoopZ xstats b d e s nst = whenAbort (True, nst) $ do
    sst <- get
    let mn = movno nst
    if pathScore s < b	-- failed low
       then do
           let nst1 = nst { movno = mn+1, killer = newKiller d s nst }
           return (False, nst1)
       else do	-- here is s >= b: failed high
           let nodes0 = sNodes xstats
               nodes1 = sNodes $ stats sst
               nodes' = nodes1 - nodes0
               !de = max d $ pathDepth s
           lift $ do
               let typ = 1	-- best move is e and is beta cut (score is lower limit)
               ttStore de typ b e nodes'
               betaCut True (absdp sst) e -- anounce a beta move (for example, update history)
           incBeta mn
           let csc = s { pathScore = b }
               nst1 = nst { cursc = csc }
           return (True, nst1)

newKiller :: Int -> Path -> NodeState -> Killer
newKiller d s nst
    | d >= 2, (mm:km:_) <- unseq $ pathMoves s,
      isKillCand (cpos nst) mm km = pushKiller km (killer nst)
    | otherwise                   = killer nst

-- Same as newKiller, but the path begins with the killer move
-- as it is coming from null move search
-- It is called before a NSt reset, so no neet to consider
-- previous killer moves
newTKiller :: MyPos -> Int -> Path -> Killer
newTKiller pos d s
    | d >= 2, (km:_) <- unseq $ pathMoves s,
      isTKillCand pos km = OneKiller km
    | otherwise          = NoKiller

-- We don't sort the moves here, they have to come sorted from genMoves
-- But we consider the best move first (TT or IID) and the killers
genAndSort :: NodeState -> Maybe Move -> Int -> Int -> Int -> Search (Alt Move)
genAndSort nst mttmv a b d = do
    path <- case mttmv of
                Just mv -> return [mv]
                Nothing -> if useIID
                              then bestMoveFromIID nst a b d	-- it will do nothing for AllNode
                              else return []		-- if not null
    lift $ do
        let kl = filter (isMoveLegal (cpos nst)) $ killerToList (killer nst)
        esp <- genMoves d
        let es = bestFirst path kl esp
        return $ Alt es

-- Late Move Reduction
-- With a variable lmrlev the reduction should stay in a region
-- where the number of researches has an acceptable level
{-# INLINE reduceLmr #-}
reduceLmr :: Bool -> Bool -> Int -> Int -> Int -> Int
reduceLmr nearmatea spec d lmrlev w
    | spec || d <= 1 || nearmatea = d
    | otherwise                   = max 1 $ d - lmrArr!(lmrlev, w)

-- Adjust the LMR related parameters in the state
moreLMR :: Bool -> Int -> Search ()
moreLMR more !d = do
    s <- get
    let !i  | more      = 1
            | otherwise = - (1 `unsafeShiftL` d)
        !i1 = lmrrs s + i
    if i1 < 0
       then if lmrlv s <= lmrLevMin
               then put s { lmrhi = find (lmrhi s), lmrrs = 0 }
               else put s { lmrlv = lmrlv s - 1, lmrrs = 0 }
       else if i1 > lmrhi s
               then if lmrlv s >= lmrLevMax
                       then put s { lmrhi = fdir (lmrhi s), lmrrs = 0 }
                       else put s { lmrlv = lmrlv s + 1, lmrrs = 0 }
               else put s { lmrrs = i1 }
    where fdir x = x `unsafeShiftL` 2
          find x = max 1 $ x `unsafeShiftR` 1

-- This is a kind of monadic fold optimized for (beta) cut
pvLoop :: Monad m => (s -> e -> m (Bool, s)) -> s -> Alt e -> m s
pvLoop _ s (Alt [])     = return s
pvLoop f s (Alt (e:es)) = do
    (cut, s') <- f s e
    if cut then return s'
           else pvLoop f s' $ Alt es

-- About futility pruning:
-- A. 3 versions would be possible:
--    - material difference
--    - static evaluation (as now)
--    - result after QS
--    Material would be very cheap, but risky, unless we know material ~ static val (statistic)
--    QS looks to be too expensive, but maybe it can be done for higher depths
--    Experiemnts should be done
-- B. When we are in check, and also much below alpha, we have even less chances to come out,
--    so it is ok to not exclude here check escapes, and maybe we should even make the margin
--    lower (experiemnts, tune!) Maybe this is also depth dependent
isPruneFutil :: Int -> Int -> Bool -> Int -> Search Bool
isPruneFutil d a pv v
    | nearmate a              = return False
    | pv && d > maxFutilDepth = return False
    | d > maxFutilDepth + 1   = return False	-- for zero window searches we allow higher futility depth
    | otherwise = do
        m <- varFutVal	-- variable futility value
        return $! v + futilMargins d m <= a

updateFutil :: Int -> Search ()
updateFutil sd = do
    s <- get
    let !so = futme s
    put s { futme = expFutSlide so sd }

expFutSlide :: Int -> Int -> Int
expFutSlide o n = (o * futDecayW + max 0 n) `unsafeShiftR` futDecayB

xchangeFutil :: Search ()
xchangeFutil
    = modify $ \s -> s { futme = futyo s, futyo = futme s }

varFutVal :: Search Int
varFutVal = max futMinVal <$> gets futme

trimaxPath :: Int -> Int -> Path -> Path
trimaxPath a b x = x { pathScore = trimax a b (pathScore x) }

trimax :: Int -> Int -> Int -> Int
trimax a b x
    | x < a     = a
    | x > b     = b
    | otherwise = x

-- PV Quiescent Search
pvQSearch :: Int -> Int -> Int -> Search Int
pvQSearch !a !b !c = do
    -- TODO: use e as first move if legal & capture
    -- (hdeep, tp, hsc, e, _) <- reTrieve >> lift ttRead
    (hdeep, tp, hsc, _, _) <- reTrieve >> lift ttRead
    -- tp == 1 => score >= hsc, so if hsc > a then we improved
    -- tp == 0 => score <= hsc, so if hsc <= asco then we fail low and
    --    can terminate the search
    if hdeep >= 0 && (
            tp == 2		-- exact score: always good
         || tp == 1 && hsc >= b	-- we will fail high
         || tp == 0 && hsc <= a	-- we will fail low
       )
       then reSucc 1 >> return (trimax a b hsc)
       else do
           -- TODO: use hsc here too, when possible
           when (hdeep < 0) reFail
           pos <- lift $ getPos
           if tacticalPos pos
              then do
                  edges <- Alt <$> lift genEscapeMoves
                  if noMove edges
                     then return $! trimax a b (-mateScore)
                     else if c >= qsMaxChess
                             then do
                                 when collectFens $ finWithNodes "ENDL"
                                 return $! trimax a b inEndlessCheck
                             else do
                                 -- for check extensions in case of very few moves (1 or 2):
                                 -- if 1 move: extend 1 (same depth)
                                 -- if 2 moves: no extension
                                 let !esc = lenmax2 $ unalt edges
                                     !nc = c + esc - 1
                                 pvQLoop b nc a edges
              else do
                  let !stp = staticScore pos
                  -- what if hsc < b?
                  if stp >= b
                     then do
                         when collectFens $ finWithNodes "BETA"
                         return b
                     else do
                         !qsdelta <- lift qsDelta
                         let !a1 = a - qsdelta - qsDeltaMargin
                         -- what if hsc + ... > a?
                         if stp < a1
                             then do
                                 when collectFens $ finWithNodes "DELT"
                                 return a
                             else do
                                 edges <- Alt <$> lift genTactMoves
                                 if noMove edges
                                    then do	-- no more captures
                                        when collectFens $ finWithNodes "NOCA"
                                        return $! trimax a b stp
                                    else if stp > a
                                            then pvQLoop b c stp edges
                                            else pvQLoop b c a   edges
    where lenmax2 (_:_:_) = 2
          lenmax2 _       = 1	-- we know here it is not empty

pvQLoop :: Int -> Int -> Int -> Alt Move -> Search Int
pvQLoop b c = go
    where go !s (Alt [])     = return s
          go !s (Alt (e:es)) = do
              (!cut, !s') <- pvQInnerLoop b c s e
              if cut then return s'
                     else go s' $ Alt es

pvQInnerLoop :: Int -> Int -> Int -> Move -> Search (Bool, Int)
pvQInnerLoop !b c !a e = timeToAbort (True, b) $ do
         r <- lift $ doQSMove e
         if legalResult r
            then do
                newNodeQS
                !sc <- negate <$> pvQSearch (-b) (-a) c
                lift undoMove
                if sc >= b
                   then return (True, b)
                   else if sc > a
                           then return (False, sc)
                           else return (False, a)
            else return (False, a)

{-# INLINE finWithNodes #-}
finWithNodes :: String -> Search ()
finWithNodes s = do
    n <- gets $ sNodes . stats
    lift $ finNode s n

{-# INLINE bestMoveFromIID #-}
bestMoveFromIID :: NodeState -> Int -> Int -> Int -> Search [Move]
bestMoveFromIID nst a b d
    | nt == PVNode  && d >= minIIDPV
          = do s <- pvSearch nst a b d'
               return $! unseq $ pathMoves s
    | nt == CutNode && (d >= minIIDCut || (d >= minIIDCutNK && killer nst == NoKiller))
          = do s <- pvZeroW nst b d' False
               return $! unseq $ pathMoves s
    | otherwise =  return []
    where d' = min maxIIDDepth (iidNewDepth d)
          nt = crtnt nst

{-# INLINE timeToAbort #-}
timeToAbort :: a -> Search a -> Search a
timeToAbort a act = do
    s <- get
    if abort s
       then return a
       else do
           let ro = ronly s
           if draft ro > 1 && timeli ro
              then if timeNodes .&. sNodes (stats s) /= 0
                      then act
                      else do
                          !abrt <- lift $ isTimeout $ abmili ro
                          if abrt
                             then do
                                 lift $ informStr "Albeta: search abort!"
                                 put s { abort = True }
                                 return a
                             else act
              else act
    where timeNodes = 4 * 1024 - 1	-- check time every so many nodes

{-# INLINE whenAbort #-}
whenAbort :: a -> Search a -> Search a
whenAbort a act = do
    !abrt <- gets abort
    if abrt then return a else act

{-# INLINE reportStats #-}
reportStats :: Search ()
reportStats = do
    s <- get
    lift $ do
       let dst = stats s
       draftStats dst
       logmes $ "Search statistics after draft " ++ show (draft $ ronly s) ++ ":"
       mapM_ logmes $ formatStats dst
       logmes $ "Variable futility params: me = " ++ show (futme s) ++ ", yo = " ++ show (futyo s)
       logmes $ "Variable LMR: hi = " ++ show (lmrhi s)
                    ++ ", lv = " ++ show (lmrlv s) ++ ", qu = " ++ show (lmrrs s)

-- Functions to keep statistics
modStat :: (SStats -> SStats) -> Search ()
modStat f = modify $ \s -> case f (stats s) of st -> s { stats = st }

incNodes :: Int -> SStats -> SStats
incNodes 1 s = incNodesQS s	-- d can't be < 0 here
incNodes _ s = case sNodes s + 1 of n1 -> s { sNodes = n1 }

incNodesQS :: SStats -> SStats
incNodesQS s = case sNodes s + 1 of
                 n1 -> case sNodesQS s + 1 of n2 -> s { sNodes = n1, sNodesQS = n2 }

incReTrieve :: SStats -> SStats
incReTrieve s = case sRetr s + 1 of n1 -> s { sRetr = n1 }

incReFail :: SStats -> SStats
incReFail s = case sRFal s + 1 of n1 -> s { sRFal = n1 }

addReSucc :: Int64 -> SStats -> SStats
addReSucc n s = s { sRFnd = sRFnd s + 1, sRSuc = sRSuc s + n }

newNode :: Int -> Search ()
newNode   = modStat . incNodes

newNodeQS :: Search ()
newNodeQS = modStat incNodesQS

reTrieve :: Search ()
reTrieve  = modStat incReTrieve

reFail :: Search ()
reFail = modStat incReFail

reSucc :: Int64 -> Search ()
reSucc n  = modStat (addReSucc n)

incBeta :: Int -> Search ()
incBeta 1 = modStat $ \s -> s { sBeta = sBeta s + 1, sBM1  = sBM1  s + 1 }
incBeta 2 = modStat $ \s -> s { sBeta = sBeta s + 1, sBM2  = sBM2  s + 1 }
incBeta 3 = modStat $ \s -> s { sBeta = sBeta s + 1, sBM3  = sBM3  s + 1 }
incBeta _ = modStat $ \s -> s { sBeta = sBeta s + 1, sBM4p = sBM4p s + 1 }

incReSe :: Int64 -> Search ()
incReSe n = modStat $ \s -> s { sReSe = sReSe s + 1, sReNo = sReNo s + n }

incRedu :: Search ()
incRedu = modStat $ \s -> s { sRedu = sRedu s + 1 }

incReBe :: Int64 -> Search ()
incReBe n = modStat $ \s -> s { sReBe = sReBe s + n }

incReMi :: Search ()
incReMi = modStat $ \s -> s { sReMi = sReMi s + 1 }

{-# SPECIALIZE bestFirst :: [Move] -> [Move] -> ([Move], [Move]) -> [Move] #-}
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
informCM :: Move -> Int -> Game ()
informCM a b = informCtx (CurrMv a b)

informStr :: String -> Game ()
informStr s = informCtx (InfoStr s)

logmes :: String -> Game ()
logmes s = informCtx (LogMes s)

informPV :: Int -> Int -> [Move] -> Search ()
informPV s d es = do
    dst <- resetStats
    lift $ do
        draftStats dst
        n <- curNodes
        informCtx (BestMv s d n es)
