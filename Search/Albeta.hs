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
scoreGrain, depthForCM, minPvDepth :: Int
scoreGrain  = 4	-- score granularity
depthForCM  = 8 -- from this depth inform current move
minPvDepth  = 2	-- from this depth we use pv search
useTTinPv :: Bool
useTTinPv   = False	-- retrieve from TT in PV?

-- Parameter for killer moves
maxKillers :: Int
maxKillers = 3

-- Parameters for late move reduction:
lmrInitLv, lmrInitLim, lmrLevMin, lmrLevMax, lmrNoDepth :: Int
lmrInitLv  = 8
lmrInitLim = 8500
lmrLevMin  = 0
lmrLevMax  = 15
lmrNoDepth = 1

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
maxFutilDepth = 5

-- Futility margins
futilMargins :: Int -> Int
futilMargins d = futMinVal `unsafeShiftL` (d-1)	-- this is ok also for d == 1

-- Parameters for futility pruning
futMinVal :: Int
futMinVal = 24

-- Parameters for null move pruning
nulMargin, nulSubmrg, nulTrig :: Int
nulMargin = 1		-- margin to search the null move (over beta) (in scoreGrain units!)
nulSubmrg = 2		-- improved margin (in scoreGrain units!)
nulTrig   = -15	-- static margin to beta, to trigger null move (in scoreGrain units!)
nulSubAct :: Bool
nulSubAct = True

-- Parameters for internal iterative deepening
minIIDPV, minIIDCut, minIIDCutNK, maxIIDDepth :: Int
minIIDPV    = 5
minIIDCutNK = 6
minIIDCut   = 8
maxIIDDepth = 2

iidNewDepth :: Int -> Int
iidNewDepth = subtract 1

-- Parameter for quiescenst search
qsDeltaMargin :: Int
qsDeltaMargin  = 100

type Search a = CState PVState Game a

alpha0, beta0 :: Int
alpha0 = minBound + 2000
beta0  = maxBound - 2000

data Pvsl = Pvsl {
        pvPath :: Path,		-- pv path
        pvNodes :: !Int64	-- number of nodes in the current search
    } deriving Show

newtype Killer = Killer [Move] deriving Show

-- Read only parameters of the search, so that we can change them programatically
data PVStRO
    = PVStRO {
          draft  :: !Int,	-- root search depth
          albest :: !Bool,	-- always choose the best move (i.e. first)
          abmil1 :: !Int,	-- abort after this millisecond when in first root move
          abmili :: !Int	-- abort after this millisecond when from second root move
    } deriving Show

data PVState
    = PVState {
          ronly   :: PVStRO,	-- read only parameters
          stats   :: SStats,	-- search statistics
          absdp   :: !Int,	-- absolute depth (root = 0)
          maxdp   :: !Int,	-- maximum reached depth (>= absdp)
          abort   :: !Bool,	-- search aborted (time)
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
          rbmch  :: !Int,	-- number of changes in root best move / score type otherwise
          killer :: !Killer,	-- the current killer moves
          cursc  :: Path,	-- current alpha value (now plus path & depth)
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
         pathMoves :: Seq Move
      } deriving Show

mated :: Int
mated = - mateScore
drawPath, matedPath :: Path
drawPath  = Path { pathScore = 0, pathDepth = 20, pathMoves = Seq [] }
matedPath = Path { pathScore = mated, pathDepth = 20, pathMoves = Seq [] }

-- Making a path from a plain score:
pathFromScore :: Int -> Path
pathFromScore s = Path { pathScore = s, pathDepth = 0, pathMoves = Seq [] }

-- Add a move to a path:
addToPath :: Move -> Path -> Path
addToPath e p = p { pathDepth = pathDepth p + 1, pathMoves = Seq $ e : unseq (pathMoves p) }

pnearmate :: Path -> Bool
pnearmate = nearmate . pathScore

pnextlev :: Path -> Path
pnextlev p = p { pathScore = - pathScore p }

noMove :: Alt Move -> Bool
noMove (Alt es) = null es

nullSeq :: Seq Move -> Bool
nullSeq (Seq es) = null es

emptySeq :: Seq Move
emptySeq = Seq []

pvsInit :: PVState
pvsInit = PVState { ronly = pvro00, stats = ssts0, absdp = 0, maxdp = 0, abort = False,
                    lmrhi = lmrInitLim, lmrlv = lmrInitLv, lmrrs = 0 }
nst0 :: NodeState
nst0 = NSt { crtnt = PVNode, nxtnt = PVNode, cursc = pathFromScore 0, rbmch = -1,
             movno = 1, spcno = 1, killer = Killer [], albe = False, cpos = initPos, pvsl = [] }
             -- we start with spcno = 1 as we consider the first move as special
             -- to avoid in any way reducing the tt move

resetNSt :: Path -> Killer -> NodeState -> NodeState
resetNSt !sc kill nst = nst { cursc = sc, movno = 1, spcno = 1, killer = kill, rbmch = 0 }

pvro00 :: PVStRO
pvro00 = PVStRO { draft = 0, albest = False, abmil1 = 0, abmili = 0 }

alphaBeta :: ABControl -> Game (Int, [Move], [Move], Bool, Int, Int)
alphaBeta abc = do
    let !d = maxdepth abc
        rmvs = Alt $ rootmvs abc
        lpv  = Seq $ lastpv abc
        searchReduced a b = pvRootSearch a      b     d lpv rmvs True
        -- We have lastpath as a parameter here (can change after fail low or high)
        searchFull    lp  = pvRootSearch alpha0 beta0 d lp  rmvs False
        pvro = PVStRO { draft = d, albest = best abc,
                            abmil1 = stoptime1 abc, abmili = stoptime abc }
        pvs0 = pvsInit { ronly = pvro }	-- :: PVState
    -- We will get a result and a final state:
    (r, s) <- if useAspirWin
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
    let timint   = abort s
        seldepth = maxdp s
    -- When aborted, return the last found good move
    -- We have to trust that abort is never done in draft 1!
    case r of (sc, Seq path, Alt rmvs', ch) -> if null path
                  then return (fromMaybe 0 $ lastscore abc, lastpv abc, [], timint, 0, 0)
                  else return (sc, path, rmvs', timint, ch, seldepth)

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
                then genAndPick nst0 { cpos = pos } Nothing a b d	-- no IID here as d==1
                else case lastpath of
                         Seq []    -> return rmvs	-- does this happen? - check to simplify!
                         Seq (e:_) -> return $ Alt $ e : delete e (unalt rmvs)
    let !nsti = nst0 { cursc = pathFromScore a, cpos = pos }
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
                let adp = absdp old + 1
                    mdp = max (maxdp old) adp
                modify $ \s -> s { absdp = adp, maxdp = mdp }
                s <- case exd of
                         Exten exd' _ _ -> pvInnerRootExten b d exd' (deepNSt nst)
                         Final   -> return drawPath
                         Illegal -> error "Cannot be illegal here"
                -- undo the move if it was legal
                lift undoMove
                lift incrementRootMoveNumber
                modify $ \s' -> s' { absdp = absdp old }
                let s' = addToPath e s
                checkFailOrPVRoot (stats old) b d e s' nst
            else return (False, nst)

pvInnerRootExten :: Int -> Int -> Int -> NodeState -> Search Path
pvInnerRootExten b d !exd nst = do
    let !pvnode = crtnt nst == PVNode
        !d1   = d + exd - 1	-- this is the normal (unreduced) depth for the next search
        a     = pathScore $ cursc nst
    if pvnode || d <= minPvDepth	-- search of principal variation
       then do
           -- Set albe only when not in PV and not already set (to spare a copy)
           let nst' = if not (pvnode || albe nst) then nst { albe = True } else nst
           pnextlev <$> pvSearch nst' (-b) (-a) d1
       else do
           -- no futility pruning & no LMR for root moves!
           -- Here we expect to fail low
           s1 <- pnextlev <$> pvZeroW nst (-a) d1
           whenAbort s1 $
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
        -- de = max d $ pathDepth s
    -- Treat all depth the same
    -- This also means, we do not have a known score for every root move
    if pathScore s <= a
       then do	-- failed low
           let xpvslb = insertToPvs d pvg (pvsl nst)
               nst1   = nst { movno = mn + 1, pvsl = xpvslb, killer = newKiller d s nst }
           return (False, nst1)
       else if pathScore s >= b
               then do
                 -- what when a root move fails high? We must be in aspiration
                 lift $ do
                     let typ = 1	-- beta cut (score is lower limit) with move e
                     ttStore d typ b e nodes'
                     betaCut (absdp sst) e
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
                     ttStore d typ sc e nodes'
                     betaCut (absdp sst) e	-- not really cut, but good move
                 let xpvslg = insertToPvs d pvg (pvsl nst)	-- the good
                     nst1 = nst { cursc = s, nxtnt = nextNodeType (nxtnt nst),
                                  movno = mn + 1, pvsl = xpvslg, rbmch = rbmch nst + 1 }
                 return (False, nst1)

insertToPvs :: Int -> Pvsl -> [Pvsl] -> [Pvsl]
insertToPvs _ p [] = [p]
insertToPvs d p ps@(q:qs)
    | d <= dscore && betters       = p : ps
    | pmate && not qmate           = p : ps
    | not pmate && qmate           = q : insertToPvs d p qs
    | pmate && betters             = p : ps
    | bettern || equaln && betters = p : ps
    | otherwise                    = q : insertToPvs d p qs
    where betters = pathScore (pvPath p) >  pathScore (pvPath q)
          equaln  = pvNodes p == pvNodes q
          bettern = pvNodes p > pvNodes q
          pmate   = pnearmate $ pvPath p
          qmate   = pnearmate $ pvPath q
          dscore  = 1

-- PV Search
pvSearch :: NodeState -> Int -> Int -> Int -> Search Path
pvSearch _ !a !b !d | d <= 0 = do
    v <- pvQSearch a b
    return $! pathFromScore v	-- ok: fail hard in QS
pvSearch nst !a !b !d = do
    let pvnode = crtnt nst == PVNode
    -- Here we are always in PV if enough depth:
    when (not $ pvnode || albe nst) $ lift $ absurd $ "pvSearch: not pvnode, not albe, nst = " ++ show nst
    -- Check first for a TT entry of the position to search
    (hdeep, tp, hsc, e, nodes') <- reTrieve >> lift ttRead
    -- Here we search for an exact score, either by alpha/beta search in the first few depths,
    -- or by PV search, in all the higher depths. Looking in the TT we can get 3 cases:
    -- tp == 2 => we got an exact score with the right depth: use it (return)
    -- tp == 1 => score >= hsc, so if hsc >= a then we improved,
    --    but we can't we use hsc in PV: the score is not exact!
    --    So we use it (return) only if better than beta; otherwise search for exact score
    -- tp == 0 => score <= hsc, so if hsc <= a then we fail low and
    --    can terminate the search
    if hdeep >= d && (useTTinPv || albe nst) && (
            tp == 2		-- exact score: always good
         || tp == 1 && hsc >= b	-- we will fail high
         || tp == 0 && hsc <= a	-- we will fail low
       )
       then do
           let !ttpath = Path { pathScore = trimax a b hsc, pathDepth = hdeep, pathMoves = Seq [e] }
           -- we will treat the beta cut here too, if it happens
           when (tp == 1 || tp == 2 && hsc > a) $ do
               adp <- gets absdp
               lift $ betaCut adp e
           reSucc nodes' >> return ttpath
       else do
           when (hdeep < 0) reFail
           -- TODO: when albe, we should do null move search
           pos <- lift getPos
           -- Use the found TT move as best move
           let mttmv = if hdeep > 0 then Just e else Nothing
               !nst'  = nst { cpos = pos }
           edges <- genAndPick nst' mttmv a b d
           if noMove edges
              then return $! failHardNoValidMove a b pos
              else do
                nodes0 <- gets (sNodes . stats)
                -- Here we could maybe raise alpha when we got tp == 1 from TT, but with hsc < b:
                -- if hsc > a then we know there must be something better than a, so we could search for it
                -- If we do so, we should put a' = hsc - scoreGrain, so that we get at least
                -- one variation. But the question is: is it possible that we don't find one?
                -- And if yes: what to do in that case?
                -- Futility pruning:
                let !prune = isPruneFutil d a True (staticScore pos)
                    !nsti  = resetNSt (pathFromScore a) (Killer []) nst'
                -- Loop thru the moves
                !nstf <- pvSLoop b d False prune nsti edges
                let s = cursc nstf
                whenAbort s $
                    if movno nstf == 1
                       then return $! failHardNoValidMove a b pos
                       else do
                           -- let de = max d $ pathDepth s
                           nodes1 <- gets (sNodes . stats)
                           lift $ do
                               let !deltan = nodes1 - nodes0
                                   mvs = pathMoves s
                                   mv | nullSeq mvs = head $ unalt edges	-- not null - on "else" of noMove
                                      | otherwise   = head $ unseq mvs
                               ttStore d (rbmch nstf) (pathScore s) mv deltan
                           return s

-- PV Zero Window
pvZeroW :: NodeState -> Int -> Int -> Search Path
pvZeroW !_ !b !d | d <= 0 = do
    v <- pvQSearch bGrain b
    return $! pathFromScore v
    where !bGrain = b - scoreGrain
pvZeroW !nst !b !d = do
    -- Check if we have it in TT
    (hdeep, tp, hsc, e, nodes') <- reTrieve >> lift ttRead
    if hdeep >= d && (tp == 2 || tp == 1 && hsc >= b || tp == 0 && hsc < b)
       then do
           let !ttpath = Path { pathScore = trimax bGrain b hsc, pathDepth = hdeep, pathMoves = Seq [e] }
           -- we will treat the beta cut here too, if it happens
           when (tp == 1 || tp == 2 && hsc >= b) $ do
               adp <- gets absdp
               lift $ betaCut adp e
           reSucc nodes' >> return ttpath
       else do
           when (hdeep < 0) reFail
           pos <- lift getPos
           !nmhigh <- nullMoveFailsHigh pos nst b d
           whenAbort (pathFromScore b) $ do
               case nmhigh of
                 NullMoveHigh -> return $ pathFromScore b
                 _ -> do
                   -- Use the TT move as best move
                   let mttmv = if hdeep > 0 then Just e else Nothing
                       !nst' = nst { cpos = pos }
                   edges <- genAndPick nst' mttmv bGrain b d
                   if noMove edges
                      then return $! failHardNoValidMove bGrain b pos
                      else do
                        !nodes0 <- gets (sNodes . stats)
                        -- futility pruning:
                        let !prune = isPruneFutil d bGrain False (staticScore pos)
                        -- Loop thru the moves
                        let kill1 = case nmhigh of
                                        NullMoveThreat s -> newTKiller pos d s
                                        _                -> Killer []
                            !nsti = resetNSt (pathFromScore bGrain) kill1 nst'
                        !nstf <- pvSLoop b d True prune nsti edges
                        let s = cursc nstf
                        whenAbort s $
                            if movno nstf == 1
                               then return $! failHardNoValidMove bGrain b pos
                               else do
                                   -- let !de = max d $ pathDepth s
                                   !nodes1 <- gets (sNodes . stats)
                                   lift $ do
                                       let !deltan = nodes1 - nodes0
                                           mvs = pathMoves s
                                           mv | nullSeq mvs = head $ unalt edges	-- not null - on "else" of noMove
                                              | otherwise   = head $ unseq mvs
                                       ttStore d (rbmch nstf) (pathScore s) mv deltan
                                   return s
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
               let nst' = deepNSt nst
               val <- if v > b + bigDiff
                         then pnextlev <$> pvZeroW nst' (-nma) d2
                         else pnextlev <$> pvZeroW nst' (-nma) d1
               lift undoMove	-- undo null move
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

isCut :: Int -> NodeState -> Bool
isCut b nst = pathScore (cursc nst) >= b

pvSLoop :: Int -> Int -> Bool -> Bool -> NodeState -> Alt Move -> Search NodeState
pvSLoop b d zw p = go
    where go !s (Alt []) = return s
          go !s (Alt (e:es)) = do
             !s' <- pvInnerLoop b d zw p s e
             if isCut b s' then return s'
                           else go s' $ Alt es

-- This is the unified inner loop of the PV & ZW search, executed at every level (except root)
-- once per possible move
-- See the parameter
-- Returns: flag if it was a beta cut and new status
pvInnerLoop :: Int 	-- current beta
            -> Int	-- current search depth
            -> Bool	-- zero window search?
            -> Bool	-- prune?
            -> NodeState 	-- node status
            -> Move	-- move to search
            -> Search NodeState
pvInnerLoop b d zw prune nst e = timeToAbort nst $
    if prune && (zw || movno nst > 1) && canPruneMove (cpos nst) e
       then return $! nst { movno = movno nst + 1 }
       else do
           !exd <- lift $ doMove e	-- do the move
           if legalResult exd
              then do
                  newNode d
                  old <- get
                  let adp = absdp old + 1
                      mdp = max (maxdp old) adp
                  modify $ \s -> s { absdp = adp, maxdp = mdp }
                  (s, nst1) <- case exd of
                      Exten exd' cap nolmr -> do
                          -- Resetting means we reduce less (only with distance to last capture)
                          let nst1 | cap       = resetSpc nst
                                   | otherwise = nst
                          s <- extenFunc b d (cap || nolmr) exd' (deepNSt nst1)
                          return (s, nst1)
                      Final   -> return (drawPath, nst)
                      Illegal -> error "Cannot be illegal here"
                  lift undoMove	-- undo the move
                  modify $ \s' -> s' { absdp = absdp old }
                  let s' = addToPath e s
                  checkFunc b d e s' nst1
              else return nst
    where extenFunc | zw        = pvInnerLoopExtenZ
                    | otherwise = pvInnerLoopExten
          checkFunc | zw        = checkFailOrPVLoopZ
                    | otherwise = checkFailOrPVLoop

resetSpc :: NodeState -> NodeState
resetSpc nst = nst { spcno = movno nst }

pvInnerLoopExten :: Int -> Int -> Bool -> Int -> NodeState -> Search Path
pvInnerLoopExten b d spec !exd nst = do
    let !pvnode = crtnt nst == PVNode
        !d1   = d + exd - 1	-- this is the normal (unreduced) depth for next search
        a     = pathScore $ cursc nst
    if pvnode || d <= minPvDepth
       then do
           -- Set albe only when not in PV and not already set (to spare a copy)
           let nst' = if not (pvnode || albe nst) then nst { albe = True } else nst
           pnextlev <$> pvSearch nst' (-b) (-a) d1
       else do
           -- Here we must be in a Cut node (will fail low)
           -- and we should have: crtnt = CutNode, nxtnt = AllNode
           old <- get
           let !d' = reduceLmr (nearmate b) spec d1 (lmrlv old) (movno nst - spcno nst)
           !s1 <- zeroWithLMR d' d1 (-a) (a+scoreGrain) nst
           whenAbort s1 $
               if pathScore s1 <= a
                  then return s1	-- failed low (as expected) or aborted
                  else do
                      -- we didn't fail low and need re-search: full window
                      let !nst1 = nst { crtnt = PVNode, nxtnt = PVNode }
                      pnextlev <$> pvSearch nst1 (-b) (-a) d1

-- For zero window
pvInnerLoopExtenZ :: Int -> Int -> Bool -> Int -> NodeState -> Search Path
pvInnerLoopExtenZ b d spec !exd nst = do
    old  <- get
    -- late move reduction
    let !d1 = d + exd - 1	-- this is the normal (unreduced) depth for next search
        !d' = reduceLmr (nearmate b) spec d1 (lmrlv old) (movno nst - spcno nst)
        !onemB = scoreGrain - b
    zeroWithLMR d' d1 onemB b nst

zeroWithLMR :: Int -> Int -> Int -> Int -> NodeState -> Search Path
zeroWithLMR !d' !d1 !onemB !b nst =
    if d' == d1
       then do
           moreLMR True 1	-- more LMR
           pnextlev <$> pvZeroW nst onemB d'
       else do
           incRedu
           nds0 <- gets $ sNodes . stats
           !sr <- pnextlev <$> pvZeroW nst onemB d'
           nds1 <- gets $ sNodes . stats
           let nodre = nds1 - nds0
           whenAbort sr $
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
                    !sf <- pnextlev <$> pvZeroW nst1 onemB d1
                    whenAbort sf $ do
                        when (pathScore sf >= b) $ moreLMR False d1
                        return sf

checkFailOrPVLoop :: Int -> Int -> Move -> Path -> NodeState -> Search NodeState
checkFailOrPVLoop b d e s nst = whenAbort nst $ do
    sst <- get
    let mn = movno nst
    if pathScore s <= pathScore (cursc nst)
       then return $! nst { movno = mn+1, killer = newKiller d s nst }
       else if pathScore s >= b
               then do
                 lift $ betaCut (absdp sst) e -- anounce a beta move (for example, update history)
                 incBeta mn
                 let fhsc = s { pathScore = b }
                 return $! nst { cursc = fhsc, movno = mn+1, rbmch = 1 }
               else do	-- means: > a && < b
                   lift $ do
                       betaCut (absdp sst) e -- not really a cut, but good move here
                       -- let de = max d $ pathDepth s
                       ttStore d 1 (pathScore s) e 0	-- best move so far (score is lower limit)
                   let nnt  = nextNodeType (nxtnt nst)
                   return $! nst { cursc = s, nxtnt = nnt, movno = mn+1, rbmch = 2 }

-- For zero window
checkFailOrPVLoopZ :: Int -> Int -> Move -> Path -> NodeState -> Search NodeState
checkFailOrPVLoopZ b d e s nst = whenAbort nst $ do
    sst <- get
    let mn = movno nst
    if pathScore s < b	-- failed low
       then return $! nst { movno = mn+1, killer = newKiller d s nst }
       else do	-- here is s >= b: failed high
           lift $ betaCut (absdp sst) e -- anounce a beta move (for example, update history)
           incBeta mn
           let fhsc = s { pathScore = b }
           return $! nst { cursc = fhsc, movno = mn+1, rbmch = 1 }

newKiller :: Int -> Path -> NodeState -> Killer
newKiller d s nst
    | d >= 2, (mm:km:_) <- unseq $ pathMoves s,
      isKillCand (cpos nst) mm km = pushKiller km (killer nst)
    | otherwise                   = killer nst

-- Same as newKiller, but the path begins with the killer move
-- as it is coming from null move search
-- It is called before a NSt reset, so no need to consider
-- previous killer moves
newTKiller :: MyPos -> Int -> Path -> Killer
newTKiller pos d s
    | d >= 2, (km:_) <- unseq $ pathMoves s,
      isTKillCand pos km = Killer [km]
    | otherwise          = Killer []

-- We generate the moves, which come already sorted from genMoves
-- But we pick the best move (TT or IID) and the killers first
genAndPick :: NodeState -> Maybe Move -> Int -> Int -> Int -> Search (Alt Move)
genAndPick !nst mttmv !a !b !d = do
    path <- case mttmv of
                Just mv -> return [mv]
                Nothing -> bestMoveFromIID nst a b d	-- it will do nothing for AllNode
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
    | spec || d <= lmrNoDepth || nearmatea = d
    | otherwise                            = max 1 $ d - lmrArr!(lmrlev, w)

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

-- Futility pruning:
isPruneFutil :: Int -> Int -> Bool -> Int -> Bool
isPruneFutil d a pv v
    | nearmate a              = False
    | pv && d > maxFutilDepth = False
    | d > maxFutilDepth + 1   = False	-- for zero window searches we allow higher futility depth
    | otherwise               = v + futilMargins d <= a

failHardNoValidMove :: Int -> Int -> MyPos -> Path
failHardNoValidMove !a !b pos = trimaxPath a b $! if tacticalPos pos then matedPath else drawPath

trimaxPath :: Int -> Int -> Path -> Path
trimaxPath !a !b x = x { pathScore = trimax a b (pathScore x) }

trimax :: Int -> Int -> Int -> Int
trimax !a !b !x
    | x <= a    = a
    | x >= b    = b
    | otherwise = x

-- Quiescent Search
pvQSearch :: Int -> Int -> Search Int
pvQSearch !a !b = do
    !s <- qSearch a b True
    return $! trimax a b s

{-# NOINLINE qSearch #-}
qSearch :: Int -> Int -> Bool -> Search Int
qSearch !a !b front = do
    (hdeep, tp, hsc, _, _) <- reTrieve >> lift ttRead
    if hdeep >= 0
       then qSearchFound    a b tp hsc front
       else qSearchNotFound a b        front

-- When we found a TT entry, we sometimes may terminate the QS immediately,
-- and sometimes we may at least improve the search limits
qSearchFound :: Int -> Int -> Int -> Int -> Bool -> Search Int
qSearchFound !a !b !tp !hsc front = do
    reSucc 1
    -- tp == 2 => we have an exact score
    -- tp == 1 => score >= hsc, so if hsc >  a then we at least improved
    -- tp == 0 => score <= hsc, so if hsc <= a then we fail low
    if    tp == 2		-- exact score: always good, terminate
       || tp == 1 && hsc >= b	-- we will fail high
       || tp == 0 && hsc <= a	-- we will fail low
       then return hsc
       else do
           -- Here we have one and only one of:
           -- tp == 1 && hsc < b
           -- tp == 0 && hsc > a
           -- We can possibly improve one of the limit
           -- This cannot happen in zero window search!
           if a + scoreGrain == b
              then qSearchLims a b front
              else if tp == 1
                      then qSearchLims (max a hsc) b           front
                      else qSearchLims a           (min b hsc) front

qSearchNotFound :: Int -> Int -> Bool -> Search Int
qSearchNotFound !a !b front = reFail >> qSearchLims a b front

qSearchLims :: Int -> Int -> Bool -> Search Int
qSearchLims !a !b front = do
    pos <- lift getPos
    if tacticalPos pos
       then qsInCheck a b (staticScore pos)
       else qsNormal  a b (staticScore pos) front

qsInCheck :: Int -> Int -> Int -> Search Int
qsInCheck !a !b !s = do
    edges <- Alt <$> lift genEscapeMoves
    if noMove edges
       then return mated
       else do
          !dcut <- lift $ qsDelta $ a - s - qsDeltaMargin
          if dcut
             then do
                 when collectFens $ finWithNodes "DELT"
                 return a
             -- else pvQLoop b (max s a) edges
             -- do not trust eval in check
             else pvQLoop b a edges

qsNormal :: Int -> Int -> Int -> Bool -> Search Int
qsNormal !a !b !s front
    | s >= b = do
         when collectFens $ finWithNodes "BETA"
         return b
    | otherwise = do
         !dcut <- lift $ qsDelta $ a - s - qsDeltaMargin
         if dcut
            then do
                when collectFens $ finWithNodes "DELT"
                return a
            else do
                edges <- Alt <$> lift (genTactMoves front)
                if noMove edges
                   then do	-- no more captures
                       when collectFens $ finWithNodes "NOCA"
                       return (max s a)
                   else pvQLoop b (max s a) edges

pvQLoop :: Int -> Int -> Alt Move -> Search Int
pvQLoop !b = go
    where go !s (Alt [])     = return s
          go !s (Alt (e:es)) = do
              !s' <- pvQInnerLoop b s e
              if s' >= b then return b
                         else go s' $ Alt es

{-# NOINLINE pvQInnerLoop #-}
pvQInnerLoop :: Int -> Int -> Move -> Search Int
pvQInnerLoop !b !a e = timeToAbort b $ do
    r <- lift $ doQSMove e
    if r
       then do
           newNodeQS
           !s <- negate <$> qSearch (-b) (-a) False
           lift undoMove
           return $! if s > a then s else a
       else return a

{-# INLINE finWithNodes #-}
finWithNodes :: String -> Search ()
finWithNodes s = do
    n <- gets $ sNodes . stats
    lift $ finNode s n

{-# INLINE bestMoveFromIID #-}
bestMoveFromIID :: NodeState -> Int -> Int -> Int -> Search [Move]
bestMoveFromIID nst a b d
    | nt == AllNode = return []
    | nt == CutNode
          = if d >= minIIDCut || (d >= minIIDCutNK && noKiller (killer nst))
               then do s <- pvZeroW nst b d'
                       return $! unseq $ pathMoves s
               else return []
    | otherwise -- => PVNode
          = if d >= minIIDPV
               then do s <- pvSearch nst a b d'
                       return $! unseq $ pathMoves s
               else return []
    where d' = min maxIIDDepth (iidNewDepth d)
          nt = crtnt nst

noKiller :: Killer -> Bool
noKiller (Killer kl) = null kl

{-# INLINE timeToAbort #-}
timeToAbort :: a -> Search a -> Search a
timeToAbort a act = do
    s <- get
    rmn <- lift getRootMoveNumber
    if abort s
       then return a
       else do
           let ro = ronly s
           if draft ro > 1 && abmili ro > 0
              then if timeNodes .&. sNodes (stats s) /= 0
                      then act
                      else do
                          !abrt <- lift $ isTimeout $ if rmn > 1 then abmili ro else abmil1 ro
                          if abrt
                             then do
                                 let msg = if rmn > 1 then "Time abort in move 1" else "Time abort"
                                 lift $ informStr msg
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

bestFirst :: [Move] -> [Move] -> ([Move], [Move]) -> [Move]
bestFirst path kl (es1, es2)
    | null path = es1 ++ kl ++ chainUniqFilters kl es2
    | otherwise = e : uniqFilter e es1 ++ kl ++ chainUniqFilters (e : kl) es2
    where (e:_)  = path

-- When filtering the TT & killer moves from the generated ones
-- we use delete, which has a bit of an inefficiency because
-- it does not assume anything about the list
-- But out move list consists of unique moves, which maybe could
-- accelerate a bit the filtering
uniqFilter :: Move -> [Move] -> [Move]
uniqFilter !e = go
    where go [] = []
          go (m:ms) | m == e    = ms
                    | otherwise = m : go ms

-- Here we chain the filters for more moves (composition)
chainUniqFilters :: [Move] -> [Move] -> [Move]
chainUniqFilters = foldr (\m f -> uniqFilter m . f) id

pushKiller :: Move -> Killer -> Killer
pushKiller e (Killer kl) = Killer $ take maxKillers $ e : uniqFilter e kl

killerToList :: Killer -> [Move]
killerToList (Killer kl) = kl

--- Communication to the outside - some convenience functions ---
informCM :: Move -> Int -> Game ()
informCM a b = informCtx (CurrMv a b)

informStr :: String -> Game ()
informStr s = informCtx (InfoStr s)

logmes :: String -> Game ()
logmes s = informCtx (LogMes s)

informPV :: Int -> Int -> [Move] -> Search ()
informPV s d es = do
    st <- get
    let ss = stats st
    lift $ do
        n <- curNodes $ sNodes ss
        informCtx (BestMv s d (maxdp st) n es)
