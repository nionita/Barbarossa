{-# LANGUAGE RankNTypes, MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}

-- The search monad (which is actually a state monad transformer in
-- continuation passing style) can be compiled strict if you
-- define the symbol SMSTRICT
-- otherwise it will be compiled lazy
module Search.CStateMonad (
    CState,
    -- return, (>>=),
    -- get, put,
    gets, modify,
    -- lift,
    -- liftIO,
    runCState, execCState
    ) where

import Control.Monad
import Control.Monad.State hiding (gets, modify)

newtype CState s m a = CState { runSTPlus :: forall r. s -> (a -> s -> m r) -> m r }

-- At least with GHC 7.4.1, we have:
-- the construct: case f a of fa -> ... is lazy, to make it strict, do
-- case f a of !fa -> ...
-- So we keep the simpler for for the lazy variant
instance Monad (CState s m) where
    return a = CState $ \s k -> k a s
    {-# INLINE return #-}
#ifdef SMSTRICT
    c >>= f  = CState $ \s0 k -> runSTPlus c s0 $ \a s1 -> case f a of !fa -> runSTPlus fa s1 k
#else
    c >>= f  = CState $ \s0 k -> runSTPlus c s0 $ \a s1 -> runSTPlus (f a) s1 k
#endif
    {-# INLINE (>>=) #-}

instance MonadState s (CState s m) where
    get   = CState $ \s k -> k s  s
    {-# INLINE get #-}
    put s = CState $ \_ k -> k () s
    {-# INLINE put #-}

instance MonadTrans (CState s) where
    {-# INLINE lift #-}
    -- lift :: Monad m => m a -> STPlus r s m a
    lift m = CState $ \s k -> m >>= \a -> k a s


instance MonadIO m => MonadIO (CState s m) where
    {-# INLINE liftIO #-}
    liftIO = lift . liftIO

runCState :: Monad m => CState s m a -> s -> m (a, s)
runCState c s = runSTPlus c s $ \a s0 -> return (a, s0)
{-# INLINE runCState #-}

execCState ms s = liftM snd $ runCState ms s
{-# INLINE execCState #-}

{-# INLINE gets #-}
gets :: Monad m => (s -> a) -> CState s m a
#ifdef SMSTRICT
gets f = CState $ \s k -> case f s of !fs -> k fs s
#else
gets f = CState $ \s k -> k (f s) s
#endif

{-# INLINE modify #-}
modify :: Monad m => (s -> s) -> CState s m ()
#ifdef SMSTRICT
modify f = CState $ \s k -> case f s of !fs -> k () fs
#else
modify f = CState $ \s k -> k () (f s)
#endif
