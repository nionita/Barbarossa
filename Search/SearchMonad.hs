{-# LANGUAGE RankNTypes, MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}

-- The search monad (which is actually a state monad transformer in
-- continuation passing style) can be compiled strict if you
-- define the symbol SMSTRICT
-- otherwise it will be compiled lazy
module Search.SearchMonad (
    STPlus,
    -- return, (>>=),
    -- get, put,
    gets, modify,
    -- lift,
    -- liftIO,
    runSearch, execSearch
    ) where

import Control.Monad
import Control.Monad.State hiding (gets, modify)

newtype STPlus r s m a = STPlus { runSTPlus :: s -> (a -> s -> m r) -> m r }
-- {-# INLINE runSTPlus #-}

-- At least with GHC 7.4.1, we have:
-- the construct f a of fa -> ... is lazy, to make it strict, do
-- case f a of !fa -> ...
-- So we keep the simpler for for the lazy variant
instance Monad (STPlus r s m) where
    return a = STPlus $ \s k -> k a s
    {-# INLINE return #-}
#ifdef SMSTRICT
    c >>= f  = STPlus $ \s0 k -> runSTPlus c s0 $ \a s1 -> case f a of !fa -> runSTPlus fa s1 k
#else
    c >>= f  = STPlus $ \s0 k -> runSTPlus c s0 $ \a s1 -> runSTPlus (f a) s1 k
#endif
    {-# INLINE (>>=) #-}

instance MonadState s (STPlus r s m) where
    get   = STPlus $ \s k -> k s  s
    {-# INLINE get #-}
    put s = STPlus $ \_ k -> k () s
    {-# INLINE put #-}

instance MonadTrans (STPlus r s) where
    {-# INLINE lift #-}
    -- lift :: Monad m => m a -> STPlus r s m a
    lift m = STPlus $ \s k -> m >>= \a -> k a s


instance MonadIO m => MonadIO (STPlus r s m) where
    {-# INLINE liftIO #-}
    liftIO = lift . liftIO

runSearch :: Monad m => STPlus (a, s) s m a -> s -> m (a, s)
runSearch c s = runSTPlus c s $ \a s0 -> return (a, s0)
{-# INLINE runSearch #-}

execSearch ms s = liftM snd $ runSearch ms s
{-# INLINE execSearch #-}

{-# INLINE gets #-}
gets :: Monad m => (s -> a) -> STPlus r s m a
#ifdef SMSTRICT
gets f = STPlus $ \s k -> case f s of !fs -> k fs s
#else
gets f = STPlus $ \s k -> k (f s) s
#endif

{-# INLINE modify #-}
modify :: Monad m => (s -> s) -> STPlus r s m ()
#ifdef SMSTRICT
modify f = STPlus $ \s k -> case f s of !fs -> k () fs
#else
modify f = STPlus $ \s k -> k () (f s)
#endif
