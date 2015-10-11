{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes, MultiParamTypeClasses, FlexibleInstances #-}
#ifdef SMSTRICT
{-# LANGUAGE BangPatterns #-}
#endif

-- The search monad (which is actually a state monad transformer in
-- continuation passing style) can be compiled strict in status if you
-- define the symbol SMSTRICT
-- otherwise it will be compiled lazy in status
-- The monad is always lazy in the values
module Search.CStateMonad (
    CState,
    gets, modify,
    runCState, execCState
    ) where

-- import Control.Applicative
import Control.Monad
import Control.Monad.State hiding (gets, modify)

newtype CState s m a = CState { runSTPlus :: forall r. s -> (a -> s -> m r) -> m r }

instance Functor (CState s m) where
    {-# INLINE fmap #-}
    fmap f c = CState $ \s0 k -> runSTPlus c s0 $ \a s1 -> k (f a) s1

{--
fmap f c = c >>= return . f
=
CState $ \s0 k -> runSTPlus c s0 $ \a s1 -> runSTPlus (return . f $ a) s1 k
=
CState $ \s0 k -> runSTPlus c s0 $ \a s1 -> runSTPlus (return (f a)) s1 k
=
CState $ \s0 k -> runSTPlus c s0 $ \a s1 -> runSTPlus (CState $ \s k -> k (f a) s) s1 k
=
CState $ \s0 k -> runSTPlus c s0 $ \a s1 -> (\s k -> k (f a) s) s1 k
=
CState $ \s0 k -> runSTPlus c s0 $ \a s1 -> k (f a) s1
--}

instance Applicative (CState s m) where
    {-# INLINE pure #-}
    pure a = CState $ \s k -> k a s
    {-# INLINE (<*>) #-}
    af <*> aa = CState $ \s0 k -> runSTPlus af s0 $ \f s1 -> runSTPlus aa s1 $ \a s2 -> k (f a) s2

{--
af <*> aa = af >>= \f -> aa >>= \a -> return (f a)
=
CState $ \s0 k -> runSTPlus af s0 $ \x s1 -> runSTPlus ((\f -> aa >>= \a -> return (f a)) x) s1 k
=
CState $ \s0 k -> runSTPlus af s0 $ \x s1 -> runSTPlus (aa >>= \a -> return (x a)) s1 k
=
CState $ \s0 k -> runSTPlus af s0 $ \x s1 -> runSTPlus (CState $ \s0 k -> runSTPlus aa s0 $ \a s2 -> runSTPlus ((\a -> return (x a)) a) s2 k) s1 k
=
CState $ \s0 k -> runSTPlus af s0 $ \x s1 -> runSTPlus (CState $ \s0 k -> runSTPlus aa s0 $ \a s2 -> runSTPlus (return (x a)) s2 k) s1 k
=
CState $ \s0 k -> runSTPlus af s0 $ \x s1 -> runSTPlus aa s1 $ \a s2 -> runSTPlus (return (x a)) s2 k
=
CState $ \s0 k -> runSTPlus af s0 $ \x s1 -> runSTPlus aa s1 $ \a s2 -> runSTPlus (CState $ \s k -> k (x a) s) s2 k
=
CState $ \s0 k -> runSTPlus af s0 $ \x s1 -> runSTPlus aa s1 $ \a s2 -> (\s k -> k (x a) s) s2 k
=
CState $ \s0 k -> runSTPlus af s0 $ \x s1 -> runSTPlus aa s1 $ \a s2 -> k (x a) s2
--}

-- At least with GHC 7.4.1, we have:
-- the construct: case f a of fa -> ... is lazy, to make it strict, do
-- case f a of !fa -> ...
-- So we keep the simpler for for the lazy variant
instance Monad (CState s m) where
    {-# INLINE return #-}
    return a = CState $ \s k -> k a s
    {-# INLINE (>>=) #-}
    c >>= f  = CState $ \s0 k -> runSTPlus c s0 $ \a s1 -> runSTPlus (f a) s1 k

instance MonadState s (CState s m) where
    {-# INLINE get #-}
    get   = CState $ \s k -> k s  s
    {-# INLINE put #-}
#ifdef SMSTRICT
    put s = CState $ \_ k -> s `seq` k () s
#else
    put s = CState $ \_ k -> k () s
#endif

instance MonadTrans (CState s) where
    {-# INLINE lift #-}
    lift m = CState $ \s k -> m >>= \a -> k a s

instance MonadIO m => MonadIO (CState s m) where
    {-# INLINE liftIO #-}
    liftIO = lift . liftIO

{-# INLINE runCState #-}
runCState :: Monad m => CState s m a -> s -> m (a, s)
runCState c s = runSTPlus c s $ \a s0 -> return (a, s0)

{-# INLINE execCState #-}
execCState :: Monad m => CState r m a -> r -> m r
execCState ms s = liftM snd $ runCState ms s

{-# INLINE gets #-}
gets :: Monad m => (s -> a) -> CState s m a
gets f = CState $ \s k -> k (f s) s

{-# INLINE modify #-}
modify :: Monad m => (s -> s) -> CState s m ()
#ifdef SMSTRICT
modify f = CState $ \s k -> case f s of !fs -> k () fs
#else
modify f = CState $ \s k -> k () (f s)
#endif
