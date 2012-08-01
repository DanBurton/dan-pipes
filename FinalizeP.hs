{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module FinalizeP where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Control.Applicative

import Pipe
import IsPipe
import IsPipeCat
import PipeTrans

pass :: Monad m => m ()
pass = return ()

data Finalize m = Finalize
  { finalizeDownstream :: m ()
  , finalizeSelf :: m ()
  , finalizeUpstream :: m ()
  }

newtype FinalizeP i o m r = FinalizeP
  ( StateT (Finalize m) (Pipe i o m) r)
  deriving (Functor, Applicative, Monad)


instance PipeTrans FinalizeP where
  liftP = FinalizeP . lift

instance MonadTrans (FinalizeP i o) where
  lift = liftP . lift

instance IsPipe FinalizeP where
  await = liftP await
  yield = liftP . yield
  pipe  = liftP . pipe
  simulatePipe onAwait onYield (FinalizeP p) = do
    (r, f) <- simulatePipe onAwait onYield (runStateT p (Finalize pass pass pass))
    lift $ finalizeUpstream f
    lift $ finalizeSelf f
    lift $ finalizeDownstream f
    return r

instance IsPipeCat FinalizeP where
  FinalizeP p1 <+< FinalizeP p2 = FinalizeP $ StateT $ \(Finalize d s u) -> do
    let p1' = runStateT p1 (Finalize d s u)
        p2' = runStateT p2 (Finalize d s u)
    p1' <+< p2'


close :: Monad m => FinalizeP i o m ()
close = do
  fs <- FinalizeP get
  lift $ finalizeUpstream fs
  FinalizeP $ put $ fs { finalizeUpstream = pass }

finallyP :: Monad m => m () -> FinalizeP i o m ()
finallyP m = FinalizeP $ modify (\fs -> fs { finalizeSelf = finalizeSelf fs >> m }) 
