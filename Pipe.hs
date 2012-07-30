{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Pipe (Pipe, Producer, Consumer, Pipeline, (<+<)) where

import Control.Monad.Trans.Free
import Control.Monad.Trans.Class
import Control.Applicative

import Control.Monad
import Data.Void

import IsPipe

data PipeF i o next
  = Yield o next
  | Await (i -> next)
  deriving Functor

newtype Pipe i o m r = Pipe
  { unPipe :: FreeT (PipeF i o) m r }
  deriving (Functor, Applicative, Monad)

type Producer o m r = Pipe () o    m r
type Consumer i m r = Pipe i  Void m r
type Pipeline   m r = Pipe () Void m r


pipeM :: m (FreeF (PipeF i o) r (FreeT (PipeF i o) m r)) -> Pipe i o m r
pipeM m = Pipe (FreeT m)

runPipeM :: Pipe i o m r -> m (FreeF (PipeF i o) r (FreeT (PipeF i o) m r))
runPipeM (Pipe (FreeT m)) = m

instance MonadTrans (Pipe i o) where
  lift = Pipe . lift

instance IsPipe Pipe where
  yield o = Pipe $ liftF $ Yield o ()
  await = Pipe $ liftF $ Await id
  pipe f = forever $ await >>= yield . f
  simulatePipe onYield onAwait = go where
    go p = do
      x <- lift $ runPipeM p
      case x of
        Pure r -> return r
        Free (Yield o next) -> onYield o >> go (Pipe next)
        Free (Await f) -> onAwait >>= go . Pipe . f


(<+<) :: Monad m => Pipe i' o m r -> Pipe i i' m r -> Pipe i o m r
p1 <+< p2 = pipeM $ do
  x1 <- runPipeM p1
  let p1' = pipeM $ return x1
  runPipeM $ case x1 of
    Pure r -> return r
    Free (Yield o next) -> Pipe $ wrap $ Yield o (unPipe (Pipe next <+< p2))
    Free (Await f) -> pipeM $ do
      x2 <- runPipeM p2
      runPipeM $ case x2 of
        Pure r -> return r
        Free (Yield o next) -> Pipe (f o) <+< Pipe next
        Free (Await g) -> Pipe $ wrap $ Await (\i -> unPipe (p1' <+< Pipe (g i)))
