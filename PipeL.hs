{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module PipeL (PipeL, forget, unawait, peek) where

import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Control.Applicative

import Pipe
import IsPipe

newtype PipeL i o m r = PipeL (StateT ([i],[i]) (Pipe i o m) r)
                    deriving (Functor, Applicative, Monad)

liftL :: Monad m => Pipe i o m r -> PipeL i o m r
liftL = PipeL . lift

instance MonadTrans (PipeL i o) where
  lift = liftL . lift

instance IsPipe PipeL where
  yield = liftL . yield

  await = do
    (awaited, unawaited) <- PipeL get
    case unawaited of
      [] -> do
        i <- liftL await
        PipeL $ put (i:awaited, unawaited)
        return i
      (i:unawaited') -> do
        PipeL $ put (i:awaited, unawaited')
        return i

  pipe f = do
    (_awaited, unawaited) <- PipeL get
    mapM_ (yield . f) unawaited
    PipeL $ put ([],[])
    liftL (pipe f)

  simulatePipe onYield onAwait (PipeL pl) = simulatePipe onYield onAwait (evalStateT pl ([],[]))



forget :: Monad m => PipeL i o m ()
forget = PipeL $ modify (\(_awaited, unawaited) -> ([], unawaited))

unawait :: Monad m => PipeL i o m ()
unawait = PipeL $ modify (\(awaited, unawaited) -> ([], foldl (flip (:)) unawaited awaited))

peek :: Monad m => PipeL i o m i
peek = await >>= \i -> unawait >> return i

