{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}


module UnawaitP (UnawaitP, withUnawait, forget, unawait, unawaitAll, peek) where

import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Control.Applicative

import Data.List (foldl')

import Pipe
import IsPipe
import PipeTrans


newtype UnawaitP i o m r = UnawaitP
  ( StateT ([i],[i]) (Pipe i o m) r )
  deriving (Functor, Applicative, Monad)

instance PipeTrans UnawaitP where
  liftP = UnawaitP . lift

instance MonadTrans (UnawaitP i o) where
  lift = liftP . lift

instance IsPipe UnawaitP where
  yield = liftP . yield

  await = do
    (awaited, unawaited) <- UnawaitP get
    case unawaited of
      [] -> do
        i <- liftP await
        UnawaitP $ put (i:awaited, unawaited)
        return i
      (i:unawaited') -> do
        UnawaitP $ put (i:awaited, unawaited')
        return i

  pipe f = do
    (_awaited, unawaited) <- UnawaitP get
    mapM_ (yield . f) unawaited
    UnawaitP $ put ([],[])
    liftP (pipe f)

  simulatePipe onYield onAwait (UnawaitP pl) =
    simulatePipe onYield onAwait (evalStateT pl ([],[]))


withUnawait :: Monad m => UnawaitP i o m r -> Pipe i o m r
withUnawait = fromPipe

forget :: Monad m => UnawaitP i o m ()
forget = UnawaitP $ modify (\(_awaited, unawaited) -> ([], unawaited))

unawait :: Monad m => UnawaitP i o m ()
unawait = UnawaitP $ modify (\s@(awaited, unawaited) -> case awaited of
                             [] -> s
                             (i:awaited') -> (awaited', i:unawaited))

unawaitAll :: Monad m => UnawaitP i o m ()
unawaitAll = UnawaitP $ modify (\(awaited, unawaited) ->
                              ([], foldl' (flip (:)) unawaited awaited))

peek :: Monad m => UnawaitP i o m i
peek = await >>= \i -> unawait >> return i

