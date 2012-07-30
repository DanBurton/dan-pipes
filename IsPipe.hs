{-# OPTIONS_GHC -Wall #-}

module IsPipe (IsPipe(..), fromPipe) where

import Control.Monad.Trans.Class

class IsPipe p where
  yield :: Monad m => o -> p i o m ()
  await :: Monad m => p i o m i
  pipe :: Monad m => (i -> o) -> p i o m r
  simulatePipe :: (MonadTrans t, Monad (t m), Monad m) => (o -> t m ()) -> t m i -> p i o m r -> t m r

  idP :: Monad m => p i i m r
  idP = pipe id


fromPipe :: (Monad m, MonadTrans (p' i o), Monad (p' i o m), IsPipe p, IsPipe p') => p i o m r -> p' i o m r
fromPipe = simulatePipe yield await
