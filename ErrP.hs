{-# LANGUAGE PolyKinds #-}

module ErrP (module X, ErrP, abort, runErrP) where

import Pipe
import PipeTrans as X
import PipeUtil as X

import Control.Monad.Trans.Class as X
import Control.Monad.Trans.Either as X

import Prelude hiding (id, (.))

type ErrP e p i o m r = p i o (EitherT e m) r

abort :: (MonadTrans (p i o), Monad m) => e -> ErrP e p i o m r
abort = lift . left

runErrP :: Monad m => Pipeline (EitherT e m) r -> EitherT e m r
runErrP = runPipeline

