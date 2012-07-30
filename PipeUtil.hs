{-# OPTIONS_GHC -Wall #-}

module PipeUtil
  ( module X
  , runPipeline
  , toPipe
  , fromList
  , printer
  ) where

import Control.Monad.Trans.Class as X
import Control.Monad.Trans.Identity as X

import Control.Monad as X
import Data.Void as X


import Pipe as X
import PipeL as X
import IsPipe as X


runPipeline :: Monad m => Pipeline m r -> m r
runPipeline = runIdentityT . simulatePipe absurd (return ())

toPipe :: (Monad m, IsPipe p) => p i o m r -> Pipe i o m r
toPipe = fromPipe

fromList :: Monad m => [i] -> Producer i m ()
fromList = mapM_ yield

printer :: Show o => Consumer o IO r
printer = forever $ await >>= lift . print
