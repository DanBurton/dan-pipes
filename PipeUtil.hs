{-# OPTIONS_GHC -Wall #-}

module PipeUtil
  ( module X
  , Producer
  , Consumer
  , Pipeline
  , runPipeline
  , toPipe
  , fromList
  , printer
  ) where

import Control.Monad.Trans.Identity


import Control.Monad.Trans.Class as X
import Control.Category as X

import Control.Monad as X
import Data.Void as X

import UnawaitP as X
import IsPipe as X
import IsPipeCat as X

import qualified Pipe as P
import Prelude hiding (id, (.))

type Pipe i o m r   = Cat P.Pipe m r i o

type Producer o m r = Pipe () o    m r
type Consumer i m r = Pipe i  Void m r
type Pipeline   m r = Pipe () Void m r


runPipelineT :: (MonadTrans t, Monad (t m), Monad m) => Pipeline m r -> t m r
runPipelineT = simulatePipe absurd (return ()) . unCat

runPipeline :: Monad m => Pipeline m r -> m r
runPipeline = runIdentityT . runPipelineT

toPipe :: (Monad m, IsPipe p) => p i o m r -> Pipe i o m r
toPipe = Cat . fromPipe

fromList :: Monad m => [i] -> Producer i m ()
fromList = Cat . mapM_ yield

printer :: Show o => Consumer o IO r
printer = Cat . forever $ await >>= lift . print

