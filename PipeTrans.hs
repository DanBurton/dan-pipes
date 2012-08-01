module PipeTrans (PipeTrans(..)) where

import Pipe

class PipeTrans p where
  liftP :: Monad m => Pipe i o m r -> p i o m r
