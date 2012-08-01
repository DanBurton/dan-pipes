{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

import ErrP hiding (Producer, fromList)
import qualified ErrP as E
import qualified Pipe as P
import Prelude hiding (id, (.), take)

import Control.Monad.IO.Class

type i :~> r = forall m. Monad m => Cat P.Pipe (EitherT () m) r i Void
type Producer o m r = Cat P.Pipe (EitherT () m) r () o

liftI :: MonadIO m => Cat P.Pipe IO r i o -> Cat P.Pipe m r i o
liftI p = Cat $ do
  r <- P.transPipe liftIO $ unCat p
  return r

liftE :: Monad m => Cat P.Pipe m () i o -> Cat P.Pipe (EitherT () m) r i o
liftE p = Cat $ do
  () <- P.transPipe lift $ unCat p
  abort ()

take :: Int -> (i :~> [i])
take 0 = Cat $ return []
take n = Cat $ do
  x <- await  
  xs <- unCat $ take (pred n)
  return (x:xs)
