module Graphics.Dxt where

import Input
import Apecs (SystemT(SystemT), runSystem )
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

import Graphics.Draw

newtype Dxt w a = Dxt (SystemT w (ReaderT Input IO) a) deriving (Functor, Applicative, Monad, MonadIO)

dxtSystem :: SystemT w (DrawM (Dxt w)) a -> DrawM (Dxt w) a
dxtSystem s = do
  w <- lift $ Dxt $ SystemT ask
  runSystem s w

dxtInput :: DrawM (Dxt w) Input
dxtInput = lift $ Dxt $ lift ask

runDxt :: MonadIO m => Input -> Dxt w a -> SystemT w m a
runDxt input (Dxt s) = do
  w <- SystemT ask
  liftIO $ runReaderT (runSystem s w) input

runDrawDxt :: MonadIO m => Input -> Draw (Dxt w) -> SystemT w m (Draw IO)
runDrawDxt i draw = do
  w <- SystemT ask
  pure $ morphDraw (flip runSystem w . runDxt i) draw
  