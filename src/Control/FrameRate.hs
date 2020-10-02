module Control.FrameRate where

import Control.Monad.IO.Class

import Data.IORef
import GHC.Clock (getMonotonicTimeNSec)
import Control.Concurrent (threadDelay)

limitExecutionRate :: (MonadIO m, MonadIO m2) => Int -> m2 (m a -> m a)
limitExecutionRate fps = do
  initial <- liftIO getMonotonicTimeNSec
  timeRef <- liftIO $ newIORef initial
  d <- pure $ toEnum (1000000000 `quot` fps)
  pure $ \action -> do
    a <- action
    t1 <- liftIO $ readIORef timeRef
    t2 <- liftIO getMonotonicTimeNSec
    let diff = t2 - t1
    let waitTime = if diff >= d
          then 0
          else d - diff
    liftIO $ writeIORef timeRef (t2 + waitTime)
    liftIO $ threadDelay $ fromEnum waitTime `quot` 1000
    pure a