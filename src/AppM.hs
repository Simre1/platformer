{-# LANGUAGE BangPatterns #-}
module AppM where

import Control.Exception (bracket)
import Control.Monad (join)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans (MonadIO (liftIO))
import Control.Monad.Trans.Reader
import Data.IORef
import Control.Signal
import GHC.Clock (getMonotonicTimeNSec)
import Control.Concurrent (threadDelay)

newtype AppM a = AppM (ReaderT AppEnv IO a) deriving (Functor, Applicative, Monad, MonadIO, MonadFix)

data AppEnv = AppEnv
  { appCleanUp :: IORef (IO ())
  }

addCleanUp :: IO () -> AppM ()
addCleanUp cleanUp = AppM $ do
  ref <- asks appCleanUp
  liftIO $ modifyIORef ref (<> cleanUp)

makeAppEnv :: IO AppEnv
makeAppEnv = AppEnv <$> newIORef mempty

finalizeAppEnv :: AppEnv -> IO ()
finalizeAppEnv appEnv = join $ readIORef (appCleanUp appEnv)

runAppM :: AppM a -> IO a
runAppM (AppM action) = do
  bracket
    makeAppEnv
    finalizeAppEnv
    (runReaderT action)