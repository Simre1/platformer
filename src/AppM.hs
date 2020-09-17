module AppM where

import Control.Monad.Trans.State
import Control.Monad.IO.Class (MonadIO)

newtype AppM a = AppM (StateT AppState IO a) deriving (Functor, Applicative, Monad, MonadIO)

data AppState = AppState
  { appCleanUp :: IO ()
  }

addCleanUp :: IO () -> AppM ()
addCleanUp cleanUp = AppM $ modify $ \appState -> appState {appCleanUp = appCleanUp appState <> cleanUp}

makeAppState :: IO AppState
makeAppState = pure $ AppState mempty

runAppM :: AppM a -> IO a
runAppM (AppM action) = do
  state <- makeAppState
  (a, newState) <- runStateT action state
  case newState of
    AppState cleanUp -> cleanUp
  pure a