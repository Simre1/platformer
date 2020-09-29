module Game.Scene where

import Apecs (SystemT, runSystem)
import Game.World
import Input
import Control.Signal
import AppM
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class (MonadTrans)
import qualified Data.Text as T

data Scene = MainMenu MainMenuState | Level LevelState

data LevelState = LevelState {levelWorld :: World}

data MainMenuState = MainMenuState 
  { projectPath :: T.Text
  , levelPath :: T.Text
  }

newtype LevelM m a = LevelM (ReaderT LevelState m a) deriving (Functor, Applicative, Monad, MonadIO, MonadTrans)

runLevelM :: LevelState -> LevelM m a -> m a
runLevelM ls (LevelM action) = runReaderT action ls

embedApecs :: MonadIO m => SystemT World (LevelM m) a -> LevelM m a
embedApecs system = do
  w <- LevelM $ asks levelWorld
  runSystem system w