module Game.Scene where

import Apecs
import Game.World
import Input
import Control.Signal
import AppM
import Control.Monad.IO.Class (MonadIO)

data Scene = MainMenu MainMenuState | Level LevelState

data LevelState = LevelState {levelWorld :: World}

data MainMenuState = MainMenuState

runApecs :: MonadIO m => LevelState -> SystemT World m a -> m a
runApecs ls system = runSystem system (levelWorld ls)