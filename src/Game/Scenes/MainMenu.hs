module Game.Scenes.MainMenu where

import AppM
import Control.Arrow
import Control.Signal
import Game.Scene
import Input
import Game.World
import Control.Monad.IO.Class (liftIO)

mainMenuSignal :: Signal AppM (Input, MainMenuState) (Either Scene MainMenuState)
mainMenuSignal = arrM_ $ Left . Level . LevelState <$> newWorld
