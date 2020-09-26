module Game.Scenes.MainMenu where

import AppM
import Apecs.System
import Control.Arrow
import Control.Monad.IO.Class (liftIO)
import Control.Signal
import qualified Data.Ogmo as O
import qualified Game.LoadOgmo as O
import Game.Scene
import Game.World
import Input

mainMenuSignal :: Signal AppM (Input, MainMenuState) (Either Scene MainMenuState)
mainMenuSignal = arrM_ $ do
  project <-
    either
      (\err -> error err)
      (id)
      <$> O.loadProject "assets/map/ogmo.ogmo"

  level <-
    either
      (\err -> error err)
      (id)
      <$> O.loadLevel "assets/map/ogmo.json"
  world <- liftIO initWorld
  runSystem (O.loadOgmo project level) world
  pure $ Left . Level . LevelState $ world
