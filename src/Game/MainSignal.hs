module Game.MainSignal where

import Game.Scene
import Control.Signal
import AppM
import Input
import Game.Scenes.MainMenu
import Game.Scenes.Level


gameSignal :: Scene -> Signal AppM Input Scene
gameSignal scene = case scene of
  MainMenu s -> customSwitch MainMenu s mainMenuSignal
  Level s -> customSwitch Level s levelSignal
  where
    customSwitch ::  (s -> Scene) -> s -> Signal AppM (Input, s) (Either Scene s) -> Signal AppM Input Scene
    customSwitch f s signal = Signal $ \i -> do
      (r,cont) <- stepSignal signal (i,s)
      case r of
        Left newScene -> stepSignal (gameSignal newScene) i
        Right newS -> pure (f newS, customSwitch f newS signal)
  
  -- feedback initialScene $ arrM $ \(input,scene) -> do
  -- (nextScene,cont) <- case scene of
  --   MainMenu s -> stepSignal (MainMenu <$> mainMenuSignal) (input,s)
  -- pure (nextScene,nextScene)
