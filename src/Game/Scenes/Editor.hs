module Game.Scenes.Editor where

import Control.Signal
import AppM
import Input
import Game.Scene

editorSignal :: Signal AppM (Input, LevelState) (Either Scene LevelState)
editorSignal = arrM $ \(i,l) -> do

  pure (Right l)