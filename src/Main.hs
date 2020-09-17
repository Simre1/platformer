module Main where

import Graphics.MainSignal
import Game.MainSignal
import Game.Scene
import Input
import qualified SDL
import Control.Signal
import Control.Arrow
import AppM

main :: IO ()
main = do
  SDL.initializeAll
  window <- SDL.createWindow "test" SDL.defaultWindow
  runAppM $
    reactimate $
      limitExecutionRate 60 $
        inputSignal
          >>> (gameSignal (MainMenu MainMenuState) >>> drawSignal window)
          *> arr
            ( \i -> if inputQuit i then Just () else Nothing
            )
  SDL.quit