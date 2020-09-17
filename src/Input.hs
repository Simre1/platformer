module Input where

import qualified SDL

import Data.Bool (bool)
import Linear.V2
import Control.Signal
import AppM

data Input = Input
  { inputDirection :: (V2 Int)
  , inputQuit :: Bool
  } deriving (Show, Eq)

inputSignal :: Signal AppM () Input
inputSignal = arrM_ $ do
  events <- SDL.pollEvents
  checkScancode <- SDL.getKeyboardState
  let y = bool 0 1 (checkScancode SDL.ScancodeUp) + bool 0 (-1) (checkScancode SDL.ScancodeDown)
      x = bool 0 1 (checkScancode SDL.ScancodeRight) + bool 0 (-1) (checkScancode SDL.ScancodeLeft) 
  let shouldQuit = foldl gatherQuit False events
  pure $ Input (V2 x y) shouldQuit
    where 
      gatherQuit :: Bool -> SDL.Event -> Bool
      gatherQuit True _ = True
      gatherQuit _ (SDL.Event _ SDL.QuitEvent) = True
      gatherQuit _ _ = False

