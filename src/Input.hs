module Input where

import qualified SDL
import qualified SDL.Event as SDL

import Data.Bool (bool)
import Linear.V2
import Control.AppM
import Apecs
import Data.IORef (readIORef, newIORef, IORef)
import Apecs.Core
import Control.Monad.IO.Class (MonadIO)

data Input = Input
  { inputDirection :: (V2 Int)
  , inputMouseLeft :: Maybe (V2 Int)
  , inputMouseRight :: Maybe (V2 Int)
  , inputQuit :: Bool
  } deriving (Show, Eq)

getInput :: MonadIO m => m Input
getInput = do
  events <- SDL.pollEvents
  checkScancode <- SDL.getKeyboardState
  let y = bool 0 1 (checkScancode SDL.ScancodeUp) + bool 0 (-1) (checkScancode SDL.ScancodeDown)
      x = bool 0 1 (checkScancode SDL.ScancodeRight) + bool 0 (-1) (checkScancode SDL.ScancodeLeft) 
  let shouldQuit = foldl gatherQuit False events
      mouseInputLeft = foldl (gatherMouse SDL.ButtonLeft) Nothing events
      mouseInputRight = foldl (gatherMouse SDL.ButtonRight) Nothing events
  pure $ Input (V2 x y) mouseInputLeft mouseInputRight shouldQuit
    where
      gatherMouse :: SDL.MouseButton -> Maybe (V2 Int) -> SDL.Event -> Maybe (V2 Int)
      gatherMouse _ (Just pos) _ = Just pos
      gatherMouse button1 _ (SDL.Event _ (SDL.MouseButtonEvent event)) =
        case event of
          SDL.MouseButtonEventData _ _ _ button2 _ (SDL.P pos) ->
            if button1 == button2
              then Just $ fromEnum <$> pos
              else Nothing
          _ -> Nothing
      gatherQuit :: Bool -> SDL.Event -> Bool
      gatherQuit True _ = True
      gatherQuit _ (SDL.Event _ SDL.QuitEvent) = True
      gatherQuit _ _ = False

