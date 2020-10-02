module Main where

import Game.Step
import Control.FrameRate
import Control.AppM
import Input

import Graphics.Step
import Game.Step
import Graphics.Step
import qualified SDL
import Control.Monad (when)
import Game.World
import System.Environment (getArgs)
import Data.Ogmo
import qualified Data.Text as T

main :: IO ()
main = do
  args <- getArgs
  case args of
    [level] -> do
      level <- fmap (either error id) $ loadLevel (T.pack level)
      SDL.initializeAll
      win <- SDL.createWindow "test" SDL.defaultWindow {SDL.windowResizable = True}
      runAppM $ do
        limit <- limitExecutionRate 60
        render <- makeGraphicsStep win
        runWorld $ do
          step <- makeGameStep level
          let loop = do
                shouldQuit <- limit $ do
                  input <- getInput
                  step input
                  render input
                  pure $ inputQuit input
                when (not shouldQuit) $ loop
          loop
      SDL.quit
    _ -> putStrLn "You need to supply an ogmo level as an argument"
