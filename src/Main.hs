module Main where

import Graphics.MainSignal
import Game.MainSignal
import Game.Scene
import Input
import qualified SDL
import Control.Signal
import Control.Arrow
import AppM

import GHC.Clock
import Control.Monad.IO.Class (MonadIO(liftIO))
import System.Environment (getArgs)
import qualified Data.Text as T

main :: IO ()
main = do
  args <- getArgs
  case args of
    [project, level] -> do
      SDL.initializeAll
      win <- SDL.createWindow "test" SDL.defaultWindow
      runAppM $
        reactimate $
          limitExecutionRate 60 $ inputSignal >>> 
            (duplicate >>> second (gameSignal (MainMenu (MainMenuState (T.pack project) (T.pack level)))) >>> drawSignal win)
              *> arr
                ( \i -> if inputQuit i then Just () else Nothing
                )
      SDL.quit
    _ -> putStrLn "You need to supply an ogmo project as well as an ogmo level as arguments"

countFrameRate :: Signal AppM a a
countFrameRate = feedback 0 $ arrM $ \(a,pT) -> do
  now <- liftIO $ getMonotonicTimeNSec
  liftIO $ print (nanosToFPS $ fromIntegral $ now - pT)
  pure (a,now)
  where 
    nanosToFPS nanos = 1000000000 / nanos