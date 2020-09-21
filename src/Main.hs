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

main :: IO ()
main = do
  SDL.initializeAll
  window <- SDL.createWindow "test" SDL.defaultWindow
  runAppM $
    reactimate $
      limitExecutionRate 60 $ inputSignal
          >>> (gameSignal (MainMenu MainMenuState) >>> drawSignal window)
          *> arr
            ( \i -> if inputQuit i then Just () else Nothing
            )
  SDL.quit

countFrameRate :: Signal AppM a a
countFrameRate = feedback 0 $ arrM $ \(a,pT) -> do
  now <- liftIO $ getMonotonicTimeNSec
  liftIO $ print (nanosToFPS $ fromIntegral $ now - pT)
  pure (a,now)
  where 
    nanosToFPS nanos = 1000000000 / nanos