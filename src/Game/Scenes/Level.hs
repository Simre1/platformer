module Game.Scenes.Level where

import AppM
import Control.Monad.IO.Class (MonadIO)
import Game.World
import Game.Scene
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class (lift)
import Control.Signal
import Apecs (SystemT, runSystem)
import Input
import Control.Arrow
import Apecs
import Apecs.Physics
import Linear.V2

levelSignal :: Signal AppM (Input, LevelState) (Either Scene LevelState)
levelSignal = arrM $ \(i,ls) -> do
  runApecs ls $ do
    stepPhysics 0.016
    cmap $ (\(Position p) -> Position $ p + (fromIntegral <$> inputDirection i))
  pure $ Right ls
