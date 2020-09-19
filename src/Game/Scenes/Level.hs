{-# LANGUAGE ScopedTypeVariables #-}
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
levelSignal = makeSignal level
  where
    makeSignal :: Signal (LevelM AppM) Input () -> Signal AppM (Input, LevelState) (Either Scene LevelState) 
    makeSignal sig = Signal $ \(i,ls) -> do
      (_,cont) <- runLevelM ls $ stepSignal sig i
      pure (Right ls, makeSignal cont)

level :: Signal (LevelM AppM) Input ()
level = arrM $ \i -> embedApecs $ do
  stepPhysics (1/60)
  cmap $ (\(Player canJump, Velocity v) -> Velocity (((V2 (fx canJump) (fy canJump)) <*> (fromIntegral <$> inputDirection i) <*> v) ))
    where fx canJump iX x
            | canJump = iX * 5 + x
            | otherwise = x + 2 * iX
          fy canJump iY y
            | iY > 0 && canJump = 400
            | otherwise = y