{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Game.Scenes.Level where

import Apecs
import Apecs (SystemT, runSystem)
import Apecs.Physics
import AppM
import Control.Arrow
import Control.Monad.IO.Class (MonadIO)
import Control.Monad (forM_)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader
import Control.Signal
import Data.Colour (withOpacity)
import Data.Colour.Names (black, white)
import Debug.Trace (traceShow, traceShowId)
import Game.Scene
import Game.World
import Input
import Linear.V2

levelSignal :: Signal AppM (Input, LevelState) (Either Scene LevelState)
levelSignal = makeSignal level
  where
    makeSignal :: Signal (LevelM AppM) Input () -> Signal AppM (Input, LevelState) (Either Scene LevelState)
    makeSignal sig = Signal $ \(i, ls) -> do
      (_, cont) <- runLevelM ls $ stepSignal sig i
      pure (Right ls, makeSignal cont)

level :: Signal (LevelM AppM) Input ()
level = feedback 0 $ arrM $ \(i,prevY) -> embedApecs $ do
  stepPhysics (1 / 60)
  jump <- cfold (\_ (Player cj, Position (V2 _ y)) -> if cj then succ prevY else 0) 0
  let (V2 iX _) = inputDirection i
  cmapM $ \(Player _, Velocity v, Position pos@(V2 pX pY), Angle a, ShapeList [e]) ->
      let canJump = jump > 10 in
      let V2 (sX,fX) (sY,fY) = V2 (fx canJump) (fy canJump) <*> inputDirection i
      in do
        set e $ SurfaceVelocity (V2 sX sY)
        pure $ (Force (V2 fX fY), SurfaceVelocity (V2 sX sY))
  pure ((), jump)
  where
    nearly a b = traceShow (a, b) $ a - b < 0.0001
    fx canJump (fromIntegral -> iX)
      | canJump = (-2000000 * iX, iX * 1500)
      | otherwise = (0, 1000 * iX)
    fy canJump (fromIntegral -> iY)
      | iY > 0 && canJump = (0, 210000)
      | otherwise = (0, 0)