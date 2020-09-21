{-# LANGUAGE ScopedTypeVariables #-}

module Game.Scenes.Level where

import AppM
import Apecs
import Apecs (SystemT, runSystem)
import Apecs.Physics
import Control.Monad.IO.Class (MonadIO)
import Control.Arrow
import Control.Monad (forM_)
import Control.Monad.Trans.Class (lift)
import Control.Signal
import Data.Colour (withOpacity)
import Data.Colour.Names (white, black)
import Game.Scene
import Game.World
import Graphics.Next
import Input
import Control.Monad.Trans.Reader
import Linear.V2

levelSignal :: Signal AppM (Input, LevelState) (Either Scene LevelState)
levelSignal = makeSignal level
  where
    makeSignal :: Signal (LevelM AppM) Input () -> Signal AppM (Input, LevelState) (Either Scene LevelState)
    makeSignal sig = Signal $ \(i, ls) -> do
      (_, cont) <- runLevelM ls $ stepSignal sig i
      pure (Right ls, makeSignal cont)

level :: Signal (LevelM AppM) Input ()
level = arrM $ \i -> embedApecs $ do
  stepPhysics (1 / 60)
  let (V2 iX _) = inputDirection i
  set global $ Camera $ Rectangle (V2 480 270) (V2 960 540)
  cmap $ \(Player canJump, Velocity v, Position p, Angle a) ->
    (,)
      (Force ((V2 (fx canJump) (fy canJump)) <*> (fromIntegral <$> inputDirection i)))
      (Draw 0 $ CopyEx TPlayer Nothing (Just (Rectangle (round <$> p) (V2 32 64))) Nothing (rotation a) (pure False))
  where
    fx canJump iX
      | canJump = iX * 3000
      | otherwise = 1000 * iX
    fy canJump iY
      | iY > 0 && canJump = 100000
      | otherwise = 0
    rotation a = (negate a / pi) * 180