module Game.Step where

import Control.Monad.IO.Class
import Game.LoadOgmo
import Data.Ogmo
import Scripts
import Apecs.Physics
import Game.World
import Control.AppM
import Input

makeGameStep :: Level -> SystemT World AppM (Input -> SystemT World AppM ())
makeGameStep level = do
  step <- loadOgmo level

  set global (Gravity (V2 0 (-600)), Damping 0.9)

  pure $ \i -> do
    step i
    stepPhysics (1/60) 