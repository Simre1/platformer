module ScriptPrelude 
  ( module Prelude
  , module Apecs
  , module Apecs.Physics
  , module Control.AppM
  , module Game.World
  , module Input
  , module Linear.V2
  , module Graphics.Draw
  , module Graphics.Dxt
  , module Data.Colour
  , module Data.Colour.Names
  , module Data.Geometry
  , EntityScript
  , EntityOptions (..)
  , defaultScript
  )

  where

import Prelude
import Apecs.Physics
import Apecs
import Control.AppM
import Input
import Game.World
import Linear.V2
import Graphics.Draw
import Graphics.Dxt
import Data.Colour
import Data.Colour.Names hiding (tan)
import Data.Geometry

type EntityScript = EntityOptions -> SystemT World AppM (Input -> SystemT World AppM ())

data EntityOptions = EntityOptions 
  { entitySize :: V2 Int
  , entityPosition :: V2 Int
  , entityTexture :: Maybe TexturePath
  }

defaultScript :: EntityScript
defaultScript _ = pure (\_ -> pure ())