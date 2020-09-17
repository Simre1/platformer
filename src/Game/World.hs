{-# LANGUAGE TemplateHaskell #-}

module Game.World where

import Prelude

import Apecs
import Apecs.Physics
import AppM
import Control.Monad.IO.Class (MonadIO)

data Player = Player

instance Component Player where
  type Storage Player = Unique Player

makeWorld "World" [''Player, ''Physics]

newWorld :: AppM World
newWorld = do
  w <- liftIO initWorld 
  flip runSystem w $ do
    setGravity
    makePlayer
  pure w

type InitWorld m = MonadIO m => SystemT World m Entity

setGravity :: InitWorld m 
setGravity = do
  newEntity (Gravity (V2 0 (-200)))

makePlayer :: InitWorld m
makePlayer = do
  newEntity (Player, DynamicBody, Position (V2 0 0), BodyMass 10, Moment 1)