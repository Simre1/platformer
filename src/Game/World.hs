{-# LANGUAGE TemplateHaskell #-}

module Game.World where

import Prelude

import Apecs
import Apecs.Physics
import AppM

data Player = Player

instance Component Player where
  type Storage Player = Unique Player

makeWorld "World" [''Player, ''Physics]

newWorld :: AppM World
newWorld = do
  w <- liftIO initWorld 
  flip runSystem w $ do
    newEntity (Player, KinematicBody, Position (V2 0 0))
  pure w