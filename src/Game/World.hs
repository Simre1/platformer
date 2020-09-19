{-# LANGUAGE TemplateHaskell #-}

module Game.World where

import Prelude

import Apecs
import Apecs.Physics
import AppM
import Control.Monad.IO.Class (MonadIO)
import qualified Data.Unique as U

data Player = Player Bool

instance Component Player where
  type Storage Player = Unique Player

data Platform = Platform (V2 Int)

instance Component Platform where
  type Storage Platform = Map Platform

makeWorld "World" [''Player, ''Physics, ''Platform]

newWorld :: AppM World
newWorld = do
  w <- liftIO initWorld 
  flip runSystem w $ do
    setGravity
    makePlayer
    makePlatform (V2 480 10) (V2 800 20) 
    makePlatform (V2 700 220) (V2 300 20)
    makePlatform (V2 200 400) (V2 350 20)

  pure w

type InitWorld m = MonadIO m => SystemT World m Entity

setGravity :: InitWorld m 
setGravity = do
  newEntity (Gravity (V2 0 (-350)))

makePlayer :: InitWorld m
makePlayer = do

  begin <- mkBeginCB $ \_ -> cmap (\(Player _) -> Player True) *> pure True
  separate <- mkSeparateCB $ \_ -> cmap (\(Player _) -> Player False)

  let handler = CollisionHandler (Wildcard 0) (Just begin) (Just separate) Nothing Nothing


  e <- newEntity (Player False, DynamicBody, Position (V2 100 100))
  shape <- newEntity (Shape e ballShape, Mass 10, playerFilter, Elasticity 1, Friction 0.75, handler)


  pure e
  where ballShape = cCircle 30

makePlatform :: V2 Int -> V2 Int -> InitWorld m
makePlatform pos size = do
  e <- newEntity (Platform size, StaticBody, Position (fromIntegral <$> pos))
  shape <- newEntity (Shape e $ cRectangle (fromIntegral <$> size), platformFilter, Elasticity 0.2, Friction 0.5)
  pure e

platformFilter :: CollisionFilter
platformFilter = CollisionFilter 0 (maskList [4]) (maskList [1,2,3,5,6])

playerFilter :: CollisionFilter
playerFilter = CollisionFilter 0 (maskList [1]) (maskList [2,3,4,5,6])