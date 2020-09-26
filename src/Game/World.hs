{-# LANGUAGE TemplateHaskell #-}

module Game.World where

import Apecs
import Apecs.Physics
import AppM
import Control.Monad (forM_, when)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO)
import qualified Data.Binary as B
import GHC.Generics (Generic)
import qualified Data.ByteString as B
import Control.Exception (throwIO)
import qualified Data.Ogmo as O
import Data.Maybe (fromMaybe)
import Debug.Trace (traceShowId)


data Player = Player {touchingGround :: Bool}

instance Component Player where
  type Storage Player = Unique Player

data Platform = Platform (V2 Int)

instance Component Platform where
  type Storage Platform = Map Platform

makeWorld "World" [''Player, ''Platform, ''Physics]

type InitWorld m = (MonadIO m, MonadFix m) => SystemT World m ()

makePlayer :: V2 Int -> V2 Int -> InitWorld m
makePlayer pos (V2 sX sY) = do
  begin <- mkBeginCB $ \c -> do
    isPlayer <- exists (collisionBodyA c) (Proxy :: Proxy Player)
    when isPlayer $ do
      let (V2 x y) = collisionNormal c
      -- cmap $ \(Player _, Velocity v) -> Velocity $ v + ((*) <$> v <*> (negate <$> V2 x y))
      when (y <= 0 && abs x < 0.5) $ cmap (\(Player _) -> Player True)
    pure True
  separate <- mkSeparateCB $ \_ -> cmap (\(Player _) -> Player False)

  let handler = CollisionHandler (Wildcard 0) (Just begin) (Just separate) Nothing Nothing

  e <- do
      newEntity
        ( Player False,
          DynamicBody,
          Position (traceShowId $ fromIntegral <$> (pos + V2 ((sX) `quot` 2) ((-sY) `quot` 2))),
          Moment (1/0),
          BodyMass 10
        )
  _ <-
    newEntity
      ( Shape e playerShape,
        playerFilter,
        handler,
        Friction 0.5
      )

  pure ()
  where
    playerShape = cRectangle $ V2 32 64

makePlatform :: V2 Int -> V2 Int -> InitWorld m
makePlatform pos size@(V2 x y) = do
  e <-
    newEntity
      ( Platform size,
        StaticBody,
        Position (traceShowId $ fromIntegral <$> (pos + V2 ((x) `quot` 2) ((-y) `quot` 2)))
      )
  _ <- newEntity (Shape e $ cRectangle (fromIntegral <$> size), platformFilter, Elasticity 0.2, Friction 0.8)
  pure ()

platformFilter :: CollisionFilter
platformFilter = CollisionFilter noCollisionGroup (maskList [4]) (maskList [1, 2, 3, 5, 6])

playerFilter :: CollisionFilter
playerFilter = CollisionFilter noCollisionGroup (maskList [1]) (maskList [2, 3, 4, 5, 6])
