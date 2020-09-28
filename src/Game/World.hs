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
import Input
import Debug.Trace (traceShowId)
import Graphics.Dxt
import Graphics.Draw
import Control.Monad.Reader (ReaderT)
import Data.Geometry
import Data.Colour (withOpacity)
import Data.Colour.Names (white)

data Player = Player {touchingGround :: Bool}

instance Component Player where
  type Storage Player = Unique Player

data Platform = Platform

instance Component Platform where
  type Storage Platform = Map Platform

data World = World
  { wPlayer :: !(Storage Player)
  , wPlatform :: !(Storage Platform)
  , wPhysics :: !(Storage Physics)
  , wDraw :: !(Storage (Draw (Dxt World)))
  , wEntities :: !(Storage EntityCounter)
  }

initWorld :: IO World
initWorld = World <$> explInit <*> explInit <*> explInit <*> explInit <*> explInit

instance Monad m => Has World m Player where
  getStore = SystemT (asks wPlayer)
instance Monad m => Has World m Physics where
  getStore = SystemT (asks wPhysics)
instance Monad m => Has World m Platform where
  getStore = SystemT (asks wPlatform)
instance Monad m => Has World m EntityCounter where
  getStore = SystemT (asks wEntities)
instance Monad m => Has World m (Draw (Dxt World)) where
  getStore = SystemT (asks wDraw)

type InitWorld m = (MonadIO m, MonadFix m) => SystemT World m ()

makePlayer :: TexturePath -> V2 Int -> V2 Int -> InitWorld m
makePlayer path pos (V2 sX sY) = do
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
          BodyMass 10,
          draw
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
    draw = makeDraw 5 . dxtSystem @World $ cmapM_ $ \(Player _, Position pos) -> 
      lift $ copyTex path Nothing (Just (Rectangle (round <$> pos) (V2 32 64))) Nothing 0 (V2 False False)


makePlatform :: V2 Int -> V2 Int -> InitWorld m
makePlatform pos size@(V2 x y) = mdo
  e <-
    newEntity
      ( Platform,
        StaticBody,
        Position (traceShowId $ fromIntegral <$> (pos + V2 ((x) `quot` 2) ((-y) `quot` 2))),
        draw e
      )
  _ <- newEntity (Shape e $ cRectangle (fromIntegral <$> size), platformFilter, Elasticity 0.2, Friction 0.8)
  pure ()
  where
    draw e = makeDraw 0 . dxtSystem @World $ do
      Position pos <- get e
      lift $ fillRectangle (white `withOpacity` 1) $ Just (Rectangle (round <$> pos) size)

platformFilter :: CollisionFilter
platformFilter = CollisionFilter noCollisionGroup (maskList [4]) (maskList [1, 2, 3, 5, 6])

playerFilter :: CollisionFilter
playerFilter = CollisionFilter noCollisionGroup (maskList [1]) (maskList [2, 3, 4, 5, 6])
