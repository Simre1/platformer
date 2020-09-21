{-# LANGUAGE TemplateHaskell #-}

module Game.World where

import Apecs
import Apecs.Physics
import Control.Monad (forM_, when)
import AppM
import Control.Monad.IO.Class (MonadIO)
import Data.Binary
import Data.Colour.Names (white, green)
import qualified Data.Unique as U
import GHC.Generics (Generic)
import Data.Colour (withOpacity)
import Graphics.Next
import qualified SDL as SDL

data Player = Player {touchingGround :: Bool}

instance Component Player where
  type Storage Player = Unique Player

data Platform = Platform

instance Component Platform where
  type Storage Platform = Map Platform

makeWorld "World" [''Player, ''Platform, ''Graphics, ''Physics]

-- data World = World
--   { sPlayer :: !(Storage Player),
--     sPhysics :: !(Storage Physics),
--     sPlatform :: !(Storage Platform),
--     sGraphics :: !(Storage Graphics),
--     sEntityCounter :: !(Storage EntityCounter)
--   }

-- initWorld :: IO World
-- initWorld = World <$> explInit <*> explInit <*> explInit <*> explInit <*> explInit

-- instance Monad m => Has World m Player where
--   getStore = SystemT (asks sPlayer)

-- instance Monad m => Has World m Physics where
--   getStore = SystemT (asks sPhysics)
-- instance Monad m => Has World m Platform where

--   getStore = SystemT (asks sPlatform)
-- instance Monad m => Has World m EntityCounter where
--   getStore = SystemT (asks sEntityCounter)


type InitWorld m = MonadIO m => SystemT World m ()

setGravity :: InitWorld m
setGravity = do
  newEntity (Gravity (V2 0 (-200)), Damping 0.9)
  pure ()

makePlayer :: V2 Int -> InitWorld m
makePlayer pos = do
  begin <- mkBeginCB $ \c -> do
    isPlayer <- exists (collisionBodyA c) (Proxy :: Proxy Player)
    when isPlayer $ do
      let (V2 x y) = collisionNormal c
      -- cmap $ \(Player _, Velocity v) -> Velocity $ v + ((*) <$> v <*> (negate <$> V2 x y))
      when (y <= 0 && abs x < 0.5) $ cmap (\(Player _) -> Player True)
    pure True
  separate <- mkSeparateCB $ \_ -> cmap (\(Player _) -> Player False)

  let handler = CollisionHandler (Wildcard 0) (Just begin) (Just separate) Nothing Nothing

  e <- newEntity (Player False, DynamicBody, Position (fromIntegral <$> pos))
  shape <- newEntity (Shape e playerShape, Mass 10, playerFilter, handler, Friction 0.9)

  pure ()
  where
    playerShape = cRectangle $ V2 32 64

makePlatform :: V2 Int -> V2 Int -> InitWorld m
makePlatform pos size = do
  e <- newEntity (Platform, StaticBody, Position (fromIntegral <$> pos), (Draw 0 $ FillRectangle (white `withOpacity` 1) $ Just (Rectangle pos size)))
  shape <- newEntity (Shape e $ cRectangle (fromIntegral <$> size), platformFilter, Elasticity 0.5, Friction 0.8)
  pure ()

platformFilter :: CollisionFilter
platformFilter = CollisionFilter noCollisionGroup (maskList [4]) (maskList [1, 2, 3, 5, 6])

playerFilter :: CollisionFilter
playerFilter = CollisionFilter noCollisionGroup (maskList [1]) (maskList [2, 3, 4, 5, 6])

type WorldGen = [GenEntity]

newWorld :: WorldGen -> AppM World
newWorld genWorld = do
  w <- liftIO $ initWorld
  flip runSystem w $ do
    setGravity
    forM_ genWorld generateEntity
  pure w

testWorldGen :: WorldGen
testWorldGen =
  [ GenPlayer (V2 100 100),
    GenPlatform (V2 480 10) (V2 800 20),
    GenPlatform (V2 700 220) (V2 300 20),
    GenPlatform (V2 200 400) (V2 350 20)
  ]

generateEntity :: MonadIO m => GenEntity -> SystemT World m ()
generateEntity = \case
  GenPlayer pos -> makePlayer pos
  GenPlatform pos size -> makePlatform pos size

data GenEntity
  = GenPlayer (V2 Int)
  | GenPlatform (V2 Int) (V2 Int)
  deriving (Generic, Show)

instance Binary GenEntity
