{-# LANGUAGE NoImplicitPrelude #-}
module Player04718852 where

import ScriptPrelude
import Data.Maybe
import Data.IORef (atomicModifyIORef', newIORef)
import Control.Monad (when)

entityScript :: EntityScript
entityScript options = do
  
  jumpRef <- liftIO $ newIORef 0

  e <- makePlayer (fromJust $ entityTexture options) (entityPosition options) (entitySize options)
  pure $ \input -> do
    cmapM $ \(Player cj, Velocity v, Position pos@(V2 pX pY), Angle a, ShapeList [e]) -> do
      canJump <- liftIO . fmap (>10) . atomicModifyIORef' jumpRef $ (\i -> (if cj then succ i else 0, i))
      let V2 (sX,fX) (sY,fY) = V2 (fx canJump) (fy canJump) <*> inputDirection input
      do
        set e $ SurfaceVelocity (V2 sX sY)
        pure $ (Force (V2 fX 0), Impulse (V2 0 fY), SurfaceVelocity (V2 sX sY))
  where
    fx canJump (fromIntegral -> iX)
      | canJump = (-4000 * iX, iX * 1500)
      | otherwise = (0, 1000 * iX)
    fy canJump (fromIntegral -> iY)
      | iY > 0 && canJump = (0, 5000)
      | otherwise = (0, 0)

makePlayer :: TexturePath -> V2 Int -> V2 Int -> SystemT World AppM Entity
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
          Position (fromIntegral <$> (pos + V2 ((sX) `quot` 2) ((-sY) `quot` 2))),
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

  pure e
  where
    playerShape = cRectangle $ V2 32 64
    draw = makeDraw 5 . dxtSystem @World $ cmapM_ $ \(Player _, Position pos) -> 
      lift $ copyTex path Nothing (Just (Rectangle (round <$> pos) (V2 32 64))) Nothing 0 (V2 False False)


playerFilter :: CollisionFilter
playerFilter = CollisionFilter noCollisionGroup (maskList [1]) (maskList [2, 3, 4, 5, 6])
