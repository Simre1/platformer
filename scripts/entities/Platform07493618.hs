{-# LANGUAGE NoImplicitPrelude #-}
module Platform07493618 where

import ScriptPrelude

entityScript :: EntityScript
entityScript options = do
  e <- makePlatform (entityPosition options) (entitySize options)
  set global (Gravity (V2 0 (-600)), Damping 0.9)
  pure $ \_ -> pure ()

makePlatform :: V2 Int -> V2 Int -> SystemT World AppM Entity
makePlatform pos size@(V2 x y) = mdo
  e <-
    newEntity
      ( Platform,
        StaticBody,
        Position (fromIntegral <$> (pos + V2 ((x) `quot` 2) ((-y) `quot` 2))),
        draw e
      )
  _ <- newEntity (Shape e $ cRectangle (fromIntegral <$> size), platformFilter, Elasticity 0.2, Friction 0.8)
  pure e
  where
    draw e = makeDraw 0 . dxtSystem @World $ do
      Position pos <- get e
      lift $ fillRectangle (white `withOpacity` 1) $ Just (Rectangle (round <$> pos) size)


platformFilter :: CollisionFilter
platformFilter = CollisionFilter noCollisionGroup (maskList [4]) (maskList [1, 2, 3, 5, 6])
