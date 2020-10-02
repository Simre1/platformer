{-# LANGUAGE TemplateHaskell #-}

module Game.World where

import Apecs
import Apecs.Physics
import Graphics.Draw
import Graphics.Dxt
import Control.Monad.IO.Class (MonadIO)

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

runWorld :: MonadIO m => SystemT World m a -> m a
runWorld s = liftIO initWorld >>= runSystem s