{-# LANGUAGE TemplateHaskell #-}
module Game.LoadOgmo where

import Apecs
import Apecs.Physics
import Control.AppM
import Control.Applicative (Alternative ((<|>)))
import Control.Monad (forM_, join)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO (..))
import qualified Data.HashMap.Strict as H
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe)
import Data.Ogmo
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as VU
import Game.World
import Graphics.Draw
import Linear.V2
import ScriptPrelude
import System.Directory
import Data.FileEmbed
import Scripts

project :: Project
project = either 
  (\l -> error l)
  (\r -> r)
  (parseProject projectByteString "ogmo")
  where projectByteString = $(embedFile "ogmo/project.ogmo")

loadOgmo :: Level -> SystemT World AppM (Input -> SystemT World AppM ())
loadOgmo level = do
  liftIO $ setCurrentDirectory $ T.unpack $ pWorkingDirectory project

  combine (lLayers level) $ \l -> do
    case lType l of
      EntityLayer entities -> do
        combine entities $ \entity -> do
          case HM.lookup (eClass entity) scriptMap of
            Nothing -> pure (\_ -> pure ())
            Just entityScript -> do
              texturePath <- fmap join . traverse makeTexturePath $ projectEntity (eClass entity) >>= peTexture
              let entityOptions = EntityOptions
                    { entitySize = fromMaybe (V2 32 32) $ eSize entity <|> peSize <$> projectEntity (eClass entity)
                    , entityPosition = inverse l $ ePos entity
                    , entityTexture = texturePath
                    }
              step <- entityScript entityOptions
              pure step
  where
    combine :: (Foldable f, Applicative b, Applicative x) => f a -> (a -> b (i -> x ())) -> b (i -> x ())
    combine l f = foldl (\a b -> a *> f b) (pure (\_ -> pure ())) l

    loadingEnv :: LoadingEnv
    loadingEnv =
      LoadingEnv
        (H.fromList $ (\e -> (peId e, e)) <$> pEntities project)
        (H.fromList $ (\ts -> (ptLabel ts, ts)) <$> pTileSets project)
    projectEntity :: T.Text -> Maybe ProjectEntity
    projectEntity key = H.lookup key (entityMap loadingEnv)
    projectTileset :: T.Text -> Maybe ProjectTileset
    projectTileset key = H.lookup key (entityTileSets loadingEnv)
    inverse :: Layer -> V2 Int -> V2 Int
    inverse l (V2 x y) =
      let (V2 _ lY) = lGridCellSize l * lGridCells l
       in V2 x (lY - y)

data LoadingEnv = LoadingEnv
  { entityMap :: H.HashMap T.Text ProjectEntity,
    entityTileSets :: H.HashMap T.Text ProjectTileset
  }
