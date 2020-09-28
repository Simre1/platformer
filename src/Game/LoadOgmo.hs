module Game.LoadOgmo where

import Apecs
import Control.Applicative (Alternative ((<|>)))
import Control.Monad (join, forM_)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO (..))
import qualified Data.HashMap.Strict as H
import Data.Maybe (fromMaybe)
import Data.Ogmo
import qualified Data.Text as T
import Game.World
import Linear.V2
import Apecs.Physics
import Graphics.Draw
import System.Directory


loadOgmo :: (MonadIO m, MonadFix m) => Project -> Level -> SystemT World m ()
loadOgmo project level = do
  liftIO $ setCurrentDirectory $ T.unpack $ pWorkingDirectory project

  set global (Gravity (V2 0 (-600)), Damping 0.9)

  let [l] = lLayers level
      entities = lEntities l
  forM_ entities $ \entity -> do
    texturePath <- fmap join . traverse makeTexturePath $ projectEntity (eClass entity) >>= peTexture

    case eName entity of
      "Player" -> makePlayer (fromMaybe DefaultPath texturePath) (inverse l $ ePos entity) (fromMaybe (V2 32 64) $ peSize <$> projectEntity (eClass entity))
      "Platform" ->
        makePlatform
          (inverse l $ ePos entity)
          ( fromMaybe (V2 100 20) $
              eSize entity
                <|> (peSize <$> projectEntity (eClass entity))
          )
      _ -> pure ()
  where
    loadingEnv :: LoadingEnv
    loadingEnv = LoadingEnv $ H.fromList $ (\e -> (peId e, e)) <$> pEntities project
    projectEntity :: T.Text -> Maybe ProjectEntity
    projectEntity key = H.lookup key (entityMap loadingEnv)
    inverse :: Layer -> V2 Int -> V2 Int
    inverse l (V2 x y) =
      let (V2 _ lY) = lGridCellSize l * lGridCells l
       in V2 x (lY - y)

data LoadingEnv = LoadingEnv
  { entityMap :: H.HashMap T.Text ProjectEntity
  }