module Data.Ogmo where

import Control.Applicative (Applicative (liftA2))
import Data.Aeson
import Data.Colour (withOpacity, AlphaColour, black, Colour)
import qualified Data.Text as T
import Linear.V2
import Data.Aeson.Types (Parser)
import Control.Monad.IO.Class (MonadIO, liftIO)


loadLevel :: MonadIO m => FilePath -> m (Either String Level)
loadLevel = liftIO . eitherDecodeFileStrict'

loadProject :: MonadIO m => FilePath -> m (Either String Project)
loadProject = liftIO . eitherDecodeFileStrict' 

data Project = Project
  { pName :: T.Text,
    pVersion :: T.Text,
    pLevelPaths :: [T.Text],
    pLevelValues :: [T.Text],
    pEntityTags :: [T.Text],
    pEntities :: [ProjectEntity],
    pTileSets :: [ProjectTileset]
  } deriving Show

instance FromJSON Project where
  parseJSON = withObject "Project" $ \v ->
    Project
      <$> v .: "name"
      <*> v .: "ogmoVersion"
      <*> v .: "levelPaths"
      <*> v .: "levelValues"
      <*> v .: "entityTags"
      <*> v .: "entities"
      <*> v .: "tilesets"

data ProjectEntity = ProjectEntity
  { peId :: T.Text,
    peName :: T.Text,
    peSize :: V2 Int,
    peOrigin :: V2 Int,
    peShape :: (EntityShape, [V2 Double]),
    peColor :: AlphaColour Double,
    rotationDegrees :: Int,
    peTags :: [T.Text],
    peValues :: [T.Text],
    peTexture :: Maybe T.Text
  } deriving Show

data ProjectTileset = ProjectTileset
  { ptLabel :: T.Text,
    ptTexture :: T.Text,
    ptTileSize :: V2 Int,
    ptTileSeparation :: V2 Int
  } deriving Show

data EntityShape = ERectangle deriving Show

instance FromJSON ProjectTileset where
  parseJSON = withObject "ProjectTileset" $ \v ->
    ProjectTileset
      <$> v .: "label" <*> v .: "path" 
      <*> (V2 <$> v .: "tileWidth" <*> v .: "tileHeight")
      <*> (V2 <$> v .: "tileSeparationX" <*> v .: "tileSeparationY")

instance FromJSON EntityShape where
  parseJSON = withText "EntityShape" $ \t ->
    case t of
      "Rectangle" -> pure ERectangle

instance FromJSON ProjectEntity where
  parseJSON = withObject "ProjectEntity" $ \v ->
    ProjectEntity
      <$> v .: "exportID"
      <*> v .: "name"
      <*> (v .: "size" >>= \v -> V2 <$> v .: "x" <*> v .: "y")
      <*> (v .: "origin" >>= \v -> V2 <$> v .: "x" <*> v .: "y")
      <*> (v .: "shape" >>= \v -> (,) <$> v .: "label" <*> (parsePoint $ v .: "points"))
      <*> parseColour (v .: "color")
      <*> v .: "rotationDegrees"
      <*> v .: "tags"
      <*> v .: "values"
      <*> v .:? "texture" 
    where 
      parsePoint :: Parser [Object] -> Parser [V2 Double]
      parsePoint parser = parser >>= traverse (\v -> V2 <$> v .: "x" <*> v .: "y") 
      parseColour :: Parser T.Text -> Parser (AlphaColour Double)
      parseColour parser = pure $ black `withOpacity` 1


instance FromJSON Entity where
  parseJSON = withObject "Entity" $ \v ->
    Entity
      <$> v .: "name"
      <*> v .: "id"
      <*> v .: "_eid"
      <*> (V2 <$> v .: "x" <*> v .: "y")
      <*> (V2 <$> v .: "originX" <*> v .: "originY")
      <*> (liftA2 V2 <$> v .:? "width" <*> v .:? "height")

instance FromJSON Layer where
  parseJSON = withObject "Layer" $ \v ->
    Layer
      <$> (V2 <$> v .: "offsetX" <*> v .: "gridCellHeight")
      <*> (V2 <$> v .: "gridCellWidth" <*> v .: "gridCellHeight")
      <*> (V2 <$> v .: "gridCellsX" <*> v .: "gridCellsY")
      <*> v .: "entities"

instance FromJSON Level where
  parseJSON = withObject "Level" $ \v ->
    Level
      <$> v .: "ogmoVersion"
      <*> (V2 <$> v .: "width" <*> v .: "height")
      <*> (V2 <$> v .: "offsetX" <*> v .: "offsetY")
      <*> v .: "layers"

data Level = Level
  { lOgmoVersion :: T.Text,
    lSize :: V2 Int,
    lOffsetPos :: V2 Int,
    lLayers :: [Layer]
  }
  deriving (Show)

data Layer = Layer
  { lOffset :: V2 Int,
    lGridCellSize :: V2 Int,
    lGridCells :: V2 Int,
    lEntities :: [Entity]
  }
  deriving (Show)

data Entity = Entity
  { eName :: T.Text,
    eId :: Int,
    eClass :: T.Text,
    ePos :: V2 Int,
    eOrigin :: V2 Int,
    eSize :: Maybe (V2 Int)
  }
  deriving (Show)
