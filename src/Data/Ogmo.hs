module Data.Ogmo where

import Control.Applicative (Applicative (liftA2))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Colour (AlphaColour, Colour, black, withOpacity)
import Data.Colour.SRGB
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Word (Word8)
import Linear.V2
import qualified System.FilePath as F

loadLevel :: MonadIO m => T.Text -> m (Either String Level)
loadLevel = liftIO . eitherDecodeFileStrict' . T.unpack

loadProject :: MonadIO m => T.Text -> m (Either String Project)
loadProject (T.unpack -> path) = liftIO $ fmap ($takeDictionary path) <$> eitherDecodeFileStrict' path
  where
    takeDictionary = T.pack . F.takeDirectory

data Project = Project
  { pName :: T.Text,
    pVersion :: T.Text,
    pLevelPaths :: [T.Text],
    pLevelValues :: [T.Text],
    pEntityTags :: [T.Text],
    pEntities :: [ProjectEntity],
    pTileSets :: [ProjectTileset],
    pWorkingDirectory :: T.Text
  }
  deriving (Show)

data ProjectEntity = ProjectEntity
  { peId :: T.Text,
    peName :: T.Text,
    peSize :: V2 Int,
    peOrigin :: V2 Int,
    peShape :: (EntityShape, [V2 Double]),
    peColor :: AlphaColour Double,
    rotationDegrees :: Int,
    peTags :: [T.Text],
    peTexture :: Maybe T.Text
  }
  deriving (Show)

data EntityShape = ERectangle deriving (Show)

data ProjectTileset = ProjectTileset
  { ptLabel :: T.Text,
    ptTexture :: T.Text,
    ptTileSize :: V2 Int,
    ptTileSeparation :: V2 Int
  }
  deriving (Show)

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
    eSize :: Maybe (V2 Int),
    eValues :: HM.HashMap T.Text T.Text
  }
  deriving (Show)

instance FromJSON (T.Text -> Project) where
  parseJSON = withObject "Project" $ \v ->
    Project
      <$> v .: "name"
      <*> v .: "ogmoVersion"
      <*> v .: "levelPaths"
      <*> v .: "levelValues"
      <*> v .: "entityTags"
      <*> v .: "entities"
      <*> v .: "tilesets"

instance FromJSON ProjectTileset where
  parseJSON = withObject "ProjectTileset" $ \v ->
    ProjectTileset
      <$> v .: "label"
      <*> v .: "path"
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
      <*> v .:? "texture"
    where
      parsePoint :: Parser [Object] -> Parser [V2 Double]
      parsePoint parser = parser >>= traverse (\v -> V2 <$> v .: "x" <*> v .: "y")
      parseColour :: Parser T.Text -> Parser (AlphaColour Double)
      parseColour = (=<<) $ \t -> do
        let (r, rest1) = T.splitAt 2 $ T.drop 1 t
            (g, rest2) = T.splitAt 2 rest1
            (b, rest3) = T.splitAt 2 rest2
            (a, _) = T.splitAt 2 rest3
        rgb <- sRGB24 <$> parseColorComponent t r <*> parseColorComponent t g <*> parseColorComponent t b
        alpha <- parseColorComponent t a
        pure $ rgb `withOpacity` alpha
        where
          parseColorComponent :: Num a => T.Text -> T.Text -> Parser a
          parseColorComponent errorText t =
            let (a, b) = T.splitAt 1 t
             in (\x -> (+) (x * 16)) <$> fromHex errorText a <*> fromHex errorText b
          fromHex :: Num a => T.Text -> T.Text -> Parser a
          fromHex errorText = \case
            "0" -> pure 0
            "1" -> pure 1
            "2" -> pure 2
            "3" -> pure 3
            "4" -> pure 4
            "5" -> pure 5
            "6" -> pure 6
            "7" -> pure 7
            "8" -> pure 8
            "9" -> pure 9
            "a" -> pure 10
            "b" -> pure 11
            "c" -> pure 12
            "d" -> pure 13
            "e" -> pure 14
            "f" -> pure 15
            _ -> fail $ "Color string (" <> show errorText <> ") could not be parsed."

instance FromJSON Entity where
  parseJSON = withObject "Entity" $ \v ->
    Entity
      <$> v .: "name"
      <*> v .: "id"
      <*> v .: "_eid"
      <*> (V2 <$> v .: "x" <*> v .: "y")
      <*> (V2 <$> v .: "originX" <*> v .: "originY")
      <*> (liftA2 V2 <$> v .:? "width" <*> v .:? "height")
      <*> (v .: "values" >>= parseValues)
    where
      parseValues :: Value -> Parser (HM.HashMap T.Text T.Text)
      parseValues = withObject "CustomValue" $ traverse parseJSON

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
