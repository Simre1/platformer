{-# LANGUAGE UndecidableInstances #-}

module Graphics.V2 where

import Apecs
import Apecs.Core
import AppM
import Control.Monad (forM_, join)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Reader (ReaderT (..), runReaderT)
import Data.Colour
import Data.Colour.SRGB (RGB (RGB), toSRGB24)
import Data.IORef
import qualified Data.Map as M
import qualified Data.Vector.Unboxed as V
import Debug.Trace (traceShow, traceShowId)
import Foreign.C.Types (CDouble(..), CInt)
import Linear.V2
import qualified SDL
import qualified SDL.Image as SDLI
import Unsafe.Coerce (unsafeCoerce)
import qualified Data.HashMap.Strict as HM
import qualified Data.PQueue.Prio.Min as Q
import Data.Foldable (Foldable(fold))
import qualified Data.Vector.Unboxed as V

import Data.Geometry

newtype Draw = Draw (Q.MinPQueue Int (DrawM ())) deriving (Semigroup, Monoid)

draw :: Int -> DrawM () -> Draw
draw i d = Draw $ Q.singleton i d

newtype DrawM a = DrawM {runDrawM :: ReaderT DrawEnv IO a} deriving (Functor, Applicative, Monad, MonadIO)

instance Semigroup (DrawM ()) where
  draw1 <> draw2 = draw2 *> draw2

instance Monoid (DrawM ()) where
  mempty = pure ()

data DrawEnv = DrawEnv
  { envRenderer :: SDL.Renderer,
    envWindow :: SDL.Window,
    envCamera :: Camera,
    envTextures :: TextureName -> SDL.Texture
  }

getDrawEnv :: DrawM DrawEnv
getDrawEnv = DrawM $ ask

loadTexture :: SDL.Renderer -> TextureName -> IO SDL.Texture
loadTexture renderer tn = SDLI.loadTexture renderer (toFilePath tn)
  where
    toFilePath = \case
      TPlayer -> "assets/textures/player.png"

loadTextures :: SDL.Renderer -> IO [SDL.Texture]
loadTextures renderer = loadTexture renderer `traverse` enumFrom (toEnum 0)

data TextureName = TPlayer deriving (Eq, Enum, Bounded)

newtype Camera = Camera (Rectangle Int) deriving (Eq, Show)

data Graphics = Graphics 
  { iWindow :: SDL.Window
  , iRenderer :: SDL.Renderer
  , iTextures :: (TextureName -> SDL.Texture)
  }

drawFrame :: MonadIO m => Graphics -> Camera -> Draw -> m ()
drawFrame (Graphics w r t) camera@(Camera (Rectangle pos size)) (Draw queue) = do
  liftIO $ do
    SDL.rendererDrawColor r SDL.$= SDL.V4 230 230 230 maxBound
    SDL.clear r
    SDL.rendererLogicalSize r SDL.$= Just (fromIntegral <$> size)
    flip runReaderT (DrawEnv r w camera t) . runDrawM $
      foldl (\others action -> others *> action) mempty queue
    SDL.present r

initGraphics :: SDL.Window -> AppM Graphics
initGraphics sdlWindow = do
  renderer <- SDL.createRenderer sdlWindow 0 SDL.defaultRenderer {SDL.rendererType = SDL.AcceleratedRenderer}
  textures <- liftIO $ loadTextures renderer
  SDL.rendererDrawBlendMode renderer SDL.$= SDL.BlendAlphaBlend
  addCleanUp $ forM_ textures $ SDL.destroyTexture
  pure $ Graphics sdlWindow renderer $ \n -> textures !! fromEnum n

getCamera :: DrawM Camera
getCamera = envCamera <$> getDrawEnv

getRenderer :: DrawM SDL.Renderer
getRenderer = envRenderer <$> getDrawEnv

getWindow :: DrawM SDL.Window
getWindow = envWindow <$> getDrawEnv

getTexture :: TextureName -> DrawM (SDL.Texture)
getTexture tn = ($tn) . envTextures <$> getDrawEnv

fillRectangle :: AlphaColour Double -> Maybe (Rectangle Int) -> DrawM ()
fillRectangle colour rectangle = do
  renderer <- getRenderer
  camera <- getCamera
  let (RGB r g b) = toSRGB24 (colour `over` black)
      alpha = round $ alphaChannel colour * 255
  let sdlRect = (calcSDLRect camera) <$> rectangle
  SDL.rendererDrawColor renderer SDL.$= SDL.V4 r g b alpha
  SDL.fillRect renderer $ sdlRect

drawRectangle :: AlphaColour Double -> Maybe (Rectangle Int) -> DrawM ()
drawRectangle colour rectangle = do
  renderer <- getRenderer
  camera <- getCamera
  let (RGB r g b) = toSRGB24 (colour `over` black)
      alpha = round $ alphaChannel colour * 255
  let sdlRect = (calcSDLRect camera) <$> rectangle
  SDL.rendererDrawColor renderer SDL.$= SDL.V4 r g b alpha
  SDL.drawRect renderer $ sdlRect

drawLine :: AlphaColour Double -> Line Int -> DrawM ()
drawLine colour line = do
  renderer <- getRenderer
  camera <- getCamera
  let (p1,p2) = calcSDLLine camera line
  let (RGB r g b) = toSRGB24 (colour `over` black)
      alpha = round $ alphaChannel colour * 255
  SDL.rendererDrawColor renderer SDL.$= SDL.V4 r g b alpha
  SDL.drawLine renderer p1 p2

copyTex :: TextureName -> Maybe (Rectangle Int) -> Maybe (Rectangle Int) -> Maybe (V2 Int) -> Double -> V2 Bool -> DrawM ()
copyTex tn sourceRect destRect rotationPoint rotation flipped = do
  renderer <- getRenderer
  camera <- getCamera
  tex <- getTexture tn
  sdlSourceRect <- case sourceRect of
    Nothing -> pure Nothing
    Just rect -> do
      SDL.TextureInfo _ _ w h <- SDL.queryTexture tex
      pure $ Just $ calcSDLRect (Camera $ Rectangle (V2 0 0) $ fromIntegral <$> V2 w h) rect
  let sdlDestRect = case destRect of
        Nothing -> Nothing
        Just (Rectangle p s) -> Just $ calcSDLRect camera $ Rectangle p s
  SDL.copyEx
    renderer
    tex
    sdlSourceRect
    sdlDestRect
    (CDouble rotation)
    (SDL.P . fmap fromIntegral <$> rotationPoint)
    flipped

withWorld :: Monad m => SystemT w IO a -> (a -> DrawM ()) -> SystemT w m (DrawM ())
withWorld s f = do
  w <- SystemT ask
  pure $ liftIO (runSystem s w) >>= f

calcSDLRect :: Camera -> Rectangle Int -> SDL.Rectangle CInt
calcSDLRect c@(Camera (Rectangle (V2 cX cY) (V2 lX lY))) r@(Rectangle (V2 sX sY) (V2 dX dY)) =
  SDL.Rectangle (toEnum <$> point) (toEnum <$> V2 dX dY)
  where
    point :: SDL.Point V2 Int
    point =
      let x = sX - cX + (lX - dX) `quot` 2
          y = lY - dY - (sY - cY) - (lY - dY) `quot` 2
       in SDL.P $ V2 x y

calcSDLPoint :: Camera -> V2 Int -> SDL.Point V2 CInt
calcSDLPoint (Camera (Rectangle (V2 cX cY) (V2 lX lY))) (V2 x y) = 
  toEnum <$> SDL.P (V2 (x - cX + lX `quot` 2) (lY `quot` 2 - y + cY))

calcSDLLine :: Camera -> Line Int -> (SDL.Point V2 CInt, SDL.Point V2 CInt)
calcSDLLine camera (Line a b) = (calcSDLPoint camera a, calcSDLPoint camera b)