{-# LANGUAGE UndecidableInstances #-}

module Graphics.Next where

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

data Rectangle a = Rectangle (V2 a) (V2 a) deriving (Eq, Show)

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

-- makeDrawEnv :: SDL.Window -> V2 Int -> AppM DrawEnv
-- makeDrawEnv window size = do
--   SDLI.initialize [SDLI.InitPNG]
--   renderer <- SDL.createRenderer window 0 SDL.defaultRenderer {SDL.rendererType = SDL.AcceleratedRenderer}
--   SDL.rendererLogicalSize renderer SV.$= Just (toEnum <$> size)
--   SDL.rendererDrawBlendMode renderer SV.$= SDL.BlendAlphaBlend
--   textures <- liftIO $ loadTexture renderer `traverse` enumFrom (toEnum 0)
--   -- addCleanUp $ do
--   --   forM_ textures SDL.destroyTexture
--   --   SDLI.quit
--   pure $ DrawEnv renderer window size $ \n -> textures !! (fromEnum n)

data TextureName = TPlayer deriving (Eq, Enum, Bounded)

data Graphics

data Draw = Draw {drawPriority :: Int, drawAction :: DrawAction}

data DrawAction
  = FillRectangle (AlphaColour Double) (Maybe (Rectangle Int))
  | CopyEx TextureName (Maybe (Rectangle Int)) (Maybe (Rectangle Int)) (Maybe Int) Double (V2 Bool)

newtype Camera = Camera (Rectangle Int) deriving (Eq, Show)

instance Component Draw where
  type Storage Draw = GraphicsManager Draw

instance Component Camera where
  type Storage Camera = GraphicsManager Camera

instance Component Graphics where
  type Storage Graphics = GraphicsManager Graphics

type instance Elem (GraphicsManager Graphics) = Graphics

type instance Elem (GraphicsManager Draw) = Draw
type instance Elem (GraphicsManager Camera) = Camera


data GraphicsManager a = GraphicsManager
  { gmCamera :: IORef Camera,
    gmActions :: IORef (HM.HashMap Int Draw)
  }

cast :: GraphicsManager a -> GraphicsManager b
cast a = unsafeCoerce a

instance MonadIO m => ExplInit m (GraphicsManager Graphics) where
  explInit = liftIO $ do
    camera <- newIORef (Camera $ Rectangle (V2 0 0) (V2 800 600))
    actions <- newIORef (HM.empty)
    pure $ GraphicsManager camera actions

instance MonadIO m => ExplGet m (GraphicsManager Camera) where
  explExists _ _ = pure True
  explGet GraphicsManager {gmCamera} _ = liftIO $ readIORef gmCamera

instance MonadIO m => ExplSet m (GraphicsManager Camera) where
  explSet GraphicsManager {gmCamera} _ camera = liftIO $ writeIORef gmCamera camera

instance MonadIO m => ExplSet m (GraphicsManager Draw) where
  explSet GraphicsManager {gmActions} i draw =
    liftIO $ modifyIORef gmActions $ HM.insert i draw

instance MonadIO m => ExplGet m (GraphicsManager Draw) where
  explExists GraphicsManager {gmActions} i = liftIO $
    HM.member i <$> readIORef gmActions
  explGet GraphicsManager {gmActions} i = liftIO $ do
    actions <- readIORef gmActions
    pure $ case HM.lookup i actions of
      Nothing -> error "No draw action exists!"
      Just x -> x

instance MonadIO m => ExplMembers m (GraphicsManager Draw) where
  explMembers GraphicsManager {gmActions} = liftIO $ do
    actions <- readIORef gmActions
    pure $ V.fromList $ HM.keys actions

instance (MonadIO m, Has w m Graphics) => Has w m Camera where
  getStore = cast @Graphics @Camera <$> getStore

instance (MonadIO m, Has w m Graphics) => Has w m Draw where
  getStore = cast @Graphics @Draw <$> getStore

data InitDraw = InitDraw SDL.Window SDL.Renderer (TextureName -> SDL.Texture)

drawGraphics :: (Has w m Graphics, MonadIO m) => InitDraw -> SystemT w m ()
drawGraphics (InitDraw w r t) = do
  GraphicsManager {gmActions, gmCamera} :: GraphicsManager Graphics <- getStore
  liftIO $ do
    SDL.rendererDrawColor r SDL.$= SDL.V4 230 230 230 maxBound
    SDL.clear r
    camera@(Camera (Rectangle _ size)) <- readIORef gmCamera
    actions <- readIORef gmActions
    SDL.rendererLogicalSize r SDL.$= Just (fromIntegral <$> size)
    let drawQueue = HM.foldl' (\queue (Draw p action) -> Q.insert p action queue) Q.empty actions
    flip runReaderT (DrawEnv r w camera t) . runDrawM $
      foldl (\others action -> others *> executeDrawAction action) mempty drawQueue
    SDL.present r

initDraw :: SDL.Window -> AppM InitDraw
initDraw sdlWindow = do
  renderer <- SDL.createRenderer sdlWindow 0 SDL.defaultRenderer {SDL.rendererType = SDL.AcceleratedRenderer}
  textures <- liftIO $ loadTextures renderer
  SDL.rendererDrawBlendMode renderer SDL.$= SDL.BlendAlphaBlend
  addCleanUp $ forM_ textures $ SDL.destroyTexture
  pure $ InitDraw sdlWindow renderer $ \n -> textures !! fromEnum n

executeDrawAction :: DrawAction -> DrawM ()
executeDrawAction action = do
  camera <- envCamera <$> getDrawEnv
  renderer <- envRenderer <$> getDrawEnv
  case action of
    FillRectangle colour rectangle -> do
      let (RGB r g b) = toSRGB24 (colour `over` black)
          alpha = round $ alphaChannel colour * 255
      let sdlRect = (calcSDLRect camera) <$> rectangle
      SDL.rendererDrawColor renderer SDL.$= SDL.V4 r g b alpha
      SDL.fillRect renderer $ sdlRect
    CopyEx tn sourceRect destRect rotationPoint rotation flipped -> do
      tex <- ($tn) . envTextures <$> getDrawEnv
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
        (fromIntegral <$> rotationPoint)
        flipped

calcSDLRect :: Camera -> Rectangle Int -> SDL.Rectangle CInt
calcSDLRect c@(Camera (Rectangle (V2 cX cY) (V2 lX lY))) r@(Rectangle (V2 sX sY) (V2 dX dY)) =
  SDL.Rectangle (toEnum <$> point) (toEnum <$> V2 dX dY)
  where
    point :: SDL.Point V2 Int
    point =
      let x = sX - cX + (lX - dX) `quot` 2
          y = lY - dY - (sY - cY) - (lY - dY) `quot` 2
       in SDL.P $ V2 x y

-- let V2 x y = pos - cPos in
-- SDL.Rectangle
--   (SDL.P $ fmap toEnum (V2 x (lY - y - dY) - fmap (`quot` 2) (V2 dX (- dY))))
--   (toEnum <$> (V2 dX dY))
