{-# LANGUAGE UndecidableInstances #-}

module Graphics.Draw where

import Apecs
import Apecs.Core
    ( Elem, ExplGet(..), ExplInit, ExplMembers(..), ExplSet(..) )
import AppM
import Control.Monad (forever, forM_, join)
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
import Control.Monad.Trans.Class

import Data.Geometry
import qualified Data.Text as T
import Data.Hashable (Hashable)
import System.Mem.Weak
import Control.Concurrent (threadDelay, Chan, forkOS, forkIO, killThread)
import Foreign (castPtr, Word8, Storable(poke))
import Linear.V3 (V3(..))
import Control.Exception (bracket)
import Control.Concurrent.MVar
import qualified Data.Sequence as S

import qualified Control.Concurrent.Chan.Unagi as U
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)

newtype Draw m = Draw (Q.MinPQueue Int (DrawM m ())) deriving (Semigroup, Monoid)

makeDraw :: Int -> DrawM m () -> Draw m
makeDraw i d = Draw $ Q.singleton i d

newtype DrawM m a = DrawM {runDrawM :: ReaderT DrawEnv m a} deriving (Functor, Applicative, Monad, MonadIO, MonadTrans)

morphDraw :: Monad g => (forall x. f x -> g x) -> Draw f -> Draw g
morphDraw f (Draw queue) = Draw $ Q.map (morphDrawM f) queue
 
morphDrawM :: Monad g => (forall x. f x -> g x) -> DrawM f a -> DrawM g a
morphDrawM f (DrawM reader) = do
  env <- DrawM $ ask
  lift $ f $ runReaderT reader env

instance Applicative m => Semigroup (DrawM m ()) where
  draw1 <> draw2 = draw2 *> draw2

instance Applicative m => Monoid (DrawM m ()) where
  mempty = pure ()

data DrawEnv = DrawEnv
  { envRenderer :: SDL.Renderer,
    envWindow :: SDL.Window,
    envCamera :: Camera,
    envTextures :: TextureManager
  }

getDrawEnv :: Monad m => DrawM m DrawEnv
getDrawEnv = DrawM $ ask

makeTexturePath :: MonadIO m => T.Text -> m (Maybe TexturePath)
makeTexturePath text = pure $ Just $ TexturePath text

loadSurface :: T.Text -> IO SDL.Surface
loadSurface path = SDLI.load (T.unpack $ path)

surfaceToTexture :: SDL.Renderer -> SDL.Surface -> IO SDL.Texture
surfaceToTexture renderer surface = SDL.createTextureFromSurface renderer surface

data TexturePath = DefaultPath | TexturePath {texturePath :: T.Text} deriving (Eq, Show, Generic)

instance Hashable TexturePath

newtype Camera = Camera (Rectangle Int) deriving (Eq, Show)

data Graphics = Graphics 
  { scheduleGraphics :: (SDL.Window -> SDL.Renderer -> TextureManager -> IO ()) -> IO ()
  }

data TextureManager = TextureManager {
  loadTexture :: TexturePath -> IO SDL.Texture
}


newtype GraphicsChan a = GraphicsChan (IORef (S.Seq a))

newGraphicsChan :: IO (GraphicsChan a)
newGraphicsChan = GraphicsChan <$> newIORef S.empty


writeGraphicsChan :: GraphicsChan a -> a -> IO ()
writeGraphicsChan (GraphicsChan ref) a = atomicModifyIORef' ref $ \s -> (s S.|> a, ()) 

readGraphicsChan :: GraphicsChan a -> IO (Maybe a)
readGraphicsChan (GraphicsChan ref) = atomicModifyIORef' ref $ \seq ->
  case S.viewl seq of
    S.EmptyL -> (seq, Nothing)
    a S.:< rest -> (rest, Just a)


data CachedTexture = CachedSurface SDL.Surface | CachedTexture SDL.Texture

initTextureManager :: GraphicsChan (IO ()) -> SDL.Renderer -> IO (TextureManager, IO ())
initTextureManager tc renderer = do
  cacheRef <- liftIO $ newIORef HM.empty

  defaultTexture <- do
    surface <- SDL.createRGBSurface (V2 1 1) SDL.RGB24
    SDL.lockSurface surface
    ptr <- SDL.surfacePixels surface
    poke (castPtr ptr) (V3 0 0 0 :: V3 Word8)
    SDL.unlockSurface surface
    SDL.createTextureFromSurface renderer surface

  (inLoading, outLoading) <- U.newChan

  threadId <- forkIO $ forever $ do
    path <- U.readChan outLoading
    cache <- readIORef cacheRef
    case HM.lookup path cache of
      Nothing -> do
        surface <- loadSurface path
        atomicModifyIORef' cacheRef $ \m -> (HM.insert path (CachedSurface surface) m, ())
        writeGraphicsChan tc $ do
          texture <- surfaceToTexture renderer surface
          atomicModifyIORef' cacheRef $ \m -> (HM.insert path (CachedTexture texture) m, ())
      _ -> pure ()
  let cleanUp = do
        killThread threadId
        readIORef cacheRef >>= mapM_ (\tex -> destroyCached tex)
        SDL.destroyTexture defaultTexture
      manager = TextureManager $ \path ->
        case path of
          DefaultPath -> pure defaultTexture
          TexturePath path -> do
            cache <- readIORef cacheRef
            case HM.lookup path cache of
              Just (CachedTexture t) -> do
                pure t
              Just (CachedSurface _) -> pure defaultTexture
              _ -> do
                U.writeChan inLoading path
                pure defaultTexture
  pure (manager, cleanUp)
  where 
    destroyCached (CachedSurface surface) = SDL.freeSurface surface
    destroyCached (CachedTexture tex) = SDL.destroyTexture tex

initGraphics :: SDL.Window -> AppM Graphics
initGraphics sdlWindow = do

  renderChan <- liftIO newGraphicsChan
  loadingChan <- liftIO newGraphicsChan

  graphicsThread <- liftIO $ forkOS $ do

    renderer <- SDL.createRenderer sdlWindow 0 SDL.defaultRenderer {SDL.rendererType = SDL.AcceleratedRenderer}
    SDL.rendererDrawBlendMode renderer SDL.$= SDL.BlendAlphaBlend

    bracket
      (initTextureManager loadingChan renderer)
      snd
      $ \(textureManager,_) -> forever $ do
        maybeTask <- readGraphicsChan renderChan
        case maybeTask of
          Just task -> task sdlWindow renderer textureManager
          Nothing ->  do
            maybeTask <- readGraphicsChan loadingChan
            case maybeTask of
              Just task -> task
              Nothing -> threadDelay 1000

  addCleanUp $ killThread graphicsThread

  pure $ Graphics $ writeGraphicsChan renderChan


drawFrame :: MonadIO m => Graphics -> Camera -> Draw IO -> m ()
drawFrame (Graphics schedule) camera@(Camera (Rectangle pos size)) (Draw queue) = do
  liftIO $ do
    schedule $ \w r t -> do
      SDL.rendererDrawColor r SDL.$= SDL.V4 230 230 230 maxBound
      SDL.clear r
      SDL.rendererLogicalSize r SDL.$= Just (fromIntegral <$> size)
      flip runReaderT (DrawEnv r w camera t) . runDrawM $
          foldl (\others action -> others *> action) mempty queue
      SDL.present r

getCamera :: Monad m => DrawM m Camera
getCamera = envCamera <$> getDrawEnv

getRenderer :: Monad m => DrawM m SDL.Renderer
getRenderer = envRenderer <$> getDrawEnv

getWindow :: Monad m => DrawM m SDL.Window
getWindow = envWindow <$> getDrawEnv

getTexture :: MonadIO m => TexturePath -> DrawM m (SDL.Texture)
getTexture tn = getDrawEnv >>= liftIO . ($tn) . loadTexture . envTextures    

fillRectangle :: MonadIO m => AlphaColour Double -> Maybe (Rectangle Int) -> DrawM m ()
fillRectangle colour rectangle = do
  renderer <- getRenderer
  camera <- getCamera
  let (RGB r g b) = toSRGB24 (colour `over` black)
      alpha = round $ alphaChannel colour * 255
  let sdlRect = (calcSDLRect camera) <$> rectangle
  SDL.rendererDrawColor renderer SDL.$= SDL.V4 r g b alpha
  SDL.fillRect renderer $ sdlRect

drawRectangle :: MonadIO m => AlphaColour Double -> Maybe (Rectangle Int) -> DrawM m ()
drawRectangle colour rectangle = do
  renderer <- getRenderer
  camera <- getCamera
  let (RGB r g b) = toSRGB24 (colour `over` black)
      alpha = round $ alphaChannel colour * 255
  let sdlRect = (calcSDLRect camera) <$> rectangle
  SDL.rendererDrawColor renderer SDL.$= SDL.V4 r g b alpha
  SDL.drawRect renderer $ sdlRect

drawLine :: MonadIO m => AlphaColour Double -> Line Int -> DrawM m ()
drawLine colour line = do
  renderer <- getRenderer
  camera <- getCamera
  let (p1,p2) = calcSDLLine camera line
  let (RGB r g b) = toSRGB24 (colour `over` black)
      alpha = round $ alphaChannel colour * 255
  SDL.rendererDrawColor renderer SDL.$= SDL.V4 r g b alpha
  SDL.drawLine renderer p1 p2

copyTex :: MonadIO m => TexturePath -> Maybe (Rectangle Int) -> Maybe (Rectangle Int) -> Maybe (V2 Int) -> Double -> V2 Bool -> DrawM m ()
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

withWorld :: MonadIO m => SystemT w IO a -> (a -> DrawM m ()) -> SystemT w m (DrawM m ())
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

instance Component (Draw m) where
  type Storage (Draw m) = (GraphicsManager m) (Draw m)

type instance Elem (GraphicsManager m Graphics) = Graphics
type instance Elem (GraphicsManager m (Draw m)) = (Draw m)

data GraphicsManager m a = GraphicsManager
  { gmActions :: IORef (HM.HashMap Int (Draw m))
  }

instance MonadIO m => ExplInit m (GraphicsManager m1 (Draw m1)) where
  explInit = liftIO $ do
    actions <- newIORef (HM.empty)
    pure $ GraphicsManager actions

instance MonadIO m => ExplSet m (GraphicsManager m1 (Draw m1)) where
  explSet GraphicsManager {gmActions} i =
    liftIO . modifyIORef gmActions . HM.insert i

instance MonadIO m => ExplGet m (GraphicsManager m1 (Draw m1)) where
  explExists GraphicsManager {gmActions} i = liftIO $
    HM.member i <$> readIORef gmActions
  explGet GraphicsManager {gmActions} i = liftIO $ do
    actions <- readIORef gmActions
    pure $ case HM.lookup i actions of
      Nothing -> error "No draw action exists!"
      Just x -> x

instance MonadIO m => ExplMembers m (GraphicsManager m1 (Draw m1)) where
  explMembers GraphicsManager {gmActions} = liftIO $ do
    actions <- readIORef gmActions
    pure $ V.fromList $ HM.keys actions

getEntityDrawActions :: forall w m m1. (Has w m (Draw m1), MonadIO m) => SystemT w m (Draw m1)
getEntityDrawActions = do
  GraphicsManager actions :: GraphicsManager m1 (Draw m1) <- getStore
  liftIO $ HM.foldl' (<>) mempty <$> readIORef actions
