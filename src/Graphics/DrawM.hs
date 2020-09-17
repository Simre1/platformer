module Graphics.DrawM where

import AppM
import Control.Arrow
import Control.Exception (bracket)
import Control.Monad (forM_)
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Control.Signal
import Data.StateVar (HasSetter (($=)))
import qualified Data.StateVar as SV
import qualified Data.Vector.Mutable as VM
import Game.Scene
import Foreign.C.Types (CInt)
import qualified SDL
import qualified SDL.Image as SDLI
import Linear.V2
import Linear.V4 (V4 (V4))

newtype DrawM a = DrawM {runDrawM :: ReaderT DrawEnv AppM a} deriving (Functor, Applicative, Monad, MonadIO)

data DrawEnv = DrawEnv
  { envRenderer :: SDL.Renderer,
    envWindow :: SDL.Window,
    envSize :: V2 Int,
    envTextures :: TextureName -> SDL.Texture
  }

data TextureName = TPlayer deriving (Eq, Enum, Bounded)

getDrawEnv :: DrawM DrawEnv
getDrawEnv = DrawM $ ask

loadTexture :: SDL.Renderer -> TextureName -> IO SDL.Texture
loadTexture renderer tn = SDLI.loadTexture renderer (toFilePath tn)
  where
    toFilePath = \case
      TPlayer -> "assets/textures/player.png"

makeDrawEnv :: SDL.Window -> V2 Int -> AppM DrawEnv
makeDrawEnv window size = do
  SDLI.initialize [SDLI.InitPNG]
  renderer <- SDL.createRenderer window 0 SDL.defaultRenderer {SDL.rendererType = SDL.AcceleratedRenderer}
  SDL.rendererLogicalSize renderer SV.$= Just (toEnum <$> size)
  SDL.rendererDrawBlendMode renderer SV.$= SDL.BlendAlphaBlend
  textures <- liftIO $ loadTexture renderer `traverse` enumFrom (toEnum 0)
  addCleanUp $ do
    forM_ textures SDL.destroyTexture
    SDLI.quit
  pure $ DrawEnv renderer window size $ \n -> textures !! (fromEnum n)

runDrawSignal :: SDL.Window -> V2 Int -> Signal DrawM a b -> Signal AppM a b
runDrawSignal window size signal = once (makeDrawEnv window size) $ \drawEnv ->
  let renderer = envRenderer drawEnv
   in arrM
        ( \a -> do
            SDL.rendererDrawColor renderer $= (V4 230 230 230 maxBound)
            SDL.clear renderer
            pure a
        )
        >>> morphContext (flip runReaderT drawEnv . runDrawM) signal
        >>> arrM (\a -> a <$ SDL.present renderer)

getTexture :: TextureName -> DrawM SDL.Texture
getTexture name = ($name) . envTextures <$> getDrawEnv

getRenderer :: DrawM SDL.Renderer
getRenderer = envRenderer <$> getDrawEnv

makeRectangle :: V2 Int -> V2 Int -> DrawM (SDL.Rectangle CInt)
makeRectangle (V2 x y) (V2 dX dY) = (rectangle . envSize) <$> getDrawEnv
  where
    rectangle (V2 _ lY) =
      SDL.Rectangle
        (SDL.P $ toEnum <$> (V2 x (lY - y - dY)))
        (toEnum <$> (V2 dX dY))
