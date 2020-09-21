module Graphics.MainSignal where

import Apecs
import Apecs.Components
import Apecs.Physics
import AppM
import Control.Signal
import Data.Colour.SRGB
import Data.Maybe (fromMaybe)
import qualified Data.StateVar as SV
import Data.Colour
import Debug.Trace (traceShow, traceShowId)
import Foreign.C.Types (CDouble (..), CInt)
import Game.Scene
import Game.World
import qualified SDL
import Data.Word (Word8)
import Graphics.Next

drawSignal :: SDL.Window -> Signal AppM Scene ()
drawSignal window = once (initDraw window) $ \d -> arrM $ \scene -> do
    case scene of
      MainMenu _ -> do
        pure ()
      Level ls -> runLevelM ls $ embedApecs $ drawGraphics d

          -- cmapM_ $ \(drawAction, Position pos) -> do
          --   case drawAction of
          --     DrawRectangle colour maybeRectangle -> do
          --       let (RGB r g b) = toSRGB24 (colour `over` black)
          --           alpha = round $ alphaChannel colour * 255
          --       let sdlRect = (makeRectangle logicalSize) <$> maybeRectangle
          --       SDL.rendererDrawColor renderer SDL.$= SDL.V4 r g b alpha
          --       SDL.fillRect renderer sdlRect
          --     CopyEx tn sourceRect destRect rotationPoint rotation flipped -> do
          --       tex <- lift $ lift $ getTexture tn
          --       sdlSourceRect <- case sourceRect of
          --         Nothing -> pure Nothing
          --         Just rect -> do
          --           SDL.TextureInfo _ _ w h <- SDL.queryTexture tex
          --           pure $ Just $ makeRectangle (fromIntegral <$> V2 w h) rect
          --       let sdlDestRect = case destRect of
          --             Nothing -> Nothing
          --             Just (Rectangle p s) -> Just $ makeRectangle logicalSize $ Rectangle (p + fmap round pos) s
          --       SDL.copyEx
          --         renderer
          --         tex
          --         sdlSourceRect
          --         sdlDestRect
          --         (CDouble rotation)
          --         (fromIntegral <$> rotationPoint)
          --         flipped
