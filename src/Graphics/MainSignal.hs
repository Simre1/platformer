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
import Graphics.V2
import Data.Geometry
import Data.Colour.Names (white)
import Apecs.Core (ExplGet, ExplMembers)

drawSignal :: SDL.Window -> Signal AppM Scene ()
drawSignal window = once (initGraphics window) $ \d -> arrM $ \scene -> do
    let drawScene scene = case scene of
          MainMenu _ -> pure ()
          Level ls -> runLevelM ls $ embedApecs $ do
            draw1 <- mapGraphics $ \(Platform size, Position pos) -> draw 0 $ 
              fillRectangle (white `withOpacity` 1) $ Just (Rectangle (round <$> pos) size)
            cameraPos <- cfold (\_ (Player _, Position pos) -> pos) (V2 0 0)
            draw2 <- mapGraphics $ \(Player _, Position pos) -> draw 0 $ 
              copyTex TPlayer Nothing (Just (Rectangle (round <$> pos) (V2 32 64))) Nothing 0 (V2 False False)
            drawFrame d (Camera $ Rectangle (round <$> cameraPos) (V2 960 540) ) (draw1 <> draw2)
            pure ()
          Editor ls -> drawScene $ Level ls
    drawScene scene

mapGraphics :: (Has w m t, ExplMembers m (Storage t), ExplGet m (Storage t)) => (t -> Draw) -> SystemT w m Draw
mapGraphics f = cfoldM (\o d -> pure $ o <> f d) mempty

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
