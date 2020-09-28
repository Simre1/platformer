module Graphics.MainSignal where

import Apecs
import Apecs.Components
import Apecs.Core (ExplGet, ExplMembers)
import Apecs.Physics
import AppM
import Control.Monad (forM_, join)
import Control.Monad.IO.Class (MonadIO)
import Control.Signal
import Data.Colour
import Data.Colour.Names (white)
import Data.Colour.SRGB
import Data.Geometry
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.StateVar as SV
import Data.Word (Word8)
import Debug.Trace (traceShow, traceShowId)
import Foreign.C.Types (CDouble (..), CInt)
import Game.Scene
import Game.World
import Graphics.Draw
import Graphics.Dxt
import qualified SDL
import Input

drawSignal :: SDL.Window -> Signal AppM (Input, Scene) ()
drawSignal win = once (initGraphics win) $ \d -> arrM $ \(input, scene) -> do
  path <- makeTexturePath "assets/textures/player.png"
  let drawScene scene = case scene of
        MainMenu _ -> pure ()
        Level ls -> runLevelM ls $ 
          embedApecs $ do
            cameraPos <- cfold (\_ (Player _, Position pos) -> pos) (V2 0 0)

            draw :: Draw (Dxt World) <- getEntityDrawActions

            ioDraw <- runDrawDxt input $ draw <> drawPhysicsShape

            lift $ drawFrame d (Camera $ Rectangle (round <$> cameraPos) (V2 960 540)) ioDraw
            pure ()
  drawScene scene

drawPhysicsShape :: Draw (Dxt World)
drawPhysicsShape = makeDraw 10 $ 
    dxtSystem $ cmapM_ $ \(Position pos, ShapeList elements) -> do
      forM_ elements $ \entity -> do
        s <- get entity
        let (Shape _ (Convex (l : lines) _)) = s
        forM_ (zip (l : lines) (lines ++ [l])) $ \(p1, p2) ->
              lift $ drawLine (black `withOpacity` 0.8) $ fmap round $ Line (pos + p1) (pos + p2)
