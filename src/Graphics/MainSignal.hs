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
import Data.Maybe (fromMaybe)
import qualified Data.StateVar as SV
import Data.Word (Word8)
import Debug.Trace (traceShow, traceShowId)
import Foreign.C.Types (CDouble (..), CInt)
import Game.Scene
import Game.World
import Graphics.V2
import qualified SDL

drawSignal :: SDL.Window -> Signal AppM Scene ()
drawSignal window = once (initGraphics window) $ \d -> arrM $ \scene -> do
  let drawScene scene = case scene of
        MainMenu _ -> pure ()
        Level ls -> runLevelM ls $ 
          embedApecs $ do
            draw1 <- cfoldMap $ \(Platform size, Position pos) ->
              draw 0 $
                fillRectangle (white `withOpacity` 1) $ Just (Rectangle (round <$> pos) size)
            cameraPos <- cfold (\_ (Player _, Position pos) -> pos) (V2 0 0)
            draw2 <- cfoldMap $ \(Player _, Position pos) ->
              draw 0 $
                copyTex TPlayer Nothing (Just (Rectangle (round <$> pos) (V2 32 64))) Nothing 0 (V2 False False)
            physics <- drawPhysicsShape
            drawFrame d (Camera $ Rectangle (round <$> cameraPos) (V2 960 540)) (draw1 <> draw2 <> physics)
            
            pure ()
  drawScene scene

drawPhysicsShape :: SystemT World (LevelM AppM) Draw
drawPhysicsShape = do
  join $
    cfold
      ( \others (Position pos, ShapeList elements) -> do
          (<>) <$> others <*> foldl (combine pos) (pure mempty) elements
      )
      (pure mempty)
  where
    combine :: V2 Double -> SystemT World (LevelM AppM) Draw -> Entity -> SystemT World (LevelM AppM) Draw
    combine pos a e = do
      s <- get e
      let (Shape _ (Convex (l : lines) _)) = s 
      let currentDraw = draw 10 $
            forM_ (zip (l : lines) (lines ++ [l])) $ \(p1, p2) ->
              drawLine (black `withOpacity` 0.8) $ fmap round $ Line (pos + p1) (pos + p2)
      (<> currentDraw) <$> a
