module Graphics.Step where

import Game.World
import Apecs.Physics
import Apecs
import qualified SDL
import Graphics.Draw
import Graphics.Dxt
import Input
import Control.AppM
import Data.Geometry
import Control.Monad
import Data.Colour

makeGraphicsStep :: SDL.Window -> AppM (Input -> SystemT World AppM ())
makeGraphicsStep win = do
  graphics <- initGraphics win
  pure $ \input -> do
    cameraPos <- cfold (\_ (Player _, Position pos) -> pos) (V2 0 0)

    draw :: Draw (Dxt World) <- getEntityDrawActions

    ioDraw <- runDrawDxt input $ draw <> drawPhysicsShape

    lift $ drawFrame graphics (Camera $ Rectangle (round <$> cameraPos) (V2 960 540)) ioDraw


drawPhysicsShape :: Draw (Dxt World)
drawPhysicsShape = makeDraw 10 $ 
    dxtSystem $ cmapM_ $ \(Position pos, ShapeList elements) -> do
      forM_ elements $ \entity -> do
        s <- get entity
        let (Shape _ (Convex (l : lines) _)) = s
        forM_ (zip (l : lines) (lines ++ [l])) $ \(p1, p2) ->
              lift $ drawLine (black `withOpacity` 0.8) $ fmap round $ Line (pos + p1) (pos + p2)