module Graphics.MainSignal where

import Graphics.DrawM
import qualified SDL
import AppM
import Control.Signal
import Game.Scene
import Apecs
import Apecs.Physics
import Game.World
import qualified Data.StateVar as SV
import Debug.Trace (traceShowId)
import Foreign.C.Types (CDouble(..), CInt)

drawSignal :: SDL.Window -> Signal AppM Scene ()
drawSignal window = runDrawSignal window (V2 100 66) $ arrM $ \gameState -> do
  tex <- getTexture TPlayer
  renderer <- getRenderer
  case gameState of
    MainMenu _ -> do
      SDL.copy renderer tex Nothing Nothing
    Level ls -> runApecs ls $ do
      cmapM_ $ \(Player, Position p) -> do
        rect <- lift $ makeRectangle (round <$> p) (V2 20 20)
        SDL.copyEx renderer tex Nothing (Just rect) (rotation p) Nothing (pure False)
  pure ()
    where rotation (V2 x y) = CDouble $ 6 * x

-- makeRectangle :: V2 Double -> V2 Double -> SDL.Rectangle CInt
-- makeRectangle (V2 x' y') (V2 rx' ry') = 
--   let x = x' / 100
--       y = y' / 100
--       rx = rx' / 400
--       ry = ry' / 300
--       p = SDL.P $ V2 (round $ x * fromIntegral 400) (round ((1-y-ry) * fromIntegral 300))
--       d = round <$> V2 rx ry
--   in traceShowId $ SDL.Rectangle p d
