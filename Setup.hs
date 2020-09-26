import Distribution.Simple.AutoExpose as AutoExpose


main :: IO ()
main = do
  AutoExpose.defaultMain
--   AutoExpose.defaultMainWithHooks $ buildAppImage simpleUserHooks

-- appImage :: AppImage
-- appImage = AppImage "game" "game.desktop" ["game"] [] Nothing

-- buildAppImage :: UserHooks -> UserHooks
-- buildAppImage hooks = 
--   hooks {postBuild = \a b c d -> (postBuild hooks) a b c d *> appImageBuildHook [appImage] a b c d}
  
