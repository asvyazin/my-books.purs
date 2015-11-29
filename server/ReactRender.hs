{-# LANGUAGE OverloadedStrings #-}
module ReactRender where


import Control.Lens
import Control.Monad.IO.Class
import Data.Aeson.Lens
import qualified Data.ByteString as B
import Data.Maybe
import qualified Data.Text as T
import Scripting.Duktape


reactModules :: [String]
reactModules =
  [ "bower_components/react/react.min.js"
  , "bower_components/react/react-dom.min.js"
  ]


render :: (MonadIO m) => String -> m (Maybe T.Text)
render jsFilename =
  createDuktapeCtx >>= mapM (\ctx -> do
    mapM_ (evalModule ctx) reactModules
    jsFile <- liftIO $ B.readFile jsFilename
    jsResult <- evalDuktape ctx jsFile
    let result = fromJust $ either (const Nothing) id jsResult
    return (fromJust (result ^? key "render" . _String)))
  where
    evalModule ctx moduleFilename = do
      moduleFile <- liftIO (B.readFile moduleFilename)
      result <- evalDuktape ctx moduleFile
      case result of
        Left err ->
          fail err
        Right _ ->
          return ()
