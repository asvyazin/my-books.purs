{-# LANGUAGE OverloadedStrings #-}
module ReactRender where


import Control.Lens
import Control.Monad.IO.Class
import Data.Aeson.Lens
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import Scripting.Duktape


reactModules :: [String]
reactModules =
  [ "bower_components/react/react.min.js"
  , "bower_components/react/react-dom.min.js"
  ]


render :: (MonadIO m) => String -> String -> m (Maybe T.Text)
render jsFilename mainModule = do
  ctxm <- createDuktapeCtx
  case ctxm of
    Nothing ->
      return Nothing
      
    Just ctx -> do
      mapM_ (evalModule ctx) reactModules
      jsFile <- liftIO $ B.readFile jsFilename
      jsResult <- evalDuktape ctx jsFile
      case jsResult of
        Left err ->
          fail err
        Right _ -> do
          jsResult2 <- evalDuktape ctx $ B.pack $ "PS['" ++ mainModule ++ "']"
          case jsResult2 of
            Left err ->
              fail err
            Right Nothing ->
              return Nothing
            Right (Just result) ->
              return (result ^? key "render" . _String)
  where
    evalModule ctx moduleFilename = do
      moduleFile <- liftIO (B.readFile moduleFilename)
      moduleResult <- evalDuktape ctx moduleFile
      case moduleResult of
        Left err ->
          fail err
        Right _ ->
          return ()
