{-# LANGUAGE OverloadedStrings #-}
module ReactRender where


import Control.Lens
import Control.Monad.IO.Class
import Data.Aeson.Lens
import qualified Data.ByteString.Char8 as B
import Scripting.Duktape
import Text.Blaze


reactModules :: [String]
reactModules =
  [ "bower_components/react/react.min.js"
  , "bower_components/react/react-dom.min.js"
  ]


render :: (MonadIO m) => String -> String -> m Markup
render jsFilename mainModule = do
  ctxm <- createDuktapeCtx
  case ctxm of
    Nothing ->
      return ""
      
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
              return ""
            Right (Just result) ->
              return (maybe "" preEscapedToMarkup (result ^? key "serverSideRender" . _String))
  where
    evalModule ctx moduleFilename = do
      moduleFile <- liftIO (B.readFile moduleFilename)
      moduleResult <- evalDuktape ctx moduleFile
      case moduleResult of
        Left err ->
          fail err
        Right _ ->
          return ()
