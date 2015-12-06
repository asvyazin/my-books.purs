{-# LANGUAGE OverloadedStrings #-}
module ReactRender where


import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.Encode
import qualified Data.ByteString.Char8 as B
import Data.Text.Lazy.Builder
import Scripting.Duktape
import Text.Blaze


reactModules :: [String]
reactModules =
  [ "bower_components/react/react.min.js"
  , "bower_components/react/react-dom.min.js"
  ]


render :: (MonadIO m) => String -> String -> [Value] -> m Markup
render jsFilename mainModule args = do
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
          jsResult2 <- callPurescriptFunc ctx mainModule "serverSideRender" args
          case jsResult2 of
            Left err ->
              fail err
            Right Nothing ->
              return ""
            Right (Just result) ->
              return $ preEscapedToMarkup $ toLazyText $ encodeToTextBuilder result
  where
    evalModule ctx moduleFilename = do
      moduleFile <- liftIO (B.readFile moduleFilename)
      moduleResult <- evalDuktape ctx moduleFile
      case moduleResult of
        Left err ->
          fail err
        Right _ ->
          return ()


callPurescriptFunc :: MonadIO m => DuktapeCtx -> String -> String -> [Value] -> m (Either String (Maybe Value))
callPurescriptFunc ctx moduleName funcName arguments =
  let
    func = B.pack $ "PS['" ++ moduleName ++ "']." ++ funcName
  in
    case arguments of
      [] -> 
        evalDuktape ctx func
      _ -> 
        callDuktape ctx Nothing func arguments
