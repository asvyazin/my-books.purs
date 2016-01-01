{-# LANGUAGE OverloadedStrings #-}
module ReactRender where


import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Scripting.Duktape
import Scripting.Duktape.Raw
import Text.Blaze


reactJsFiles :: [FilePath]
reactJsFiles =
 [ "bower_components/react/react.min.js"
 , "bower_components/react/react-dom.min.js" ]


hsResult :: (MonadIO m) => CDukContext -> m Int -> m ()
hsResult ctx func = do
  res <- func
  when (res /= 0) $ do
     err <- liftIO $ dukSafeToLString ctx (-1)
     fail err


render :: (MonadIO m) => String -> [Value] -> m Markup
render jsFilename args =
  liftIO $ do
    ctx <- dukCreateHeapDefault
    forM_ reactJsFiles $ \jsFile -> hsResult ctx $ dukPEvalFile ctx jsFile
    hsResult ctx $ dukPEvalFile ctx jsFilename
    jsResult <- callPurescriptFunc ctx "serverSideRender" args
    return $ preEscapedToMarkup jsResult


callPurescriptFunc :: CDukContext -> String -> [Value] -> IO String
callPurescriptFunc ctx funcName arguments = do
  case arguments of
    [] ->
      guard =<< dukGetPropString ctx (-1) funcName -- [result]
    _ -> do
      void $ dukPushString ctx funcName -- [funcname]
      mapM_ (pushValue ctx) arguments -- [funcname, arguments...]
      let argsLen = length arguments
      dukCallProp ctx (-2 - argsLen) argsLen -- [result]
  result <- dukGetString ctx (-1)
  dukPop ctx
  return result