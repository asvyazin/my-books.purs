{-# LANGUAGE OverloadedStrings #-}
module ReactRender where


import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Scripting.Duktape
import Scripting.Duktape.Raw
import Text.Blaze


reactModules :: [String]
reactModules =
  [ "bower_components/react/react.min.js"
  , "bower_components/react/react-dom.min.js"
  ]


render :: (MonadIO m) => String -> String -> [Value] -> m Markup
render jsFilename mainModule args =
  liftIO $ do
    ctx <- dukCreateHeapDefault
    mapM_ (dukPEvalFileNoResult ctx) reactModules
    dukEvalFileNoResult ctx jsFilename
    jsResult <- callPurescriptFunc ctx mainModule "serverSideRender" args
    return $ preEscapedToMarkup jsResult


callPurescriptFunc :: CDukContext -> String -> String -> [Value] -> IO String
callPurescriptFunc ctx moduleName funcName arguments = do
  guard =<< dukGetGlobalString ctx "PS"-- [PS]
  guard =<< dukGetPropString ctx (-1) moduleName -- [PS, module]
  case arguments of
    [] ->
      guard =<< dukGetPropString ctx (-1) funcName -- [PS, module, result]
    _ -> do
      void $ dukPushString ctx funcName -- [PS, module, funcname]
      mapM_ (pushValue ctx) arguments -- [PS, module, funcname, arguments...]
      let argsLen = length arguments
      dukCallProp ctx (-2 - argsLen) argsLen -- [PS, module, result]
  result <- dukGetString ctx (-1)
  dukPop ctx
  dukPop ctx
  dukPop ctx
  return result
