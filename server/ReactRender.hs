{-# LANGUAGE OverloadedStrings #-}
module ReactRender where


import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Scripting.Duktape
import Scripting.Duktape.Raw
import Text.Blaze


hsResult :: (MonadIO m) => CDukContext -> m Int -> m ()
hsResult ctx func = do
  res <- func
  when (res /= 0) $ liftIO $ do
       isError <- dukIsError ctx (-1)
       when isError $ void $ dukGetPropString ctx (-1) "stack"
       err <- dukSafeToLString ctx (-1)
       fail err


render :: (MonadIO m) => String -> [Value] -> m Markup
render jsFilename args =
  liftIO $ do
    ctx <- dukCreateHeapDefault
    hsResult ctx $ dukPEvalFileNoResult ctx "js/duktape-console-shim.js"
    hsResult ctx $ dukPEvalFile ctx jsFilename
    jsResult <- callPurescriptFunc ctx "serverSideRender" args
    return $ preEscapedToMarkup jsResult


callPurescriptFunc :: CDukContext -> String -> [Value] -> IO String
callPurescriptFunc ctx funcName arguments = do
  guard =<< dukGetPropString ctx (-1) funcName
  forM_ arguments $ \arg -> do
    pushValue ctx arg
    hsResult ctx $ dukPcall ctx 1
  result <- dukGetString ctx (-1)
  dukPop ctx
  return result
