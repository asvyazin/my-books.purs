{-# LANGUAGE OverloadedStrings #-}
module Web.ReactRender where


import Control.Monad (when, void, guard, forM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (Value)
import Scripting.Duktape (pushValue)
import Scripting.Duktape.Raw (CDukContext,
                              dukIsError,
                              dukGetPropString,
                              dukSafeToLString,
                              dukCreateHeapDefault,
                              dukPEvalFileNoResult,
                              dukPEvalFile,
                              dukPcall,
                              dukGetString,
                              dukPop)
import Text.Blaze (Markup, preEscapedToMarkup)


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
