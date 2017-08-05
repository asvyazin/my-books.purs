{-# LANGUAGE OverloadedStrings #-}
module Common.Onedrive where


import Data.Text (Text, pack)
import System.Environment (lookupEnv)


getOnedriveClientSecret :: IO Text
getOnedriveClientSecret =
  maybe "-4tKnVPaAyIEAgYrBp8R6jTYY0zClN6c" pack <$> lookupEnv "ONEDRIVE_CLIENT_SECRET"
