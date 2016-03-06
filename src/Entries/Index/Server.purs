module Entries.Index.Server where


import Data.Maybe
import Prelude
import React as R
import ReactDOM as R
import Entries.Index.Class


serverSideRender :: String -> String -> String
serverSideRender user onedriveToken =
  let
    props =
      { onedriveToken
      , db: Nothing
      , userName: Just user
      }
  in
   R.renderToString (R.createFactory component props)
