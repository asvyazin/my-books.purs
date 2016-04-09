module Entries.Index.Server where


import Data.Maybe (Maybe(..))
import React (createFactory) as R
import ReactDOM (renderToString) as R
import Entries.Index.Class (component)


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
