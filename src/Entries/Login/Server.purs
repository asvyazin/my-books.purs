module Entries.Login.Server where


import React (createFactory) as R
import ReactDOM (renderToString) as R
import Entries.Login.Class


serverSideRender :: String -> String -> String
serverSideRender appBaseUrl clientId =
  let
    props =
      { scope : "wl.signin onedrive.readonly"
      , clientId : clientId
      , appBaseUrl : appBaseUrl
      }
  in
   R.renderToString (R.createFactory component props)
