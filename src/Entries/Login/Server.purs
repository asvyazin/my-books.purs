module Entries.Login.Server where


import React as R
import ReactDOM as R
import Entries.Login.Class


serverSideRender :: String
serverSideRender =
  let
    props =
      { scope : "wl.signin onedrive.readonly"
      , clientId : "000000004816D42C"
      }
  in
   R.renderToString (R.createFactory component props)
