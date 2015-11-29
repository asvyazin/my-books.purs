module Entries.Login.Server where


import qualified React as R
import Entries.Login.Class


serverSideRender :: String
serverSideRender =
  R.renderToString (R.createFactory component {})
