module Entries.Index.Server where


import qualified React as R
import Entries.Index.Class


serverSideRender :: String
serverSideRender =
  R.renderToString (R.createFactory component {})
