module Entries.Index.Server where


import Data.Maybe
import Prelude
import qualified React as R
import Entries.Index.Class


serverSideRender :: String -> String
serverSideRender user =
  R.renderToString (R.createFactory (component $ Just user) {})
