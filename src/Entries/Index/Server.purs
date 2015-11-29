module Entries.Index.Server where

import Prelude
import qualified React as R
import Entries.Index.Class


render :: String
render =
  R.renderToString (R.createFactory component {})
