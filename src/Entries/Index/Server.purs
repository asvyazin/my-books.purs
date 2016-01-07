module Entries.Index.Server where


import Data.Maybe
import Prelude
import qualified React as R
import Entries.Index.Class


serverSideRender :: String -> String -> String
serverSideRender user onedriveToken =
  let
    props =
      { onedriveToken
      , db: Nothing
      }
  in
   R.renderToString (R.createFactory (component (Just user) Nothing) props)
