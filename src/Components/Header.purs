module Components.Header where

import Data.Array
import Data.Maybe
import Prelude
import qualified React as R
import qualified React.DOM as R
import qualified React.DOM.Props as RP
import qualified Thermite as T


type Model =
  { title :: String
  , userName :: Maybe String
  , error :: Maybe String
  }

              
render :: T.Render Model _ _
render dispatch _ state _ =
  [ R.nav
    [ RP.className "navbar navbar-default"
    , RP.role "navigation" ]
    [ R.div
      [ RP.className "navbar-header navbar-brand" ]
      [ R.text state.title ]
    , R.ul
      [ RP.className "nav navbar-nav" ]
      [ R.li'
        [ R.a
          [ RP.href "/login" ]
          [ R.text "Get new access token" ]
        ]
      ]
    , R.ul
      [ RP.className "nav navbar-nav navbar-right" ]
      [ R.li'
        (catMaybes [ renderUserName <$> state.userName
                   , renderError <$> state.error ])
      ]
    ]
  ]
  where
    renderUserName user =
      R.div [ RP.className "navbar-text" ] [ R.text user ]
    renderError error =
      R.div [ RP.className "alert alert-danger" ] [ R.text error ]


spec :: T.Spec _ Model _ _
spec =
  T.simpleSpec T.defaultPerformAction render
