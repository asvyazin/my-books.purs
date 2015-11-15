module Header where

import Data.Maybe
import qualified Thermite as T
import qualified React as R
import qualified React.DOM as R
import qualified React.DOM.Props as RP


type Model =
  { title :: String
  , userName :: Maybe String
  , error :: Maybe String
  }


initialState :: Model
initialState =
  { title: "MyBooks"
  , userName: Nothing
  , error: Nothing
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
    ]
  ]


spec :: T.Spec _ Model _ _
spec = T.simpleSpec T.defaultPerformAction render
