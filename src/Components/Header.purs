module Components.Header where

import Data.Array
import Data.Maybe
import Prelude
import qualified React as R
import qualified React.DOM as R
import qualified React.DOM.Props as RP
import qualified Thermite as T


type Props =
  { title :: String
  , userName :: Maybe String
  , error :: Maybe String
  }

              
render :: forall state action. T.Render state Props action
render dispatch props _ _ =
  [ R.nav
    [ RP.className "navbar navbar-default"
    , RP.role "navigation" ]
    [ R.div
      [ RP.className "navbar-header navbar-brand" ]
      [ R.text props.title ]
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
        (catMaybes [ renderUserName <$> props.userName
                   , renderError <$> props.error ])
      ]
    ]
  ]
  where
    renderUserName user =
      R.div [ RP.className "navbar-text" ] [ R.text user ]
    renderError error =
      R.div [ RP.className "alert alert-danger" ] [ R.text error ]


spec :: forall eff state action. T.Spec eff state Props action
spec =
  T.simpleSpec T.defaultPerformAction render


header :: Props -> R.ReactElement
header props =
  R.createElement (T.createClass spec {}) props []
