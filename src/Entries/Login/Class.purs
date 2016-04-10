module Entries.Login.Class where

import Common.React
import Data.List
import Data.Maybe
import Data.String (joinWith)
import Data.Tuple
import Global (encodeURIComponent)
import Prelude
import qualified React as R
import qualified React.DOM as R
import qualified React.DOM.Props as RP
import qualified Thermite as T

import qualified Components.Header as Header
import qualified Components.Wrappers.Button as Button
import qualified Components.Wrappers.Glyphicon as Glyphicon


type Props =
  { clientId :: String
  , scope :: String
  }


loginButton :: forall eff state action. T.Spec eff state Props action
loginButton =
  T.simpleSpec T.defaultPerformAction render
  where
    render dispatch props _ _ =
      [ R.div
        [ RP.className "col-md-offset-5 col-md-2" ]
        [ Button.button
          { bsSize: "large"
          , href: loginUrl props
          }
          [ Glyphicon.glyphicon' "cloud"
          , R.text " Login to OneDrive"
          ]
        ]
      ]

    loginParams props =
      [ (Tuple "client_id" props.clientId)
      , (Tuple "scope" props.scope)
      , (Tuple "response_type" "code")
      , (Tuple "redirect_uri" "http://localhost:8000/onedrive-redirect")
      ]

    loginUrl =
      buildUrl "https://login.live.com/oauth20_authorize.srf" <<< toList <<< loginParams


buildUrl :: String -> List (Tuple String String) -> String
buildUrl baseUrl Nil =
  baseUrl
buildUrl baseUrl params =
  baseUrl ++ "?" ++ queryString
  where
    formatParam (Tuple p v) =
      encodeURIComponent p ++ "=" ++ encodeURIComponent v

    queryString =
      joinWith "&" $ fromList $ map formatParam params


spec :: forall eff state action. T.Spec eff state Props action
spec =
  mapProps (\_ -> headerProps) Header.spec <> loginButton
  where
    headerProps =
      { title: "MyBooks"
      , userName: Nothing
      , error: Nothing
      }


component :: R.ReactClass Props
component =
  T.createClass spec {}
