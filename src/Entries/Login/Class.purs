module Entries.Login.Class where

import Data.List
import Data.Maybe
import Data.String (joinWith)
import Data.Tuple
import Prelude
import qualified React as R
import qualified React.DOM as R
import qualified React.DOM.Props as RP
import qualified Thermite as T

import qualified Components.Header as H
import qualified Components.Wrappers.Button as Button
import qualified Components.Wrappers.Glyphicon as Glyphicon


type Props =
  { clientId :: String
  , scope :: String
  }


render :: forall state. T.Render state Props (Array R.ReactElement)
render dispatch props _ _ =
  [ H.header
    { title: "MyBooks"
    , userName: Nothing
    , error: Nothing
    }
  , R.div
    [ RP.className "col-md-offset-5 col-md-2" ]
    [ Button.button
      { bsSize: "large"
      , href: loginUrl
      }
      [ Glyphicon.glyphicon' "cloud"
      , R.text " Login to OneDrive"
      ]
    ]
  ]
  where
    loginParams =
      [ (Tuple "client_id" props.clientId)
      , (Tuple "scope" props.scope)
      , (Tuple "response_type" "code")
      , (Tuple "redirect_uri" "http://localhost:8000/onedrive-redirect")
      ]
    loginUrl =
      buildUrl "https://login.live.com/oauth20_authorize.srf" $ toList loginParams 


buildUrl :: String -> List (Tuple String String) -> String
buildUrl baseUrl Nil =
  baseUrl
buildUrl baseUrl params =
  baseUrl ++ "?" ++ queryString
  where
    formatParam (Tuple p v) =
      p ++ "=" ++ v
      
    queryString =
      joinWith "&" $ fromList $ map formatParam params


spec :: forall eff state. T.Spec eff state Props (Array R.ReactElement)
spec =
  T.simpleSpec T.defaultPerformAction render


component :: R.ReactClass Props
component =
  T.createClass spec {}
