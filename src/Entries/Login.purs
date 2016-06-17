module Entries.Login where


import Common.Data.ServerEnvironmentInfo (ServerEnvironmentInfo(..), getServerEnvironment)
import Common.React (mapProps, maybeState)
import Components.Header as Header
import Components.Wrappers.Button as Button
import Components.Wrappers.Glyphicon as Glyphicon
import Control.Monad.Aff (launchAff)
import Control.Monad.Eff.Class (liftEff)
import Data.List (List(..), fromList, toList)
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Data.Tuple (Tuple(Tuple))
import Global (encodeURIComponent)
import Prelude
import React (ReactClass, transformState, createClass) as R
import React.DOM (text, div) as R
import React.DOM.Props as RP
import Thermite as T


type State =
  { clientId :: String
  , baseUrl :: String
  }


component :: forall props. R.ReactClass props
component =
  R.createClass reactSpec.spec { componentDidMount = componentDidMount }
  where
    reactSpec =
      T.createReactSpec spec Nothing
    
    componentDidMount this = launchAff $ do
      (ServerEnvironmentInfo serverEnvironment) <- getServerEnvironment
      let newState =
            { clientId : serverEnvironment.onedriveClientId
            , baseUrl : serverEnvironment.baseUrl
            }
      liftEff $ R.transformState this (\_ -> Just newState)


spec :: forall eff props action. T.Spec eff (Maybe State) props action
spec =
  mapProps (\_ -> headerProps) Header.spec <> maybeState loginButton
  where
    headerProps =
      { title: "MyBooks"
      , userName: Nothing
      , error: Nothing
      }


loginButton :: forall eff props action. T.Spec eff State props action
loginButton =
  T.simpleSpec T.defaultPerformAction render
  where
    render dispatch _ state _ =
      [ R.div
        [ RP.className "col-md-offset-5 col-md-2" ]
        [ Button.button
          { bsSize : "large"
          , href : loginUrl state
          }
          [ Glyphicon.glyphicon' "cloud"
          , R.text " Login to OneDrive"
          ]
        ]
      ]

    loginParams state =
      [ (Tuple "client_id" state.clientId)
      , (Tuple "scope" "wl.signin wl.offline_access onedrive.readonly")
      , (Tuple "response_type" "code")
      , (Tuple "redirect_uri" (state.baseUrl <> "/onedrive-redirect"))
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
