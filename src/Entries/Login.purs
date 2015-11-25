module Entries.Login where

import Control.Monad.Eff
import Data.Either
import Data.Lens
import Data.List
import Data.Maybe
import Data.Maybe.Unsafe
import Data.Nullable
import Data.String (joinWith)
import Data.Tuple
import DOM
import DOM.HTML
import DOM.HTML.Document
import DOM.HTML.Types
import DOM.HTML.Window
import DOM.Node.ParentNode
import Prelude
import qualified React as R
import qualified React.DOM as R
import qualified React.DOM.Props as RP
import qualified Thermite as T

import qualified Components.Header as H


type Model =
  { header :: H.Model
  , clientId :: String
  , scope :: String
  }


render :: T.Render Model _ _
render dispatch _ state _ =
  [ R.div
    [ RP.className "col-md-offset-5 col-md-2" ]
    [ R.a
      [ RP.className "btn btn-default btn-lg"
      , RP.href loginUrl
      ]
      [ R.span
        [ RP.className "glyphicon glyphicon-cloud" ]
        [ ]
      , R.text " Go to OneDrive"
      ]
    ]
  ]
  where
    loginParams =
      [ (Tuple "client_id" state.clientId)
      , (Tuple "scope" state.scope)
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


header :: LensP Model H.Model
header =
  lens _.header (_ { header = _ })


initialState :: Model
initialState =
  { header :
    { title : "MyBooks"
    , userName : Nothing
    , error : Nothing
    }
  , scope : "wl.signin onedrive.readonly"
  , clientId : "000000004816D42C"
  }


spec :: T.Spec _ Model _ _
spec =
  T.focusState header H.spec <> T.simpleSpec T.defaultPerformAction render


main :: Eff (dom :: DOM) Unit
main = do
  let component = T.createClass spec initialState
  node <- htmlDocumentToParentNode <$> (window >>= document)
  container <- (fromJust <<< toMaybe) <$> querySelector ".application" node
  void $ R.render (R.createFactory component {}) container
