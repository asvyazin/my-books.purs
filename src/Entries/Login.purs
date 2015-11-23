module Entries.Login where

import Control.Monad.Eff
import Data.Either
import Data.Lens
import Data.Maybe
import Data.Maybe.Unsafe
import Data.Nullable
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
  }


render :: T.Render Model _ _
render dispatch _ state _ =
  [ R.div
    [ RP.className "col-md-offset-5 col-md-2" ]
    [ R.a
      [ RP.className "btn btn-default btn-lg"
      , RP.href "http://onedrive.login.url"
      ]
      [ R.span
        [ RP.className "glyphicon glyphicon-cloud" ]
        [ ]
      , R.text " Go to OneDrive"
      ]
    ]
  ]


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
