module Main where

import Control.Monad.Eff
import Data.Either
import Data.Lens
import Data.Maybe.Unsafe
import Data.Nullable
import DOM
import DOM.HTML
import DOM.HTML.Document
import DOM.HTML.Types
import DOM.HTML.Window
import DOM.Node.ParentNode
import Prelude
import qualified Thermite as T
import qualified React as R
import qualified React.DOM as R
import qualified React.DOM.Props as RP

import qualified Header as H


type Model =
  { header :: H.Model
  }


header :: LensP Model H.Model
header = lens _.header (_ { header = _ })


initialState :: Model
initialState =
  { header : H.initialState
  }


spec :: T.Spec _ Model _ _
spec = T.focusState header H.spec


main :: Eff (dom :: DOM) Unit
main = do
  let component = T.createClass spec initialState
  doc <- window >>= document
  container <- querySelector ".application" $ htmlDocumentToParentNode doc
  void $ R.render (R.createFactory component {}) $ fromJust $ toMaybe container
