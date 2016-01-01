module Entries.Index.Class where

import Data.Lens
import Data.Maybe
import Prelude
import qualified React as R
import qualified Thermite as T

import qualified Components.Header as H
import qualified Components.BooksDirectory as BD


type Model =
  { header :: H.Model
  , booksDirectory :: BD.State
  }


header :: LensP Model H.Model
header =
  lens _.header (_ { header = _ })


booksDirectory :: LensP Model BD.State
booksDirectory =
  lens _.booksDirectory (_ { booksDirectory = _ })


initialState :: Maybe String -> Model
initialState user =
  { header :
    { title : "MyBooks"
    , userName : user
    , error : Nothing
    }
  , booksDirectory :
    { directory : Nothing
    }
  }


spec :: forall eff props. T.Spec eff Model props (Array R.ReactElement)
spec =
  T.focusState header H.spec <> T.focusState booksDirectory BD.spec


component :: forall props. Maybe String -> R.ReactClass props
component user =
  T.createClass spec $ initialState user