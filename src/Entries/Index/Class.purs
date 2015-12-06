module Entries.Index.Class where

import Data.Lens
import Data.Maybe
import qualified React as R
import qualified Thermite as T

import qualified Components.Header as H


type Model =
  { header :: H.Model
  }


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


spec :: forall eff props. T.Spec eff Model props (Array R.ReactElement)
spec =
  T.focusState header H.spec


component :: forall props. R.ReactClass props
component =
  T.createClass spec initialState
