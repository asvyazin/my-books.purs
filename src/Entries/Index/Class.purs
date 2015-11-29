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


spec :: T.Spec _ Model _ _
spec =
  T.focusState header H.spec


component :: R.ReactClass _
component =
  T.createClass spec initialState
