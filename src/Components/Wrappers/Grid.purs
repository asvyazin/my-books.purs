module Components.Wrappers.Grid where


import Common.React (wrapperSpec)
import React as R
import Thermite as T


foreign import gridFFI :: forall props. R.ReactClass props


grid :: forall props. props -> Array R.ReactElement -> R.ReactElement
grid =
  R.createElement gridFFI


spec :: forall eff state props action. T.Spec eff state props action
spec =
  wrapperSpec grid
