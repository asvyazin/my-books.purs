module Components.Wrappers.Row where


import Common.React (wrapperSpec)
import React as R
import Thermite as T


foreign import rowFFI :: forall props. R.ReactClass props


row :: forall props. props -> Array R.ReactElement -> R.ReactElement
row =
  R.createElement rowFFI


spec :: forall eff state props action. T.Spec eff state props action
spec =
  wrapperSpec row
