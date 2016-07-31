module Components.Wrappers.Checkbox where


import Common.React (wrapperSpec)
import React as R
import Thermite as T


foreign import checkboxFFI :: forall props. R.ReactClass props


checkbox :: forall props. props -> Array R.ReactElement -> R.ReactElement
checkbox =
  R.createElement checkboxFFI


spec :: forall eff state props action. T.Spec eff state props action
spec =
  wrapperSpec checkbox
