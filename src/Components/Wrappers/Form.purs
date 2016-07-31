module Components.Wrappers.Form where


import React as R
import Thermite as T


foreign import formFFI :: forall props. R.ReactClass props


form :: forall props. props -> Array R.ReactElement -> R.ReactElement
form =
  R.createElement formFFI


spec :: forall eff state props action. T.Spec eff state props action
spec =
  T.simpleSpec T.defaultPerformAction render
  where
    render _ p _ c =
      [ form p c ]
