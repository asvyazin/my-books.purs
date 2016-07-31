module Components.Wrappers.Col where


import React as R
import Thermite as T


foreign import colFFI :: forall props. R.ReactClass props


col :: forall props. props -> Array R.ReactElement -> R.ReactElement
col =
  R.createElement colFFI


spec :: forall eff state props action. T.Spec eff state props action
spec =
  T.simpleSpec T.defaultPerformAction render
  where
    render _ p _ c =
      [ col p c ]
