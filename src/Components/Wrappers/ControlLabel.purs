module Components.Wrappers.ControlLabel where


import React as R
import Thermite as T


foreign import controlLabelFFI :: forall props. R.ReactClass props


controlLabel :: forall props. props -> Array R.ReactElement -> R.ReactElement
controlLabel =
  R.createElement controlLabelFFI


spec :: forall eff state props action. T.Spec eff state props action
spec =
  T.simpleSpec T.defaultPerformAction render
  where
    render _ p _ c =
      [ controlLabel p c ]
