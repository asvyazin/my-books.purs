module Components.Wrappers.FormGroup where


import React as R
import Thermite as T


foreign import formGroupFFI :: forall props. R.ReactClass props


formGroup :: forall props. props -> Array R.ReactElement -> R.ReactElement
formGroup =
  R.createElement formGroupFFI


spec :: forall eff state props action. T.Spec eff state props action
spec =
  T.simpleSpec T.defaultPerformAction render
  where
    render _ p _ c =
      [ formGroup p c ]
