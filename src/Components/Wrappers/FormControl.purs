module Components.Wrappers.FormControl where


import React as R
import Thermite as T


foreign import formControlFFI :: forall props. R.ReactClass props
foreign import formControlStaticFFI :: forall props. R.ReactClass props


formControl :: forall props. props -> Array R.ReactElement -> R.ReactElement
formControl =
  R.createElement formControlFFI


spec :: forall eff state props action. T.Spec eff state props action
spec =
  T.simpleSpec T.defaultPerformAction render
  where
    render _ p _ c =
      [ formControl p c ]


formControlStatic :: forall props. props -> Array R.ReactElement -> R.ReactElement
formControlStatic =
  R.createElement formControlStaticFFI


staticSpec :: forall eff state props action. T.Spec eff state props action
staticSpec =
  T.simpleSpec T.defaultPerformAction render
  where
    render _ p _ c =
      [ formControlStatic p c ]
