module Components.Wrappers.Panel where


import React as R
import Thermite as T


foreign import panelFFI :: forall props. R.ReactClass props


panel :: forall props. props -> Array R.ReactElement -> R.ReactElement
panel =
  R.createElement panelFFI


spec :: forall eff state props action. T.Spec eff state props action
spec =
  T.simpleSpec T.defaultPerformAction render
  where
    render _ p _ c =
      [ panel p c ]
