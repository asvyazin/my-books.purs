module Components.Wrappers.Alert where


import qualified React as R
import qualified Thermite as T


foreign import alertFFI :: forall props. R.ReactClass props


alert :: forall props. props -> Array R.ReactElement -> R.ReactElement
alert =
  R.createElement alertFFI


spec :: forall eff state props action. T.Spec eff state props action
spec =
  T.simpleSpec T.defaultPerformAction render
  where
    render _ p _ c =
      [ alert p c ]
