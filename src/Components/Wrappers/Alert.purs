module Components.Wrappers.Alert where


import qualified React as R


foreign import alertFFI :: forall props. R.ReactClass props


alert :: forall props. props -> Array R.ReactElement -> R.ReactElement
alert =
  R.createElement alertFFI
