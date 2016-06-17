module Components.Wrappers.Button where


import React as R


foreign import buttonFFI :: forall props. R.ReactClass props


button :: forall props. props -> Array R.ReactElement -> R.ReactElement
button props children =
  R.createElement buttonFFI props children
