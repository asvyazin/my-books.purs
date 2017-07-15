module Components.Wrappers.Router where


import React as R


foreign import routerFFI :: forall props. R.ReactClass props
foreign import routeFFI :: forall props. R.ReactClass props
foreign import linkFFI :: forall props. R.ReactClass props

foreign import data History :: Type
foreign import hashHistory :: History
foreign import browserHistory :: History


router :: forall props. props -> Array R.ReactElement -> R.ReactElement
router =
  R.createElement routerFFI


route :: forall props. props -> Array R.ReactElement -> R.ReactElement
route =
  R.createElement routeFFI


link :: forall props. props -> Array R.ReactElement -> R.ReactElement
link =
  R.createElement linkFFI
