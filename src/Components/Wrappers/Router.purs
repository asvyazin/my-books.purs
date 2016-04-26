module Components.Wrappers.Router where


import React as R


foreign import routerFFI :: forall props. R.ReactClass props
foreign import routeFFI :: forall props. R.ReactClass props

foreign import data History :: *
foreign import hashHistory :: History
foreign import browserHistory :: History


router :: forall props. props -> Array R.ReactElement -> R.ReactElement
router =
  R.createElement routerFFI


route :: forall props. props -> Array R.ReactElement -> R.ReactElement
route =
  R.createElement routeFFI
