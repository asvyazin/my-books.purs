module Components.Wrappers.Modal where


import qualified React as R


foreign import modalFFI :: forall props. R.ReactClass props


modal :: forall props. props -> Array R.ReactElement -> R.ReactElement
modal = R.createElement modalFFI


foreign import headerFFI :: forall props. R.ReactClass props


header :: forall props. props -> Array R.ReactElement -> R.ReactElement
header = R.createElement headerFFI


foreign import titleFFI :: forall props. R.ReactClass props


title :: forall props. props -> Array R.ReactElement -> R.ReactElement
title = R.createElement titleFFI


foreign import bodyFFI :: forall props. R.ReactClass props


body :: forall props. props -> Array R.ReactElement -> R.ReactElement
body = R.createElement bodyFFI


foreign import footerFFI :: forall props. R.ReactClass props


footer :: forall props. props -> Array R.ReactElement -> R.ReactElement
footer = R.createElement footerFFI
