module Components.Wrappers.Thumbnail where


import Common.React (wrapperSpec)
import React as R
import Thermite as T


foreign import thumbnailFFI :: forall props. R.ReactClass props


thumbnail :: forall props. props -> Array R.ReactElement -> R.ReactElement
thumbnail =
  R.createElement thumbnailFFI


spec :: forall eff state props action. T.Spec eff state props action
spec =
  wrapperSpec thumbnail
