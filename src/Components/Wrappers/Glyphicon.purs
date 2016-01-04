module Components.Wrappers.Glyphicon where


import qualified React as R


type Props =
  { glyph :: String
  }


foreign import glyphiconFFI :: R.ReactClass Props


glyphicon :: String -> Array R.ReactElement -> R.ReactElement
glyphicon glyph children =
  R.createElement glyphiconFFI { glyph : glyph } children


glyphicon' :: String -> R.ReactElement
glyphicon' glyph =
  glyphicon glyph []
