module Components.Wrappers.Button where


import Data.Maybe
import qualified React as R


data Style =
  Success
  | Warning
  | Danger
  | Info
  | Default
  | Primary
  | Link


data Size =
  Large
  | Small
  | XSmall


type Props =
  { bsStyle :: Maybe Style
  , bsSize :: Maybe Size
  }


defaultProps :: Props
defaultProps =
  { bsStyle : Nothing
  , bsSize : Nothing
  }


foreign import buttonFFI :: R.ReactClass Props


button :: Props -> Array R.ReactElement -> R.ReactElement
button props children =
  R.createElement buttonFFI props children
