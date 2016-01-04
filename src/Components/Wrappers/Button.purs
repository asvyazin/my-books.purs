module Components.Wrappers.Button where


import Data.Foreign
import Data.Maybe
import Prelude
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


convertProps :: Props -> Foreign
convertProps props =
  case props.bsStyle of
    Nothing ->
      case props.bsSize of
        Nothing ->
          toForeign {}
        Just sz ->
          toForeign { bsSize : convertBsSize sz }
    Just st ->
      case props.bsSize of
        Nothing ->
          toForeign { bsStyle : convertBsStyle st }
        Just sz ->
          toForeign { bsStyle : convertBsStyle st
                    , bsSize : convertBsSize sz }
  where
    convertBsStyle Success = "success"
    convertBsStyle Warning = "warning"
    convertBsStyle Danger = "danger"
    convertBsStyle Info = "info"
    convertBsStyle Default = "default"
    convertBsStyle Primary = "primary"
    convertBsStyle Link = "link"

    convertBsSize Large = "large"
    convertBsSize Small = "small"
    convertBsSize XSmall = "xsmall"


defaultProps :: Props
defaultProps =
  { bsStyle : Nothing
  , bsSize : Nothing
  }


foreign import buttonFFI :: R.ReactClass Foreign


button :: Props -> Array R.ReactElement -> R.ReactElement
button props children =
  R.createElement buttonFFI (convertProps props) children
