module Components.BookThumbnails.Thumbnail where


import Components.Wrappers.Checkbox as Checkbox
import Components.Wrappers.Thumbnail as Thumbnail
import React (ReactElement, createElement) as R
import React.DOM (h3, text, p) as R
import Thermite as T


type Props =
  { id :: String
  , imageUrl :: String
  , size :: String
  , title :: String
  , author :: String
  , isRead :: Boolean
  }


data Action =
  ReadToggle


spec :: forall eff state. T.Spec eff state Props Action
spec =
  T.simpleSpec T.defaultPerformAction render
  where
    render dispatch props _ _ =
      [ Thumbnail.thumbnail
        { src : props.imageUrl
        , alt : props.size
        , key : props.id
        }
        [ R.h3 [] [ R.text props.title ]
        , R.p [] [ R.text props.author ]
        , Checkbox.checkbox
          { checked : props.isRead
          , onChange : dispatch ReadToggle
          }
          [ R.text "Read" ]
        ]
      ]


thumbnail :: Props -> R.ReactElement
thumbnail props =
  R.createElement (T.createClass spec {}) props []
