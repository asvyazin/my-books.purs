module Components.BookThumbnails.Thumbnail where


import Components.Wrappers.Checkbox as Checkbox
import Components.Wrappers.Thumbnail as Thumbnail
import Control.Applicative ((<$>))
import Data.Array (catMaybes)
import Data.Maybe (Maybe(Just), maybe)
import Data.Monoid ((<>))
import React (ReactElement, createElement) as R
import React.DOM (h3, text, p, a) as R
import React.DOM.Props (href)
import Thermite as T


type Props =
  { id :: String
  , imageUrl :: String
  , size :: String
  , title :: String
  , author :: String
  , isRead :: Boolean
  , epubVersion :: Maybe String
  , viewEpubUrl :: Maybe String
  }


data Action =
  ReadToggle


spec :: forall eff state. T.Spec eff state Props Action
spec =
  T.simpleSpec T.defaultPerformAction render
  where
    render dispatch props _ _ =
      let
        children = catMaybes
          [ Just (renderTitle props.viewEpubUrl props.title)
          , Just (renderAuthor props.author)
          , renderEpubVersion <$> props.epubVersion
          , Just (renderIsRead dispatch props.isRead)
          ]
      in
        [ Thumbnail.thumbnail
          { src : props.imageUrl
          , alt : props.size
          , key : props.id
          }
          children
        ]
    renderTitle url title =
      let
        txt =
          R.text title
        titleHtml =
          maybe txt (\u -> R.a [ href u ] [ txt ]) url
      in
        R.h3 [] [ titleHtml ]
    renderAuthor author =
      R.p [] [ R.text author ]
    renderEpubVersion version =
      R.p [] [ R.text ("EPUB: " <> version) ]
    renderIsRead dispatch isRead =
      Checkbox.checkbox
      { checked : isRead
      , onChange : dispatch ReadToggle
      }
      [ R.text "Read" ]


thumbnail :: Props -> R.ReactElement
thumbnail props =
  R.createElement (T.createClass spec {}) props []
