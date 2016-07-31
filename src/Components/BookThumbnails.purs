module Components.BookThumbnails where


import Common.React (foreachProps, mapProps)
import Components.BookThumbnails.Thumbnail as Thumbnail
import Components.Wrappers.Col as Col
import Components.Wrappers.Grid as Grid
import Components.Wrappers.Row as Row
import Data.Lens (over)
import Data.List (List, fromFoldable)
import Data.Tuple (Tuple)
import Prelude
import React as R
import Thermite as T


spec :: forall eff state props. T.Spec eff state props (Tuple Int Thumbnail.Action)
spec =
  let
    thumbnails =
      [ { imageUrl: "https://steamcdn-a.akamaihd.net/steam/apps/379720/header_292x136.jpg"
        , size: "292x136"
        , title: "Title"
        , author: "Author"
        , isRead: false
        , id: "1"
        },
        { imageUrl: "https://steamcdn-a.akamaihd.net/steam/apps/379720/header_292x136.jpg"
        , size: "292x136"
        , title: "Title"
        , author: "Author"
        , isRead: false
        , id: "2"
        },
        { imageUrl: "https://steamcdn-a.akamaihd.net/steam/apps/379720/header_292x136.jpg"
        , size: "292x136"
        , title: "Title"
        , author: "Author"
        , isRead: false
        , id: "3"
        },
        { imageUrl: "https://steamcdn-a.akamaihd.net/steam/apps/379720/header_292x136.jpg"
        , size: "292x136"
        , title: "Title"
        , author: "Author"
        , isRead: false
        , id: "4"
        },
        { imageUrl: "https://steamcdn-a.akamaihd.net/steam/apps/379720/header_292x136.jpg"
        , size: "292x136"
        , title: "Title"
        , author: "Author"
        , isRead: false
        , id: "5"
        },
        { imageUrl: "https://steamcdn-a.akamaihd.net/steam/apps/379720/header_292x136.jpg"
        , size: "292x136"
        , title: "Title"
        , author: "Author"
        , isRead: false
        , id: "6"
        },
        { imageUrl: "https://steamcdn-a.akamaihd.net/steam/apps/379720/header_292x136.jpg"
        , size: "292x136"
        , title: "Title"
        , author: "Author"
        , isRead: false
        , id: "7"
        },
        { imageUrl: "https://steamcdn-a.akamaihd.net/steam/apps/379720/header_292x136.jpg"
        , size: "292x136"
        , title: "Title"
        , author: "Author"
        , isRead: false
        , id: "8"
        }
      ]
  in
    mapProps (const $ fromFoldable thumbnails) thumbnailsPage


thumbnailsPage :: forall eff state. T.Spec eff state (List Thumbnail.Props) (Tuple Int Thumbnail.Action)
thumbnailsPage =
  wrapGrid $ foreachProps $ wrapCol Thumbnail.spec
  where
    wrapGrid =
      over T._render wrapRender
      where
        wrapRender origRender dispatch p s c =
          [ Grid.grid {}
            [ Row.row {}
              (origRender dispatch p s c)
            ]
          ]
    wrapCol =
      over T._render wrapRender
      where
        wrapRender origRender dispatch p s c =
          [ Col.col
            { lg: 3 }
            (origRender dispatch p s c)
          ]


bookThumbnails :: R.ReactElement
bookThumbnails =
  R.createElement (T.createClass spec {}) {} []
