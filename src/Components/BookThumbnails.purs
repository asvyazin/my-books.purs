module Components.BookThumbnails where


import Common.Data.BookInfo (BookInfo(BookInfo))
import Common.React (foreachProps, mapPropsWithState)
import Components.BookThumbnails.Thumbnail as Thumbnail
import Components.Wrappers.Col as Col
import Components.Wrappers.Grid as Grid
import Components.Wrappers.Row as Row
import Control.Monad.Aff (launchAff)
import Control.Monad.Eff.Class (liftEff)
import Data.Lens (over)
import Data.List (List, fromFoldable)
import Data.Tuple (Tuple)
import Libs.PouchDB (PouchDB)
import Libs.PouchDB.Json (query)
import Libs.PouchDB.QueryItem (getDoc)
import Prelude
import React as R
import Thermite as T


type Props =
  { db :: PouchDB
  , currentPage :: Int
  }


type State =
  { thumbnails :: List Thumbnail.Props
  }


spec :: forall eff. T.Spec eff State Props (Tuple Int Thumbnail.Action)
spec =
  mapPropsWithState (\_ st -> st.thumbnails) thumbnailsPage


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


component :: R.ReactClass Props
component =
  R.createClass reactSpec.spec { componentDidMount = componentDidMount }
  where
    reactSpec =
      T.createReactSpec spec defaultState

    componentDidMount this = do
      props <- R.getProps this
      void $ launchAff $ do
        res <- query props.db "books/all" { limit: pageSize, include_docs: true, skip: props.currentPage * pageSize }
        liftEff $ R.transformState this $ \state ->
          state { thumbnails = map (getThumbnail <<< getDoc) res.rows }

    getThumbnail :: BookInfo -> Thumbnail.Props
    getThumbnail (BookInfo t) =
      { imageUrl: "https://steamcdn-a.akamaihd.net/steam/apps/379720/header_292x136.jpg"
      , size: "292x136"
      , title: "Title"
      , author: "Author"
      , isRead: t.read
      , id: t._id
      }


pageSize :: Int
pageSize = 24


defaultState :: State
defaultState =
  { thumbnails : fromFoldable []
  }


bookThumbnails :: Props -> R.ReactElement
bookThumbnails props =
  R.createElement component props []
