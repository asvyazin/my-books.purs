module Components.BookThumbnails where


import Common.Data.BookInfo (BookInfo(BookInfo))
import Common.React (foreachProps, mapPropsWithState)
import Components.BookThumbnails.Pager as Pager
import Components.BookThumbnails.Thumbnail as Thumbnail
import Components.Wrappers.Col as Col
import Components.Wrappers.Grid as Grid
import Components.Wrappers.Row as Row
import Control.Monad.Aff (launchAff)
import Control.Monad.Eff.Class (liftEff)
import Data.Array (fromFoldable, concat) as A
import Data.Foldable (fold)
import Data.Lens (over)
import Data.List (List(Nil, Cons), fromFoldable, reverse)
import Data.Maybe (fromMaybe)
import Data.Tuple (Tuple(Tuple))
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
  , pageCount :: Int
  }


spec :: forall eff. T.Spec eff State Props (Tuple Int Thumbnail.Action)
spec =
  fold
  [ mapPropsWithState (\_ st -> st.thumbnails) thumbnailsPage
  , mapPropsWithState convertProps Pager.spec
  ]
  where
    convertProps props state =
      { currentPage : props.currentPage
      , pageCount : state.pageCount
      }


thumbnailsPage :: forall eff state. T.Spec eff state (List Thumbnail.Props) (Tuple Int Thumbnail.Action)
thumbnailsPage =
  wrapGrid $ batchProps 4 $ wrapRow $ foreachProps $ wrapCol Thumbnail.spec
  where
    wrapGrid =
      over T._render wrapRender
      where
        wrapRender origRender dispatch p s c =
          [ Grid.grid {}
            (origRender dispatch p s c)
          ]
    wrapRow =
      over T._render wrapRender
      where
        wrapRender origRender dispatch p s c =
          [ Row.row {}
            (origRender dispatch p s c)
          ]
    wrapCol =
      over T._render wrapRender
      where
        wrapRender origRender dispatch p s c =
          [ Col.col
            { lg: 3 }
            (origRender dispatch p s c)
          ]


takeDrop :: forall a. Int -> List a -> Tuple (List a) (List a)
takeDrop = go Nil
  where
    go acc 0 t = Tuple (reverse acc) t
    go acc _ Nil = Tuple (reverse acc) Nil
    go acc n (Cons x xs) = go (Cons x acc) (n - 1) xs


batch :: forall a. Int -> List a -> List (List a)
batch n = go Nil
  where
    go acc Nil = reverse acc
    go acc l =
      case takeDrop n l of
        Tuple t d ->
          go (Cons t acc) d


batchProps :: forall eff state props action. Int -> T.Spec eff state (List props) action -> T.Spec eff state (List props) action
batchProps batchSize origSpec =
  over T._render wrapRender origSpec
  where
    wrapRender origRender dispatch p s _ =
      A.concat $ A.fromFoldable $ map (\p' -> origRender dispatch p' s []) $ batch batchSize p


component :: R.ReactClass Props
component =
  R.createClass reactSpec.spec
  { componentDidMount = componentDidMount
  , componentWillReceiveProps = componentWithReceiveProps
  }
  where
    reactSpec =
      T.createReactSpec spec defaultState

    componentDidMount this = do
      props <- R.getProps this
      reloadThumbnails this props

    reloadThumbnails this props =
      void $ launchAff $ do
        res <- query props.db "books/all" { limit: pageSize, include_docs: true, skip: props.currentPage * pageSize }
        liftEff $ R.transformState this $ \state ->
          state
          { thumbnails = map (getThumbnail <<< getDoc) res.rows
          , pageCount = (res.total_rows + 1) / pageSize
          }

    componentWithReceiveProps this newProps =
      reloadThumbnails this newProps

    getThumbnail :: BookInfo -> Thumbnail.Props
    getThumbnail (BookInfo t) =
      { imageUrl: "https://steamcdn-a.akamaihd.net/steam/apps/379720/header_292x136.jpg"
      , size: "292x136"
      , title: fromMaybe "Unknown Title" t.title
      , author: fromMaybe "Unknown Author" t.author
      , isRead: t.read
      , id: t._id
      , epubVersion: t.epubVersion
      }


pageSize :: Int
pageSize = 24


defaultState :: State
defaultState =
  { thumbnails : fromFoldable []
  , pageCount : 0
  }


bookThumbnails :: Props -> R.ReactElement
bookThumbnails props =
  R.createElement component props []
