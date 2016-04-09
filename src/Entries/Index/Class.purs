module Entries.Index.Class where


import Common.React (mapProps)
import Components.BooksDirectory as BooksDirectory
import Components.Header as Header
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Foldable (fold)
import Data.Maybe (Maybe(..))
import Libs.PouchDB as DB
import Network.HTTP.Affjax (AJAX)
import Prelude (($))
import React as R
import Thermite as T


type Props =
  { onedriveToken :: String
  , db :: Maybe DB.PouchDB
  , userName :: Maybe String
  }


spec :: forall eff state action. T.Spec (ajax :: AJAX, err :: EXCEPTION, pouchdb :: DB.POUCHDB | eff) state Props action
spec =
  fold [ mapProps convertToHeaderProps Header.spec
       , T.simpleSpec T.defaultPerformAction renderBooksDirectory
       ]
  where
    convertToHeaderProps p =
      { title: "MyBooks"
      , userName: p.userName
      , error: Nothing
      }

    renderBooksDirectory _ props _ _ =
      [ BooksDirectory.booksDirectory $ convertToBooksDirectoryProps props ]

    convertToBooksDirectoryProps p =
      { onedriveToken: p.onedriveToken
      , db: p.db
      }


component :: R.ReactClass Props
component =
  T.createClass spec {}
