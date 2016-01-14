module Entries.Index.Class where


import Common.React
import qualified Components.BooksDirectory as BooksDirectory
import qualified Components.Header as Header
import Control.Monad.Eff.Exception
import Data.Foldable
import Data.Maybe
import qualified Libs.PouchDB as DB
import Network.HTTP.Affjax
import Prelude
import qualified React as R
import qualified Thermite as T


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
