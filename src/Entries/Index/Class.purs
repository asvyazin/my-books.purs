module Entries.Index.Class where

import Control.Monad.Eff.Exception
import Data.Maybe
import Network.HTTP.Affjax
import Prelude
import qualified React as R
import qualified Thermite as T

import qualified Components.Header as H
import qualified Components.BooksDirectory as BD
import qualified Libs.PouchDB as DB


type Props =
  { onedriveToken :: String
  , db :: Maybe DB.PouchDB
  , userName :: Maybe String
  }


spec :: forall eff state action. T.Spec (ajax :: AJAX, err :: EXCEPTION, pouchdb :: DB.POUCHDB | eff) state Props action
spec =
  T.simpleSpec T.defaultPerformAction render
  where
    render :: T.Render state Props action
    render _ props _ _ =
      [ H.header
        { title: "MyBooks"
        , userName: props.userName
        , error: Nothing
        }
      , BD.booksDirectory
        { onedriveToken: props.onedriveToken
        , db: props.db
        }
      ]


component :: R.ReactClass Props
component =
  T.createClass spec {}
