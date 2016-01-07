module Entries.Index.Class where

import Control.Monad.Eff.Exception
import Data.Lens
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
  }


type Model =
  { header :: H.Model
  , booksDirectory :: BD.State
  }


data Action
  = BooksDirectoryAction BD.Action


header :: LensP Model H.Model
header =
  lens _.header (_ { header = _ })


booksDirectory :: LensP Model BD.State
booksDirectory =
  lens _.booksDirectory (_ { booksDirectory = _ })


booksDirectoryAction :: PrismP Action BD.Action
booksDirectoryAction =
  prism' BooksDirectoryAction tryBD
  where
    tryBD (BooksDirectoryAction a) = Just a


initialState :: Maybe String -> Maybe BD.DirectoryInfo -> Model
initialState user directory =
  { header :
    { title : "MyBooks"
    , userName : user
    , error : Nothing
    }
  , booksDirectory :
    { directory
    , showModal : false
    }
  }


spec :: forall eff refs. T.Spec (ajax :: AJAX, err :: EXCEPTION, refs :: R.ReactRefs refs, state :: R.ReactState R.ReadWrite, props :: R.ReactProps, pouchdb :: DB.POUCHDB | eff) Model Props Action
spec =
  T.focusState header H.spec <> T.focus booksDirectory booksDirectoryAction BD.spec


component :: Maybe String -> Maybe BD.DirectoryInfo -> R.ReactClass Props
component user directory =
  T.createClass spec $ initialState user directory
