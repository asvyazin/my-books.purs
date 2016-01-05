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


type Props =
  { onedriveToken :: String
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


initialState :: Maybe String -> Model
initialState user =
  { header :
    { title : "MyBooks"
    , userName : user
    , error : Nothing
    }
  , booksDirectory :
    { directory : Nothing
    , showModal : false
    }
  }


spec :: forall eff. T.Spec (ajax :: AJAX, err :: EXCEPTION | eff) Model Props Action
spec =
  T.focusState header H.spec <> T.focus booksDirectory booksDirectoryAction BD.spec


component :: Maybe String -> R.ReactClass Props
component user =
  T.createClass spec $ initialState user
