module Common.Settings where


import Data.Foreign.Class
import Data.Generic
import Data.Maybe
import qualified Libs.PouchDB as DB
import Prelude


data Settings =
  Settings
  { booksDirectory :: String
  }


derive instance genericSettings :: Generic Settings


instance showSettings :: Show Settings where
  show = gShow


instance isForeignSettings :: IsForeign Settings where
  read o = do
    _booksDirectory <- readProp "booksDirectory" o
    return $ Settings
      { booksDirectory: _booksDirectory
      }


getSettings :: forall e. DB.PouchDB -> DB.PouchDBAff e Settings
getSettings db =
  DB.get db "settings"


tryGetSettings :: forall e. DB.PouchDB -> DB.PouchDBAff e (Maybe Settings)
tryGetSettings db =
  DB.tryGet db "settings"
