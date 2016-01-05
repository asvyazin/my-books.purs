module Common.OneDriveApi where


import Control.Monad.Aff
import Control.Monad.Eff.Exception
import Control.Monad.Error.Class
import Data.Argonaut.Combinators
import Data.Argonaut.Core
import Data.Argonaut.Decode
import Data.Argonaut.Parser
import Data.Either
import Data.Generic
import Data.Maybe
import qualified Data.StrMap as M
import Data.Traversable
import Network.HTTP.Affjax
import Network.HTTP.Method
import Network.HTTP.RequestHeader
import Prelude


newtype UserInfo =
  UserInfo
  { id :: String
  , name :: String
  , firstName :: Maybe String
  , lastName :: Maybe String
  }


derive instance genericUserInfo :: Generic UserInfo


instance showUserInfo :: Show UserInfo where
  show = gShow
  

mFail :: forall a b. a -> Maybe b -> Either a b
mFail err =
  maybe (Left err) Right


(.??) :: forall a. (DecodeJson a) => JObject -> String -> Either String (Maybe a)
(.??) jObj field =
  maybe (pure Nothing) decode $ M.lookup field jObj
  where
    decode json = Just <$> decodeJson json


instance decodeJsonUserInfo :: DecodeJson UserInfo where
  decodeJson json = do
    o <- mFail "Expected object" $ toObject json
    id <- o .? "id"
    name <- o .? "name"
    firstName <- o .?? "first_name"
    lastName <- o .?? "last_name"
    return $ UserInfo { id, name, firstName, lastName }


getUserInfo :: forall e. String -> Aff (ajax :: AJAX | e) UserInfo
getUserInfo token = do
  let url = "https://apis.live.net/v5.0/me?access_token=" ++ token
  resp <- get url
  getJson resp


newtype OneDriveFolderFacet =
  OneDriveFolderFacet { childCount :: Int }


newtype OneDriveFileFacet =
  OneDriveFileFacet { mimeType :: String }


newtype ItemReference =
  ItemReference
  { driveId :: String
  , id :: String
  , path :: String
  }


newtype OneDriveItem =
  OneDriveItem
  { id :: String
  , name :: String
  , folder :: Maybe OneDriveFolderFacet
  , file :: Maybe OneDriveFileFacet
  , parentReference :: Maybe ItemReference
  }


newtype OneDriveItems =
  OneDriveItems
  { value :: Array OneDriveItem
  }


derive instance genericOneDriveFolderFacet :: Generic OneDriveFolderFacet


derive instance genericOneDriveFileFacet :: Generic OneDriveFileFacet


derive instance genericItemReference :: Generic ItemReference


derive instance genericOneDriveItem :: Generic OneDriveItem


derive instance genericOneDriveItems :: Generic OneDriveItems


instance decodeJsonOneDriveFolderFacet :: DecodeJson OneDriveFolderFacet where
  decodeJson json = do
    o <- mFail "Expected object" $ toObject json
    childCount <- o .? "childCount"
    return $ OneDriveFolderFacet { childCount }


instance decodeJsonOneDriveFileFacet :: DecodeJson OneDriveFileFacet where
  decodeJson json = do
    o <- mFail "Expected object" $ toObject json
    mimeType <- o .? "mimeType"
    return $ OneDriveFileFacet { mimeType }


instance decodeJsonOneDriveItemReference :: DecodeJson ItemReference where
  decodeJson json = do
    o <- mFail "Expected object" $ toObject json
    driveId <- o .? "driveId"
    id <- o .? "id"
    path <- o .? "path"
    return $ ItemReference { driveId, id, path }


instance decodeJsonOneDriveItem :: DecodeJson OneDriveItem where
  decodeJson json = do
    o <- mFail "Expected object" $ toObject json
    id <- o .? "id"
    name <- o .? "name"
    folder <- o .?? "folder"
    file <- o .?? "file"
    parentReference <- o .?? "parentReference"
    return $ OneDriveItem { id, name, folder, file, parentReference }


instance decodeJsonOneDriveItems :: DecodeJson OneDriveItems where
  decodeJson json = do
    o <- mFail "Expected object" $ toObject json
    jValue <- mFail "Expected 'value'" $ M.lookup "value" o
    jArr <- mFail "Expected array" $ toArray jValue
    value <- traverse decodeJson jArr
    return $ OneDriveItems { value }


getChildrenByItemId :: forall e. String -> Maybe String -> Aff (ajax :: AJAX | e) (Array OneDriveItem)
getChildrenByItemId token itemId =
  let
    url =
      maybe "/drive/root/children" (\x -> "/drive/items/" ++ x ++ "/children") itemId
  in
   getOneDriveItems token url


getChildrenByItemPath :: forall e. String -> Maybe String -> Aff (ajax :: AJAX | e) (Array OneDriveItem)
getChildrenByItemPath token itemPath =
  let
    url =
      maybe "/drive/root/children" (\x -> "/drive/root:/" ++ x ++ ":/children") itemPath
  in
   getOneDriveItems token url


getOneDriveItems :: forall e. String -> String -> Aff (ajax :: AJAX | e) (Array OneDriveItem)
getOneDriveItems token url = do
  let req = onedriveGetRequest token url
  getItems <$> doJsonRequest req
  where
    getItems (OneDriveItems items) =
      items.value


getOneDriveItem :: forall e. String -> Maybe String -> Aff (ajax :: AJAX | e) OneDriveItem
getOneDriveItem token itemId = do
  let
    url =
      maybe "/drive/root" ("/drive/items/" ++) itemId
    req =
      onedriveGetRequest token url
  doJsonRequest req


onedriveGetRequest :: String -> String -> AffjaxRequest Unit
onedriveGetRequest token url =
  defaultRequest
  { method = GET
  , url = onedriveApiBaseUrl ++ url
  , headers = [ RequestHeader "Authorization" ("bearer " ++ token) ]
  }


onedriveApiBaseUrl :: String
onedriveApiBaseUrl = "https://api.onedrive.com/v1.0"


doJsonRequest :: forall e a. (DecodeJson a) => AffjaxRequest Unit -> Aff (ajax :: AJAX | e) a
doJsonRequest req =
  affjax req >>= getJson


getJson :: forall m a. (DecodeJson a, MonadError Error m) => AffjaxResponse String -> m a
getJson resp = do
  let result = jsonParser resp.response >>= decodeJson
  either (throwError <<< error) return result
