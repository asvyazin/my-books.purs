module Common.OneDriveApi where


import Common.Json ((.??))
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Exception (Error, error)
import Control.Monad.Error.Class (class MonadError, throwError)
import Data.Argonaut.Combinators ((.?), (?>>=))
import Data.Argonaut.Core (toArray, toObject)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (either)
import Data.Generic (class Generic)
import Data.Maybe (Maybe, maybe)
import Data.StrMap as M
import Data.Traversable (traverse)
import Global (encodeURIComponent)
import Network.HTTP.Affjax (AJAX, AffjaxRequest, AffjaxResponse, defaultRequest, affjax)
import Network.HTTP.Method (Method(GET))
import Network.HTTP.RequestHeader (RequestHeader(RequestHeader))
import Prelude


newtype UserInfo =
  UserInfo
  { _id :: String
  , displayName :: String
  }


instance decodeJsonUserInfo :: DecodeJson UserInfo where
  decodeJson json = do
    o <- toObject json ?>>= "Expected object"
    _id <- o .? "id"
    displayName <- o .? "name"
    return $ UserInfo { _id, displayName }


getUserInfo :: forall e. String -> Aff (ajax :: AJAX | e) UserInfo
getUserInfo token =
  doJsonRequest $ graphGetRequest token "/me"


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
    o <- toObject json ?>>= "Expected object"
    childCount <- o .? "childCount"
    return $ OneDriveFolderFacet { childCount }


instance decodeJsonOneDriveFileFacet :: DecodeJson OneDriveFileFacet where
  decodeJson json = do
    o <- toObject json ?>>= "Expected object"
    mimeType <- o .? "mimeType"
    return $ OneDriveFileFacet { mimeType }


instance decodeJsonOneDriveItemReference :: DecodeJson ItemReference where
  decodeJson json = do
    o <- toObject json ?>>= "Expected object"
    driveId <- o .? "driveId"
    id <- o .? "id"
    path <- o .? "path"
    return $ ItemReference { driveId, id, path }


instance decodeJsonOneDriveItem :: DecodeJson OneDriveItem where
  decodeJson json = do
    o <- toObject json ?>>= "Expected object"
    id <- o .? "id"
    name <- o .? "name"
    folder <- o .?? "folder"
    file <- o .?? "file"
    parentReference <- o .?? "parentReference"
    return $ OneDriveItem { id, name, folder, file, parentReference }


instance decodeJsonOneDriveItems :: DecodeJson OneDriveItems where
  decodeJson json = do
    o <- toObject json ?>>= "Expected object"
    jValue <- M.lookup "value" o ?>>= "Expected 'value'"
    jArr <- toArray jValue ?>>= "Expected array"
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
      maybe "/drive/root" ("/drive/items/" ++ _) itemId
    req =
      onedriveGetRequest token url
  doJsonRequest req


onedriveGetRequest :: String -> String -> AffjaxRequest Unit
onedriveGetRequest token url =
  getRequest token $ onedriveApiBaseUrl ++ url


graphGetRequest :: String -> String -> AffjaxRequest Unit
graphGetRequest token url =
  getRequestTokenInUrl token $ graphApiBaseUrl ++ url


getRequest :: String -> String -> AffjaxRequest Unit
getRequest token url =
  defaultRequest
  { method = GET
  , url = url
  , headers = [ RequestHeader "Authorization" ("Bearer " ++ token) ]
  }


getRequestTokenInUrl :: String -> String -> AffjaxRequest Unit
getRequestTokenInUrl token url =
  defaultRequest
  { method = GET
  , url = url ++ "?access_token=" ++ encodeURIComponent token
  }


onedriveApiBaseUrl :: String
onedriveApiBaseUrl = "https://api.onedrive.com/v1.0"


graphApiBaseUrl :: String
graphApiBaseUrl = "https://apis.live.net/v5.0"


doJsonRequest :: forall e a. (DecodeJson a) => AffjaxRequest Unit -> Aff (ajax :: AJAX | e) a
doJsonRequest req =
  affjax req >>= getJson


getJson :: forall m a. (DecodeJson a, MonadError Error m) => AffjaxResponse String -> m a
getJson resp = do
  let result = jsonParser resp.response >>= decodeJson
  either (throwError <<< error) return result
