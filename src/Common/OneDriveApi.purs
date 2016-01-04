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


getUserInfo :: forall e. String -> Aff (ajax :: AJAX | e) (Either String UserInfo)
getUserInfo token = do
  let url = "https://apis.live.net/v5.0/me?access_token=" ++ token
  resp <- get url
  let result = jsonParser resp.response >>= decodeJson
  return result


newtype OneDriveFolderFacet =
  OneDriveFolderFacet { childCount :: Int }


newtype OneDriveFileFacet =
  OneDriveFileFacet { mimeType :: String }


newtype OneDriveItem =
  OneDriveItem
  { id :: String
  , name :: String
  , folder :: Maybe OneDriveFolderFacet
  , file :: Maybe OneDriveFileFacet
  }


newtype OneDriveItems =
  OneDriveItems
  { value :: Array OneDriveItem
  }


derive instance genericOneDriveFolderFacet :: Generic OneDriveFolderFacet


derive instance genericOneDriveFileFacet :: Generic OneDriveFileFacet


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


instance decodeJsonOneDriveItem :: DecodeJson OneDriveItem where
  decodeJson json = do
    o <- mFail "Expected object" $ toObject json
    id <- o .? "id"
    name <- o .? "name"
    folder <- o .?? "folder"
    file <- o .?? "file"
    return $ OneDriveItem { id, name, folder, file }


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
  let
    req =
      defaultRequest
      { method = GET
      , url = "https://api.onedrive.com/v1.0" ++ url
      , headers = [ RequestHeader "Authorization" ("bearer " ++ token) ]
      }
  resp <- affjax req
  let result = jsonParser resp.response >>= decodeJson
  either (throwError <<< error) getItems result
    where
      getItems (OneDriveItems items) =
        return items.value
