module Entries.Index where


import Control.Bind
import Control.Monad.Aff
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Exception
import Data.Argonaut.Combinators
import Data.Argonaut.Core
import Data.Argonaut.Decode
import Data.Argonaut.Parser
import Data.Either
import Data.Generic
import Data.Maybe
import Data.Maybe.Unsafe
import Data.Nullable
import DOM
import DOM.HTML
import DOM.HTML.Types
import DOM.HTML.Window
import DOM.Node.ParentNode
import qualified Libs.PouchDB as DB
import Network.HTTP.Affjax
import Prelude
import qualified React as R
import Web.Cookies


import Entries.Index.Class
import Common.Settings


main :: Eff (cookie :: COOKIE, ajax :: AJAX, dom :: DOM, err :: EXCEPTION, pouchdb :: DB.POUCHDB) Unit
main = launchAff $ do
  db <- liftEff $ DB.newPouchDB "MyBooks.purs"
  settings <- tryGetSettings db
  onedriveToken <- liftEff $ getCookie "onedriveToken"
  let getName (UserInfo userInfo) =
        userInfo.name
  let fromEither =
        eitherToMaybe >=> (return <<< getName)
  let maybeName =
        getUserInfo >=> (return <<< fromEither)
  user <- maybe (return Nothing) maybeName onedriveToken
  liftEff $ renderMain user
    

eitherToMaybe :: forall a b. Either a b -> Maybe b
eitherToMaybe =
  either (const Nothing) Just


renderMain :: forall e. Maybe String -> Eff (dom :: DOM | e) Unit
renderMain user = do
  node <- htmlDocumentToParentNode <$> (window >>= document)
  container <- (fromJust <<< toMaybe) <$> querySelector ".application" node
  void $ R.render (R.createFactory (component user) {}) container


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
  

instance decodeJsonUserInfo :: DecodeJson UserInfo where
  decodeJson = foldJson invalidJson invalidJson invalidJson invalidJson invalidJson fromJObject
    where
      invalidJson :: forall a b. (Show a) => a -> Either String b
      invalidJson x =
        Left $ "Invalid Json: " ++ show x

      fromJObject :: JObject -> Either String UserInfo
      fromJObject o = do
        _id <- o .? "id"
        _name <- o .? "name"
        _firstName <- o .? "first_name"
        _lastName <- o .? "last_name"
        return $ UserInfo
          { id : _id
          , name : _name
          , firstName : _firstName
          , lastName : _lastName
          }


getUserInfo :: forall e. String -> Aff (ajax :: AJAX | e) (Either String UserInfo)
getUserInfo token = do
  let url = "https://apis.live.net/v5.0/me?access_token=" ++ token
  resp <- get url
  let result = jsonParser resp.response >>= decodeJson
  return result
