module Entries.Index where


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
import Network.HTTP.Affjax
import Prelude
import qualified React as R
import Web.Cookies


import Entries.Index.Class


main :: Eff (cookie :: COOKIE, ajax :: AJAX, dom :: DOM, err :: EXCEPTION) Unit
main = launchAff $ do
  onedriveToken <- liftEff $ getCookie "onedriveToken"
  user <- case onedriveToken of
    Nothing ->
      return Nothing
    Just token -> do
      result <- getUserInfo token
      return $ case result of
        Right (UserInfo userInfo) ->
          Just $ userInfo.name
        _ ->
          Nothing
  liftEff $ renderMain user


renderMain :: forall e. Maybe String -> Eff (dom :: DOM | e) Unit
renderMain user = do
  node <- htmlDocumentToParentNode <$> (window >>= document)
  container <- (fromJust <<< toMaybe) <$> querySelector ".application" node
  void $ R.render (R.createFactory (component user) {}) container


data UserInfo =
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
        return $
          UserInfo
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
