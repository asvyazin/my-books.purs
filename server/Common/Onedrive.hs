{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Common.Onedrive where


import Common.JSONHelper (jsonParser)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Aeson.Types (FromJSON,
                         parseJSON,
                         Value(String, Object),
                         typeMismatch,
                         (.:),
                         (.:?))
import qualified Data.ByteString as B
import Data.Conduit (($$))
import Data.Conduit.Attoparsec (sinkParser)
import qualified Data.Text as T (Text, append, pack)
import qualified Data.Text.Encoding as T (encodeUtf8)
import Network.HTTP.Client.Conduit (HasHttpManager
                                   , parseUrl
                                   , method
                                   , requestBody
                                   , RequestBody (RequestBodyBS)
                                   , requestHeaders
                                   , responseBody
                                   , withResponse)
import Network.HTTP.Types.Header (hContentType, hAuthorization)
import Network.HTTP.Types.URI (renderSimpleQuery)
import System.Environment (lookupEnv)


data OauthTokenRequest =
  OauthTokenRequest
  { clientId :: T.Text
  , redirectUri :: T.Text
  , clientSecret :: T.Text
  } deriving (Eq, Show)


data OauthTokenType =
  Bearer deriving (Eq, Show)


data OauthTokenResponse =
  OauthTokenResponse
  { tokenType :: OauthTokenType
  , expiresIn :: Int
  , scope :: T.Text
  , accessToken :: T.Text
  , refreshToken :: Maybe T.Text
  } deriving (Eq, Show)


instance FromJSON OauthTokenType where
  parseJSON (String "bearer") =
    return Bearer
    
  parseJSON invalid =
    typeMismatch "OauthTokenType" invalid


instance FromJSON OauthTokenResponse where
  parseJSON (Object o) =
    OauthTokenResponse
    <$> o .: "token_type"
    <*> o .: "expires_in"
    <*> o .: "scope"
    <*> o .: "access_token"
    <*> o .:? "refresh_token"
    
  parseJSON invalid =
    typeMismatch "OauthTokenResponse" invalid


authorizationTokenRequest :: OauthTokenRequest -> T.Text -> B.ByteString
authorizationTokenRequest req code =
  let
    initParams =
      serializeOauthTokenRequest req
    params =
      [ ("code", T.encodeUtf8 code)
      , ("grant_type", "authorization_code")
      ] ++ initParams
  in
    renderSimpleQuery False params


refreshTokenRequest :: OauthTokenRequest -> T.Text -> B.ByteString
refreshTokenRequest req tok =
  let
    params =
      [ ("refresh_token", T.encodeUtf8 tok)
      , ("grant_type", "refresh_token")
      ] ++ serializeOauthTokenRequest req
  in
    renderSimpleQuery False params


serializeOauthTokenRequest :: OauthTokenRequest -> [(B.ByteString, B.ByteString)]
serializeOauthTokenRequest req =
  [ ("client_id",  T.encodeUtf8 $ clientId req)
  , ("redirect_uri",  T.encodeUtf8 $ redirectUri req)
  , ("client_secret",  T.encodeUtf8 $ clientSecret req)
  ]


oauthTokenRequest :: (MonadThrow m, MonadIO m, MonadReader env m, HasHttpManager env, MonadBaseControl IO m) => OauthTokenRequest -> T.Text -> m OauthTokenResponse
oauthTokenRequest req code = do
  initReq <- parseUrl "https://login.live.com/oauth20_token.srf"
  let
    httpReq =
      initReq
      { method = "POST"
      , requestBody = RequestBodyBS (authorizationTokenRequest req code)
      , requestHeaders = [(hContentType, "application/x-www-form-urlencoded")]
      }
  withResponse httpReq $ \resp ->
    responseBody resp $$ sinkParser jsonParser


oauthRefreshTokenRequest :: (MonadThrow m, MonadIO m, MonadReader env m, HasHttpManager env, MonadBaseControl IO m) => OauthTokenRequest -> T.Text -> m OauthTokenResponse
oauthRefreshTokenRequest req tok = do
  initReq <- parseUrl "https://login.live.com/oauth20_token.srf"
  let
    httpReq =
      initReq
      { method = "POST"
      , requestBody = RequestBodyBS (refreshTokenRequest req tok)
      , requestHeaders = [(hContentType, "application/x-www-form-urlencoded")]
      }
  withResponse httpReq $ \resp ->
    responseBody resp $$ sinkParser jsonParser


me :: (MonadThrow m, MonadIO m, MonadReader env m, HasHttpManager env, MonadBaseControl IO m) => T.Text -> m UserInfo
me token = do
  initReq <- parseUrl "https://apis.live.net/v5.0/me"
  let
    httpReq =
      initReq { requestHeaders = [(hAuthorization, T.encodeUtf8 ("Bearer " `T.append` token))] }
  withResponse httpReq $ \resp ->
    responseBody resp $$ sinkParser jsonParser


data UserInfo =
  UserInfo
  { __id :: T.Text
  , _displayName :: T.Text
  } deriving (Eq, Show)


instance FromJSON UserInfo where
  parseJSON (Object o) =
    UserInfo
    <$> o .: "id"
    <*> o .: "name"

  parseJSON invalid =
    typeMismatch "UserInfo" invalid


getOnedriveClientSecret :: IO T.Text
getOnedriveClientSecret =
  maybe "-4tKnVPaAyIEAgYrBp8R6jTYY0zClN6c" T.pack <$> lookupEnv "ONEDRIVE_CLIENT_SECRET"
