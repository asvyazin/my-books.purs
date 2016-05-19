{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Common.Onedrive where


import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson.Types (FromJSON,
                         parseJSON,
                         Value(String, Object),
                         typeMismatch,
                         (.:),
                         (.:?))
import qualified Data.ByteString as B
import qualified Data.Text as T (Text, append, pack)
import qualified Data.Text.Encoding as T (encodeUtf8)
import Network.HTTP.Simple (parseRequest, httpJSON, getResponseBody, setRequestHeaders, setRequestMethod, setRequestBodyURLEncoded)
import Network.HTTP.Types.Header (hAuthorization)
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


serializeOauthTokenRequest :: OauthTokenRequest -> [(B.ByteString, B.ByteString)]
serializeOauthTokenRequest req =
  [ ("client_id",  T.encodeUtf8 $ clientId req)
  , ("redirect_uri",  T.encodeUtf8 $ redirectUri req)
  , ("client_secret",  T.encodeUtf8 $ clientSecret req)
  ]


oauthTokenRequest :: (MonadThrow m, MonadIO m) => OauthTokenRequest -> T.Text -> m OauthTokenResponse
oauthTokenRequest req code = do
  initReq <- parseRequest "https://login.live.com/oauth20_token.srf"
  let
    initParams =
      serializeOauthTokenRequest req
    params =
      [ ("code", T.encodeUtf8 code)
      , ("grant_type", "authorization_code")
      ] ++ initParams
    httpReq =
      setRequestMethod "POST" $ setRequestBodyURLEncoded params initReq
  getResponseBody <$> httpJSON httpReq


oauthRefreshTokenRequest :: (MonadThrow m, MonadIO m) => OauthTokenRequest -> T.Text -> m OauthTokenResponse
oauthRefreshTokenRequest req tok = do
  initReq <- parseRequest "https://login.live.com/oauth20_token.srf"
  let
    initParams =
      serializeOauthTokenRequest req
    params =
      [ ("refresh_token", T.encodeUtf8 tok)
      , ("grant_type", "refresh_token")
      ] ++ initParams
    httpReq =
      setRequestMethod "POST" $ setRequestBodyURLEncoded params initReq
  getResponseBody <$> httpJSON httpReq


me :: (MonadThrow m, MonadIO m) => T.Text -> m UserInfo
me token = do
  initReq <- parseRequest "https://apis.live.net/v5.0/me"
  let
    httpReq =
      setRequestHeaders [(hAuthorization, T.encodeUtf8 ("Bearer " `T.append` token))] initReq
  getResponseBody <$> httpJSON httpReq


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
