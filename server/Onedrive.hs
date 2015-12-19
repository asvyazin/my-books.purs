{-# LANGUAGE OverloadedStrings #-}
module Onedrive where

import Control.Lens
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Data.Aeson
import Data.Aeson.Lens
import Data.Aeson.Types
import qualified Data.Attoparsec.ByteString as A
import qualified Data.ByteString as B
import Data.Conduit
import Data.Conduit.Attoparsec
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Network.HTTP.Client.Conduit
import Network.HTTP.Types.Header
import Network.HTTP.Types.URI
import UserInfo


data OauthTokenRequest =
  OauthTokenRequest
  { clientId :: T.Text
  , redirectUri :: T.Text
  , clientSecret :: T.Text
  , code :: T.Text
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


serializeRequest :: OauthTokenRequest -> B.ByteString
serializeRequest req =
    let
      qqsParams =
        [ ("client_id", Just $ T.encodeUtf8 $ clientId req)
        , ("redirect_uri", Just $ T.encodeUtf8 $ redirectUri req)
        , ("client_secret", Just $ T.encodeUtf8 $ clientSecret req)
        , ("code", Just $ T.encodeUtf8 $ code req)
        , ("grant_type", Just "authorization_code")
        ]
    in
      renderQuery False qqsParams


oauthTokenResponseParser :: A.Parser OauthTokenResponse
oauthTokenResponseParser = do
  v <- json'
  case fromJSON v of
    Success r ->
      return r
    Error e ->
      fail e


oauthTokenRequest :: (MonadThrow m, MonadIO m, MonadReader env m, HasHttpManager env) => OauthTokenRequest -> m OauthTokenResponse
oauthTokenRequest req = do
  initReq <- parseUrl "https://login.live.com/oauth20_token.srf"
  let
    httpReq =
      initReq
      { method = "POST"
      , requestBody = RequestBodyBS (serializeRequest req)
      , requestHeaders = [(hContentType, "application/x-www-form-urlencoded")]
      }
  httpResp <- responseOpen httpReq
  responseBody httpResp $$ sinkParser oauthTokenResponseParser


me :: (MonadThrow m, MonadIO m, MonadReader env m, HasHttpManager env) => T.Text -> m UserInfo
me token = do
  initReq <- parseUrl "https://apis.live.net/v5.0/me"
  let
    qsParams =
      [ ("access_token", Just $ T.encodeUtf8 token ) ]
    httpReq =
      initReq { queryString = renderQuery False qsParams }
  httpResp <- responseOpen httpReq
  responseBody httpResp $$ sinkParser userInfoParser


mePictureLocation :: (MonadThrow m, MonadIO m, MonadReader env m, HasHttpManager env) => T.Text -> m T.Text
mePictureLocation token = do
  initReq <- parseUrl "https://apis.live.net/v5.0/me/picture"
  let
    qsParams =
      [ ("access_token", Just $ T.encodeUtf8 token ) ]
    httpReq =
      initReq { queryString = renderQuery False qsParams }
  httpResp <- responseOpen httpReq
  v <- responseBody httpResp $$ sinkParser json'
  return (v ^. key "location" . _String)
