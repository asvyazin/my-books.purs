{-# LANGUAGE OverloadedStrings #-}
module Main(main) where


import Control.Lens ((^.), set)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Maybe (runMaybeT, MaybeT(MaybeT))
import CouchAuthProxy.UserToken (UserToken(..))
import CouchDB.Requests (putObject, getObject)
import CouchDB.Types.Auth (Auth(..))
import CouchDB.Types.Views (defaultViewQueryParameters, key, limit, rows, ViewResult)
import qualified CouchDB.Types.Views as V (id_)
import CouchDB.Views (getView)
import Data.Aeson (Value)
import Data.ByteString.Char8 (unpack, pack, ByteString)
import Data.CaseInsensitive (CI(original))
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T (unpack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Network.HTTP.Simple ( defaultRequest
                           , setRequestHeaders
                           , setRequestSecure
                           , setRequestHost
                           , setRequestPort
                           , setRequestMethod
                           , setRequestPath
                           , setRequestQueryString
                           , setRequestBodyLBS
                           , httpLBS
                           , getResponseStatus
                           , getResponseHeaders
                           , getResponseBody )
import Network.HTTP.Types.Header (HeaderName)
import Network.Wai (requestHeaders, requestMethod, rawPathInfo, queryString, lazyRequestBody)
import Onedrive.Session (newSessionWithToken)
import Onedrive.Types.UserInfo (UserInfo, id_)
import Onedrive.User (me)
import System.Environment (lookupEnv)
import Web.Spock.Action (request, setStatus, setHeader, lazyBytes)
import Web.Spock.Core (hookAny, runSpock, spockT, StdMethod(GET, POST, PUT, DELETE, OPTIONS))


main :: IO ()
main = do
  port <- getPort
  couchdbServer <- getCouchdbServer
  runSpock port $ spockT id $ do
    hookAny GET $ processRequest couchdbServer
    hookAny POST $ processRequest couchdbServer
    hookAny PUT $ processRequest couchdbServer
    hookAny DELETE $ processRequest couchdbServer
    hookAny OPTIONS $ processRequest couchdbServer
    where
      processRequest couchdbServer _ = do
        req <- request
        let
          headers = requestHeaders req
          maybeHeader = find ((== onedriveTokenHeaderName) . fst) headers
          maybeOnedriveToken = snd <$> maybeHeader
          method = requestMethod req
        newHeaders <- updateHeaders headers maybeOnedriveToken method
        body <- liftIO $ lazyRequestBody req
        let
          newRequest =
            setRequestBodyLBS body $
            setRequestQueryString (queryString req) $
            setRequestPath (rawPathInfo req) $
            setRequestMethod method $
            setRequestSecure False $
            setRequestHost couchdbServer $
            setRequestPort 5984 $
            setRequestHeaders newHeaders defaultRequest
        resp <- httpLBS newRequest
        setStatus $ getResponseStatus resp
        mapM_ (uncurry setHeader) $ map convertHeader $ getResponseHeaders resp
        lazyBytes $ getResponseBody resp
      convertHeader (k, v) =
        (decodeUtf8 (original k), decodeUtf8 v)
      updateHeaders headers onedriveToken _ =
        case onedriveToken of
          Nothing ->
            return headers
          Just token -> do
            liftIO $ putStrLn $ "onedriveToken found: " ++ unpack token
            userId <- liftIO $ getUserIdByToken token
            liftIO $ putStrLn $ "userId: " ++ T.unpack userId
            let
              newHeaders =
                (couchdbRolesHeaderName, "users") :
                (couchdbUserNameHeaderName, encodeUtf8 userId) :
                headers
            return newHeaders


getPort :: IO Int
getPort =
  maybe 8001 read <$> lookupEnv "PROXY_PORT"


getCouchdbServer :: IO ByteString
getCouchdbServer =
  (pack . fromMaybe "localhost") <$> lookupEnv "COUCHDB_SERVER"


onedriveTokenHeaderName :: HeaderName
onedriveTokenHeaderName = "X-Onedrive-Token"


couchdbRolesHeaderName :: HeaderName
couchdbRolesHeaderName = "X-Auth-CouchDB-Roles"


couchdbUserNameHeaderName :: HeaderName
couchdbUserNameHeaderName = "X-Auth-CouchDB-UserName"


getUserIdByToken :: ByteString -> IO Text
getUserIdByToken tokenBS = do
  let
    token = decodeUtf8 tokenBS
  maybeUserId <- tryGetUserIdFromCouchDB token
  case maybeUserId of
    Nothing -> do
      userInfo <- getUserByToken tokenBS
      let
        userId = userInfo ^. id_
      putUserIdToCouchDB token userId
      return userId
    Just userId ->
      return userId


tryGetUserIdFromCouchDB :: Text -> IO (Maybe Text)
tryGetUserIdFromCouchDB token = do
  auth <- getAuth
  proxyServer <- proxyServerName
  let
    params =
      set key (Just token) $
      set limit (Just 1)
      defaultViewQueryParameters
    loadViewResult :: IO (ViewResult Text Value)
    loadViewResult = getView proxyServer proxyDatabaseName auth "users" "by-token" params
  res <- loadViewResult
  case res ^. rows of
    [] ->
      return Nothing
    [r] ->
      return $ Just $ r ^. V.id_
    _ ->
      error "Invalid users/by-token response"


putUserIdToCouchDB :: Text -> Text -> IO ()
putUserIdToCouchDB token userId = do
  auth <- getAuth
  proxyServer <- proxyServerName
  oldObj <- getObject proxyServer proxyDatabaseName auth userId
  let
    rev =
      oldObj >>= userTokenRev
    obj =
      UserToken userId rev token
  putObject proxyServer proxyDatabaseName auth userId obj


getAuth :: IO Auth
getAuth =
  maybe NoAuth auth <$> tryGetAdminAuth
  where
    auth (username, password) =
      BasicAuth username password


tryGetAdminAuth :: IO (Maybe (ByteString, ByteString))
tryGetAdminAuth = runMaybeT $ do
  username <- MaybeT $ lookupEnv "COUCHDB_ADMIN_USERNAME"
  password <- MaybeT $ lookupEnv "COUCHDB_ADMIN_PASSWORD"
  return (pack username, pack password)


proxyDatabaseName :: Text
proxyDatabaseName =
  "my-books-proxy"


proxyServerPort :: Text
proxyServerPort = "5984"


proxyServerName :: IO Text
proxyServerName = do
  serverHost <- getCouchdbServer
  pure $ "http://" <> decodeUtf8 serverHost <> ":" <> proxyServerPort


getUserByToken :: ByteString -> IO UserInfo
getUserByToken tokenBS = do
  session <- newSessionWithToken $ decodeUtf8 tokenBS
  me session
