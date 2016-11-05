{-# LANGUAGE OverloadedStrings #-}
module Main(main) where


import Control.Lens ((^.))
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.ByteString.Char8 (unpack, ByteString)
import Data.CaseInsensitive (CI(original))
import Data.List (find)
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
  runSpock port $ spockT id $ do
    hookAny GET processRequest
    hookAny POST processRequest
    hookAny PUT processRequest
    hookAny DELETE processRequest
    hookAny OPTIONS processRequest
    where
      processRequest _ = do
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
            setRequestHost "localhost" $
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
            userInfo <- liftIO $ getUserByToken token
            let userId = userInfo ^. id_
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


onedriveTokenHeaderName :: HeaderName
onedriveTokenHeaderName = "X-Onedrive-Token"


getUserByToken :: (MonadThrow m, MonadIO m) => ByteString -> m UserInfo
getUserByToken tokenBS = do
  session <- liftIO $ newSessionWithToken $ decodeUtf8 tokenBS
  me session 


couchdbRolesHeaderName :: HeaderName
couchdbRolesHeaderName = "X-Auth-CouchDB-Roles"


couchdbUserNameHeaderName :: HeaderName
couchdbUserNameHeaderName = "X-Auth-CouchDB-UserName"
