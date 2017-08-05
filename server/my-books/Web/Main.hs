{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where


import Common.Database (userDatabaseName)
import Common.Onedrive (getOnedriveClientSecret)
import Common.OnedriveInfo (onedriveInfoId, defaultOnedriveInfo, token, refreshToken)
import Common.ServerEnvironmentInfo (ServerEnvironmentInfo(..), getServerEnvironment)
import Control.Concurrent (forkIO)
import Control.Lens ((^.), set)
import Control.Monad (when, void)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (runMaybeT, MaybeT(MaybeT))
import CouchDB.Requests (getObject, putObject)
import CouchDB.Types.Auth (Auth(NoAuth, BasicAuth))
import Data.ByteString.Char8 (ByteString, pack)
import Data.Maybe (fromJust, fromMaybe)
import Data.Monoid ((<>))
import qualified Data.Text as T (Text, concat, empty)
import qualified Data.Text.Encoding as T (decodeUtf8)
import qualified Data.Text.Lazy as TL
import Network.HTTP.Types.Status (unauthorized401)
import Network.Wai (queryString)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Static (staticPolicy, addBase)
import Onedrive.Auth (requestToken)
import Onedrive.Items (content)
import Onedrive.Session (newSessionWithToken)
import Onedrive.Types.OauthTokenRequest (OauthTokenRequest(OauthTokenRequest))
import qualified Onedrive.Types.OauthTokenResponse as Resp (OauthTokenResponse, refreshToken, accessToken)
import Onedrive.Types.UserInfo (id_)
import Onedrive.User (me)
import System.Directory (getCurrentDirectory)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import qualified Text.Blaze.Html.Renderer.Text as H (renderHtml)
import qualified Text.Blaze.Html5 as H (Html,
                                        textComment,
                                        docTypeHtml,
                                        head,
                                        meta,
                                        title,
                                        (!),
                                        body,
                                        p,
                                        text,
                                        strong,
                                        a,
                                        script,
                                        div,
                                        toValue)
import qualified Text.Blaze.Html5.Attributes as HA
import Web.Routing.Combinators (wildcard)
import Web.Spock.Core (runSpock,
                       spockT,
                       get,
                       middleware,
                       redirect,
                       html,
                       request,
                       setCookie,
                       cookie,
                       defaultCookieSettings,
                       json,
                       hookAny,
                       setHeader,
                       setStatus,
                       bytes,
                       var,
                       (<//>),
                       StdMethod(GET))
import Web.ViewEpub (loadEpubItem, itemBytes, contentType, firstPagePath)


main :: IO ()
main = do
  port <- getPort
  onedriveClientSecret <- getOnedriveClientSecret
  serverEnvironment <- getServerEnvironment
  runSpock port $ spockT id $ do
    middleware logStdoutDev
    currentDirectory <- liftIO getCurrentDirectory
    middleware $ staticPolicy (addBase (currentDirectory </> "public"))

    get "onedrive-redirect" $ do
      qs <- queryString <$> request
      let c = pa "code" qs
      let req =
            OauthTokenRequest (_onedriveClientId serverEnvironment) (_appBaseUrl serverEnvironment <> "/onedrive-redirect") onedriveClientSecret
      resp <- lift $ requestToken req $ T.decodeUtf8 $ fromJust c
      void $ liftIO $ forkIO $ updateOnedriveInfoSync (_couchdbServer serverEnvironment) resp
      setCookie "onedriveToken" (resp ^. Resp.accessToken) defaultCookieSettings
      redirect "/"

    get "server-environment" $
      json serverEnvironment

    get ("view-epub" <//> var <//> wildcard) $ \itemId path -> do
                                liftIO $ print itemId
                                liftIO $ print path
                                mbAccessToken <- cookie "onedriveToken"
                                case mbAccessToken of
                                  Nothing ->
                                    setStatus unauthorized401
                                  Just accessToken -> do
                                    session <- liftIO $ newSessionWithToken accessToken
                                    bs <- liftIO $ content session itemId
                                    if path == T.empty
                                      then do
                                      result <- liftIO $ runExceptT $ firstPagePath bs
                                      case result of
                                        Right indexPath ->
                                          redirect $ "/view-epub/" <> itemId <> "/" <> indexPath
                                        Left err ->
                                          fail $ "Error getting EPUB index item: " ++ err
                                      else do
                                      result <- liftIO $ runExceptT $ loadEpubItem bs path
                                      case result of
                                        Right res -> do
                                          setHeader "Content-Type" $ res ^. contentType
                                          bytes $ res ^. itemBytes
                                        Left err ->
                                          fail $ "Error getting EPUB item: " ++ err


    hookAny GET $ \_ ->
      html $ renderHtml appPage

  where
    pa _ [] = Nothing
    pa par ((p, v) : qs)
      | p == par = v
      | otherwise = pa par qs


renderHtml :: H.Html -> T.Text
renderHtml =
  TL.toStrict . H.renderHtml


appPage :: H.Html
appPage =
  withMaster "/app.bundle.js" $ H.div H.! HA.class_ "application container" $ ""


withMaster :: T.Text -> H.Html -> H.Html
withMaster mainScript childrenMarkup = H.docTypeHtml $ do
  H.head $ do
    H.meta H.! HA.charset "UTF-8"
    H.title "My Books"
  H.body $ do
    ie10comment $ H.p H.! HA.class_ "browserupgrade" $ do
      H.text "You are using an "
      H.strong "outdated"
      H.text " browser. Please "
      H.a H.! HA.href "http://browserhappy.com/" $ "upgrade your browser"
      H.text " to improve your experience."
    childrenMarkup
    H.script H.! HA.src (H.toValue mainScript) $ ""


ie10comment :: H.Html -> H.Html
ie10comment htmlContent = H.textComment $ T.concat ["[if lt IE 10]>", renderedContent, "<![endif]"]
  where renderedContent = renderHtml htmlContent


getPort :: IO Int
getPort =
  maybe 8000 read <$> lookupEnv "PORT"


updateOnedriveInfoSync :: T.Text -> Resp.OauthTokenResponse -> IO ()
updateOnedriveInfoSync couchdbUrl resp = do
  let
    tok =  resp ^. Resp.accessToken
  session <- newSessionWithToken tok
  user <- me session
  updateOnedriveInfoIfNeeded couchdbUrl (user ^. id_) resp


updateOnedriveInfoIfNeeded :: (MonadThrow m, MonadIO m) => T.Text -> T.Text -> Resp.OauthTokenResponse -> m ()
updateOnedriveInfoIfNeeded couchdbUrl userId tokenResp = do
  auth <- getAuth
  let
    databaseId = userDatabaseName userId
    newToken = tokenResp ^. Resp.accessToken
    newRefreshToken = tokenResp ^. Resp.refreshToken
  currentInfo <- fromMaybe defaultOnedriveInfo <$> getObject couchdbUrl databaseId auth onedriveInfoId
  when ((currentInfo ^. token) /= newToken || (currentInfo ^. Common.OnedriveInfo.refreshToken) /= newRefreshToken) $ do
    let
      newInfo = set Common.OnedriveInfo.refreshToken newRefreshToken $ set token newToken currentInfo
    putObject couchdbUrl databaseId auth onedriveInfoId newInfo


getAuth :: MonadIO m => m Auth
getAuth =
  maybe NoAuth auth <$> tryGetAdminAuth
  where
    auth (username, password) =
      BasicAuth username password


tryGetAdminAuth :: MonadIO m => m (Maybe (ByteString, ByteString))
tryGetAdminAuth = runMaybeT $ do
  username <- MaybeT $ liftIO $ lookupEnv "COUCHDB_ADMIN_USERNAME"
  password <- MaybeT $ liftIO $ lookupEnv "COUCHDB_ADMIN_PASSWORD"
  return (pack username, pack password)
