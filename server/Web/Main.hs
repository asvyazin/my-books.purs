{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Main (main) where


import Common.Database (userDatabaseName)
import Common.Onedrive (getOnedriveClientSecret)
import Common.OnedriveInfo (getOnedriveInfo, setOnedriveInfo, token, refreshToken)
import Common.ServerEnvironmentInfo (ServerEnvironmentInfo(..), getServerEnvironment)
import Control.Concurrent (forkIO)
import Control.Lens ((^.), set)
import Control.Monad (when, void)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Class (lift)
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import qualified Data.Text as T (Text, concat)
import qualified Data.Text.Encoding as T (decodeUtf8)
import qualified Data.Text.Lazy as TL
import Network.Wai (queryString)
import qualified Network.Wai.Middleware.Static as Wai
import Onedrive.Auth (requestToken)
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
                                        link,
                                        body,
                                        p,
                                        text,
                                        strong,
                                        a,
                                        script,
                                        div,
                                        toValue)
import qualified Text.Blaze.Html5.Attributes as HA
import Web.Spock (runSpock,
                  spockT,
                  get,
                  middleware,
                  redirect,
                  html,
                  request,
                  setCookie,
                  defaultCookieSettings,
                  json,
                  hookAny,
                  StdMethod(GET))


main :: IO ()
main = do
  port <- getPort
  onedriveClientSecret <- getOnedriveClientSecret
  serverEnvironment <- getServerEnvironment
  runSpock port $ spockT id $ do
    currentDirectory <- liftIO getCurrentDirectory
    middleware $ Wai.staticPolicy (Wai.addBase (currentDirectory </> "public"))
  
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
  withMaster "/app.bundle.js" $ H.div H.! HA.class_ "application" $ ""


withMaster :: T.Text -> H.Html -> H.Html
withMaster mainScript childrenMarkup = H.docTypeHtml $ do
  H.head $ do
    H.meta H.! HA.charset "UTF-8"
    H.title "My Books"
    H.link H.! HA.rel "stylesheet" H.! HA.type_ "text/css" H.! HA.href "/bootstrap/css/bootstrap.min.css"
    H.link H.! HA.rel "stylesheet" H.! HA.type_ "text/css" H.! HA.href "/react-treeview/css/react-treeview.css"
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
ie10comment htmlContent = H.textComment $ T.concat ["[if lt IE 10]>", content, "<![endif]"]
  where content = renderHtml htmlContent


getPort :: IO Int
getPort =
  maybe 8000 read <$> lookupEnv "PORT"


updateOnedriveInfoSync :: T.Text -> Resp.OauthTokenResponse -> IO ()
updateOnedriveInfoSync couchdbUrl resp = do
  let
    tok =  resp ^. Resp.accessToken
  user <- me tok
  updateOnedriveInfoIfNeeded couchdbUrl (user ^. id_) resp


updateOnedriveInfoIfNeeded :: (MonadThrow m, MonadIO m) => T.Text -> T.Text -> Resp.OauthTokenResponse -> m ()
updateOnedriveInfoIfNeeded couchdbUrl userId tokenResp = do
  let
    databaseId = userDatabaseName userId
    newToken = tokenResp ^. Resp.accessToken
    newRefreshToken = tokenResp ^. Resp.refreshToken
  currentInfo <- getOnedriveInfo couchdbUrl databaseId
  when ((currentInfo ^. token) /= newToken || (currentInfo ^. Common.OnedriveInfo.refreshToken) /= newRefreshToken) $ do
    let
      newInfo = set Common.OnedriveInfo.refreshToken newRefreshToken $ set token newToken currentInfo
    setOnedriveInfo couchdbUrl databaseId newInfo
