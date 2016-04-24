{-# LANGUAGE OverloadedStrings #-}
module Main (main) where


import Control.Error.Util (hoistMaybe, maybeT)
import Control.Lens ((^.))
import Control.Monad.Catch (catch, throwM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Data.Aeson (toJSON)
import Data.Maybe (fromJust, maybe)
import Data.Monoid ((<>))
import qualified Data.Text as T (Text, concat, pack)
import qualified Data.Text.Encoding as T (decodeUtf8)
import qualified Data.Text.Lazy as TL
import qualified  Network.HTTP.Client.Conduit as C
import Network.HTTP.Conduit (HttpException(StatusCodeException))
import Network.HTTP.Types.Status (unauthorized401)
import Network.Wai (queryString)
import qualified Network.Wai.Middleware.Static as Wai
import Onedrive (OauthTokenRequest(..), oauthTokenRequest, me, OauthTokenResponse(..))
import Options.Applicative (Parser, switch, long, help, execParser, info, helper, fullDesc)
import ReactRender (render)
import ServerEnvironmentInfo (ServerEnvironmentInfo(..))
import System.Directory (getCurrentDirectory)
import System.Environment (lookupEnv, getExecutablePath)
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
import UserInfo (displayName)
import Web.Spock (runSpock,
                  spockT,
                  get,
                  cookie,
                  middleware,
                  redirect,
                  html,
                  request,
                  setCookie,
                  defaultCookieSettings,
                  json)


ie10comment :: H.Html -> H.Html
ie10comment htmlContent = H.textComment $ T.concat ["[if lt IE 10]>", content, "<![endif]"]
  where content = renderHtml htmlContent


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


indexPage :: H.Html -> H.Html
indexPage rendered =
  withMaster "/js/index.bundle.js" $ H.div H.! HA.class_ "application" $ rendered


loginPage :: H.Html -> H.Html
loginPage rendered =
  withMaster "/js/login.bundle.js" $ H.div H.! HA.class_ "application" $ rendered


renderHtml :: H.Html -> T.Text
renderHtml =
  TL.toStrict . H.renderHtml


main :: IO ()
main = do
  port <- getPort
  onedriveClientId <- getOnedriveClientId
  onedriveClientSecret <- getOnedriveClientSecret
  appBaseUrl <- getAppBaseUrl
  opts <- execParser $ info (helper <*> options) fullDesc
  runSpock port $ spockT id $ do
    currentDirectory <- liftIO $ getCurrentDirectory
    let policy =
          Wai.only [ ("bootstrap/css/bootstrap.min.css", currentDirectory </> "bower_components" </> "bootstrap-theme-bootswatch-flatly" </> "css" </> "bootstrap.min.css")
                   , ("bootstrap/fonts/glyphicons-halflings-regular.eot", currentDirectory </> "bower_components" </> "bootstrap" </> "dist" </> "fonts" </> "glyphicons-halflings-regular.eot")
                   , ("bootstrap/fonts/glyphicons-halflings-regular.svg", currentDirectory </> "bower_components" </> "bootstrap" </> "dist" </> "fonts" </> "glyphicons-halflings-regular.svg")
                   , ("bootstrap/fonts/glyphicons-halflings-regular.ttf", currentDirectory </> "bower_components" </> "bootstrap" </> "dist" </> "fonts" </> "glyphicons-halflings-regular.ttf")
                   , ("bootstrap/fonts/glyphicons-halflings-regular.woff", currentDirectory </> "bower_components" </> "bootstrap" </> "dist" </> "fonts" </> "glyphicons-halflings-regular.woff")
                   , ("bootstrap/fonts/glyphicons-halflings-regular.woff2", currentDirectory </> "bower_components" </> "bootstrap" </> "dist" </> "fonts" </> "glyphicons-halflings-regular.woff2")
                   , ("react-treeview/css/react-treeview.css", currentDirectory </> "node_modules" </> "react-treeview" </> "react-treeview.css")
                   , ("js/login.bundle.js", currentDirectory </> "public" </> "js" </> "login.bundle.js")
                   , ("js/index.bundle.js", currentDirectory </> "public" </> "js" </> "index.bundle.js")
                   , ("images/ajax-loader.gif", currentDirectory </> "images" </> "ajax-loader.gif")
                   ]
    middleware $ Wai.staticPolicy policy
  
    get "/" $ maybeT (redirect "/login") return $ do
      onedriveTokenCookie <- hoistMaybeM $ cookie "onedriveToken"
      rendered <-
        if serverSideRendering opts
        then do
          meUser <- hoistMaybeM $ lift $ catchUnauthorizedException $ C.withManager $ me onedriveTokenCookie
          lift $ render "public/js/index.server.bundle.js" [toJSON $ meUser ^. displayName, toJSON onedriveTokenCookie]
        else
          return ""
      lift $ html $ renderHtml $ indexPage rendered

    get "login" $ do
      rendered <-
        if serverSideRendering opts
        then
          render "public/js/login.server.bundle.js" [toJSON appBaseUrl, toJSON onedriveClientId]
        else
          return ""
      html $ renderHtml $ loginPage rendered
  
    get "onedrive-redirect" $ do
      qs <- queryString <$> request
      let c = pa "code" qs
      let req =
            OauthTokenRequest
            { clientId = onedriveClientId
            , redirectUri = appBaseUrl <> "/onedrive-redirect"
            , clientSecret = onedriveClientSecret
            , code = T.decodeUtf8 $ fromJust c
            }
      tokenResp <- lift $ C.withManager $ oauthTokenRequest req
      setCookie "onedriveToken" (accessToken tokenResp) defaultCookieSettings
      redirect "/"

    get "server-environment" $
      json $ ServerEnvironmentInfo appBaseUrl onedriveClientId

  where
    pa _ [] = Nothing
    pa par ((p, v) : qs)
      | p == par = v
      | otherwise = pa par qs

    hoistMaybeM r =
      lift r >>= hoistMaybe

    catchUnauthorizedException r =
      fmap Just r `catch` processUnauthorizedException

    processUnauthorizedException e@(StatusCodeException s _ _) =
      if s == unauthorized401
      then return Nothing
      else throwM e
    processUnauthorizedException e =
      throwM e


getPort :: IO Int
getPort =
  maybe 8000 read <$> lookupEnv "PORT"


getOnedriveClientId :: IO T.Text
getOnedriveClientId =
  maybe "000000004816D42C" T.pack <$> lookupEnv "ONEDRIVE_CLIENT_ID"


getOnedriveClientSecret :: IO T.Text
getOnedriveClientSecret =
  maybe "-4tKnVPaAyIEAgYrBp8R6jTYY0zClN6c" T.pack <$> lookupEnv "ONEDRIVE_CLIENT_SECRET"


getAppBaseUrl :: IO T.Text
getAppBaseUrl =
  maybe "http://localhost:8000" T.pack <$> lookupEnv "APP_BASE_URL"


data Options =
  Options
  { serverSideRendering :: Bool
  } deriving (Show)


options :: Parser Options
options =
  Options <$> switch (long "server-side-rendering" <> help "Enables server-side react rendering")
