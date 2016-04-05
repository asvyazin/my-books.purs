{-# LANGUAGE OverloadedStrings #-}
module Main (main) where


import Control.Error.Util (hoistMaybe, maybeT)
import Control.Lens ((^.))
import Control.Monad.Catch (catch, throwM)
import Control.Monad.Trans.Class (lift)
import Data.Aeson (toJSON)
import Data.Maybe (fromJust)
import qualified Data.Text as T (Text, concat)
import qualified Data.Text.Encoding as T (decodeUtf8)
import qualified Data.Text.Lazy as TL
import qualified  Network.HTTP.Client.Conduit as C
import Network.HTTP.Conduit (HttpException(StatusCodeException))
import Network.HTTP.Types.Status (unauthorized401)
import Network.Wai (queryString)
import qualified Network.Wai.Middleware.Static as Wai
import Onedrive (OauthTokenRequest(..), oauthTokenRequest, me, OauthTokenResponse(..))
import ReactRender (render)
import System.Environment (lookupEnv)
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
                  defaultCookieSettings)


ie10comment :: H.Html -> H.Html
ie10comment htmlContent = H.textComment $ T.concat ["[if lt IE 10]>", content, "<![endif]"]
  where content = renderHtml htmlContent


withMaster :: T.Text -> H.Html -> H.Html
withMaster mainScript childrenMarkup = H.docTypeHtml $ do
  H.head $ do
    H.meta H.! HA.charset "UTF-8"
    H.title "My Books"
    H.link H.! HA.rel "stylesheet" H.! HA.type_ "text/css" H.! HA.href "/bower_components/bootstrap-theme-bootswatch-flatly/css/bootstrap.min.css"
    H.link H.! HA.rel "stylesheet" H.! HA.type_ "text/css" H.! HA.href "/node_modules/react-treeview/react-treeview.css"
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
  withMaster "/public/js/index.bundle.js" $ H.div H.! HA.class_ "application" $ rendered


loginPage :: H.Html -> H.Html
loginPage rendered =
  withMaster "/public/js/login.bundle.js" $ H.div H.! HA.class_ "application" $ rendered


renderHtml :: H.Html -> T.Text
renderHtml =
  TL.toStrict . H.renderHtml


onedriveClientId :: T.Text
onedriveClientId = "000000004816D42C"


onedriveClientSecret :: T.Text
onedriveClientSecret = "-4tKnVPaAyIEAgYrBp8R6jTYY0zClN6c"


getPort :: IO Int
getPort = do
  maybePortStr <- lookupEnv "PORT"
  case maybePortStr of
    Just portStr ->
      return $ read portStr
    Nothing ->
      return 8000


main :: IO ()
main = do
  port <- getPort
  runSpock port $ spockT id $ do
    middleware Wai.static
  
    get "/" $ maybeT (redirect "/login") return $ do
      onedriveTokenCookie <- hoistMaybeM $ cookie "onedriveToken"
      meUser <- hoistMaybeM $ lift $ catchUnauthorizedException $ C.withManager $ me onedriveTokenCookie
      rendered <- lift $ render "public/js/index.server.bundle.js" [toJSON $ meUser ^. displayName, toJSON onedriveTokenCookie]
      lift $ html $ renderHtml $ indexPage rendered

    get "login" $ do
      rendered <- render "public/js/login.server.bundle.js" []
      html $ renderHtml $ loginPage rendered
  
    get "onedrive-redirect" $ do
      qs <- queryString <$> request
      let c = pa "code" qs
      let req =
            OauthTokenRequest
            { clientId = onedriveClientId
            , redirectUri = "http://localhost:8000/onedrive-redirect"
            , clientSecret = onedriveClientSecret
            , code = T.decodeUtf8 $ fromJust c
            }
      tokenResp <- lift $ C.withManager $ oauthTokenRequest req
      setCookie "onedriveToken" (accessToken tokenResp) defaultCookieSettings
      redirect "/"

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
