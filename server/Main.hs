{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Control.Monad.Catch
import Control.Monad.Trans.Class
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified  Network.HTTP.Client.Conduit as C
import Network.Wai
import qualified Network.Wai.Middleware.Static as Wai
import qualified Text.Blaze as H
import qualified Text.Blaze.Html.Renderer.Text as H
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA
import Web.Spock

import Onedrive
import ReactRender


instance (MonadThrow m) => MonadThrow (ActionCtxT ctx m) where
  throwM e = lift $ throwM e


ie10comment :: H.Html -> H.Html
ie10comment htmlContent = H.textComment $ T.concat ["[if lt IE 10]>", content, "<![endif]"]
  where content = renderHtml htmlContent


withMaster :: T.Text -> H.Html -> H.Html
withMaster mainScript children = H.docTypeHtml $ do
  H.head $ do
    H.meta H.! HA.charset "UTF-8"
    H.title "My Books"
    H.link H.! HA.rel "stylesheet" H.! HA.type_ "text/css" H.! HA.href "/bower_components/bootstrap/dist/css/bootstrap.min.css"
  H.body $ do
    ie10comment $ H.p H.! HA.class_ "browserupgrade" $ do
      H.text "You are using an "
      H.strong "outdated"
      H.text " browser. Please "
      H.a H.! HA.href "http://browserhappy.com/" $ "upgrade your browser"
      H.text " to improve your experience."
    children
    H.script H.! HA.src "/bower_components/react/react.min.js" $ ""
    H.script H.! HA.src "/bower_components/react/react-dom.min.js" $ ""
    H.script H.! HA.src (H.toValue mainScript) $ ""


indexPage :: H.Html -> H.Html
indexPage rendered =
  withMaster "/public/js/index.js" $ H.div H.! HA.class_ "application" $ rendered


loginPage :: H.Html
loginPage =
  withMaster "/public/js/login.js" $ H.div H.! HA.class_ "application" $ ""


renderHtml :: H.Html -> T.Text
renderHtml =
  TL.toStrict . H.renderHtml


onedriveClientId :: T.Text
onedriveClientId = "000000004816D42C"


onedriveClientSecret :: T.Text
onedriveClientSecret = "-4tKnVPaAyIEAgYrBp8R6jTYY0zClN6c"


main :: IO ()
main = runSpock 8000 $ spockT id $ do
  middleware Wai.static
  
  get "/" $ do
    onedriveTokenCookie <- cookie "onedriveToken"
    case onedriveTokenCookie of
      Just _ -> do
        rendered <- render "public/js/index-server.js" "Entries.Index.Server"
        html $ renderHtml $ indexPage rendered
      _ ->
        redirect "/login"
        
  get "login" $ html $ renderHtml loginPage
  
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
    tokenResp <- C.withManager $ oauthTokenRequest req
    setCookie "onedriveToken" (accessToken tokenResp) defaultCookieSettings
    redirect "/"

  where
    pa _ [] = Nothing
    pa par ((p, v) : qs)
      | p == par = v
      | otherwise = pa par qs
