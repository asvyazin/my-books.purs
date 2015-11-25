{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.IO.Class
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Network.Wai
import qualified Network.Wai.Middleware.Static as Wai
import qualified Text.Blaze.Html.Renderer.Text as H
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA
import Web.Spock


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


indexPage :: H.Html
indexPage =
  withMaster "/public/js/main.js" $ H.div H.! HA.class_ "application" $ ""


loginPage :: H.Html
loginPage =
  withMaster "/public/js/login.js" $ H.div H.! HA.class_ "application" $ ""


renderHtml :: H.Html -> T.Text
renderHtml =
  TL.toStrict . H.renderHtml


main :: IO ()
main = runSpock 8000 $ spockT id $ do
  middleware Wai.static
  
  get "/" $ do
    onedriveTokenCookie <- cookie "onedriveToken"
    case onedriveTokenCookie of
      Just _ ->
        html $ renderHtml indexPage
      _ ->
        redirect "/login"
        
  get "login" $ html $ renderHtml loginPage
  
  get "onedrive-redirect" $ do
    qs <- queryString <$> request
    let code = fromJust $ param "code" qs
    liftIO $ print code
    redirect "/login"

  where
    param _ [] = Nothing
    param par ((p, v) : qs)
      | p == par = Just v
      | otherwise = param par qs
