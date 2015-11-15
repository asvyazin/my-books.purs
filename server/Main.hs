{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Network.Wai.Middleware.Static as Wai
import qualified Text.Blaze.Html.Renderer.Text as H
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA
import Web.Spock


ie10comment :: H.Html -> H.Html
ie10comment htmlContent = H.textComment $ T.concat ["[if lt IE 10]>", content, "<![endif]"]
  where content = renderHtml htmlContent


withMaster :: H.Html -> H.Html
withMaster children = H.docTypeHtml $ do
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
    H.script H.! HA.src "/bundle.js" $ ""


indexPage :: H.Html
indexPage =
  withMaster $ H.div H.! HA.class_ "application" $ ""


loginPage :: H.Html
loginPage =
  withMaster $ H.div H.! HA.class_ "application" $ ""


renderHtml :: H.Html -> T.Text
renderHtml = TL.toStrict . H.renderHtml


main :: IO ()
main = runSpock 8000 $ spockT id $ do
  middleware Wai.static
  get "/" $ html $ renderHtml indexPage
  get "login" $ html $ renderHtml loginPage
