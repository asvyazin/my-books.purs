module Entries.App where


import Control.Monad.Eff (Eff)
import Data.Maybe.Unsafe (fromJust)
import Data.Nullable (toMaybe)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToParentNode)
import DOM.HTML.Window (document)
import DOM.Node.ParentNode (querySelector)
import Entries.App.Class (component)
import Prelude
import React (createFactory) as R
import ReactDOM (render) as R


main :: Eff (dom :: DOM) Unit
main = do
    node <- htmlDocumentToParentNode <$> (window >>= document)
    container <- (fromJust <<< toMaybe) <$> querySelector ".application" node
    void $ R.render (R.createFactory component {}) container
