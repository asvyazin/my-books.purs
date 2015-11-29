module Entries.Login where

import Control.Monad.Eff
import Data.Maybe.Unsafe
import Data.Nullable
import DOM
import DOM.HTML
import DOM.HTML.Document
import DOM.HTML.Types
import DOM.HTML.Window
import DOM.Node.ParentNode
import Prelude
import qualified React as R

import Entries.Login.Class

main :: Eff (dom :: DOM) Unit
main = do
  node <- htmlDocumentToParentNode <$> (window >>= document)
  container <- (fromJust <<< toMaybe) <$> querySelector ".application" node
  void $ R.render (R.createFactory component {}) container
