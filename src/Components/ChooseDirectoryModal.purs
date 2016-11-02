module Components.ChooseDirectoryModal where


import Common.React (mapProps)
import Components.OneDriveFileTree as FileTree
import Components.Wrappers.Button as Button
import Components.Wrappers.Modal as Modal
import Control.Coroutine (cotransform)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Foldable (fold)
import Data.Lens (Prism', Lens', lens, prism', over)
import Data.Maybe (Maybe(Nothing, Just))
import Data.Tuple (Tuple(Tuple))
import Libs.PouchDB (POUCHDB, PouchDB) as DB
import Network.HTTP.Affjax (AJAX())
import Prelude
import React.DOM as R
import Thermite as T


type Props =
  { onedriveToken :: String
  , db :: DB.PouchDB
  }


type State =
  { show :: Boolean
  , fileTreeState :: FileTree.State
  }


defaultState :: State
defaultState =
  { show: false
  , fileTreeState: FileTree.defaultState
  }


fileTreeState :: Lens' State FileTree.State
fileTreeState =
  lens _.fileTreeState (_ { fileTreeState = _ })


data Action
  = ShowModal
  | HideModal
  | FileTreeAction FileTree.Action


fileTreeAction :: Prism' Action FileTree.Action
fileTreeAction =
  prism' FileTreeAction getFileTreeAction
  where
    getFileTreeAction (FileTreeAction x) = Just x
    getFileTreeAction _ = Nothing


wrapModal :: forall eff props. T.Spec eff State props Action -> T.Spec eff State props Action
wrapModal =
  over T._render wrapRender
  where
    wrapRender origRender dispatch p s c =
      [ Modal.modal
        { show: s.show
        , onHide: dispatch HideModal
        }
        [ Modal.header
          { closeButton: true }
          [ R.text "Choose directory" ]
        , Modal.body {}
          (origRender dispatch p s c)
        , Modal.footer {}
          [ Button.button
            { onClick: dispatch HideModal }
            [ R.text "Close" ]
          ]
        ]
      ]


spec :: forall eff. T.Spec (ajax :: AJAX, err :: EXCEPTION, pouchdb :: DB.POUCHDB | eff) State Props Action
spec =
  fold
  [ T.simpleSpec performAction T.defaultRender
  , wrapModal $ mapProps (\p ->
                           { name: "root"
                           , onedriveToken: p.onedriveToken
                           , itemId: Nothing
                           , key: "root"
                           }) $ T.focus fileTreeState fileTreeAction FileTree.spec
  ]
  where
    performAction (FileTreeAction action) props state =
      case FileTree.unwrapChildAction action of
        Tuple itemId FileTree.SelectDirectory ->
          void $ cotransform $ \x -> x { show = false }
        _ ->
          pure unit

    performAction ShowModal _ state =
      void $ cotransform $ \x -> x { show = true }

    performAction HideModal _ state =
      void $ cotransform $ \x -> x { show = false }
