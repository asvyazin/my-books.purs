module Components.ChooseDirectoryModal where


import Common.Monad
import Common.React
import Common.Settings
import qualified Components.OneDriveFileTree as FileTree
import qualified Components.Wrappers.Button as Button
import qualified Components.Wrappers.Modal as Modal
import Control.Monad.Aff
import Control.Monad.Eff.Exception
import Data.Foldable
import Data.Lens
import Data.Maybe
import Data.Tuple
import qualified Libs.PouchDB as DB
import Network.HTTP.Affjax (AJAX())
import Prelude
import qualified React.DOM as R
import qualified Thermite as T


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


fileTreeState :: LensP State FileTree.State
fileTreeState =
  lens _.fileTreeState (_ { fileTreeState = _ })


data Action
  = ShowModal
  | HideModal
  | FileTreeAction FileTree.Action


fileTreeAction :: PrismP Action FileTree.Action
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
    performAction (FileTreeAction action) props state update =
      case FileTree.unwrapChildAction action of
        Tuple itemId FileTree.SelectDirectory ->
          launchAff $ do
            void $ updateSettings props.db (\(Settings s) -> Settings (s { booksDirectory = itemId }))
            (liftEff' $ update $ \x -> x { show = false }) >>= guardEither
        _ ->
          pure unit

    performAction ShowModal _ state update =
      update $ \x -> x { show = true }

    performAction HideModal _ state update =
      update $ \x -> x { show = false }
