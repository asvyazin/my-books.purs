module Components.BooksDirectory where


import Control.Monad.Aff
import Control.Monad.Eff.Exception
import Data.Maybe
import Data.String
import Network.HTTP.Affjax
import Prelude
import qualified React as R
import qualified React.DOM as R
import qualified React.DOM.Props as RP
import qualified Thermite as T

import Common.Monad
import Common.Settings
import Common.OneDriveApi
import qualified Components.OneDriveFileTree as FileTree
import qualified Components.Wrappers.Button as Button
import qualified Components.Wrappers.Glyphicon as Glyphicon
import qualified Components.Wrappers.Modal as Modal
import qualified Libs.PouchDB as DB


type Props =
  { onedriveToken :: String
  , db :: Maybe DB.PouchDB
  }


type State =
  { directory :: Maybe DirectoryInfo
  , showModal :: Boolean
  }


type DirectoryInfo =
  { itemId :: Maybe String
  , path :: String
  }


data Action
  = HideModal
  | ShowModal
  | DirectorySelected (Maybe String)


getDirectoryInfo :: forall e. String -> Maybe String -> Aff (ajax :: AJAX | e) DirectoryInfo
getDirectoryInfo token itemId = do
  item <- getItem <$> getOneDriveItem token itemId
  let parentPath = fromMaybe "" (getPath <$> item.parentReference)
  return { itemId, path: parentPath ++ "/" ++ item.name }
  where
    getItem (OneDriveItem item) = item
    getPath (ItemReference reference) =
      let
        idx = indexOf ":" reference.path
      in
       maybe reference.path (\i -> drop (i + 1) reference.path) idx


spec :: forall eff refs. T.Spec (ajax :: AJAX, err :: EXCEPTION, refs :: R.ReactRefs refs, state :: R.ReactState R.ReadWrite, props :: R.ReactProps, pouchdb :: DB.POUCHDB | eff) State Props Action
spec =
  T.simpleSpec performAction render
  where
    performAction :: T.PerformAction (ajax :: AJAX, err :: EXCEPTION, refs :: R.ReactRefs refs, state :: R.ReactState R.ReadWrite, props :: R.ReactProps, pouchdb :: DB.POUCHDB | eff) State Props Action
    performAction HideModal _ state update =
      update $ state { showModal = false }

    performAction ShowModal _ state update =
      update $ state { showModal = true }

    performAction (DirectorySelected itemId) props state update =
      launchAff $ do
        directory <- getDirectoryInfo props.onedriveToken itemId
        maybe (return unit) (\db -> void $ updateSettings db (\(Settings s) -> Settings (s { booksDirectory = itemId }))) props.db
        (liftEff' $ update $ state { showModal = false, directory = Just directory }) >>= guardEither

    render :: T.Render State Props Action
    render dispatch props state _ =
      [ maybe renderChooseButton renderChoosedDirectory state.directory
      , renderChooseModal state.showModal
      ]
      where
        renderChooseButton =
          let
            buttonProps =
              { bsSize : "large"
              , onClick : dispatch ShowModal
              }
          in
           renderMiddle
           [ Button.button buttonProps
             [ R.div
               [ RP.className "col-md-2" ]
               [ Glyphicon.glyphicon' "book"
               , R.text " Choose books directory..."
               ]
             ]
           ]

        renderChoosedDirectory directory =
          renderMiddle
          [ R.span
            [ RP.className "default" ]
            [ R.text directory.path ]
          , Button.button
            { onClick: dispatch ShowModal
            , bsStyle: "link"
            }
            [ R.text "Choose another" ]
          ]

        renderMiddle elems =
          R.div
          [ RP.className "col-md-offset-5" ]
          elems

        renderChooseModal show =
          Modal.modal
          { show
          , onHide: dispatch HideModal
          }
          [ Modal.header
            { closeButton: true }
            [ R.text "Choose directory" ]
          , Modal.body {}
            [ FileTree.fileTree
              { name: "root"
              , onedriveToken: props.onedriveToken
              , itemId: Nothing
              , key: "root"
              , onSelect: dispatch <<< DirectorySelected
              }
            ]
          , Modal.footer {}
            [ Button.button
              { onClick: dispatch HideModal }
              [ R.text "Close" ]
            ]
          ]
