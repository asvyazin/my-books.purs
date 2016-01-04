module Components.BooksDirectory where


import Data.Maybe
import Prelude
import qualified React.DOM as R
import qualified React.DOM.Props as RP
import qualified Thermite as T

import qualified Components.OneDriveFileTree as FileTree
import qualified Components.Wrappers.Button as Button
import qualified Components.Wrappers.Glyphicon as Glyphicon
import qualified Components.Wrappers.Modal as Modal


type Props =
  { onedriveToken :: String
  }


type State =
  { directory :: Maybe String
  , showModal :: Boolean
  }


data Action
  = HideModal
  | ShowModal


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
         [ Glyphicon.glyphicon' "book"
         , R.text " Choose books directory..."
         ]
       ]

    renderChoosedDirectory dirName =
      renderMiddle
      [ R.span
        [ RP.className "default" ]
        [ R.text dirName ]
      ]

    renderMiddle elems =
      R.div
      [ RP.className "col-md-offset-5 col-md-2" ]
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
          }
        ]
      , Modal.footer {}
        [ Button.button
          { onClick: dispatch HideModal }
          [ R.text "Close" ]
        ]
      ]


performAction :: forall eff. T.PerformAction eff State Props Action
performAction HideModal _ state update =
  update $ state { showModal = false }
performAction ShowModal _ state update =
  update $ state { showModal = true }


spec :: forall eff. T.Spec eff State Props Action
spec =
  T.simpleSpec performAction render
