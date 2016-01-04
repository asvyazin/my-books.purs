module Components.BooksDirectory where


import Data.Maybe
import Prelude
import qualified React.DOM as R
import qualified React.DOM.Props as RP
import qualified Thermite as T

import qualified Components.Wrappers.Button as Button
import qualified Components.Wrappers.Glyphicon as Glyphicon
import qualified Components.Wrappers.Modal as Modal


type State =
  { directory :: Maybe String
  , showModal :: Boolean
  }


data Action
  = HideModal
  | ShowModal


render :: forall props. T.Render State props Action
render dispatch _ state _ =
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
         , R.text "Choose books directory..." ]
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
      Modal.modal { show, onHide : dispatch HideModal } []


performAction :: forall eff props. T.PerformAction eff State props Action
performAction HideModal _ state update =
  update $ state { showModal = false }
performAction ShowModal _ state update =
  update $ state { showModal = true }


spec :: forall eff props. T.Spec eff State props Action
spec =
  T.simpleSpec performAction render
