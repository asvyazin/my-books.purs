module Components.BooksDirectory where


import Data.Maybe
import qualified React as R
import qualified React.DOM as R
import qualified React.DOM.Props as RP
import qualified Thermite as T


type State =
  { directory :: Maybe String
  }


render :: forall props. T.Render State props (Array R.ReactElement)
render dispatch _ state _ =
  [ maybe renderChooseButton renderChoosedDirectory state.directory
  ]
  where
    renderChooseButton =
      renderMiddle
      [ R.button
        [ RP.className "btn btn-default btn-lg" ]
        [ R.text "Choose books directory..." ]
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


spec :: forall eff props. T.Spec eff State props (Array R.ReactElement)
spec =
  T.simpleSpec T.defaultPerformAction render