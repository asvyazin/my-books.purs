module Components.AjaxLoader (ajaxLoader) where


import qualified React as R
import qualified React.DOM as R
import qualified React.DOM.Props as RP
import qualified Thermite as T


render :: forall state props action. T.Render state props action
render _ _ _ _ =
  [ R.img
    [ RP.className "center-block text-center"
    , RP.src "/images/ajax-loader.gif"
    ]
    []
  ]


spec :: forall eff state props action. T.Spec eff state props action
spec =
  T.simpleSpec T.defaultPerformAction render


ajaxLoader :: R.ReactElement
ajaxLoader =
  R.createElement (T.createClass spec {}) {} []
