module Components.AjaxLoader where


import React (ReactElement, createElement) as R
import React.DOM (img) as R
import React.DOM.Props as RP
import Thermite as T


spec :: forall eff state props action. T.Spec eff state props action
spec =
  T.simpleSpec T.defaultPerformAction render
  where
    render _ _ _ _ =
      [ R.img
        [ RP.className "center-block text-center"
        , RP.src "/images/ajax-loader.gif"
        ]
        []
      ]


ajaxLoader :: R.ReactElement
ajaxLoader =
  R.createElement (T.createClass spec {}) {} []
