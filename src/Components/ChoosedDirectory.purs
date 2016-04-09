module Components.ChoosedDirectory (Props, spec) where


import Components.ChooseDirectoryModal as ChooseDirectoryModal
import Components.Wrappers.Button as Button
import React.DOM (span, text) as R
import React.DOM.Props as RP
import Thermite as T


type Action =
  ChooseDirectoryModal.Action


type Props =
  { directoryPath :: String
  }


spec :: forall eff state. T.Spec eff state Props Action
spec =
  T.simpleSpec T.defaultPerformAction render
  where
    render dispatch props _ _ =
      [ R.span
        [ RP.className "default" ]
        [ R.text props.directoryPath ]
      , Button.button
        { onClick: dispatch ChooseDirectoryModal.ShowModal
        , bsStyle: "link"
        }
        [ R.text "Choose another" ]
      ]
