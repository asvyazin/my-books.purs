module Components.OneDriveFileTree where


import Control.Monad
import Control.Monad.Aff
import Control.Monad.Eff.Exception
import Data.Maybe
import Network.HTTP.Affjax
import Prelude
import qualified React as R
import qualified React.DOM as R
import qualified React.DOM.Props as RP
import qualified Thermite as T

import Components.AjaxLoader
import qualified Components.Wrappers.Alert as Alert
import qualified Components.Wrappers.Glyphicon as Glyphicon
import qualified Components.Wrappers.TreeView as TreeView
import Common.OneDriveApi


type Props =
  { name :: String
  , onedriveToken :: String
  , itemId :: Maybe String
  , key :: String
  }


type State =
  { collapsed :: Boolean
  , loaded :: Boolean
  , errorText :: Maybe String
  , children :: Array Props
  }


data Action
  = ToggleCollapsed


render :: T.Render State Props Action
render dispatch props state _ =
  let
    glyphicon =
      Glyphicon.glyphicon' $ if state.collapsed then "folder-close" else "folder-open"

    children =
      if state.collapsed
      then []
      else
        if state.loaded
        then
          map fileTree2 state.children
        else
          case state.errorText of
            Nothing ->
              [ ajaxLoader ]
            Just err ->
              [ Alert.alert { bsStyle: "danger" } [ R.text err ] ]

    fileTree2 props2 =
      R.createElement reactClass2 props2 []

    reactClass2 =
      T.createClass spec2 defaultState

    spec2 =
      T.simpleSpec performAction render
  in
   [ TreeView.treeview
     { collapsed: state.collapsed
     , nodeLabel:
       R.span
       [ RP.onClick $ const $ dispatch ToggleCollapsed ]
       [ glyphicon, R.text $ " " ++ props.name ]
     }
     children
   ]


performAction :: forall eff. T.PerformAction (ajax :: AJAX, err :: EXCEPTION | eff) State Props Action
performAction ToggleCollapsed props state update =
  if (not state.collapsed)
  then
    update $ state { collapsed = true }
  else do
    update $ state { collapsed = false }
    when (not state.loaded) $
      runAff onError onSuccess $ getChildrenByItemId props.onedriveToken props.itemId
  where
    onSuccess childrenData = do
      let
        children =
          map (\ (OneDriveItem d) -> { onedriveToken: props.onedriveToken, name: d.name, itemId: Just d.id, key: d.id }) childrenData
      update $ state { collapsed = false, loaded = true, children = children }

    onError error =
      throwException error


spec :: forall eff. T.Spec (ajax :: AJAX, err :: EXCEPTION | eff) State Props Action
spec =
  T.simpleSpec performAction render


defaultState :: State
defaultState =
  { collapsed: true
  , loaded: false
  , errorText: Nothing
  , children: []
  }


reactClass :: R.ReactClass Props
reactClass =
  T.createClass spec defaultState


fileTree :: Props -> R.ReactElement
fileTree props =
  R.createElement reactClass props []
