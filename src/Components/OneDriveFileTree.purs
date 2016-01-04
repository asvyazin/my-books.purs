module Components.OneDriveFileTree where


import Control.Monad
import Control.Monad.Aff
import Control.Monad.Eff.Exception
import Data.Maybe
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


defaultState :: State
defaultState =
  { collapsed: true
  , loaded: false
  , errorText: Nothing
  , children: []
  }


fileTree :: Props -> R.ReactElement
fileTree props =
  R.createElement reactClass props []  
  where
    reactClass =
      T.createClass spec defaultState

    spec =
      T.simpleSpec performAction render

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
              map fileTree state.children
            else
              case state.errorText of
                Nothing ->
                  [ ajaxLoader ]
                Just err ->
                  [ Alert.alert { bsStyle: "danger" } [ R.text err ] ]
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
