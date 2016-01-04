module Components.OneDriveFileTree where


import Control.Monad
import Control.Monad.Aff
import Control.Monad.Eff.Class
import Data.Array
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
  , selected :: Boolean
  }


data Action
  = ToggleCollapsed
  | ToggleSelected


defaultState :: State
defaultState =
  { collapsed: true
  , loaded: false
  , errorText: Nothing
  , children: []
  , selected: false
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
      launchAff performActionAff
      where
        performActionAff =
          if (not state.collapsed)
          then
            liftEff $ update $ state { collapsed = true }
          else do
            liftEff $ update $ state { collapsed = false }
            when (not state.loaded) $ do
              childrenData <- getChildrenByItemId props.onedriveToken props.itemId
              let
                children =
                  map (\ (OneDriveItem d) -> { onedriveToken: props.onedriveToken, name: d.name, itemId: Just d.id, key: d.id }) childrenData
              liftEff $ update $ state { collapsed = false, loaded = true, children = children }

    performAction ToggleSelected _ state update =
      update $ state { selected = not state.selected }

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

        selectedClass =
          if state.selected
          then (Just $ RP.className "bg-primary")
          else Nothing

        itemProps =
          catMaybes [ Just $ RP.onClick $ const $ dispatch ToggleSelected, selectedClass ]

        itemLabel =
          R.span itemProps [ glyphicon, R.text $ " " ++ props.name ]
      in
       [ TreeView.treeview
         { collapsed: state.collapsed
         , nodeLabel: itemLabel
         , onClick: dispatch ToggleCollapsed
         }
         children
       ]
