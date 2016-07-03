module Components.OneDriveFileTree where


import Prelude
import Components.AjaxLoader.AjaxLoader as AjaxLoader
import Components.Wrappers.Alert as Alert
import Components.Wrappers.Glyphicon as Glyphicon
import Components.Wrappers.TreeView as TreeView
import React.DOM.Props as RP
import Thermite as T
import Common.OneDriveApi (OneDriveItem(OneDriveItem), getChildrenByItemId)
import Common.React (mapPropsWithState)
import Control.Coroutine (transform, transformCoTransformL, transformCoTransformR, cotransform)
import Control.Error.Util (hoistMaybe)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Maybe.Trans (lift, runMaybeT)
import Data.Foldable (foldl, fold)
import Data.Lens (PrismP, LensP, view, over, prism', _2, set, lens, (^.))
import Data.List (fromFoldable, (!!), modifyAt, findIndex, List, zip, zipWith, filter, null)
import Data.Maybe (Maybe(Nothing, Just), fromMaybe, isJust)
import Data.Monoid (mempty, (<>))
import Data.Tuple (Tuple(Tuple), fst)
import Network.HTTP.Affjax (AJAX)
import React (ReactElement, createElement) as R
import React.DOM (text, span) as R


type Props =
  { name :: String
  , onedriveToken :: String
  , itemId :: Maybe String
  , key :: String
  }


newtype State =
  State
  { collapsed :: Boolean
  , loaded :: Boolean
  , errorText :: Maybe String
  , children :: List (Tuple Props State)
  }


children :: LensP State (List (Tuple Props State))
children =
  lens getChildren setChildren
  where
    getChildren (State s) =
      s.children
    setChildren (State s) c =
      State s { children = c }


errorText :: LensP State (Maybe String)
errorText =
  lens getErrorText setErrorText
  where
    getErrorText (State s) =
      s.errorText
    setErrorText (State s) e =
      State s { errorText = e }


mapZip :: forall a b. LensP a b -> LensP (List a) (List b)
mapZip l =
  lens (map $ view l) (zipWith $ flip $ set l)


childrenState :: LensP State (List State)
childrenState =
  children <<< mapZip _2


data Action
  = ToggleCollapsed
  | SelectDirectory
  | ChildAction (Tuple (Maybe String) Action)


childAction :: PrismP Action (Tuple (Maybe String) Action)
childAction =
  prism' ChildAction fromChildAction
  where
    fromChildAction (ChildAction x) = Just x
    fromChildAction _ = Nothing


defaultState :: State
defaultState =
  State
  { collapsed: true
  , loaded: false
  , errorText: Nothing
  , children: fromFoldable []
  }


wrapTreeView :: forall eff. T.Spec eff State Props Action -> T.Spec eff State Props Action
wrapTreeView =
  over T._render wrapTreeViewRender
  where
    wrapTreeViewRender :: T.Render State Props Action -> (Action -> T.EventHandler) -> Props -> State -> Array R.ReactElement -> Array R.ReactElement
    wrapTreeViewRender render dispatch p (State s) c =
      let
        glyphicon =
          Glyphicon.glyphicon' $ if s.collapsed then "folder-close" else "folder-open"

        itemProps =
          [ RP.onClick $ const $ dispatch $ SelectDirectory ]

        itemLabel =
          R.span itemProps [ glyphicon, R.text $ " " <> p.name ]
      in
       [ TreeView.treeview
         { collapsed: s.collapsed
         , nodeLabel: itemLabel
         , onClick: dispatch ToggleCollapsed
         }
         (render dispatch p (State s) c)
       ]


spec :: forall eff. T.Spec (ajax :: AJAX, err :: EXCEPTION | eff) State Props Action
spec =
  fold
  [ T.simpleSpec performAction T.defaultRender
  , wrapTreeView $ T.withState (\ (State st) ->
                                 case st.errorText of
                                   Just error ->
                                     T.focusState errorText Alert.spec
                                   Nothing ->
                                     if not st.loaded
                                     then
                                       AjaxLoader.spec
                                     else
                                       if null st.children
                                       then
                                         mempty
                                       else
                                         mapPropsWithState (\ _ (State s) -> map fst s.children) $ T.focus childrenState childAction $ childrenSpec spec
                               )
  ]
  where
    performAction ToggleCollapsed props (State state) =
      if (not state.collapsed)
      then
        void $ cotransform $ \(State s) -> State s { collapsed = true }
      else do
        void $ cotransform $ \(State s) -> State s { collapsed = false }
        when (not state.loaded) $ do
          childrenData <- lift $ getChildrenByItemId props.onedriveToken props.itemId
          let
            newChildren =
              map child $ filter isDirectory childrenData
          void $ cotransform $ \(State s) -> State s { collapsed = false, loaded = true, children = newChildren }
            where
              child :: OneDriveItem -> Tuple Props State
              child (OneDriveItem item) =
                Tuple
                { onedriveToken: props.onedriveToken
                , name: item.name
                , itemId: Just item.id
                , key: item.id
                }
                defaultState
              
    performAction _ _ _ =
      pure unit

    isDirectory (OneDriveItem item) =
      isJust item.folder


childrenSpec :: forall eff. T.Spec eff State Props Action -> T.Spec eff (List State) (List Props) (Tuple (Maybe String) Action)
childrenSpec origSpec =
  T.simpleSpec performAction render
  where
    performAction (Tuple itemId action) propsArr stateArr =
      void $ runMaybeT $ do
        idx <- hoistMaybe $ findIndex (\x -> x.itemId == itemId) propsArr
        childProps <- hoistMaybe $ propsArr !! idx
        childState <- hoistMaybe $ stateArr !! idx
        lift $ transform (_ >>= (_ !! idx))
          `transformCoTransformL` view T._performAction origSpec action childProps childState
          `transformCoTransformR` transform (modifying idx)
      where
        modifying :: Int -> (State -> State) -> List State -> List State
        modifying i f sts' = fromMaybe sts' (modifyAt i f sts')

    render dispatch propsArr stateArr _ =
      foldl (\ els (Tuple props state) -> els <> view T._render origSpec (dispatch <<< Tuple props.itemId) props state []) [] $ zip propsArr stateArr


fileTree :: Props -> R.ReactElement
fileTree props =
  R.createElement (T.createClass spec defaultState) props []


unwrapChildAction :: Action -> Tuple (Maybe String) Action
unwrapChildAction action =
  unwrapRec action Nothing
  where
    unwrapRec (ChildAction (Tuple childItemId action)) _ =
      unwrapRec action childItemId
    unwrapRec action itemId =
      Tuple itemId action
