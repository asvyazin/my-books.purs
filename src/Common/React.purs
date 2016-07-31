module Common.React where


import Control.Coroutine (transformCoTransformL, transformCoTransformR, transform)
import Control.Monad.Maybe.Trans (runMaybeT)
import Control.Monad.Rec.Class (forever)
import Control.Monad.Trans (lift)
import Control.Error.Util (hoistMaybe)
import Data.Lens (view)
import Data.List (List(..), (!!))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Prelude
import React as R
import Thermite as T


mapProps :: forall props1 props2 eff state action. (props1 -> props2) -> T.Spec eff state props2 action -> T.Spec eff state props1 action
mapProps convert spec2 =
  T.simpleSpec performAction render
  where
    performAction a p1 s =
      view T._performAction spec2 a (convert p1) s

    render dispatch p1 s children =
      view T._render spec2 dispatch (convert p1) s children


mapPropsWithState :: forall props1 props2 eff state action. (props1 -> state -> props2) -> T.Spec eff state props2 action -> T.Spec eff state props1 action
mapPropsWithState convert spec2 =
  T.simpleSpec performAction render
  where
    performAction a p1 s =
      view T._performAction spec2 a (convert p1 s) s

    render dispatch p1 s children =
      view T._render spec2 dispatch (convert p1 s) s children


withProps :: forall eff props state action. (props -> T.Spec eff state props action) -> T.Spec eff state props action
withProps f =
  T.simpleSpec performAction render
  where
    performAction a p s =
      view T._performAction (f p) a p s
    render d p s c =
      view T._render (f p) d p s c


maybeState :: forall eff props state action. T.Spec eff state props action -> T.Spec eff (Maybe state) props action
maybeState origSpec =
  T.simpleSpec performAction render
  where
    performAction a p s =
      case s of
        Nothing ->
          pure unit
        Just s' ->
          forever (transform (_ >>= id))
          `transformCoTransformL` view T._performAction origSpec a p s'
          `transformCoTransformR` forever (transform map)

    render d p s c =
      case s of
        Nothing ->
          []
        Just s' ->
          view T._render origSpec d p s' c


maybeProps :: forall eff props state action. T.Spec eff state props action -> T.Spec eff state (Maybe props) action
maybeProps origSpec =
  T.simpleSpec performAction render
  where
    performAction a p s =
      case p of
        Nothing ->
          pure unit
        Just p' ->
          view T._performAction origSpec a p' s
    render d p s c =
      case p of
        Nothing ->
          []
        Just p' ->
          view T._render origSpec d p' s c


wrapperSpec :: forall eff state props action. (props -> Array R.ReactElement -> R.ReactElement) -> T.Spec eff state props action
wrapperSpec elementCreator =
  T.simpleSpec T.defaultPerformAction render
  where
    render _ p _ c =
      [ elementCreator p c ]

      
foreachProps :: forall eff state props action. T.Spec eff state props action -> T.Spec eff state (List props) (Tuple Int action)
foreachProps origSpec =
  T.simpleSpec performAction render
  where
    performAction (Tuple idx action) propsArr state =
      void $ runMaybeT $ do
        childProps <- hoistMaybe $ propsArr !! idx
        lift $ view T._performAction origSpec action childProps state

    render dispatch propsArr state _ =
      foldWithIndex (\ idx props els -> els <> view T._render origSpec (dispatch <<< Tuple idx) props state []) propsArr []

foldWithIndex :: forall a r. (Int -> a -> r -> r) -> List a -> r -> r
foldWithIndex f = go 0
  where
    go _ Nil         r = r
    go i (Cons x xs) r = go (i + 1) xs (f i x r)
