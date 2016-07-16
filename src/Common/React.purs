module Common.React where


import Control.Coroutine (transformCoTransformL, transformCoTransformR, transform)
import Control.Monad.Rec.Class (forever)
import Data.Lens (view)
import Data.Maybe (Maybe(..))
import Prelude
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
