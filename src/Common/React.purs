module Common.React where


import Data.Lens
import qualified Thermite as T


mapProps :: forall props1 props2 eff state action. (props1 -> props2) -> T.Spec eff state props2 action -> T.Spec eff state props1 action
mapProps convert spec2 =
  T.simpleSpec performAction render
  where
    performAction a p1 s update =
      view T._performAction spec2 a (convert p1) s update

    render dispatch p1 s children =
      view T._render spec2 dispatch (convert p1) s children


mapPropsWithState :: forall props1 props2 eff state action. (props1 -> state -> props2) -> T.Spec eff state props2 action -> T.Spec eff state props1 action
mapPropsWithState convert spec2 =
  T.simpleSpec performAction render
  where
    performAction a p1 s update =
      view T._performAction spec2 a (convert p1 s) s update

    render dispatch p1 s children =
      view T._render spec2 dispatch (convert p1 s) s children


withProps :: forall eff props state action. (props -> T.Spec eff state props action) -> T.Spec eff state props action
withProps f =
  T.simpleSpec performAction render
  where
    performAction a p s u =
      view T._performAction (f p) a p s u
    render d p s c =
      view T._render (f p) d p s c
