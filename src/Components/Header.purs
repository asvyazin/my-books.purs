module Components.Header where


import Common.React
import Data.Foldable
import Data.Lens
import Data.Maybe
import Prelude
import qualified React as R
import qualified React.DOM as R
import qualified React.DOM.Props as RP
import qualified Thermite as T


type Props =
  { title :: String
  , userName :: Maybe String
  , error :: Maybe String
  }


spec :: forall eff state action. T.Spec eff state Props action
spec =
  navbar $ fold [ mapProps _.title navbarBrand
                , nav $ navItem loginLink
                , navRight $ navItem userNameOrError
                ]
  where
    userNameOrError =
      mapProps _.userName (maybeProps userNameSpec) <> mapProps _.error (maybeProps errorSpec)


navbar :: forall eff state props action. T.Spec eff state props action -> T.Spec eff state props action
navbar =
  over T._render (\render d p s c ->
                   [ R.nav
                     [ RP.className "navbar navbar-default"
                     , RP.role "navigation"
                     ]
                     (render d p s c)
                   ]
                 )


navbarBrand :: forall eff state action. T.Spec eff state String action
navbarBrand =
  T.simpleSpec T.defaultPerformAction render
  where
    render _ props _ _ =
      [ R.div
        [ RP.className "navbar-header navbar-brand" ]
        [ R.text props ]
      ]


nav :: forall eff state props action. T.Spec eff state props action -> T.Spec eff state props action
nav =
  over T._render (\render d p s c ->
                   [ R.ul
                     [ RP.className "nav navbar-nav" ]
                     (render d p s c)
                   ]
                 )


navRight :: forall eff state props action. T.Spec eff state props action -> T.Spec eff state props action
navRight =
  over T._render (\render d p s c ->
                   [ R.ul
                     [ RP.className "nav navbar-nav navbar-right" ]
                     (render d p s c)
                   ]
                 )


navItem :: forall eff state props action. T.Spec eff state props action -> T.Spec eff state props action
navItem =
  over T._render (\render d p s c -> [ R.li' (render d p s c) ])


loginLink :: forall eff state props action. T.Spec eff state props action
loginLink =
  T.simpleSpec T.defaultPerformAction render
  where
    render _ _ _ _ =
      [ R.a
        [ RP.href "/login" ]
        [ R.text "Get new access token" ]
      ]


userNameSpec :: forall eff state action. T.Spec eff state String action
userNameSpec =
  T.simpleSpec T.defaultPerformAction render
  where
    render _ props _ _ =
      [ R.div [ RP.className "navbar-text" ] [ R.text props ] ]


errorSpec :: forall eff state action. T.Spec eff state String action
errorSpec =
  T.simpleSpec T.defaultPerformAction render
  where
    render _ props _ _ =
      [ R.div [ RP.className "alert alert-danger" ] [ R.text props ] ]


header :: Props -> R.ReactElement
header props =
  R.createElement (T.createClass spec {}) props []
