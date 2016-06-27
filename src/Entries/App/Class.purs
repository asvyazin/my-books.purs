module Entries.App.Class where


import Components.Wrappers.Router (router, route, browserHistory)
import Entries.Index as Index
import Entries.Login as Login
import React as R
import Thermite as T


spec :: forall eff state props action. T.Spec eff state props action
spec =
  T.simpleSpec T.defaultPerformAction render
  where
    render _ _ _ _ =
      [ router
        { history : browserHistory }
        [ route { path : "/"
                , component : Index.component
                } []
        , route { path : "/login"
                , component : Login.component
                } []
        ]
      ]


component :: forall props. R.ReactClass props
component =
  T.createClass spec {}
