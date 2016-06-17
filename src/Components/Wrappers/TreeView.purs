module Components.Wrappers.TreeView where


import React as R
import Thermite as T


type Props =
  { collapsed :: Boolean
  , nodeLabel :: R.ReactElement
  , onClick :: T.EventHandler
  }


foreign import treeviewFFI :: R.ReactClass Props


treeview :: Props -> Array R.ReactElement -> R.ReactElement
treeview =
  R.createElement treeviewFFI
