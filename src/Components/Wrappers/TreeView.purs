module Components.Wrappers.TreeView where


import qualified React as R


type Props =
  { collapsed :: Boolean
  , nodeLabel :: R.ReactElement
  }


foreign import treeviewFFI :: R.ReactClass Props


treeview :: Props -> Array R.ReactElement -> R.ReactElement
treeview =
  R.createElement treeviewFFI
