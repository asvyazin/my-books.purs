module Entries.IndexProps where


import Control.Error.Util (note)
import Data.Argonaut.Core (toObject)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode.Combinators ((.?))
import Data.Int (fromString)
import Data.Lens (LensP, lens)
import Data.Maybe (Maybe(Nothing), maybe)
import Prelude


newtype Props =
  Props
  { params :: Params
  }


params :: LensP Props Params
params =
  lens get set
  where
    get (Props p) = p.params
    set (Props p) q = Props $ p { params = q }


instance decodeJsonProps :: DecodeJson Props where
  decodeJson json = do
    o <- note "Expected object" $ toObject json
    params' <- o .? "params"
    pure $ Props { params : params' }


newtype Params =
  Params
  { page :: Maybe Int
  }


page :: LensP Params (Maybe Int)
page =
  lens get set
  where
    get (Params p) = p.page
    set (Params p) q = Params $ p { page = q }


instance decodeJsonParams :: DecodeJson Params where
  decodeJson json = do
    o <- note "Expected object" $ toObject json
    page' <- maybe Nothing fromString <$> o .? "page"
    pure $ Params { page : page' }


defaultProps :: Props
defaultProps =
  Props { params : Params { page : Nothing } }
