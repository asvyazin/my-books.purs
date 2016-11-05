{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module CouchAuthProxy.UserToken where


import Control.Lens (makeLensesWith, camelCaseFields)
import Data.Aeson (ToJSON(toJSON), object, (.=), FromJSON(parseJSON), (.:), (.:?), Value(Object))
import Data.Maybe (catMaybes)
import Data.Text (Text)


data UserToken
  = UserToken
  { userTokenId_ :: Text
  , userTokenRev :: Maybe Text
  , userTokenToken :: Text
  }


instance ToJSON UserToken where
  toJSON o =
    let
      kv = [ Just ("_id" .= userTokenId_ o)
           , Just ("token" .= userTokenToken o)
           , Just ("type" .= ("userToken" :: Text))
           , renderRev <$> userTokenRev o
           ]
    in
      object $ catMaybes kv
    where
      renderRev rev =
        "_rev" .= rev


instance FromJSON UserToken where
  parseJSON (Object o) =
    UserToken <$> (o .: "_id") <*> (o .:? "_rev") <*> (o .: "token")
  parseJSON _ =
    error "Invalid UserToken JSON"


makeLensesWith camelCaseFields ''UserToken
