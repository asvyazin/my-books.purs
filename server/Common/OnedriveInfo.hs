{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
module Common.OnedriveInfo where


import Control.Lens (makeLenses, (^.))
import Data.Aeson (FromJSON(parseJSON), Value(Object), (.:), (.:?), ToJSON(toJSON), object, (.=))
import Data.Maybe (catMaybes)
import Data.Text (Text)


data OnedriveInfo =
  OnedriveInfo
  { __id :: Text
  , __rev :: Maybe Text
  , _token :: Text
  , _refreshToken :: Maybe Text
  } deriving (Show)


makeLenses ''OnedriveInfo


instance FromJSON OnedriveInfo where
  parseJSON (Object v) =
    OnedriveInfo <$> v .: "_id" <*> v .:? "_rev" <*> v .: "token" <*> v .:? "refresh_token"
  parseJSON _ =
    error "Invalid OnedriveInfo JSON"


instance ToJSON OnedriveInfo where
  toJSON o =
    let
      state =
        [ Just ("_id" .= (o ^. _id))
        , Just ("token" .= (o ^. token))
        , Just ("refresh_token" .= (o ^. refreshToken))
        , formatRev <$> (o ^. _rev)
        ]
      formatRev str =
        "_rev" .= str
    in
      object $ catMaybes state


onedriveInfoId :: Text
onedriveInfoId =
  "onedriveInfo"


defaultOnedriveInfo :: OnedriveInfo
defaultOnedriveInfo =
  OnedriveInfo onedriveInfoId Nothing "" Nothing
