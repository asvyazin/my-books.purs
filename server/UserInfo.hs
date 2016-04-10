{-# LANGUAGE OverloadedStrings #-}
module UserInfo where


import Control.Lens
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Attoparsec.ByteString as A
import qualified Data.Text as T


data UserInfo =
  UserInfo
  { __id :: T.Text
  , _displayName :: T.Text
  } deriving (Eq, Show)


_id :: Lens' UserInfo T.Text
_id =
  lens __id (\u x -> u { __id = x })


displayName :: Lens' UserInfo T.Text
displayName =
  lens _displayName (\u x -> u { _displayName = x })


instance FromJSON UserInfo where
  parseJSON (Object o) =
    UserInfo
    <$> o .: "id"
    <*> o .: "name"

  parseJSON invalid =
    typeMismatch "UserInfo" invalid


userInfoParser :: A.Parser UserInfo
userInfoParser = do
  v <- json'
  case fromJSON v of
    Success r ->
      return r
    Error e ->
      fail e
