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
  , _name :: T.Text
  , _firstName :: Maybe T.Text
  , _lastName :: Maybe T.Text
  } deriving (Eq, Show)


_id :: Lens' UserInfo T.Text
_id =
  lens __id (\u x -> u { __id = x })


name :: Lens' UserInfo T.Text
name =
  lens _name (\u x -> u { _name = x })


firstName :: Lens' UserInfo (Maybe T.Text)
firstName =
  lens _firstName (\u x -> u { _firstName = x })


lastName :: Lens' UserInfo (Maybe T.Text)
lastName =
  lens _lastName (\u x -> u { _lastName = x })


instance FromJSON UserInfo where
  parseJSON (Object o) =
    UserInfo
    <$> o .: "id"
    <*> o .: "name"
    <*> o .:? "first_name"
    <*> o .:? "last_name"

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
