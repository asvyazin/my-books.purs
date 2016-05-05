{-# LANGUAGE OverloadedStrings #-}
module Web.UserInfo where


import Control.Lens (Lens', lens)
import Data.Aeson (json', (.:))
import Data.Aeson.Types (FromJSON(parseJSON), Value(Object), fromJSON, typeMismatch, Result(Success, Error))
import qualified Data.Attoparsec.ByteString as A
import qualified Data.Text as T


data UserInfo =
  UserInfo
  { __id :: T.Text
  , _displayName :: T.Text
  } deriving (Eq, Show)


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


_id :: Lens' UserInfo T.Text
_id =
  lens __id (\u x -> u { __id = x })


displayName :: Lens' UserInfo T.Text
displayName =
  lens _displayName (\u x -> u { _displayName = x })
