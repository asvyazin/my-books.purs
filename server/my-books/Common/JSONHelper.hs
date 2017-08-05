module Common.JSONHelper where


import Data.Aeson (FromJSON, fromJSON, json', Result(Success, Error))
import Data.Attoparsec.ByteString (Parser)


jsonParser :: FromJSON a => Parser a
jsonParser = do
  v <- json'
  case fromJSON v of
    Success r ->
      return r
    Error e ->
      fail e
