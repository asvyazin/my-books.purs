{-# LANGUAGE OverloadedStrings #-}
module Common.Database where


import Common.HTTPHelper (textUrlEncode)
import Data.Monoid ((<>))
import Data.Text (Text)


usersDatabaseName :: Text
usersDatabaseName =
  "my-books"


userDatabaseName :: Text -> Text
userDatabaseName userId =
  textUrlEncode False $ usersDatabaseName <> "/" <> userId


indexerDatabaseName :: Text
indexerDatabaseName =
  "my-books-indexer"


usersFilter :: Text
usersFilter =
  "users/all"
