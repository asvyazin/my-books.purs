{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module BookIndexer.BookMetadataReader where


import Codec.Epub (getPkgPathXmlFromBS, getMetadata)
import Codec.Epub.Data.Metadata (Metadata(..), Creator(..), Title(..))
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Error.Class (MonadError)
import qualified Data.ByteString.Lazy as BL (toStrict)
import Data.Text (Text, takeEnd, intercalate, pack)
import Onedrive.Items (content)
import Onedrive.Session (Session)


data BookMetadata =
  BookMetadata
  { author :: Text
  , title :: Text
  }


loadMetadata :: (MonadThrow m, MonadIO m, MonadError String m) => Session -> Text -> Text -> m (Maybe BookMetadata)
loadMetadata session filename itemId = do
  if isEpub filename
    then do
    c <- content session itemId
    (_, xmlString) <- getPkgPathXmlFromBS $ BL.toStrict c
    metadata <- getMetadata xmlString
    return $ Just $ BookMetadata (getAuthor metadata) (getTitle metadata)
    else
    return Nothing
  where
    isEpub filename =
      let
        ext = takeEnd 5 filename
      in
        ext == ".epub"


getAuthor :: Metadata -> Text
getAuthor meta =
  intercalate ", " $ map (pack . creatorText) $ metaCreators meta


getTitle :: Metadata -> Text
getTitle meta =
  intercalate "; " $ map (pack . titleText) $ metaTitles meta
