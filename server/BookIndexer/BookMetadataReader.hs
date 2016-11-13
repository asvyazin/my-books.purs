{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module BookIndexer.BookMetadataReader where


import Codec.Epub (getPkgPathXmlFromBS, getMetadata, getPackage)
import Codec.Epub.Data.Metadata (Metadata(..), Creator(..), Title(..))
import Codec.Epub.Data.Package (Package(pkgVersion))
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
  , epubVersion :: Text
  }


loadMetadata :: (MonadThrow m, MonadIO m, MonadError String m) => Session -> Text -> Text -> m (Maybe BookMetadata)
loadMetadata session filename itemId = do
  if isEpub filename
    then do
    c <- content session itemId
    (_, xmlString) <- getPkgPathXmlFromBS $ BL.toStrict c
    package <- getPackage xmlString
    metadata <- getMetadata xmlString
    let
      epubVsn = pack $ pkgVersion package
    return $ Just $ BookMetadata (getAuthor metadata) (getTitle metadata) epubVsn
    else
    return Nothing
  where
    isEpub fn =
      let
        ext = takeEnd 5 fn
      in
        ext == ".epub"


getAuthor :: Metadata -> Text
getAuthor meta =
  intercalate ", " $ map (pack . creatorText) $ metaCreators meta


getTitle :: Metadata -> Text
getTitle meta =
  intercalate "; " $ map (pack . titleText) $ metaTitles meta
