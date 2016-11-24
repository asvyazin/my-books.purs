{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
module Web.ViewEpub where


import Codec.Archive.Zip (toArchive, findEntryByPath, fromEntry)
import Codec.Epub (getPkgPathXmlFromBS, getManifest, getSpine)
import Codec.Epub.Data.Manifest (Manifest(..), ManifestItem(..))
import Codec.Epub.Data.Spine (Spine(..), SpineItemref(..))
import Control.Lens (makeLenses)
import Control.Monad.Error.Class (MonadError(throwError))
import Control.Monad.IO.Class (MonadIO)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import qualified Data.ByteString.Lazy as BL (ByteString)
import Data.List (find)
import Data.Maybe (listToMaybe)
import Data.Text (Text, unpack, pack)
import System.FilePath (takeDirectory, (</>))


data LoadResult
  = LoadResult
  { _itemBytes :: ByteString
  , _contentType :: Text
  }


makeLenses ''LoadResult


loadEpubItem :: (MonadIO m, MonadError String m) => BL.ByteString -> Text -> m LoadResult
loadEpubItem bytes path = do
  let
    strictBytes = toStrict bytes
  (pkgFilePath, xmlString) <- getPkgPathXmlFromBS strictBytes
  (Manifest items) <- getManifest xmlString
  let
    currentDir = takeDirectory pkgFilePath
    rawPathStr = unpack path
    mbResult = do
      item <- find (\i -> mfiHref i == rawPathStr) items
      let
        pathStr = currentDir </> rawPathStr
        archive = toArchive bytes
      entry <- findEntryByPath pathStr archive
      pure $ LoadResult (toStrict (fromEntry entry)) (pack (mfiMediaType item))
  maybe (throwError ("Not found EPUB item: " ++ rawPathStr)) return mbResult


firstPagePath :: (MonadIO m, MonadError String m) => BL.ByteString -> m Text
firstPagePath bytes = do
  let
    strictBytes = toStrict bytes
  (_, xmlString) <- getPkgPathXmlFromBS strictBytes
  (Manifest items) <- getManifest xmlString
  spine <- getSpine xmlString
  let
    mbResult = do
      idRef <- siIdRef <$> listToMaybe (spineItemrefs spine)
      item <- find (\i -> mfiId i == idRef) items
      pure $ mfiHref item
  maybe (throwError ("Not found EPUB index item")) (return . pack) mbResult
