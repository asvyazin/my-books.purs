module Common.HTTPHelper where


import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Network.HTTP.Types.URI (urlEncode)


textUrlEncode :: Bool -> Text -> Text
textUrlEncode flag =
  decodeUtf8 . urlEncode flag . encodeUtf8
