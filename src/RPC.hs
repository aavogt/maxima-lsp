-- roughly https://hackage-content.haskell.org/package/lsp-2.8.0.0/docs/src/Language.LSP.Server.Control.html#prependHeader
-- doesn't skip Content-Type:
module RPC where

import Control.Concurrent
import Control.Exception
import Control.Lens
import Data.Aeson
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Char
import Data.Map.Strict (Map)
import System.IO
import Text.Regex.Applicative

contentLength :: (Read a) => RE Char a
contentLength = do
  string "Content-Length: "
  n <- some (psym isDigit)
  pure (read n)

addContentLength :: L8.ByteString -> L8.ByteString
addContentLength txt = "Content-Length: " <> L8.pack (show (L8.length txt)) <> "\r\n\r\n" <> txt

message :: IO Value
message =
  do
    Just (n, _) <- C8.getLine <&> findLongestPrefixWithUncons C8.uncons contentLength
    C8.getLine
    maybe message return . decode =<< L8.hGet stdin n
    `catch` \SomeException {} -> do
      threadDelay (100 * 1000)
      message

