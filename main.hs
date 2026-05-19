{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -ddump-splices #-}

module Main where

import Control.Exception (IOException, SomeException (..), catch, try, PatternMatchFail (..))
import Control.Lens
import Control.Lens.Regex.Text
import Data.Aeson (FromJSON, Result (..), Value (..), decodeStrict, encode, fromJSON)
import Data.Aeson.QQ
import qualified Data.ByteString.Lazy as BL
import Data.Char
import Data.Functor ((<&>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import GHC.Generics (Generic)
import Pun (json)
import Text.Regex.Applicative hiding (match, match)
import Text.Regex.PCRE.Light (compile)
import System.IO
import Text.Regex.Applicative hiding (match)
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Int
import Data.Aeson.Decoding (decode)
import Data.Foldable (for_, traverse_)
import Control.Monad
import Data.Aeson.Types (ToJSON (toJSON))

contentLength :: Read a => RE Char a
contentLength = do
  string "Content-Length: "
  n <- some (psym isDigit)
  pure (read n)

addContentLength :: L8.ByteString -> L8.ByteString
addContentLength txt = "Content-Length: " <> L8.pack (show (L8.length txt)) <> "\r\n\r\n" <> txt

message :: IO Value
message = do
  Just (n, _) <- C8.getLine <&> findLongestPrefixWithUncons C8.uncons contentLength
  C8.getLine
  maybe message return . decode =<< L8.hGet stdin n
 `catch` \SomeException{} -> message

-- Content-Length: nnn\r\n\r\n<nnn bytes>
main :: IO ()
main = forever $ try @PatternMatchFail do
    m <- message
    mr <- fmap (addContentLength . encode) <$> lsp m
    traverse_ L8.putStr mr
    hFlush stdout

lsp :: Value -> IO (Maybe Value)
lsp [json| { method params id } |] = case method :: Text of
  "initialize" -> pure (resp [aesonQQ| { capabilities : { renameProvider : { prepareProvider : true, workDoneProgress : false } }, textDocumentSync : 1 } |])
  "textDocument/prepareRename" -> respIo (prepareRename params)
  "textDocument/rename" -> respIo (rename params)
  _ -> pure Nothing
  where
    resp (result :: Value) = Just [aesonQQ| { jsonrpc: "2.0", id : #{id :: Int}, result : #{result} } |]
    respIo ioResult = ioResult <&> resp
lsp _ = return Nothing

rename :: Value -> IO Value
rename [json| { _textDocument{uri} _position{line character} newName } |]= do
  let fp = uriToFilePath uri
      pos = Position line character
  Just content  <- readFileMaybe fp
  let Just (left, right) = identifierUnderCursor (Just content) pos
  let ident = left <> right
      regex = compile (T.encodeUtf8 ident) []
      newContent = content & regexing regex . match .~ newName
      editRange = wholeRange (Just content) -- newContent can be longer...
  pure $! workspaceEditValue fp editRange newContent
 `catch` \PatternMatchFail {} -> pure Null

prepareRename :: Value -> IO Value
prepareRename [json| { _textDocument{uri} _position{line character} } |] = do
  Just content <- readFileMaybe $ uriToFilePath uri
  let pos = Position line character
  Just r <- return $ toJSON . toPrrFrom line character <$> identifierUnderCursor (Just content) pos
  return $! r
 `catch` \PatternMatchFail {} -> pure Null

data Position = Position
  { line :: Int,
    character :: Int
  }
  deriving (Eq, Show, Generic)

data Range = Range
  { start :: Position,
    end :: Position
  }
  deriving (Eq, Show, Generic)

instance ToJSON Range
instance ToJSON Position

readFileMaybe :: FilePath -> IO (Maybe Text)
readFileMaybe fp = do
  result <- try (T.readFile fp) :: IO (Either IOException Text)
  pure (either (const Nothing) Just result)

identifierUnderCursor :: Maybe Text -> Position -> Maybe (Text, Text)
identifierUnderCursor mp (Position n c) =
  mp ^? _Just . to T.lines . ix n
    <&> T.splitAt c
    <&> _1 . reversed %~ alphaNumThenAlpha -- allowed to be empty
    <&> _2 %~ T.takeWhile isAlphaNum

wholeRange :: Maybe Text -> Range
wholeRange mp = Range (Position 0 0) (Position endLine endChar)
  where
    endLine = maybe 0 (length . T.lines) mp
    endChar = maybe 0 T.length $ (mlast . T.lines) =<< mp

    mlast [] = Nothing
    mlast xs = Just (last xs)

toPrrFrom :: Int -> Int -> (Text, Text) -> Range
toPrrFrom line col (T.length -> nl, T.length -> nr) = rangeLine line (col - nl) (col + nr)

rangeLine :: Int -> Int -> Int -> Range
rangeLine line c1 c2 = Range (Position line (max 0 c1)) (Position line (max 0 c2))

alphaNumThenAlpha :: Text -> Text
alphaNumThenAlpha =
  (maybe "" fst .) . findLongestPrefixWithUncons T.uncons $ do
    xs <- many (psym isAlphaNum)
    x <- psym isAlpha
    pure (T.pack xs `T.snoc` x)

uriToFilePath :: Text -> FilePath
uriToFilePath uri =
  case T.stripPrefix "file://" uri of
    Just path -> T.unpack path
    Nothing -> T.unpack uri

filePathToUri :: FilePath -> String
filePathToUri fp = "file://" <> fp

workspaceEditValue :: FilePath -> Range -> Text -> Value
workspaceEditValue fp range newText =
  let uri = filePathToUri fp
   in [aesonQQ| { changes: { $uri: [ { range: #{range}, newText: #{newText} }  ] } } |]
