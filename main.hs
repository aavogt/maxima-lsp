{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Exception (IOException, try)
import Control.Lens
import Control.Lens.Regex.Text
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, ToJSON)
import Data.Char
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp (run)
import Servant.API ((:<|>) (..))
import Servant.Server (Handler, Server, serve)
import Servant.Server.JsonRpc (JSONRPC, JsonRpc, JsonRpcErr (..), RawJsonRpc, invalidParamsCode)
import Text.Regex.Applicative hiding (match)
import Text.Regex.PCRE.Light (compile)

main :: IO ()
main = run 8080 $ serve (Proxy @(RawJsonRpc JSONRPC API)) server

-- JSON-RPC API

type PrepareRename = JsonRpc "prepareRename" PrepareRenameParams String (Maybe Range)

type Rename = JsonRpc "rename" RenameParams String (Maybe TextEdit)

type API = PrepareRename :<|> Rename

server = handlePrepareRename :<|> handleRename

-- JSON payloads

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

data TextEdit = TextEdit
  { range :: Range,
    newText :: Text
  }
  deriving (Eq, Show, Generic)

data PrepareRenameParams = PrepareRenameParams
  { filePath :: FilePath,
    position :: Position
  }
  deriving (Eq, Show, Generic)

data RenameParams = RenameParams
  { filePath :: FilePath,
    position :: Position,
    newName :: Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON Position

instance ToJSON Position

instance FromJSON Range

instance ToJSON Range

instance FromJSON TextEdit

instance ToJSON TextEdit

instance FromJSON PrepareRenameParams

instance ToJSON PrepareRenameParams

instance FromJSON RenameParams

instance ToJSON RenameParams

handlePrepareRename :: PrepareRenameParams -> Handler (Either (JsonRpcErr String) (Maybe Range))
handlePrepareRename (PrepareRenameParams fp pos) = do
  mp <- liftIO $ readFileMaybe fp
  case mp of
    Nothing -> pure $ Left (err "can't read file " fp)
    Just content -> do
      let mident = identifierUnderCursor (Just content) pos
      pure $ Right $ fmap (toPrrFrom pos) mident

handleRename :: RenameParams -> Handler (Either (JsonRpcErr String) (Maybe TextEdit))
handleRename (RenameParams fp pos newNameText) = do
  mp <- liftIO $ readFileMaybe fp
  case mp of
    Nothing -> pure $ Left (err "can't read file " fp)
    Just content ->
      case identifierUnderCursor (Just content) pos of
        Nothing -> pure $ Left (err "can't find identifier at " pos)
        Just (left, right) -> do
          let ident = left <> right
              regex = compile (T.encodeUtf8 ident) []
              newContent = content & regexing regex . match .~ newNameText
              edit = TextEdit (textRange (Just content)) newContent
          pure $ Right (Just edit)

readFileMaybe :: FilePath -> IO (Maybe Text)
readFileMaybe fp = do
  result <- try (T.readFile fp) :: IO (Either IOException Text)
  pure (either (const Nothing) Just result)

identifierUnderCursor :: Maybe Text -> Position -> Maybe (Text, Text)
identifierUnderCursor mp (Position n c) =
  mp ^? _Just . to T.lines . ix n
    <&> T.splitAt c
    >>= _1 . reversed %%~ alphaNumThenAlpha
    <&> _2 %~ T.takeWhile isAlphaNum

textRange :: Maybe Text -> Range
textRange mp = Range (Position 0 0) (Position endLine endChar)
  where
    endLine = maybe 0 (length . T.lines) mp
    endChar = maybe 0 T.length $ (mlast . T.lines) =<< mp

    mlast [] = Nothing
    mlast xs = Just (last xs)

toPrrFrom :: Position -> (Text, Text) -> Range
toPrrFrom pos (left, right) = toPrr (T.length left) (T.length right) pos

toPrr :: Int -> Int -> Position -> Range
toPrr left right pos = Range (shift (-left) pos) (shift right pos)

shift :: Int -> Position -> Position
shift n (Position line col) = Position line (max 0 (col + n))

err :: (Show a) => Text -> a -> JsonRpcErr String
err txt info = JsonRpcErr invalidParamsCode (T.unpack (txt <> T.pack (show info))) Nothing

alphaNumThenAlpha :: Text -> Maybe Text
alphaNumThenAlpha =
  fmap (fmap fst) . findLongestPrefixWithUncons T.uncons $ do
    xs <- many (psym isAlphaNum)
    x <- psym isAlpha
    pure (T.pack xs `T.snoc` x)
