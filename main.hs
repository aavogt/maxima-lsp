import Control.Concurrent
import Control.Exception
import Control.Lens
import Control.Lens.Regex.Text
import Control.Monad
import Data.Aeson
import Data.Aeson.QQ
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Char
import Data.Foldable
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import GHC.Generics (Generic)
import Pun (json)
import System.IO
import System.Posix.Resource
import Text.Regex.Applicative hiding (match)
import Text.Regex.PCRE.Light (compile)

import LoadHover

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

limit :: Resource -> Integer -> IO ()
limit resource amt = setResourceLimit resource (ResourceLimits (ResourceLimit amt) (ResourceLimit amt))

type Documents = Map Text Text

main :: IO ()
main = do
  limit ResourceTotalMemory (1024 * 1024 * 1024 * 1024 * 1024)
  limit ResourceCPUTime 10
  db <- openHoverDB
  documents <- newMVar mempty
  forever do
    m <- message
    mr <- fmap (addContentLength . encode) <$> lsp db documents m
    for_ mr \r -> do
      L8.putStr r
      hFlush stdout
    threadDelay (100 * 1000) -- 0.1s


lsp :: HoverDB -> MVar Documents -> Value -> IO (Maybe Value)
lsp db documents [json| { method params id } |] = case method :: Text of
  "initialize" ->
    pure
      ( resp
          [aesonQQ|
            { capabilities :
              { renameProvider : { prepareProvider : true, workDoneProgress : false }
              , hoverProvider : true
              , textDocumentSync :
                  { openClose : true
                  , change : 1
                  , save : { includeText : false }
                  }
              }
            }
          |]
      )
  "textDocument/prepareRename" -> respIo (prepareRename documents params)
  "textDocument/rename" -> respIo (rename documents params)
  "textDocument/hover" -> respIo (findHoverRequest db documents params)
  _ -> pure Nothing
  where
    resp (result :: Value) = Just [aesonQQ| { jsonrpc: "2.0", id : #{id :: Int}, result : #{result} } |]
    respIo ioResult = do
      r <- ioResult `catch` \SomeException{} -> pure Null
      pure (resp r)
lsp findHover documents [json| { method params } |] = do
  handleNotification documents method params
  pure Nothing
lsp _ _ _ = return Nothing

findHoverRequest :: HoverDB -> MVar Documents -> Value -> IO Value
findHoverRequest db documents [json| { _textDocument{uri} _position{line character} } |] = do
  Just content <- getDocumentContent documents uri
  hoverText <- traverse (hover db) $ identifierUnderCursor content line character
  pure $ maybe Null hoverResult hoverText
  where
    hoverResult txt =
      [aesonQQ|
        { contents:
            { kind: "markdown"
            , value: #{txt}
            }
        }
      |]
findHoverRequest _ _ _ = pure Null

rename :: MVar Documents -> Value -> IO Value
rename documents [json| { _textDocument{uri} _position{line character} newName } |] = do
  Just content <- getDocumentContent documents uri
  let pos = Position line character
  let Just ident = identifierUnderCursor content line character
  let regex = compile (T.encodeUtf8 ident) []
      newContent = content & regexing regex . match .~ newName
      editRange = wholeRange (Just content) -- newContent can be longer...
  pure $! workspaceEditValue (uriToFilePath uri) editRange newContent

prepareRename :: MVar Documents -> Value -> IO Value
prepareRename documents [json| { _textDocument{uri} _position{line character} } |] = do
  Just content <- getDocumentContent documents uri
  let Just r = toJSON . toPrrFrom line character <$> splitPos content line character
  return $! r

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

handleNotification :: MVar Documents -> Text -> Value -> IO ()
handleNotification documents method params = case method :: Text of
  "textDocument/didOpen" ->
    case params of
      [json| { textDocument{ uri text } } |] ->
        modifyMVar_ documents (pure . Map.insert uri text)
      _ -> pure ()
  "textDocument/didChange" ->
    case params of
      [json| { _textDocument{ uri } _contentChanges[ { text } ] } |] ->
        modifyMVar_ documents (pure . Map.insert uri text)
      _ -> pure ()
  "textDocument/didClose" ->
    case params of
      [json| { textDocument{ uri } } |] ->
        modifyMVar_ documents (pure . Map.delete uri)
      _ -> pure ()
  _ -> pure ()

getDocumentContent :: MVar Documents -> Text -> IO (Maybe Text)
getDocumentContent documents uri = do
  docMap <- readMVar documents
  case Map.lookup uri docMap of
    Just content -> pure (Just content)
    Nothing -> readFileMaybe (uriToFilePath uri)

identifierUnderCursor :: Text -> Int -> Int -> Maybe Text
identifierUnderCursor content line character = do
  (left, right) <- splitPos content line character
  guardValidIdent left right
  Just (left <> right)

guardValidIdent :: Text -> Text -> Maybe ()
guardValidIdent left right =
  when (T.null left) $ guard $ has (_head . filtered \c -> isAlpha c || c == '_') right

splitPos :: Text -> Int -> Int -> Maybe (Text, Text)
splitPos content line character = T.lines content ^? ix line <&> \ lineText -> lineText
  & T.splitAt character
  & _1 . reversed %~ alphaNumThenAlpha -- allowed to be empty
  & _2 %~ T.takeWhile \c -> isAlphaNum c || c == '_'

alphaNumThenAlpha :: Text -> Text
alphaNumThenAlpha =
  (maybe "" fst .) . findLongestPrefixWithUncons T.uncons $ do
    xs <- many (psym isAlphaNum <|> sym '_')
    x <- psym isAlpha
    pure (T.pack xs `T.snoc` x)

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
