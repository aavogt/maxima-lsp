import Control.Concurrent
import Control.Exception
import Control.Lens hiding ((.=))
import Control.Lens.Regex.Text
import Control.Monad
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.QQ
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Foldable
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import System.IO
import System.Posix.Resource
import Text.Regex.PCRE.Light (compile)

import Ident ( identifierUnderCursor, splitPos )
import LoadHover ( openHoverDB, HoverDB(hover, completions) )
import Pun (json)
import RPC ( addContentLength, message )
import Range ( toPrrFrom, wholeRange, rangeLine, Range )

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
              , completionProvider : { resolveProvider : true }
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
  "textDocument/completion" -> respIo (completionRequest db documents params)
  "completionItem/resolve" -> respIo (completionItemResolve db params)
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

completionRequest :: HoverDB -> MVar Documents -> Value -> IO Value
completionRequest db documents [json| { _textDocument{uri} _position{line character} } |] = do
  Just content <- getDocumentContent documents uri
  completionsList <- completions db
  items <- completionItems content line character completionsList
  pure [aesonQQ| { isIncomplete: false, items: #{items} } |]
completionRequest _ _ _ = pure Null

completionItems :: Text -> Int -> Int -> [(Text, Bool)] -> IO [Value]
completionItems content line character identifiers = do
  let replaceRange = maybe (rangeLine line character character) (toPrrFrom line character) (splitPos content line character)
  traverse (completionItemValue replaceRange) identifiers

completionItemValue :: Range -> (Text, Bool) -> IO Value
completionItemValue range (ident, isFunction) = do
  let insertText = if isFunction then ident <> "(" else ident
  pure
    [aesonQQ|
      { label: #{ident}
      , textEdit: { range: #{range}, newText: #{insertText} }
      , data: { identifier: #{ident} }
      }
    |]

completionItemResolve :: HoverDB -> Value -> IO Value
completionItemResolve db item@[json| { _data{identifier} } |] = do
  hoverText <- hover db identifier
  pure $ maybe item (addDocumentation item) hoverText
  where
    addDocumentation (Object obj) txt =
      Object (KM.insert "documentation" (object ["kind" .= ("markdown" :: Text), "value" .= txt]) obj)
    addDocumentation other _ = other
completionItemResolve _ item = pure item

rename :: MVar Documents -> Value -> IO Value
rename documents [json| { _textDocument{uri} _position{line character} newName } |] = do
  Just content <- getDocumentContent documents uri
  let Just ident = identifierUnderCursor content line character
  let regex = compile (T.encodeUtf8 ident) []
      newContent = content & regexing regex . match .~ newName
      editRange = wholeRange content -- newContent can be longer...
  pure $! workspaceEditValue (uriToFilePath uri) editRange newContent

prepareRename :: MVar Documents -> Value -> IO Value
prepareRename documents [json| { _textDocument{uri} _position{line character} } |] = do
  Just content <- getDocumentContent documents uri
  let Just r = toJSON . toPrrFrom line character <$> splitPos content line character
  return $! r

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

readFileMaybe :: FilePath -> IO (Maybe Text)
readFileMaybe fp = do
  result <- try (T.readFile fp) :: IO (Either IOException Text)
  pure (either (const Nothing) Just result)

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
