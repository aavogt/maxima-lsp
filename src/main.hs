import Control.Concurrent
import Control.Exception
import Control.Lens
import Control.Lens.Regex.Text
import Control.Monad
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.QQ
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Foldable
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import System.IO
import System.Posix.Resource
import Text.Regex.PCRE.Light (compile)

import Ident ( identifierUnderCursor, splitPos )
import DB ( openDB, DB(hover, completions) )
import Pun (json)
import RPC ( addContentLength, message )
import Range ( toPrrFrom, wholeRange, rangeLine, Range )
import NParse (commentedVars)

limit :: Resource -> Integer -> IO ()
limit resource amt = setResourceLimit resource (ResourceLimits (ResourceLimit amt) (ResourceLimit amt))

type Documents = Map Text Text

main :: IO ()
main = do
  limit ResourceTotalMemory (1024 * 1024 * 1024 * 1024 * 1024)
  limit ResourceCPUTime 10
  db <- openDB
  documents <- newMVar mempty
  forever do
    m <- message
    mr <- fmap (addContentLength . encode) <$> lsp db documents m
    for_ mr \r -> do
      L8.putStr r
      hFlush stdout
    threadDelay (100 * 1000) -- 0.1s


lsp :: DB -> MVar Documents -> Value -> IO (Maybe Value)
lsp db documents [json| method params id |] = case method :: Text of
  "initialize" ->
      resp
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
  "textDocument/prepareRename" -> respIo (prepareRename documents params)
  "textDocument/rename" -> respIo (rename documents params)
  "textDocument/hover" -> respIo (findHoverRequest db documents params)
  "textDocument/completion" -> respIo (completionRequest db documents params)
  "completionItem/resolve" -> respIo (completionItemResolve db params)
  _ -> pure Nothing
  where
    resp (result :: Value) = pure $ Just [aesonQQ| { jsonrpc: "2.0", id : #{id :: Int}, result : #{result} } |]
    respIo ioResult = do
      mr <- try @SomeException ioResult
      case mr of
        Left msg -> do
          hPrint stderr msg
          return Nothing
        Right r -> resp r
lsp findHover documents [json| method params |] = do
  handleNotification documents method params
  pure Nothing
lsp _ _ _ = return Nothing

findHoverRequest :: DB -> MVar Documents -> Value -> IO Value
findHoverRequest db documents [json| _textDocument{uri} _position{line character} |] = do
  Just content <- getDocumentContent documents uri
  Just (Just hoverText) <- identifierUnderCursor content line character & _Just %%~ hover db
  pure $! hoverResult hoverText
  where
    hoverResult txt =
      [aesonQQ|
        { contents:
            { kind: "markdown"
            , value: #{txt}
            }
        }
      |]

completionRequest :: DB -> MVar Documents -> Value -> IO Value
completionRequest db documents [json| _textDocument{uri} _position{line character} |] = do
  Just content <- getDocumentContent documents uri
  items <- completionItems content line character <$> completionsWithComments db content
  pure [aesonQQ| { isIncomplete: false, items: #{items} } |]

completionsWithComments :: DB -> Text -> IO [(Text, Bool, Maybe Text)]
completionsWithComments db content = do
  let fileCompletions = mapMaybe toCompletionWithComment (commentedVars content)
  dbCompletions <- completions db
  let dbItems = map (\(ident, isFunction) -> (ident, isFunction, Nothing)) dbCompletions
  pure (fileCompletions <> dbItems)

toCompletionWithComment :: (Text, (String, Bool)) -> Maybe (Text, Bool, Maybe Text)
toCompletionWithComment (comment, (ident, isFunction)) =
  let identText = T.pack ident
      commentText = normalizeComment comment
  in Just (identText, isFunction, commentText)

normalizeComment :: Text -> Maybe Text
normalizeComment comment =
  let trimmed = T.strip comment
  in if T.null trimmed
       then Nothing
       else Just trimmed

completionItems :: Text -> Int -> Int -> [(Text, Bool, Maybe Text)] -> [Value]
completionItems content line character =
  let replaceRange = maybe (rangeLine line character character) (toPrrFrom line character) (splitPos content line character)
  in map \(ident, isFunction, mComment) ->
    let insertText = if isFunction then ident <> "(" else ident
        baseItem =
          [aesonQQ|
            { label: #{ident}
            , textEdit: { range: #{replaceRange}, newText: #{insertText} }
            , data: { identifier: #{ident} }
            }
          |]
    in case mComment of
        Nothing -> baseItem
        Just comment ->
          baseItem `unsafeAppend`
            [aesonQQ|
              { documentation:
                  { kind: "markdown"
                  , value: #{comment}
                  }
              }
            |]

completionItemResolve :: DB -> Value -> IO Value
completionItemResolve db item = case item of
  [json| _data{identifier} |] -> do
    mHoverText <- hover db identifier
    pure $ case mHoverText of
      Nothing -> item
      Just hoverText ->
        item `unsafeAppend` [aesonQQ| { documentation : { kind : "markdown", value : #{hoverText} } } |]
  _ -> pure item

unsafeAppend :: Value -> Value -> Value
unsafeAppend (Object a) (Object b) = Object (a<>b)
-- PatternMatchFail will be caught

rename :: MVar Documents -> Value -> IO Value
rename documents [json| _textDocument{uri} _position{line character} newName |] = do
  Just content <- getDocumentContent documents uri
  let Just ident = identifierUnderCursor content line character
  let regex = compile (T.encodeUtf8 ident) []
      newContent = content & regexing regex . match .~ newName
      editRange = wholeRange content
  pure $! workspaceEditValue (uriToFilePath uri) editRange newContent

prepareRename :: MVar Documents -> Value -> IO Value
prepareRename documents [json| _textDocument{uri} _position{line character} |] = do
  Just content <- getDocumentContent documents uri
  let Just r = toJSON . toPrrFrom line character <$> splitPos content line character
  return $! r

handleNotification :: MVar Documents -> Text -> Value -> IO ()
handleNotification documents method params = case method :: Text of
  "textDocument/didOpen" ->
    case params of
      [json| textDocument{ uri text } |] ->
        modifyMVar_ documents (pure . Map.insert uri text)
      _ -> pure ()
  "textDocument/didChange" ->
    case params of
      [json| _textDocument{ uri } _contentChanges[ { text } ] |] ->
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
