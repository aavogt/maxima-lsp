{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

{- HLINT ignore "Functor law" -}

module Main where

import Control.Lens
import Control.Lens.Regex.Text
import Control.Monad
import qualified Data.Aeson as Aeson
import Data.Char
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Language.LSP.Protocol.Lens hiding (length, to)
import qualified Language.LSP.Protocol.Lens hiding (length, to)
import qualified Language.LSP.Protocol.Message as LSP
import qualified Language.LSP.Protocol.Types as LSP
import qualified Language.LSP.Server as LSP
import Text.Regex.Applicative hiding (match)
import Text.Regex.PCRE.Light (compile)
import Control.Monad.IO.Class

-- TODO ./deps/nparse.lisp

main = LSP.runServer serverDefinition

serverDefinition :: LSP.ServerDefinition Conf
serverDefinition =
  LSP.ServerDefinition
    { defaultConfig = Conf,
      configSection,
      parseConfig,
      onConfigChange,
      doInitialize,
      staticHandlers,
      options = LSP.defaultOptions,
      interpretHandler
    }

data Conf = Conf

type A = LSP.LanguageContextEnv Conf

type M = LSP.LspT Conf IO

interpretHandler :: A -> M LSP.<~> IO
interpretHandler env = LSP.Iso (LSP.runLspT env)liftIO

staticHandlers :: LSP.ClientCapabilities -> LSP.Handlers M
staticHandlers _ = 
  LSP.requestHandler LSP.SMethod_Initialize initialize <>
  (LSP.notificationHandler LSP.SMethod_Initialized \ _ -> return ())
  <> LSP.requestHandler LSP.SMethod_TextDocumentRename handleRename
  <> LSP.requestHandler LSP.SMethod_TextDocumentPrepareRename handlePrepareRename

initialize :: LSP.TRequestMessage LSP.Method_Initialize 
  -> (Either (LSP.TResponseError LSP.Method_Initialize) LSP.InitializeResult -> LSP.LspT Conf IO ()) -> LSP.LspT Conf IO ()
initialize _ f = f (Right caps)

caps :: LSP.InitializeResult
caps = LSP.InitializeResult caps1 Nothing

caps1 :: LSP.ServerCapabilities
caps1 = case Aeson.decodeStrictText "{ \"renameProvider\" : { \"prepareProvider\" : true, \"workDoneProgress\" : false } }" of Just a -> a
  -- parseJSON = Aeson.withObject "ServerCapabilities" $ \arg -> ServerCapabilities <$> arg Language.LSP.Protocol.Types.Common..:!? "positionEncoding" <*> arg Language.LSP.Protocol.Types.Common..:!? "textDocumentSync" <*> arg Language.LSP.Protocol.Types.Common..:!? "notebookDocumentSync" <*> arg Language.LSP.Protocol.Types.Common..:!? "completionProvider" <*> arg Language.LSP.Protocol.Types.Common..:!? "hoverProvider" <*> arg Language.LSP.Protocol.Types.Common..:!? "signatureHelpProvider" <*> arg Language.LSP.Protocol.Types.Common..:!? "declarationProvider" <*> arg Language.LSP.Protocol.Types.Common..:!? "definitionProvider" <*> arg Language.LSP.Protocol.Types.Common..:!? "typeDefinitionProvider" <*> arg Language.LSP.Protocol.Types.Common..:!? "implementationProvider" <*> arg Language.LSP.Protocol.Types.Common..:!? "referencesProvider" <*> arg Language.LSP.Protocol.Types.Common..:!? "documentHighlightProvider" <*> arg Language.LSP.Protocol.Types.Common..:!? "documentSymbolProvider" <*> arg Language.LSP.Protocol.Types.Common..:!? "codeActionProvider" <*> arg Language.LSP.Protocol.Types.Common..:!? "codeLensProvider" <*> arg Language.LSP.Protocol.Types.Common..:!? "documentLinkProvider" <*> arg Language.LSP.Protocol.Types.Common..:!? "colorProvider" <*> arg Language.LSP.Protocol.Types.Common..:!? "workspaceSymbolProvider" <*> arg Language.LSP.Protocol.Types.Common..:!? "documentFormattingProvider" <*> arg Language.LSP.Protocol.Types.Common..:!? "documentRangeFormattingProvider" <*> arg Language.LSP.Protocol.Types.Common..:!? "documentOnTypeFormattingProvider" <*> arg Language.LSP.Protocol.Types.Common..:!? "renameProvider" <*> arg Language.LSP.Protocol.Types.Common..:!? "foldingRangeProvider" <*> arg Language.LSP.Protocol.Types.Common..:!? "selectionRangeProvider" <*> arg Language.LSP.Protocol.Types.Common..:!? "executeCommandProvider" <*> arg Language.LSP.Protocol.Types.Common..:!? "callHierarchyProvider" <*> arg Language.LSP.Protocol.Types.Common..:!? "linkedEditingRangeProvider" <*> arg Language.LSP.Protocol.Types.Common..:!? "semanticTokensProvider" <*> arg Language.LSP.Protocol.Types.Common..:!? "monikerProvider" <*> arg Language.LSP.Protocol.Types.Common..:!? "typeHierarchyProvider" <*> arg Language.LSP.Protocol.Types.Common..:!? "inlineValueProvider" <*> arg Language.LSP.Protocol.Types.Common..:!? "inlayHintProvider" <*> arg Language.LSP.Protocol.Types.Common..:!? "diagnosticProvider" <*> arg Language.LSP.Protocol.Types.Common..:!? "workspace" <*> arg Language.LSP.Protocol.Types.Common..:!? "experimental"
{-
data InitializeResult = InitializeResult 
  { {-|
  The capabilities the language server provides.
  -}
  _capabilities :: Language.LSP.Protocol.Internal.Types.ServerCapabilities.ServerCapabilities
  , {-|
  Information about the server.

  @since 3.15.0
  -}
  _serverInfo :: (Maybe Language.LSP.Protocol.Internal.Types.ServerInfo.ServerInfo)
  }
  -}

handlePrepareRename :: LSP.TRequestMessage LSP.Method_TextDocumentPrepareRename
  -> (Either (LSP.TResponseError LSP.Method_TextDocumentPrepareRename) (LSP.PrepareRenameResult LSP.|? LSP.Null) -> LSP.LspT Conf IO ())
  -> LSP.LspT Conf IO ()
handlePrepareRename rq = handlePrepare2 (rq ^. params)

handlePrepare2 :: LSP.PrepareRenameParams
  -> (Either (LSP.TResponseError LSP.Method_TextDocumentPrepareRename) (LSP.PrepareRenameResult LSP.|? LSP.Null) -> LSP.LspT Conf IO ())
  -> LSP.LspT Conf IO ()
handlePrepare2 rp f = do
  let fp = rp ^. textDocument . uri & LSP.uriToFilePath
  mp <- traverse (liftIO . T.readFile) fp
  let mident = identifierUnderCursor mp (rp^.position)
  case mident of
    Just (left, right) -> f (Right (LSP.InL (toPrr (T.length left) (T.length right) (rp^. position))))
  return ()

toPrr :: Int -> Int -> LSP.Position -> LSP.PrepareRenameResult
toPrr left right pos = LSP.PrepareRenameResult (LSP.InL (toPrr1 left right pos))

toPrr1 :: Int -> Int -> LSP.Position -> LSP.Range
toPrr1 left right pos = LSP.Range (shift (-left) pos) (shift right pos)

shift :: Int -> LSP.Position -> LSP.Position
shift n (LSP.Position line col) = LSP.Position line (fromIntegral (max 0 (fromIntegral col + n)))

{-
data PrepareRenameParams = PrepareRenameParams 
  { {-|
  The text document.
  -}
  _textDocument :: Language.LSP.Protocol.Internal.Types.TextDocumentIdentifier.TextDocumentIdentifier
  , {-|
  The position inside the text document.
  -}
  _position :: Language.LSP.Protocol.Internal.Types.Position.Position
  , {-|
  An optional token that a server can use to report work done progress.
  -}
  _workDoneToken :: (Maybe Language.LSP.Protocol.Internal.Types.ProgressToken.ProgressToken)
  -}
handleRename ::
  LSP.TRequestMessage LSP.Method_TextDocumentRename ->
  (Either (LSP.TResponseError LSP.Method_TextDocumentRename) (LSP.WorkspaceEdit LSP.|? LSP.Null) -> M ()) ->
  M ()
handleRename LSP.TRequestMessage {..} = handleRename2 _params

handleRename2 ::
  LSP.RenameParams ->
  (Either (LSP.TResponseError LSP.Method_TextDocumentRename) (LSP.WorkspaceEdit LSP.|? LSP.Null) -> M ()) ->
  M ()
handleRename2 rp@LSP.RenameParams {..} f = do
  let fp = rp ^. textDocument . uri & LSP.uriToFilePath
  when (isNothing fp) $ f (err "can't get file path " (rp ^. textDocument . uri, fp))
  mp <- traverse (liftIO . T.readFile) fp
  let pos@(LSP.Position (fromEnum -> n) (fromEnum -> c)) = rp ^. position
  let mident = identifierUnderCursor mp pos <&> uncurry (<>)
  when (isNothing mident) (f $ err "can't find identifier at" (n, c))
  let mp' = do
        ident <- mident
        let regex = compile (T.encodeUtf8 ident) [] -- plus something else?
        p <- mp
        Just $ p & regexing regex . match .~ _newName

  case mp' of
    Just p' -> f (Right (LSP.InL wsEdit))
      where
        wsEdit :: LSP.WorkspaceEdit
        wsEdit = LSP.WorkspaceEdit mempty (Just [LSP.InL mkTDE]) mempty

        mkTDE :: LSP.TextDocumentEdit
        mkTDE = LSP.TextDocumentEdit (nullVersion $ rp ^. textDocument) [LSP.InL mkTE]

        mkTE :: LSP.TextEdit
        mkTE = LSP.TextEdit (textRange mp) p'
    _ -> return ()
  return ()

identifierUnderCursor :: Maybe Text -> LSP.Position -> Maybe (Text, Text)
identifierUnderCursor mp (LSP.Position (fromEnum -> n) (fromEnum -> c)) =
  mp ^? _Just . to T.lines . ix n
    <&> T.splitAt c
    >>= _1 . reversed %%~ alphaNumThenAlpha
    <&> _2 %~ T.takeWhile isAlphaNum

textRange :: Maybe Text -> LSP.Range
textRange mp = LSP.Range (LSP.Position 0 0) (LSP.Position endLine endChar)
  where
    endLine = maybe 0 (fromIntegral . length . T.lines) mp
    endChar = maybe 0 (fromIntegral . T.length) $ (mlast . T.lines) =<< mp

    mlast [] = Nothing
    mlast xs = Just (last xs)

nullVersion :: LSP.TextDocumentIdentifier -> LSP.OptionalVersionedTextDocumentIdentifier
nullVersion tid = LSP.OptionalVersionedTextDocumentIdentifier (tid ^. uri) (LSP.InR LSP.Null)

err :: (Show a) => Text -> a -> Either (LSP.TResponseError LSP.Method_TextDocumentRename) t
err txt mfp = Left (LSP.TResponseError (LSP.InL LSP.LSPErrorCodes_RequestFailed) (txt <> T.pack (show mfp)) Nothing)

alphaNumThenAlpha =
  fmap fst . findLongestPrefixWithUncons T.uncons do
    xs <- many (psym isAlphaNum)
    x <- psym isAlpha
    pure (T.pack xs `T.snoc` x)


doInitialize env _ = return (Right env)

onConfigChange :: Conf -> M ()
onConfigChange _ = return ()

parseConfig :: Conf -> Aeson.Value -> Either Text Conf
parseConfig _ _ = Right Conf

configSection :: Text
configSection = "maxima-lsp"

-- data a |? b
-- = InL a
-- \| InR b
-- data Null = Null

