module Main where

import Control.Concurrent
import Control.Lens
import Control.Monad.IO.Class
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Char
import Data.Foldable
import Data.List.Split
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import Database.LMDB.Simple
import PyF
import System.IO
import System.Process
import Text.HTML.TagSoup
import UnliftIO.Async

makePrisms ''Tag

main = do
  ts <- parseTags <$> TL.readFile "../deps/maxima_singlepage.html"

  -- index
  let es :: [(TL.Text, TL.Text)]
      es = extract (skipToIndexTable ts)

  -- definitions
  let allIds :: [Tag TL.Text] -> [TL.Text]
      allIds = toListOf (traversed . _TagOpen . _2 . to (lookup "id") . _Just)
  let ds =
        ts
          & splitOn [TagClose "dl"]
          & \cs -> M.fromList [(i, c) | c <- cs, i <- allIds c]

  system "touch hoverdb.lmdb"

  -- many threads call python html2text
  ch <- newChan
  forkIO do
    pooledForConcurrently_ es \(n, k) -> do
      v <- traverse (html2text . renderTags) (M.lookup k ds)
      writeChan ch (Just (n, v))
    writeChan ch Nothing -- done
  env <- openEnvironment "hoverdb.lmdb" defaultLimits {mapSize = 100 * 2 * 1024 * 1024, maxDatabases = 2}
  -- single thread to write to the db
  readWriteTransaction env do
    hover <- getDatabase (Just "hover")
    isFunc <- getDatabase (Just "isfunc")
    let loop = do
          mnv <- liftIO $ readChan ch
          case mnv of
            Just (n, Just v) -> do
              put hover n (Just v)
              put isFunc n (Just ("Function:" `T.isPrefixOf` v))
              loop
            Just {} -> loop
            Nothing -> return ()
    loop

skipToIndexTable = dropWhile \case
  TagOpen "table" (lookup "class" -> Just "index-fn") -> False
  _ -> True

extract = \case
  TagOpen "a" (lookup "href" -> Just dest) : (getName -> Just (n, rest))
    | Just ('#', dest) <- TL.uncons dest ->
        (n, dest) : extract rest
  x : xs -> extract xs
  [] -> []

getName = \case
  TagText a : xs -> Just (a, xs)
  TagOpen "code" _ : TagText a : xs -> Just (a, xs)
  _ -> Nothing

py :: [String]
py =
  [ "-c",
    unlines . map (dropWhile isSpace) . filter (not . null) . lines $
      [fmt|
    import sys, html2text
    h = html2text.HTML2Text()
    h.ignore_links = True
    h.body_width = 0
    sys.stdout.write(h.handle(sys.stdin.read()))
    |]
  ]

-- most of the time is spent here.
-- cabal run make-hoverdb  362.81s user 63.41s system 1239% cpu 34.399 total
-- vs. about 7s if html2Text = pure . TL.toStrict
html2text :: TL.Text -> IO Text
html2text html = do
  ph@(Just pyin, Just pyout, _, _) <- createProcess (proc "python3" py) {std_in = CreatePipe, std_out = CreatePipe}
  TL.hPutStr pyin html
  hClose pyin
  out <- T.hGetContents pyout
  cleanupProcess ph
  return out
