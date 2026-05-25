module DB where

import Data.Text (Text)
import Database.LMDB.Simple
import Paths_maxima_lsp
import System.IO
import Database.LMDB.Simple.Extra (toList)

data DB = DB
  { -- | get the maxima manual section
    hover :: Text -> IO (Maybe Text),
    -- | built-in @[(name, isFunction)]@ ie. @[("create_list", True), ("ratprint", False)]@
    completions :: IO [(Text, Bool)],
    importStatements :: Text -> IO (Maybe [Text])
  }

-- | load lmdb written by ../hoverdb/hoverdb.hs
openDB = do
  lmdb <- getDataFileName "hoverdb/hoverdb.lmdb"
  hPutStrLn stderr ("opening: " ++ lmdb)
  env <- openEnvironment @ReadOnly lmdb defaultLimits {mapSize = 4 * 2 * 1024 * 1024, maxDatabases = 3}
  let hover q = readOnlyTransaction env do
              db <- getDatabase (Just "hover")
              get db q
  -- would be better to do IO in the transaction than to allocate a list for all keys?
  let completions = readOnlyTransaction env do
        db <- getDatabase (Just "isfunc")
        toList db
  let importStatements q = readOnlyTransaction env do
        db <- getDatabase (Just "importstatements")
        get db q
  return $ DB {..}
