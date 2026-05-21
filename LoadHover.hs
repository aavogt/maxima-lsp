module LoadHover where

-- once we've run hoverdb.hs

import Data.Text (Text)
import Database.LMDB.Simple
import Paths_maxima_lsp
import System.IO
import Database.LMDB.Simple.Extra (keys)

data HoverDB = HoverDB
  { hover :: Text -> IO (Maybe Text),
    completions :: IO [Text]
  }

openHoverDB = do
  lmdb <- getDataFileName "hoverdb/hoverdb.lmdb"
  hPutStrLn stderr ("opening: " ++ lmdb)
  env <- openEnvironment @ReadOnly lmdb defaultLimits {mapSize = 4 * 2 * 1024 * 1024}
  let hover q = readOnlyTransaction env do
              db <- getDatabase Nothing
              get db q
  let completions = readOnlyTransaction env do
        db <- getDatabase Nothing
        keys db
  return $ HoverDB {..}
