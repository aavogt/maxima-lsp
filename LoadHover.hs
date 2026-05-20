module LoadHover where

-- once we've run hoverdb.hs
import Paths_maxima_lsp
import Database.LMDB.Simple
import Data.Text (Text)
import System.IO

lmdbHover :: IO (Text -> IO (Maybe Text))
lmdbHover = do
    lmdb <- getDataFileName "hoverdb/hoverdb.lmdb"
    hPutStrLn stderr ("opening: "++lmdb)
    env <- openEnvironment @ReadOnly lmdb defaultLimits {mapSize = 4 * 2 * 1024 * 1024}
    return $ \q -> readOnlyTransaction env do
      db <- getDatabase Nothing
      get db q
