module Range where

import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import GHC.Generics (Generic)

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


wholeRange = mwholeRange . Just

mwholeRange :: Maybe Text -> Range
mwholeRange mp = Range (Position 0 0) (Position endLine endChar)
  where
    endLine = maybe 0 (length . T.lines) mp
    endChar = maybe 0 T.length $ (mlast . T.lines) =<< mp

    mlast [] = Nothing
    mlast xs = Just (last xs)

toPrrFrom :: Int -> Int -> (Text, Text) -> Range
toPrrFrom line col (T.length -> nl, T.length -> nr) = rangeLine line (col - nl) (col + nr)

rangeLine :: Int -> Int -> Int -> Range
rangeLine line c1 c2 = Range (Position line (max 0 c1)) (Position line (max 0 c2))
