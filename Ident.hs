module Ident where
import Control.Lens
import Control.Monad
import Data.Char
import Data.Text (Text)
import qualified Data.Text as T
import Text.Regex.Applicative

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
