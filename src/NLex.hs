module NLex where

import Data.Char
import Data.List (mapAccumL)
import Data.Text (Text)
import qualified Data.Text as T

data DelimState = DelimState
  { _parenDepth :: Int,
    _bracketDepth :: Int,
    _braceDepth :: Int,
    _inString :: Bool,
    _inChar :: Bool,
    _inBlockComment :: Bool
  }
  deriving (Show, Eq)

initialDelimState :: DelimState
initialDelimState = DelimState 0 0 0 False False False

insideDelim :: DelimState -> Bool
insideDelim st =
  _inBlockComment st
    || _inString st
    || _inChar st
    || _parenDepth st > 0
    || _bracketDepth st > 0
    || _braceDepth st > 0

countIndent = T.length . T.takeWhile isSpace

scan :: Text -> [(Maybe Int, Text)]
scan input = reverse (go initialDelimState (insideDelim initialDelimState) [] (T.unpack input) [])
  where
    go st lineStartInside current rest acc = case rest of
      [] ->
        let lineText = T.pack (reverse current)
            entry = (if lineStartInside then Nothing else Just (countIndent lineText), lineText)
         in entry : acc
      '\n' : [] ->
        let lineText = T.pack (reverse current)
            entry = (if lineStartInside then Nothing else Just (countIndent lineText), lineText)
         in entry : acc
      '\n' : xs ->
        let lineText = T.pack (reverse current)
            entry = (if lineStartInside then Nothing else Just (countIndent lineText), lineText)
            nextLineStartInside = insideDelim st
         in go st nextLineStartInside [] xs (entry : acc)
      '\\' : x : xs
        | _inString st || _inChar st ->
            go st lineStartInside (x : '\\' : current) xs acc
      '/' : '*' : xs
        | not (_inString st || _inChar st || _inBlockComment st) ->
            let st' = st {_inBlockComment = True}
             in go st' lineStartInside ('*' : '/' : current) xs acc
      '*' : '/' : xs
        | _inBlockComment st ->
            let st' = st {_inBlockComment = False}
             in go st' lineStartInside ('/' : '*' : current) xs acc
      '"' : xs
        | not (_inBlockComment st || _inChar st) ->
            let st' = st {_inString = not (_inString st)}
             in go st' lineStartInside ('"' : current) xs acc
      '\'' : xs
        | not (_inBlockComment st || _inString st) ->
            let st' = st {_inChar = not (_inChar st)}
             in go st' lineStartInside ('\'' : current) xs acc
      '(' : xs
        | not (insideDelim st && (_inString st || _inChar st || _inBlockComment st)) ->
            let st' = st {_parenDepth = _parenDepth st + 1}
             in go st' lineStartInside ('(' : current) xs acc
      ')' : xs
        | not (insideDelim st && (_inString st || _inChar st || _inBlockComment st)) ->
            let st' = st {_parenDepth = max 0 (_parenDepth st - 1)}
             in go st' lineStartInside (')' : current) xs acc
      '[' : xs
        | not (insideDelim st && (_inString st || _inChar st || _inBlockComment st)) ->
            let st' = st {_bracketDepth = _bracketDepth st + 1}
             in go st' lineStartInside ('[' : current) xs acc
      ']' : xs
        | not (insideDelim st && (_inString st || _inChar st || _inBlockComment st)) ->
            let st' = st {_bracketDepth = max 0 (_bracketDepth st - 1)}
             in go st' lineStartInside (']' : current) xs acc
      '{' : xs
        | not (insideDelim st && (_inString st || _inChar st || _inBlockComment st)) ->
            let st' = st {_braceDepth = _braceDepth st + 1}
             in go st' lineStartInside ('{' : current) xs acc
      '}' : xs
        | not (insideDelim st && (_inString st || _inChar st || _inBlockComment st)) ->
            let st' = st {_braceDepth = max 0 (_braceDepth st - 1)}
             in go st' lineStartInside ('}' : current) xs acc
      x : xs ->
        go st lineStartInside (x : current) xs acc
