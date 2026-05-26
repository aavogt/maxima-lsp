module CSE where

import Ident
import Data.Text (Text)
import PyF
import Data.Char (isAlpha, isAlphaNum, isSpace)
import qualified Data.Text as T
import Data.Maybe (fromMaybe, listToMaybe)
import Prelude hiding (getLine)
import Text.Pretty.Simple

uncse :: Int -> Int -> Text -> Text
uncse l c txt =
  case identifierUnderCursor txt l c of
    Nothing -> txt
    Just name ->
      let ls = T.lines txt
          defIdx = findDefLine name ls
       in case defIdx of
            Nothing ->
              case findBlockDefInLines name ls of
                Nothing -> txt
                Just (i, param, body, lineWithout) ->
                  let body' = T.strip (T.dropWhileEnd (== ';') body)
                      line' = replaceCall name param body' lineWithout
                   in T.unlines (setLine i line' ls)
            Just i ->
              let defLine = ls !! i
                  (param, body) = parseDef defLine
                  body' = T.strip (T.dropWhileEnd (== ';') body)
                  replaceLine idx line
                    | idx == i = Nothing
                    | otherwise = Just (replaceCall name param body' line)
                  ls' = mapMaybeWithIndex replaceLine ls
               in T.unlines ls'

cse :: Int -> Int -> Int -> Int -> Text -> Text
cse l1 c1 _l2 c2 txt =
  case listToMaybe [ (c, name) | c <- [c1 .. c2-1],
        Just name <- [identifierUnderCursor txt l1 c], not (any isSpace (T.unpack name))] of
    Nothing -> txt
    Just (c1, name) ->
      let ls = T.lines txt
       in case getLine l1 ls of
            Nothing -> txt
            Just line ->
              let (s, e) = if c1 <= c2 then (c1, c2) else (c2, c1)
                  start = fromMaybe s (findIdentStart (T.unpack line) s)
                  expr = extractExpr line start
                  paramExpr = innermostArg expr
               in if T.null expr || T.null paramExpr || not (name `T.isPrefixOf` expr)
                    then txt
                    else
                      let base = baseIdent paramExpr
                          paramName = if T.null base then "x1" else base <> "1"
                          defExpr = replaceInnermostArg expr paramName
                          callArg = if isSimpleIdent paramExpr then paramExpr <> "+1" else paramExpr
                          name2 = T.filter isAlphaNum defExpr
                          callExpr = name2 <> "(" <> callArg <> ")"
                          line' = replaceOnce expr callExpr line
                       in if isBlockLine line
                            then
                              let line'' = insertBindingInBlock name2 paramName defExpr line'
                               in T.unlines (setLine l1 line'' ls)
                            else
                              let defLine = name2 <> " (" <> paramName <> ") := " <> defExpr <> ";"
                               in T.unlines (defLine : setLine l1 line' ls)

-- helpers

getLine :: Int -> [Text] -> Maybe Text
getLine i xs
  | i < 0 = Nothing
  | otherwise = listToMaybe (drop i xs)

setLine :: Int -> Text -> [Text] -> [Text]
setLine i x xs =
  let (a, b) = splitAt i xs
   in case b of
        [] -> xs
        (_ : rest) -> a <> (x : rest)

safeSliceInclusive :: Int -> Int -> Text -> Text
safeSliceInclusive s e t
  | s < 0 = safeSliceInclusive 0 e t
  | e < s = ""
  | otherwise = T.take (e - s + 1) (T.drop s t)

extractExpr :: Text -> Int -> Text
extractExpr line pos =
  let s = T.unpack line
      len = length s
      start = fromMaybe 0 (findIdentStart s pos)
      end = fromMaybe (min (len - 1) (start + 1)) (findExprEnd s start)
   in T.pack (take (end - start + 1) (drop start s))

findIdentStart :: String -> Int -> Maybe Int
findIdentStart s pos
  | pos < 0 = Nothing
  | pos >= length s = findIdentStart s (length s - 1)
  | otherwise =
      let go i
            | i < 0 = Nothing
            | isAlpha (s !! i) = Just i
            | otherwise = go (i - 1)
       in go pos

findExprEnd :: String -> Int -> Maybe Int
findExprEnd s start =
  let len = length s
      identEnd = scanIdentEnd s start
   in if identEnd + 1 < len && s !! (identEnd + 1) == '('
        then findMatchingParen s (identEnd + 1)
        else Just identEnd

scanIdentEnd :: String -> Int -> Int
scanIdentEnd s i
  | i >= length s = length s - 1
  | isAlphaNum (s !! i) || s !! i == '_' = scanIdentEnd s (i + 1)
  | otherwise = i - 1

findMatchingParen :: String -> Int -> Maybe Int
findMatchingParen s openIdx =
  let len = length s
      go i depth
        | i >= len = Nothing
        | s !! i == '(' = go (i + 1) (depth + 1)
        | s !! i == ')' =
            if depth == 1
              then Just i
              else go (i + 1) (depth - 1)
        | otherwise = go (i + 1) depth
   in if openIdx < len && s !! openIdx == '('
        then go openIdx 0
        else Nothing

innermostArg :: Text -> Text
innermostArg expr =
  let s = T.unpack expr
      lastOpen = lastIndex '(' s
   in case lastOpen of
        Nothing -> ""
        Just o ->
          let close = fromMaybe (length s - 1) (findNext ')' s o)
           in T.pack (take (close - o - 1) (drop (o + 1) s))

replaceInnermostArg :: Text -> Text -> Text
replaceInnermostArg expr repl =
  let s = T.unpack expr
      lastOpen = lastIndex '(' s
   in case lastOpen of
        Nothing -> expr
        Just o ->
          case findNext ')' s o of
            Nothing -> expr
            Just c ->
              let prefix = take (o + 1) s
                  suffix = drop c s
               in T.pack (prefix <> T.unpack repl <> suffix)

replaceOnce :: Text -> Text -> Text -> Text
replaceOnce needle repl hay =
  case T.breakOn needle hay of
    (a, b) | T.null b -> hay
    (a, b) -> a <> repl <> T.drop (T.length needle) b

isSimpleIdent :: Text -> Bool
isSimpleIdent t = not (T.null t) && T.all isAlpha t

baseIdent :: Text -> Text
baseIdent t =
  let s = T.dropWhile (not . isAlpha) t
   in T.takeWhile isAlpha s

lastIndex :: Char -> String -> Maybe Int
lastIndex ch s =
  let idxs = [i | (i, c) <- zip [0 ..] s, c == ch]
   in if null idxs then Nothing else Just (last idxs)

findNext :: Char -> String -> Int -> Maybe Int
findNext ch s start =
  let go i
        | i >= length s = Nothing
        | s !! i == ch = Just i
        | otherwise = go (i + 1)
   in go (start + 1)

isBlockLine :: Text -> Bool
isBlockLine line = "block([" `T.isInfixOf` line

insertBindingInBlock :: Text -> Text -> Text -> Text -> Text
insertBindingInBlock name param defExpr line =
  case findBracketRange line of
    Nothing -> line
    Just (openIdx, closeIdx) ->
      let insertText = ", " <> name <> "(" <> param <> ") := " <> defExpr
          prefix = T.take closeIdx line
          suffix = T.drop closeIdx line
       in prefix <> insertText <> suffix

findBracketRange :: Text -> Maybe (Int, Int)
findBracketRange line =
  let s = T.unpack line
      openIdx = findNextChar '[' s
   in case openIdx of
        Nothing -> Nothing
        Just o ->
          case findMatchingBracket s o of
            Nothing -> Nothing
            Just c -> Just (o, c)

findNextChar :: Char -> String -> Maybe Int
findNextChar ch s = findNext ch s (-1)

findMatchingBracket :: String -> Int -> Maybe Int
findMatchingBracket s openIdx =
  let len = length s
      go i depth
        | i >= len = Nothing
        | s !! i == '[' = go (i + 1) (depth + 1)
        | s !! i == ']' =
            if depth == 1
              then Just i
              else go (i + 1) (depth - 1)
        | otherwise = go (i + 1) depth
   in if openIdx < len && s !! openIdx == '['
        then go openIdx 0
        else Nothing

findDefLine :: Text -> [Text] -> Maybe Int
findDefLine name ls =
  let predLine t = name `T.isPrefixOf` T.strip t && ":=" `T.isInfixOf` t
   in listToMaybe [i | (i, t) <- zip [0 ..] ls, predLine t]

findBlockDefInLines :: Text -> [Text] -> Maybe (Int, Text, Text, Text)
findBlockDefInLines name ls =
  listToMaybe
    [ (i, param, body, lineWithout)
    | (i, line) <- zip [0 ..] ls
    , Just (param, body, lineWithout) <- [findBlockDef name line]
    ]

findBlockDef :: Text -> Text -> Maybe (Text, Text, Text)
findBlockDef name line =
  case T.breakOn "block([" line of
    (_, rest) | T.null rest -> Nothing
    _ ->
      let token = ", " <> name <> "("
       in case T.breakOn token line of
            (_, b) | T.null b -> Nothing
            (a, b) ->
              let afterToken = T.drop (T.length token) b
                  (param, rest1) = T.breakOn ")" afterToken
                  rest2 = T.drop 1 rest1
                  rest3 = T.stripStart rest2
               in if not (":=" `T.isPrefixOf` rest3)
                    then Nothing
                    else
                      let bodyStart = T.stripStart (T.drop 2 rest3)
                          (body, restBody) = T.breakOn "]," bodyStart
                          (body', restTail) =
                            if not (T.null restBody)
                              then (body, restBody)
                              else
                                let (body2, rest2') = T.breakOn "]" bodyStart
                                 in (body2, rest2')
                       in if T.null restTail
                            then Nothing
                            else Just (param, T.strip body', a <> restTail)

parseDef :: Text -> (Text, Text)
parseDef line =
  let (before, after) = T.breakOn ":=" line
      param = betweenParens before
      body = T.drop 2 after
   in (param, T.strip body)

betweenParens :: Text -> Text
betweenParens t =
  case T.breakOn "(" t of
    (_, rest) ->
      case T.breakOn ")" (T.drop 1 rest) of
        (inside, _) -> inside

replaceCall :: Text -> Text -> Text -> Text -> Text
replaceCall name param body line =
  case T.breakOn (name <> "(") line of
    (a, b) | T.null b -> line
    (a, b) ->
      let rest = T.drop (T.length name) b
       in case matchParenSpan rest of
            Nothing -> line
            Just (arg, rest2) ->
              let body' = T.replace param arg body
               in a <> body' <> rest2

matchParenSpan :: Text -> Maybe (Text, Text)
matchParenSpan txt =
  case T.uncons txt of
    Just ('(', _) ->
      let s = T.unpack txt
       in case findMatchingParen s 0 of
            Nothing -> Nothing
            Just endIdx ->
              let arg = T.pack (take (endIdx - 1) (drop 1 s))
                  rest2 = T.pack (drop (endIdx + 1) s)
               in Just (arg, rest2)
    _ -> Nothing

mapMaybeWithIndex :: (Int -> a -> Maybe b) -> [a] -> [b]
mapMaybeWithIndex f xs =
  [y | (i, x) <- zip [0 ..] xs, Just y <- [f i x]]

test3 :: IO Bool
test3 = do
  let inp = [fmt|x : block([z : 3], f(z, g(y+1)));|]
      expected = [fmt|x : block([z : 3, fzgy1(y1) := f(z, g(y1))], fzgy1(y+1));|]
      out = cse 0 18 0 24 inp
      uncseOut = uncse 0 21 out
  pPrint (inp, uncseOut)
  return (T.strip out == T.strip expected && T.strip uncseOut == T.strip inp)

test2 :: IO Bool
test2 = do
  let inp = [fmt|x : block([z : 3], f(z, g(y+1)));|]
      expected = [fmt|x : block([z : 3, fzgy1(y1) := f(z, g(y1))], fzgy1(y+1));|]
      out = cse 0 19 0 24 inp
      uncseOut = uncse 0 18 out
  pPrint (inp, uncseOut)
  return (T.strip out == T.strip expected && T.strip uncseOut == T.strip inp)

test1 :: IO Bool
test1 = do
  let inp = [fmt|x : f(g(y+1));|]
  let out = cse 0 4 0 6 inp
      expected = [fmtTrim|
        fgy1 (y1) := f(g(y1));
        x : fgy1(y+1); |]
      uncseOut = uncse 0 2 out
  pPrint (out, expected)
  return (T.strip out == T.strip expected && T.strip uncseOut == T.strip inp)
