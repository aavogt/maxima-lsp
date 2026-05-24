-- |
--
-- refer to maxima's nparse.lisp ../deps/nparse.lisp
--
-- requirements: 
--
-- semivalid code: I can have a syntax error in one place (missing ] etc. ), but it's desirable to 
-- have a best attempt on the rest of it which assumes layout can stand in for the missing delimiter
--
-- currently supports completions but it will also help with renaming
module NParse (commentedVars) where

import Control.Applicative
import Control.Lens
import Data.Char
import Data.Functor
import Data.List (mapAccumL)
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import NLex (scan)
import PyF
import Text.Pretty.Simple
import Text.Regex.Applicative

data T = T0 {_i :: Int, _n :: Text, _ns :: [T], _binds :: [Text]} | NT {_n :: Text} deriving (Show, Eq)

pattern T {_i, _n, _ns} = T0 {_binds = [], ..}

makePrisms ''T
makeLenses ''T

toTrees :: Text -> [T]
toTrees text = foldr (\(i, x) t -> insertT i x t) [] (scan text)

insertT :: Maybe Int -> Text -> [T] -> [T]
insertT Nothing x t = NT x : t
insertT (Just i) x t
  | T.all isSpace x = T i x [] : t
  | otherwise =
      let (children, rest) =
            span
              (\t0@(~T {..}) -> isn't _T0 t0 || _i > i || (_i == i && T.all isSpace _n))
              t
       in T i x children : rest

fromTrees :: [T] -> Text
fromTrees = T.concat . map unTree1

unTree1 :: T -> Text
unTree1 NT {_n} = _n <> "\n"
unTree1 T {_n, _ns} = _n <> "\n" <> fromTrees _ns

testNparse :: IO Bool
testNparse = do
  let orig = "a\n b\n\n c\nd\n  e\n f\ng\n h\n  i\n"
  let gs = toTrees orig
  mapM_ print (T.lines orig)
  mapM_ print (T.lines (fromTrees gs))
  let expected =
        [ T 0 "a" [T 1 "b" [], T 0 "" [], T 1 "c" []],
          T 0 "d" [T 2 "e" [], T 1 "f" []],
          T 0 "g" [T 1 "h" [T 2 "i" []]]
        ]
  pPrint gs

  return (fromTrees gs == orig)

-- each character needs a state
testTC :: IO Bool
testTC = do
  let orig = "/* explains\n this\nfunction \n in a few lines*/\nf(x) := block()$\n"
  let gs = toTrees orig
  mapM_ print (T.lines orig)
  mapM_ print (T.lines (fromTrees gs))
  pPrint gs
  return True

spaces = void (many (psym isSpace))

comment =
  "/*"
    *> many
      ( do
          a <- psym (/= '*')
          pure [a]
          <|> do
            a <- psym (== '*')
            b <- psym (/= '/')
            pure [a, b]
      )
    <* "*/"

commentedVar :: RE Char (String, Bool)
commentedVar = do
  ident <- do
    x <- psym \c -> isAlpha c || c == '_'
    xs <- many (psym \c -> isAlphaNum c || c == '_')
    pure (x : xs)
  spaces
  isFunction <-
    True <$ "("
      <|> do
        ":"
        spaces
        "lambda("
        pure True
      <|> pure False
  pure (ident, isFunction)

groupLR (Left a : Right b : xs) = (a, b) : groupLR xs
groupLR (Left _ : xs) = groupLR xs
groupLR (Right b : xs) = ("", b) : groupLR xs
groupLR [] = []

commentedVars :: Text -- ^ .mac file contents
  -> [(Text, (String, Bool))] -- ^ @(leading comment, (ident, isFunction))@
commentedVars = groupLR . commentedVars1

commentedVars1 input =
  [ maybe (Left (fromTrees (pure t))) Right (r & _Just %~ fst)
    | t <- toTrees input,
      let r = findFirstPrefixWithUncons T.uncons commentedVar . fromTrees $ pure t
  ]

testCV :: IO Bool
testCV = do
  let inp =
        [fmt| 
/* description */
f (x) := y$

/* another comment
that spans
multiple lines
but this is okay
 and this is okay too
 */
a : lambda([x], x);
b : lambda([x], x);
        |]
  pPrint (toTrees inp)
  pPrint (commentedVars inp)
  return True
