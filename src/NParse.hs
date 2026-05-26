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
module NParse (commentedVars, renameScoped) where

import Control.Applicative
import Control.Lens
import Control.Lens.Regex.Text (groups, match, regex, regexing)
import Control.Lens.Unsound
import Control.Monad
import Control.Zipper
import Data.Char
import Data.Data (Data)
import Data.Data.Lens (template)
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lens as T
import NLex (scan)
import PyF
import Text.Pretty.Simple
import Text.Regex.Applicative hiding (match)
import Text.Regex.PCRE.Light (compile)

-- | syntax tree: _i indentation level, _j line number [1..],  _n original line, _ns children,
-- _binds variables bound on this line probably in scope for _ns
--
-- it would be preferable to have T a = T0 { .. , _binds :: a},
-- then T [Text] -> T (Map Text Int)
-- but NT complicates it
data T = T0 {_i, _j :: Int, _n :: Text, _ns :: [T], _binds :: [Text], _bindline :: Map Text Int}
  | NT {_j :: Int, _n :: Text, _bindline :: Map Text Int} deriving (Show, Eq, Data)

instance Plated T

pattern T {_i, _j, _n, _ns} = T0 {_bindline = Empty, _binds = [], ..}

makePrisms ''T
makeLenses ''T

toTrees :: Text -> [T]
toTrees text = map setBindsm $ addBinds $ foldr (\((i, x), j) t -> insertT i j x t) [] (scan text `zip` [0 ..])

insertT :: Maybe Int -> Int -> Text -> [T] -> [T]
insertT Nothing j x t = NT j x mempty : t
insertT (Just i) j x t
  | T.all isSpace x = T i j x [] : t
  | otherwise =
      let (children, rest) =
            span
              (\t0@(~T {..}) -> isn't _T0 t0 || _i > i || (_i == i && T.all isSpace _n))
              t
       in T i j x children : rest

setBindsm :: T -> T
setBindsm =
  runIdentity
    . preorderScope
      ( \m t -> Identity $ case t of
          NT {..} -> NT {_bindline = m, ..}
          T0 {..} -> T0 {_bindline = m, ..}
      )
      mempty

-- | preorder traversal the current scope available `Map _binds _j`
preorderScope :: (Applicative f) => (Map Text Int -> T -> f T) -> Map Text Int -> T -> f T
preorderScope f e t0@NT {} = f e t0
preorderScope f e t0@T0 {..} = do
  let e' = M.fromList (map (,_j) _binds) <> e
  ~(T0 {_ns = _, ..}) <- f e' t0
  _ns <- traversed (preorderScope f e') _ns
  pure T0 {..}

-- | don't edit _ns from T0, don't change from T0 to NS
preorder :: Traversal' T T
preorder f t0@NT {} = f t0
preorder f t0@T0{ _ns = []} = f t0
preorder f t0 = do
  t1 <- f t0
  ns1 <- (traversed . preorder) f (t0 ^. ns)
  pure (t1 & ns .~ ns1)

renameScoped :: Text -> Int -> Int -> Text -> Either [String] Text
renameScoped file line col newname = do
  (_, oldname, withRenameLine) <- renameScoped_ (toTrees file) line col
  when (null oldname) (Left ["oldname null"])
  let regex = compile (T.encodeUtf8 ("\\b" <> T.pack oldname <> "\\b")) []
  withRenameLine (regexing regex . match .~ newname)

testSN :: IO Bool
testSN = do
  pPrint $ renameScoped "f(x) := block(x+y,\n x);\nx : 3;" 0 2 "z"
  return False

-- HasCallStack?
-- [msg| line col |] expands to line:{line} col:{col} {prettyCallStack callStack}
-- single qq 
-- [err| description |] f
f ? msg = \x -> case f <$> x of
  Right Nothing -> Left [msg]
  Right (Just a) -> Right a
  Left msg  -> Left msg

-- Ident.identifierUnderCursor redone
renameScoped_ (t :: [T]) line col =
  Right (zipper t)
    & within (traversed . preorder) ? "empty tree"
    & jerkTo line ? [fmt|can't jump to line:{line}|]
    & within (n . T.text) ? "empty n . T.text"
    & jerkTo col ? [fmt|can't jump to col:{col}|]
    <&> collectLRIdent t

collectLRIdent t0 z =
  ( identBindingLine,
    ident,
    applyRename
  )
  where
    ident = dropWhile isDigit $ unfoldr leftChars z `revAppend` (maybeToList (midChar z) ++ unfoldr rightChars z)
    identBindingLine = maybe (Left ["ident:" ++ ident ++ " not in bindline" ++ show (upward z ^? focus)]) Right 
        $ upward z ^? focus . bindline . ix (T.pack ident)
    applyRename renameLine = do
      identBindingLine <&> \jTarget ->
        let sameBinding u = u^?bindline . ix (T.pack ident) == Just jTarget
        -- linear search by lines...
        in fromTrees $ transformOn (traversed . filtered sameBinding) (n %~ renameLine) t0

revAppend xs ys = foldl (flip (:)) ys xs

midChar z = do
  f <- z ^? focus
  guard (isAlphaNum f || f == '_')
  Just f

leftChars z = do
  z <- leftward z
  f <- z ^? focus
  guard (isAlphaNum f || f == '_')
  Just (f, z)

rightChars z = do
  z <- rightward z
  f <- z ^? focus
  guard (isAlphaNum f || f == '_')
  Just (f, z)

-- find ident based on line/col in the [T]
-- go up looking at _binds
-- search/replace lines below unless there's another _binds
-- Control.Zipper

-- Data.List.break but the left list is reversed
breakRev :: (a -> Bool) -> [a] -> ([a], [a])
breakRev f (x : xs)
  | f x = ([x], xs)
  | otherwise = breakRev f xs & _1 %~ (x :)
breakRev _ _ = ([], [])

addBinds :: [T] -> [T]
addBinds = map addBinds1

addBinds1 :: T -> T
addBinds1 = \case
  T0 {..} -> T0 {_binds = boundVars _n, _ns = map addBinds1 _ns, ..}
  x -> x

boundVars :: Text -> [Text]
boundVars x =
  x
    ^.. adjoin 
          [regex|\(([^)]*)\)\s*:+=|]                     -- f (x,y) :=
          [regex|(?:(?:block|lambda)\s*\(\[([^]]*))*\]|] -- lambda([x,y],
      . groups
      . traversed
      . to (T.splitOn ",")
      . traversed
      . filtered (not . T.null)
      . to T.strip

testMatchBinds :: IO Bool
testMatchBinds = do
  let bv = boundVars "x : block([a, b123, x_], [zz, ww]);"
  return (bv == T.words "a b123 x_")

testFunctionParams :: IO Bool
testFunctionParams = do
  let bv = boundVars "f (x,y,z) := block([a, b123, x_], a : 3, zz : 4);"
  return (bv == T.words "x y z a b123 x_")

fromTrees :: [T] -> Text
fromTrees = T.concat . map unTree1

unTree1 :: T -> Text
unTree1 NT {_n} = _n <> "\n"
unTree1 T0 {_n, _ns} = _n <> "\n" <> fromTrees _ns

testNparse :: IO Bool
testNparse = do
  let orig = "a\n b\n\n c\nd\n  e\n f\ng\n h\n  i\n"
  let gs = toTrees orig
  mapM_ print (T.lines orig)
  mapM_ print (T.lines (fromTrees gs))
  -- let expected =
  --       [ T 0 "a" [T 1 "b" [], T 0 "" [], T 1 "c" []],
  --         T 0 "d" [T 2 "e" [], T 1 "f" []],
  --         T 0 "g" [T 1 "h" [T 2 "i" []]]
  --       ]
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

-- TODO trim /* */ from a?
groupLR (Left a : Right b : xs) = (a, b) : groupLR xs
groupLR (Left _ : xs) = groupLR xs
groupLR (Right b : xs) = ("", b) : groupLR xs
groupLR [] = []

commentedVars ::
  -- | .mac file contents
  Text ->
  -- | @(leading comment, (ident, isFunction))@
  [(Text, (String, Bool))]
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
