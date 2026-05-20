{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

{- HLINT ignore "Use fmap" -}

-- | Description : pun-style quasiquoter for Aeson 'Value' / instances of FromJSON
--
-- Adapts the parser from "Data.HList.RecordPuns"; field names are plain
-- Haskell identifiers (fine for LSP, where every key is lowercase ASCII).
--
-- [@patterns@]
--
-- >>> :set -XQuasiQuotes -XViewPatterns
-- >>> Just [json| method params |] = decode "{\"method\":\"hi\",\"params\":42}"
-- >>> (method, params)
-- (Just (String "hi"), Just (Number 42.0))
--
-- Nested objects:
--
-- >>> Just [json| params{textDocument{uri} position{line character}} |] = decode "..."
-- -- uri, line, character :: Maybe Value
--
-- a leading underscores means the json field is accessed but the value is not bound
-- to a haskell variable.
--
-- Array fields use @[ ]@:
--
-- >>> Just [json| result[a b] |] = decode "{\"result\":[1,2]}"
-- -- a, b :: Value
--
-- TODO: extra fields are always accepted
-- TODO: no way to get b :: Maybe Value
-- TODO: avoid Pun.nth instead of
-- (\ (jsonArray -> a_ahi5) -> Just [(nth a_ahi5 0 >>= jsonGet "text")]))) -> Just [Just text]
-- (jsonArray -> Just [jsonGet "text" -> Just text])
--
-- expressions not tested/used
--
-- [@expressions@]
--
-- >>> let method = String "textDocument/completion"; id_ = Number 1
-- >>> [json| id_ method |]
-- Object (fromList [("id_",Number 1.0),("method",String "textDocument/completion")])
--
-- Nested construction:
--
-- >>> let uri = String "file:///foo.hs"; line = Number 0
-- >>> [json| params{textDocument{uri} position{line}} |]
-- Object (fromList [("params",Object (fromList [("textDocument",...),("position",...)]))])
--
-- Merge with @\@@  (expression only):
--
-- >>> let extra = object ["a" .= (1::Int)]
-- >>> [json| extra@{ b } |]   -- {a:1, b:b} .<>. extra
module Pun
  ( json,

    -- * names in generated splices
    -- $suggestion
    -- import Pun (json)
    -- unless you're looking at ghc -ddump-splices
    Value,
    jsonGet,
    tupleE,
    jsonArray,
    (^?),
    _Just,
    _JSON,
    ix,
    (&),
    nth,
  )
where

import Control.Lens hiding (contains, inside, (.=))
import Control.Monad
import Control.Monad.Trans.Reader
import Data.Aeson (FromJSON, Result (..), ToJSON, Value (..), fromJSON, object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Lens hiding (nth)
import Data.List (isPrefixOf, stripPrefix, unfoldr)
import Data.Maybe
import qualified Data.Vector as V
import Language.Haskell.TH
import Language.Haskell.TH.Quote

-- | In this section define types to simplify view pattern construction
-- whereas template-haskell defines Exp and ExpQ = Q Exp
-- we define EP and EPM = M EP
--
-- >  M t = Tag -> t
-- >   EP = M EP_
-- >   EP_ ~ (ExpQ, Maybe PatQ)
-- >   EP = Tag -> (ExpQ, Maybe PagQ)
type M = Reader Tag

data Tag
  = -- | @{ }@  → JSON mx curly
    C
  | -- | @[ ]@  array
    A
  | -- | @( )@  → JSON Just x next free lettter
    D
  deriving (Show, Eq)

-- | EP expression pattern `data Pat = ViewP Exp Pat | ...`
--
-- with a leading _, `_foo` store Nothing instead of wildP
data EP = EP ExpQ (Maybe PatQ)

type EPM = M EP

makeFields ''EP

ep0 :: ExpQ -> EP
ep0 e = EP e Nothing

ep1 :: ExpQ -> PatQ -> EP
ep1 e p = EP e (Just p)

epm0 :: ExpQ -> EPM
epm0 = pure . ep0

-- could be a pattern synonym
epm :: ExpQ -> PatQ -> EPM
epm e p = pure (ep1 e p)

tupleE f g x = Just (f x, g x)

-- | horizontal
instance Semigroup EP where
  EP e Nothing <> EP f q = EP (e >> f) q
  EP e p <> EP f Nothing = EP (f >> e) p
  EP e (Just p) <> EP f (Just q) = ep1 [|($e `tupleE` $f)|] [p|Just ($p, $q)|]

instance Monoid EP where
  mempty = EP [|id|] Nothing

instance Semigroup EPM where
  a <> b = liftA2 (<>) a b

instance Monoid EPM where
  mempty = pure mempty

-- | vertical
contains_ :: EP -> EP -> EP
contains_ (EP f Nothing) (EP g q) = EP [|$f >=> $g|] q
contains_ (EP f (Just p)) (EP g (Just q)) = ep1 [|\v -> let fv = $f v in (fv, fv >>= $g)|] [p|($p, $q)|]
contains_ fp (EP g Nothing) = fp

infixr 6 `contains_`

contains :: EPM -> EPM -> EPM
contains = liftA2 contains_

nth :: (FromJSON a, ToJSON a, AsJSON a) => Maybe [a] -> Int -> Maybe a
nth a i = a ^? _Just . ix i . _JSON

epList :: ExpQ -> [EPM] -> EPM
-- epList e [f] = ep0 e `contains` f
epList e esps = do
  (es, ps) <- unzip <$> sequenceEPs esps
  let n = length es
  epm
    [|
      \($e -> a) ->
        Just
          $( listE
               [ [|nth a i >>= $e|]
                 | (i, e) <- zip [0 :: Int ..] es
               ]
           )
      |]
    [p|Just $(listP ps)|]

sequenceEPs :: [EPM] -> M [(ExpQ, PatQ)]
sequenceEPs eps = do
  s <- ask
  pure $ mapMaybe (_2 id . epTuple . (`runReader` s)) eps

epTuple :: EP -> (ExpQ, Maybe PatQ)
epTuple (EP a b) = (a, b)

-- ── Runtime helpers ───────────────────────────────────────────────────────────

-- | Look up a field in an Object, returning 'Nothing' for any other 'Value'.
jsonGet :: (FromJSON a, ToJSON a) => String -> Value -> Maybe a
jsonGet k o = o ^? key (Key.fromString k) . _JSON

jsonArray :: Value -> Maybe [Value]
jsonArray (Array vec) = Just (V.toList vec)
jsonArray _ = Nothing

-- | Left-biased shallow merge of two Objects (non-Objects pass through as-is).
jsonMerge :: Value -> Value -> Value
jsonMerge (Object a) (Object b) = Object (a <> b)
jsonMerge a _ = a

-- ── Parser (verbatim from Data.HList.RecordPuns) ──────────────────────────────
data Tree
  -- | branch tagged with the parenthesis type
  = B Tag [Tree]
  | -- | variable / label
    V String
  deriving (Show)

ppTree :: Tree -> String
ppTree (B tag ts) = wrap tag $ unwords (map ppTree ts)
  where
    wrap = \case
      C -> \inside -> "{" ++ inside ++ "}"
      A -> \inside -> "[" ++ inside ++ "]"
      D -> \inside -> "[" ++ inside ++ "]"
ppTree (V x) = x

parseRec :: String -> Tree
parseRec str = case parseRec' 0 0 0 [[]] (lexing str) of
  [x] -> x
  xs -> B C (reverse xs)

parseRec' :: Int -> Int -> Int -> [[Tree]] -> [String] -> [Tree]
parseRec' n m o acc         ("{" : rest) = parseRec' (n + 1) m o ([] : acc) rest
parseRec' n m o acc         ("(" : rest) = parseRec' n (m + 1) o ([] : acc) rest
parseRec' n m o acc         ("[" : rest) = parseRec' n m (o + 1) ([] : acc) rest
parseRec' n m o (a : b : c) ("}" : rest) = parseRec' (n - 1) m o ((B C (reverse a) : b) : c) rest
parseRec' n m o (a : b : c) (")" : rest) = parseRec' n (m - 1) o ((B D (reverse a) : b) : c) rest
parseRec' n m o (a : b : c) ("]" : rest) = parseRec' n m (o - 1) ((B A (reverse a) : b) : c) rest
parseRec' n m o (b : c) (a : rest)
  | a `notElem` ["{", "}", "(", ")"] = parseRec' n m o ((V a : b) : c) rest
parseRec' 0 0 0 (a : _) [] = a
parseRec' _ _ o acc e =
  error $
    "Data.Aeson.QQ.Pun.parseRec': unexpected "
      ++ show e
      ++ "\nparsed: "
      ++ show (map (map ppTree) (reverse acc))

lexing :: String -> [String]
lexing = unfoldr $ \v -> case lex v of
  ("", "") : _ -> Nothing
  tok : _ -> Just tok
  _ -> Nothing

-- ── Quasiquoter ───────────────────────────────────────────────────────────────

-- | Pun-style quasiquoter for Aeson 'Value'.
--
-- Curly braces @{ }@ denote optional Object fields (ie. field names FromJSON a => Maybe a);
-- parentheses @( )@ mandatory Object fields (ie. denote Array
-- elements.  A bare identifier list implicitly gets @{ }@-wrapping, matching
-- "Data.HList.RecordPuns" behaviour.
json :: QuasiQuoter
json =
  QuasiQuoter
    { quoteExp = jExp . addRoot . parseRec,
      quotePat = jPat . addRoot . parseRec,
      quoteDec = error "Data.Aeson.QQ.Pun.quoteDec: not supported",
      quoteType = error "Data.Aeson.QQ.Pun.quoteType: not supported"
    }

-- | Wrap a bare @V x@ in @C [V x]@ (implicit braces at top level).
addRoot :: Tree -> Tree
addRoot (V a) = B C [V a]
addRoot t = t

-- ── Expression ────────────────────────────────────────────────────────────────
jExp :: Tree -> ExpQ
jExp (B C as) = [|object $(listE (map mkPair (jMes as)))|]
  where
    mkPair (l, e) = [|Key.fromString $(litE (stringL l)) .= $e|]
jExp (B D as) = [|toJSON $(listE (map jExpElem as))|]
  where
    jExpElem (V a) = varE (mkName a)
    jExpElem t = jExp t
jExp (V a) = varE (mkName a) -- should only appear inside mes

jMes :: [Tree] -> [(String, ExpQ)]
jMes (V a : V "@" : b : rest) = (a, [|jsonMerge $(jExp b) $(varE (mkName a))|]) : jMes rest
jMes (V a : B t b : rest) = (a, jExp (B t b)) : jMes rest
jMes (V a : rest) = (a, varE (mkName a)) : jMes rest
jMes [] = []
jMes inp = error $ "Data.Aeson.QQ.Pun.jMes: unexpected: " ++ show (map ppTree inp)

-- ── Pattern ───────────────────────────────────────────────────────────────────
jPat :: Tree -> PatQ
jPat tree = case jPat1 tree `runReader` C of
  EP e (Just p) -> [p|($e -> $p)|]
  _ -> wildP

jPat1 :: Tree -> EPM
jPat1 (V (unesc -> EV wasOdd a))
  | wasOdd = epm0 [|jsonGet @Value a|] -- wildP
  | otherwise = do
    tag <- ask
    let e = if tag == A then [|Just|] else [|jsonGet a|]
        addJust
            | tag /= D = \r -> [p| Just $r |]
            | otherwise = id
    epm e (addJust (varP (mkName a)))
jPat1 (B C (V a : B cd xs : rest)) =
  let ma    = jPat1 (V a)
      mxs   = jPat1 (B cd xs)
      mrest = jPat1 (B C rest)
   in enter cd $ (ma `contains` mxs) <> mrest
jPat1 (B t xs) = enter t $ case t of
  A -> epList [|jsonArray|] $ map jPat1 xs
  _ -> foldMap jPat1 xs

data EV = EV { wasOdd :: Bool, str :: String }

-- | unescaping
--
-- given @2 * n + k@ leading underscores (k is 0 or 1),
-- EV (k==1) (n leading underscores)
--
-- > __ -> _
-- > _x -> x
unesc :: String -> EV
unesc a
  | (underscores, xs) <- span (=='_') a,
    let n = length underscores,
    even n
      = EV False (replicate (div n 2) '_' ++ xs)
  | Just a <- stripPrefix "_" a = EV True a
  | otherwise = EV False a

enter t = local (const t)
