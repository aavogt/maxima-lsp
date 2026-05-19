{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Description : pun-style quasiquoter for Aeson 'Value'
--
-- Reuses the parser from "Data.HList.RecordPuns"; field names are plain
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
-- Array fields use @( )@:
--
-- >>> Just [json| result(a b) |] = decode "{\"result\":[1,2]}"
-- -- a, b :: Value
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

    -- * Helpers used in generated splices
    jsonGet,
    jsonMerge,
  )
where

import Control.Lens hiding ((.=))
import Data.Aeson (FromJSON, Result (..), ToJSON, Value (..), fromJSON, object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Lens
import Data.List (isPrefixOf, unfoldr)
import qualified Data.Vector as V
import Language.Haskell.TH
import Language.Haskell.TH.Quote

makePrisms ''Result

-- ── Runtime helpers ───────────────────────────────────────────────────────────

-- | Look up a field in an Object, returning 'Nothing' for any other 'Value'.
jsonGet :: (FromJSON a, ToJSON a) => String -> Value -> Maybe a
jsonGet k o = o ^? key (Key.fromString k) . _JSON

-- | Left-biased shallow merge of two Objects (non-Objects pass through as-is).
jsonMerge :: Value -> Value -> Value
jsonMerge (Object a) (Object b) = Object (a <> b)
jsonMerge a _ = a

-- ── Parser (verbatim from Data.HList.RecordPuns) ──────────────────────────────

data Tree
  = -- | @{ }@  → JSON mx
    C [Tree]
  | -- | @( )@  → JSON Just x
    D [Tree]
  | -- | @[ ]@  array
    A [Tree]
  | -- | variable / label
    V String
  deriving (Show)

ppTree :: Tree -> String
ppTree (C ts) = "{" ++ unwords (map ppTree ts) ++ "}"
ppTree (D ts) = "(" ++ unwords (map ppTree ts) ++ ")"
ppTree (V x) = x

parseRec :: String -> Tree
parseRec str = case parseRec' 0 0 0 [[]] (lexing str) of
  [x] -> x
  xs -> C (reverse xs)

parseRec' :: Int -> Int -> Int -> [[Tree]] -> [String] -> [Tree]
parseRec' n m o acc ("{" : rest) = parseRec' (n + 1) m o ([] : acc) rest
parseRec' n m o acc ("(" : rest) = parseRec' n (m + 1) o ([] : acc) rest
parseRec' n m o acc ("[" : rest) = parseRec' n m (o + 1) ([] : acc) rest
parseRec' n m o (a : b : c) ("}" : rest) = parseRec' (n - 1) m o ((C (reverse a) : b) : c) rest
parseRec' n m o (a : b : c) (")" : rest) = parseRec' n (m - 1) o ((D (reverse a) : b) : c) rest
parseRec' n m o (a : b : c) ("]" : rest) = parseRec' n m (o - 1) ((A (reverse a) : b) : c) rest
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
addRoot (V a) = C [V a]
addRoot t = t

-- ── Expression ────────────────────────────────────────────────────────────────

jExp :: Tree -> ExpQ
jExp (C as) = [|object $(listE (map mkPair (jMes as)))|]
  where
    mkPair (l, e) = [|Key.fromString $(litE (stringL l)) .= $e|]
jExp (D as) = [|toJSON $(listE (map jExpElem as))|]
  where
    jExpElem (V a) = varE (mkName a)
    jExpElem t = jExp t
jExp (V a) = varE (mkName a) -- should only appear inside mes

jMes :: [Tree] -> [(String, ExpQ)]
jMes (V a : V "@" : b : rest) = (a, [|jsonMerge $(jExp b) $(varE (mkName a))|]) : jMes rest
jMes (V a : C b : rest) = (a, jExp (C b)) : jMes rest
jMes (V a : D b : rest) = (a, jExp (D b)) : jMes rest
jMes (V a : rest) = (a, varE (mkName a)) : jMes rest
jMes [] = []
jMes inp = error $ "Data.Aeson.QQ.Pun.jMes: unexpected: " ++ show (map ppTree inp)

-- ── Pattern ───────────────────────────────────────────────────────────────────

-- | Object pattern: view function extracts a tuple of @Maybe Value@,
-- one per non-wildcard field.
jpatCD f as = do
  v <- newName "v"
  let (extracts, ps) = unzip (jMpsFlat [|Just $(varE v)|] as)
  viewP (lamE [varP v] (tupE extracts)) (tupP (map f ps))

jPat :: Tree -> PatQ
jPat (C as) = jpatCD id as
jPat (D as) = jpatCD (\q -> [p|Just $q|]) as
jPat (A as) =
  let ps = jArrMps as
   in viewP
        [|\v -> case v of Array vec -> Just (V.toList vec); _ -> Nothing|]
        [p|Just $(listP ps)|]
jPat (V a) = varP (mkName a)

-- | Each entry is @(extractExp, patQ)@ where patQ matches @Maybe Value@.
-- The extractExp produces a @Maybe Value@ for the corresponding field.
jMpsFlat :: ExpQ -> [Tree] -> [(ExpQ, PatQ)]
jMpsFlat base (V "_" : rest) = (base >> [|const Nothing|], wildP) : jMpsFlat base rest
jMpsFlat base (V a : C b : rest) =
  let (field, discard) = stripDiscardSigil a
      next = [|$base >>= jsonGet $(litE (stringL field))|]
   in if discard
        then jMpsFlat next b ++ jMpsFlat base rest
        else (next, conP 'Just [jPat (C b)]) : jMpsFlat base rest
jMpsFlat base (V a : D b : rest) =
  let (field, discard) = stripDiscardSigil a
      next = [|$base >>= jsonGet $(litE (stringL field))|]
   in if discard
        then jMpsFlat next b ++ jMpsFlat base rest
        else (next, conP 'Just [jPat (D b)]) : jMpsFlat base rest
jMpsFlat base (V a : rest) =
  let (field, discard) = stripDiscardSigil a
      next = [|$base >>= jsonGet $(litE (stringL field))|]
      pat = if discard then wildP else varP (mkName a)
   in (next, conP 'Just [pat]) : jMpsFlat base rest
jMpsFlat _ [] = []
jMpsFlat _ inp = error $ "Data.Aeson.QQ.Pun.jMpsFlat: unexpected: " ++ show (map ppTree inp)

stripDiscardSigil :: String -> (String, Bool)
stripDiscardSigil name = case name of
  '_' : rest@(_ : _) -> (rest, True)
  _ -> (name, False)

-- | Each entry is a patQ matching a @Value@ (no Maybe — these are list elements).
jArrMps :: [Tree] -> [PatQ]
jArrMps (V "_" : rest) = wildP : jArrMps rest
jArrMps (V a : rest) = varP (mkName a) : jArrMps rest
jArrMps (C b : rest) = jPat (C b) : jArrMps rest
jArrMps (D b : rest) = jPat (D b) : jArrMps rest
jArrMps [] = []
jArrMps inp = error $ "Data.Aeson.QQ.Pun.jArrMps: unexpected: " ++ show (map ppTree inp)
