{-|
Module: VMap
Description: Provides a wrapper around
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module VMap ( VMap
            , VMapInternal
            , VMVal, HashAlg
            , empty, emptySHA1
            , deref
            , deref'
            , lookup, insertRawBlob, insertRawSpan
            , insert
            , appVM
            , show'
            , SelVMap, newSelVMap
            , registerSpanInsertion
            , pprintSVM
            , locateIdx
            ) where

-- TODO:
-- □  Impl 'VMap' interface to hide the backend. As persistence will become more
-- desirable in the late stages of prototyping, we will neeed to insulate those
-- parts of the internals that can be decoupled from the value-map persistence
-- logic from unnecessary breakage.

import Val

import Prelude hiding (lookup)

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import qualified Data.ByteArray as B

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Data.ByteString.Conversion
import Data.String (IsString)

import Data.Monoid ((<>))
import Data.Maybe (fromJust, catMaybes)
import Data.List (sortBy)

import Crypto.Hash
--import Crypto.Hash.Types 
import Data.Byteable (toBytes, Byteable)
import Select
import Helpers (Key)

newtype VMap alg c = VMap { getVVMap :: M.Map (Key alg) (Val alg c) } deriving (Eq)

type VMapInternal alg c = M.Map (Key alg) (Val alg c)

type VMVal c = (Monoid c, Eq c, ToByteString c, Selectable c)
type HashAlg alg = HashAlgorithm alg

instance Monoid (VMap alg c) where
    mempty = VMap mempty
    mappend (VMap m) (VMap m') = VMap (m <> m')

instance Functor (VMap alg) where
    fmap f (VMap m) = VMap (M.map (fmap f) m)

instance (Show c, Show alg) => Show (VMap alg c) where
    show (VMap m) = "VMap { " ++ m' ++ "}"
        where m' = show $ map showAbbrev (M.toList m)
              showAbbrev (k, v) = (take 7 $ show k, v)


show' :: forall alg c. (HashAlg alg, Show c, Show alg) 
      => VMap alg c -> String
show' (VMap m) = "VMap:\n" ++  ( go $ M.toList m)
    where go :: [(Key alg, Val alg c)] -> String
          go [] = []
          go ((k, v):xs) = take 7 (show k) ++ " -> " 
                        ++ take 35 v' ++ rest
                        ++ "\n"
                        ++ go xs
                            where l = length v'
                                  v' = show v
                                  rest = if l > 35
                                            then "..."
                                            ++ drop (l - 35) v'
                                            else ""

empty :: VMap alg c
empty = VMap M.empty

appVM :: (M.Map (Key alg) (Val alg c) -> a) -> VMap alg c -> a
appVM f (VMap m) = f m

toVMap :: (M.Map (Key alg) (Val alg c) -> a) -> VMap alg c -> a
toVMap f (VMap m) = f m

deref :: (HashAlg alg, VMVal c)
      => VMap alg c -> Key alg -> Maybe c
deref m k = case lookup m k of
              Just (Blob _ b) -> Just b
              Just (Span key sel') -> 
                  deref m key >>= return . getSel . flip sel sel'
              _ -> Nothing

deref' :: (HashAlg alg, VMVal c)
       => VMap alg c -> Key alg -> Either String c
deref' m k = case lookup m k of
              Just (Blob _ b) -> Right b
              Just (Span key sel') -> 
                  case deref m key >>= return . getSel . flip sel sel' of
                    Just c -> Right c
                    Nothing -> Left "span inner key note found in vmap"
              _ -> Left "key not found in vmap"



lookup :: VMap alg c -> Key alg -> Maybe (Val alg c)
lookup (VMap m) key = M.lookup key m

emptySHA1 :: VMap SHA1 c
emptySHA1 = empty

-- | For internal use. Takes a blob, lifts it into a 'Val', generates a 'Key',
-- and inserts (key, val) into 'VMap'.
insertRawBlob :: forall c alg. (VMVal c, HashAlg alg)
              => c -> VMap alg c -> (VMap alg c, Maybe (Key alg))
insertRawBlob c (VMap m) = 
    let val :: Val alg c
        val = mkBlob c 
     in insert val (VMap m)


-- | PARTIAL FUNCTION --- BEWARE
insert :: (VMVal c, HashAlg alg)
       => Val alg c -> VMap alg c -> (VMap alg c, Maybe (Key alg))
insert val@(Blob l c) (VMap m) =
    let key = hash (toByteString' c)
        m' = M.insert key val m
     in (VMap m', Just key)
insert (Span k s) m = case insertRawSpan k s m of
                        Just (m', k') -> (m', Just k')
                        Nothing -> (m, Nothing)

-- | Applies selection to @deref m k@, hashes result, inserts @(key', Span key
-- sel)@, and returns the updated map, and the 'Key' of the inserted 'Span'.
-- 
-- The 'Maybe' in the return type is due to the (potentially) arbitrary
-- recursion performed by deref and/or the fact that cycles, orphaned-'Span's
-- are not prohibited, per se, by the current set of types.
insertRawSpan :: forall c alg. (VMVal c, HashAlg alg)
              => Key alg -> Selection -> VMap alg c -> Maybe (VMap alg c, Key alg)
insertRawSpan k s (VMap m) = do
    c <- deref (VMap m) k
    let sel' = getSel . sel c $ s
        k' = hash (toByteString' sel')
        m' = VMap . M.insert k' (mkSpan k s) 
    return (m' m, k')


-- SelVMap

newtype SelVMap alg = 
    SelVMap { getSelVMap :: M.Map (Key alg) [Key alg] } deriving (Eq, Show)

pprintSVM :: (HashAlg alg) 
          => SelVMap alg -> String
pprintSVM (SelVMap m) = "SelVMap\n" ++ (go $ M.toList m)
    where go [] = []
          go ((k, ks):xs) = take 7 (show k) ++ " -> " 
                        ++ (show $ map (take 7 . show) ks)
                        ++ "\n"
                        ++ go xs



newSelVMap :: SelVMap alg 
newSelVMap = SelVMap M.empty

-- | Record the creation of a 'Span' in a 'SelVMap' to ensure convenient
-- lookup of keys which span a given key of some source content.
--
-- Returns 'Nothing' if passed a 'Blob' variant of 'Val'. This partiality may
-- be removed by injectiing newtypes into 'Val' variants so we can match on
-- variants and write "total" partial functions--ha!
registerSpanInsertion :: Key alg -> Val alg c->  SelVMap alg -> Maybe (SelVMap alg)
registerSpanInsertion spanKey (Span sourceKey s) (SelVMap m) = 
    case M.lookup sourceKey m of
      Just xs -> Just . SelVMap $  M.insert sourceKey (spanKey:xs) m
      Nothing -> Just . SelVMap $ M.insert sourceKey [spanKey] m
registerSpanInsertion spanKey (Blob _ _) m = Nothing



-- | Given the 'Key' of some 'Blob' and an index, return a list of keys sorted
-- smallest to largest by length.
-- 
-- Looks up 
locateIdx :: (HashAlg alg, VMVal c)
          => VMap alg c -> SelVMap alg -> Key alg -> Int -> Maybe [Key alg]
locateIdx vm (SelVMap m) k idx = 
    case M.lookup k m of
      Just ks -> let xs = catMaybes $ map (\k -> do
                           v <- lookup vm k
                           return (fit' idx v, k)) ks
                     compare' (i, k) (j, k') = compare i j
                     sorted = sortBy compare' xs
                  in Just $ map (\(i, k) -> k) sorted
      Nothing -> Nothing


fit' idx (Blob l _) = l
fit' idx (Span _ (Sel s e)) = e - s
-- | Assess the "fit" of a 'Val' w.r.t. a given index, which index is
-- /relative/ to the blob--it is __not__ a global index.
-- 
-- TODO: This "algorithm" desperately needs improvement.
-- USELESS, a verbose length function

fit :: Int -> Val alg c-> Maybe Int
fit i (Blob l b) 
  | i > l = Nothing
  | i < 0 = Nothing
  | otherwise = 
      let (before, after) = (abs i, abs $ l - i)
       in Just $ before + after
fit i (Span _ (Sel s e)) = 
    let (before, after) = (abs i, abs $ (e - s) - i)
     in Just $ before + after



