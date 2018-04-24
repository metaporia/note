{-|
Module: Map
Description: Provides a wrapper around
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Map ( Map
           , MapInternal
           , MVal, HashAlg
           , empty, emptySHA1
           , deref
           , lookup, insertRawBlob, insertRawSpan
           , insert
           ) where

-- TODO:
-- □  Impl 'Map' interface to hide the backend. As persistence will become more
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
import Data.Maybe (fromJust)

import Crypto.Hash
import Select
import Helpers (Key)


newtype Map alg c = Map { getMap :: M.Map (Key alg) (Val alg c) } deriving (Eq)

type MapInternal alg c = M.Map (Key alg) (Val alg c)

type MVal c = (Monoid c, Eq c, ToByteString c, Selectable c)
type HashAlg alg = HashAlgorithm alg

instance Monoid (Map alg c) where
    mempty = Map mempty
    mappend (Map m) (Map m') = Map (m <> m')

instance Functor (Map alg) where
    fmap f (Map m) = Map (M.map (fmap f) m)

instance (Show c, Show alg) => Show (Map alg c) where
    show (Map m) = "Map { " ++ m' ++ "}"
        where m' = show $ map showAbbrev (M.toList m)
              showAbbrev (k, v) = (take 7 $ show k, v)

empty :: Map alg c
empty = Map M.empty

toMap :: (M.Map (Key alg) (Val alg c) -> a) -> Map alg c -> a
toMap f (Map m) = f m

deref :: (HashAlg alg, MVal c)
      => Map alg c -> Key alg -> Maybe c
deref m k = case lookup m k of
              Just (Blob _ b) -> Just b
              Just (Span key sel') -> 
                  deref m key >>= return . getSel . flip sel sel'
              _ -> Nothing


lookup :: Map alg c -> Key alg -> Maybe (Val alg c)
lookup (Map m) key = M.lookup key m

emptySHA1 :: Map SHA1 c
emptySHA1 = empty

-- | For internal use. Takes a blob, lifts it into a 'Val', generates a 'Key',
-- and inserts (key, val) into 'Map'.
insertRawBlob :: forall c alg. (MVal c, HashAlg alg)
              => c -> Map alg c -> (Map alg c, Key alg)
insertRawBlob c (Map m) = 
    let val :: Val alg c
        val = mkBlob c 
     in fromJust $ insert val (Map m)


-- | PARTIAL FUNCTION --- BEWARE
insert :: (MVal c, HashAlg alg)
           => Val alg c -> Map alg c -> Maybe (Map alg c, Key alg)
insert val@(Blob l c) (Map m) =
    let key = hash (toByteString' c)
        m' = M.insert key val m
     in Just (Map m', key)
insert (Span k s) m = insertRawSpan k s m

-- | Applies selection to @deref m k@, hashes result, inserts @(key', Span key
-- sel)@, and returns the updated map, and the 'Key' of the inserted 'Span'.
-- 
-- The 'Maybe' in the return type is due to the (potentially) arbitrary
-- recursion performed by deref and/or the fact that cycles, orphaned-'Span's
-- are not prohibited, per se, by the current set of types.
insertRawSpan :: forall c alg. (MVal c, HashAlg alg)
              => Key alg -> Selection -> Map alg c -> Maybe (Map alg c, Key alg)
insertRawSpan k s (Map m) = do
    c <- deref (Map m) k
    let sel' = getSel . sel c $ s
        k' = hash (toByteString' sel')
        m' = Map . M.insert k' (mkSpan k s) 
    return (m' m, k')

-- SelMap

newtype SelMap alg = 
    SelMap { getSelMap :: M.Map (Key alg) [Key alg] } deriving (Eq, Show)

-- | Record the creation of a 'Span' in a 'SelMap' to ensure convenient
-- lookup of keys which span a given key of some source content.
--
-- Returns 'Nothing' if passed a 'Blob' variant of 'Val'. This partiality may
-- be removed by injectiing newtypes into 'Val' variants so we can match on
-- variants and write "total" partial functions--ha!
registerSpanInsertion :: Key alg -> Val alg c->  SelMap alg -> Maybe (SelMap alg)
registerSpanInsertion spanKey (Span sourceKey s) (SelMap m) = 
    case M.lookup sourceKey m of
      Just xs -> Just . SelMap $  M.insert sourceKey (spanKey:xs) m
      Nothing -> Just . SelMap $ M.insert sourceKey [spanKey] m
registerSpanInsertion spanKey (Blob _ _) m = Nothing
