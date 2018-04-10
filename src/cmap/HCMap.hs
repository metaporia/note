{-|
Module : HCMap
Description : Wrapper around 'Data.Map', mapping 'Crypto.Digest' alg to 'CMap.Content' alg c.

* Currently uses Data.Map.Lazy
* Attempts to uphold hash equivalency of hash to blob turned hash to @[hash]@
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module HCMap where

import Prelude hiding (lookup)
import Data.Maybe (fromJust)
import Data.Monoid ((<>))

import Crypto.Hash --(hashWith, hash, SHA1(..), SHA256(..))

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Data.ByteString.Conversion
import qualified Data.ByteArray as B

import qualified Data.Text as T

import qualified Data.Map as M
import Data.String (IsString)

import Content
import Blob
import Helpers
import Select 
import IdList

exampleHashWith :: ByteString -> IO ()
exampleHashWith msg = do
    putStrLn $ " sha1(" ++ show msg ++ ") = " ++ show (hashWith SHA1 msg)
    putStrLn $ " sha256(" ++ show msg ++ ") = " ++ show (hashWith SHA256 msg)

main = exampleHashWith "hello world"


newtype HCMap alg c = Map { hcmap :: M.Map (Digest alg) (Content alg c) } deriving (Eq)

--(HashAlgorithm alg, Monoid c, Eq c, ToByteString c, Selectable c) => 
type HCMContent c = (Monoid c, Eq c, ToByteString c, Selectable c)
type HashAlg alg = HashAlgorithm alg

instance Monoid (HCMap alg c) where
    mempty = Map mempty
    mappend (Map m) (Map m') = Map (m `mappend` m')

instance Functor (HCMap alg) where
    fmap f (Map m) = Map (M.map (fmap f) m)

instance (Show c, Show alg) => Show (HCMap alg c) where
    show (Map m) = "Map { " ++ m' ++ "}"
        where m' = show $ map showAbbrev (M.toList m)
              showAbbrev (k, v) = (take 7 $ show k, v)

empty :: HCMap hash c
empty = Map M.empty

-- | Hashes value of type c, interior of 'Blob'. Inserts 'Content hash c'
-- returns Nothing if called on an 'IdList' as this requires hash-equivalence
-- checking; as a result this is not a high level function, total only by
-- virtue of 'Maybe'.
insertBlob :: forall alg c. (HashAlg alg, HCMContent c) => 
    Content alg c -> HCMap alg c -> Maybe (HCMap alg c, Digest alg)
insertBlob (AnIdList _) m = Nothing
insertBlob content@(ABlob (Blob c)) m = Just $ (Map $ M.insert hash' content map', hash')
    where hash' :: Digest alg
          hash' = hash (toByteString' c)
          map' = hcmap m

-- | Replaces the value attached to param 'hash' with the given 'Content alg c'
-- if it is of the 'AnIdList' variant and returns Just the new map-- otherwise
-- returns Nothing.
insertIdList :: (HashAlg alg, HCMContent c) =>
    Digest alg -> Content alg c -> HCMap alg c -> Maybe (HCMap alg c) 
insertIdList _ (ABlob _) _ = Nothing
insertIdList hash idl@(AnIdList _) mapp = Just map'
    where map' = Map $ M.insert hash idl m
          m = hcmap mapp

-- | Like 'insertIdList' -- nearly identical, in fact --, but checks that the
-- hash of the dereffed 'IdList' equals the given hash; that is, barring hash
-- collision, the derefable content associated with any key/hash *may not
-- change*. Only the representation may change.
replaceBlob :: forall alg c. (HashAlg alg, HCMContent c) 
            => Digest alg -> Content alg c -> HCMap alg c -> Maybe (HCMap alg c)
replaceBlob _ (ABlob _) _ = Nothing
replaceBlob hash idl@(AnIdList (IdList ids)) mapp = 
    case hashEqHuh of 
      True -> insertIdList hash idl mapp
      False -> Nothing
    where hashEqHuh = deref mapp hash == (derefIds mapp ids)

-- | Given __any__ @'Content.Content' alg c@ instance, 'deref' will return a
-- @'Maybe' c@; a worst case traversal has an arbitrary runtime--O(n) or
-- worse--due to the n-tree structure of 'HCMap'.
--
-- NB: Hash collisions may corrupt the return value--__beware__!
deref :: (HashAlg alg, Monoid c)
           => HCMap alg c -> Digest alg -> Maybe c
deref mapp hash = 
    case lookup mapp hash of
      Just (ABlob (Blob content)) -> Just content
      Just (AnIdList (IdList ids)) -> mconcat $ map derefIds ids
          where derefIds id = deref mapp id 
      Nothing -> Nothing

derefIds :: (Monoid c, HashAlg alg) =>
    HCMap alg c -> [Digest alg] -> Maybe c
derefIds mapp ids = mconcat $ map derefId ids
    where derefId id = deref mapp id 

--insert' :: (ToByteString c) => Content alg c -> HCMap alg c -> Maybe (HCMap alg c, Digest alg)
--insert' (AnIdList _) m = Nothing
--insert' content@(ABlob (Blob c)) m = Just $ (Map $ M.insert hash' content map', hash')
--    where hash' = hash $ toByteString c
--          map' = hcmap m

lookup :: HCMap alg c -> Digest alg -> Maybe (Content alg c)
lookup mapp hash = M.lookup hash (hcmap mapp)

emptySHA1 :: HCMap SHA1 c
emptySHA1 = empty

-- | Takes a 'Content alg id' and if `ABlob` then splits the given blob
-- according to the 'Selection'. Returns `Nothing` if variant is `AnIdList`.
-- How should the map be mutated? the value pointed to by id will be replaced
-- with the `IdList` of the chunks of the 'Blob' param.
-- error handling?
selectFromBlob :: forall alg c. (HashAlg alg, HCMContent c) =>
    Digest alg -> Selection -> HCMap alg c-> Maybe (HCMap alg c)
selectFromBlob id s m = 
    case lookup m id of
      Just (ABlob (Blob c)) -> do 
          content <- mContent
          map' <- mMap
          replaceBlob id content map'

          where chunks = toList $ sel c s
                inserts = sequence $ map (\b-> insertBlob (toBlob b) m) chunks
                mMapNIds = foldr (\(m, h) (m', hs) -> (m <> m', h:hs)) (empty, []) <$> inserts
                mMap :: Maybe (HCMap alg c)
                mMap = fmap fst mMapNIds
                mContent :: Maybe (Content alg c)
                mContent = fmap (toIdList . snd) mMapNIds
      Just (AnIdList _) -> Nothing
      Nothing -> Nothing 



