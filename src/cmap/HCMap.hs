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
import Data.Either (rights, lefts)
import Data.Monoid ((<>))
import Control.Monad.Except
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
import Helpers hiding (len)
import Select 
import IdList

exampleHashWith :: ByteString -> IO ()
exampleHashWith msg = do
    putStrLn $ " sha1(" ++ show msg ++ ") = " ++ show (hashWith SHA1 msg)
    putStrLn $ " sha256(" ++ show msg ++ ") = " ++ show (hashWith SHA256 msg)

main = exampleHashWith "hello world"


newtype HCMap alg c = Map { hcmap :: M.Map (Digest alg) (Content alg c) } deriving (Eq)

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

toMap f (Map m) = f m

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

-- | Given __any__ @'Content' alg c@ instance, 'deref' will return a
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

-- SELECTION LOGIC

-- | Takes a 'Content alg id' and if `ABlob` then splits the given blob
-- according to the 'Selection'. Returns `Nothing` if variant is `AnIdList`,
-- otherwise returns @Maybe (HCMap alg c, Digest alg)@.
--
-- How should the map be mutated? the value pointed to by id will be replaced
-- with the `IdList` of the chunks of the 'Blob' param.
--
-- error handling?
selectFromBlobId :: forall alg c. (HashAlg alg, HCMContent c) =>
    Digest alg -> Selection -> HCMap alg c-> Maybe (HCMap alg c, Digest alg)
selectFromBlobId id s m = 
    case lookup m id of
      Just (ABlob blob) -> selectFromBlob id blob s m
      Just (AnIdList _) -> Nothing
      Nothing -> Nothing 

-- | Nearly identical to 'selectFromBlobId', 'selectFromBlob' performs the same
-- operations--insert chunks, replace @(id, blob)@ with @(id, idList)@--, but
-- does not perform a 'lookup', is for use in functions that have already
-- fetched the relevant 'Content'.
selectFromBlob :: forall alg c. (HashAlg alg, HCMContent c) => 
    Digest alg -> Blob c -> Selection -> HCMap alg c -> Maybe (HCMap alg c, Digest alg)
selectFromBlob id (Blob c) s m = do 
          (selIdx, chunks) <- case getSelIdx $ sel c s of
                     Left _ -> Nothing
                     Right tup -> Just tup
          let inserts = sequence $ map (\b-> insertBlob (toBlob b) m) chunks
              mMapNIds = foldr (\(m, h) (m', hs) -> (m <> m', h:hs)) (empty, []) <$> inserts

          ids <- snd <$> mMapNIds
          let content = toIdList ids
          map' <- fmap fst mMapNIds
          map'' <- replaceBlob id content map'
          Just (map'', ids !! selIdx)

selectFromBlob' id (Blob c) s m = chunks
          where chunks = toList $ sel c s
                selId = id
                inserts = sequence $ map (\b-> insertBlob (toBlob b) m) chunks
                mMapNIds = foldr (\(m, h) (m', hs) -> (m <> m', h:hs)) (empty, []) <$> inserts
                mMap = fmap fst mMapNIds
                mContent = fmap (toIdList . snd) mMapNIds

-- | Takes an id/key/hash and a 'Selection'; fetches the id's attached
-- 'Content' and if its a 'Blob' then calls 'selectFromBlob', otherwise
-- contextually appropriate 'Selection''s are generated for each id the
-- 'IdList' and 'select' is called recursively on each @(id, sel)@ pair. 
-- The return value is a tuple @(map', id)@ where @map'@ is the updated 'HCMap'
-- and @id@ is the id/key/hash of the requested 'Selection'.
--
-- This may be terribly inefficient.
--select :: (HashAlg alg, HCMContent c) => Digest alg -> Selection -> HCMap alg c -> Maybe (HCMap alg c, Digest alg)
select id s mapp =
    case lookup mapp id of
      Just (ABlob b) -> selectFromBlob id b s mapp
      Just (AnIdList ids) -> Nothing -- genSels is roughly the first call needed
      Nothing -> Nothing

-- | Generates "contextually apropriate" 'Selection''s for each given
-- @hash :: Digest alg@ and returns a list of @(hash, mSel)@.
--
-- TODO:
-- □ Add cutoff with 'continue', 'classifySel', or 'blobInSel'.
-- □ Test--but how?  
genSels :: forall c alg. (Splittable c, HCMContent c, HashAlg alg) =>
    [Digest alg] -> HCMap alg c -> Selection -> [(Digest alg, Maybe Selection)]
genSels ids mapp s = go ids mapp (Just s) (Just 0)
    where go [] _ _ _ = [] 
          go (id:ids) m s lastLen = 
              let bLen :: Maybe Int
                  bLen = len <$> (deref m id)
                  s' :: Maybe Selection
                  s' = do
                      sel' <- s
                      lastLen' <- lastLen
                      return (pruneSel sel' lastLen')
                  in (id, s'):(go ids m s' bLen)

bg = "hello world the" :: T.Text
(base, h0) = fromJust $ insertBlob (toBlob bg) emptySHA1 
(mm, h1) = fromJust $ selectFromBlob h0 (Blob bg) (Sel 3 12) base
(mmm, h2) = fromJust $ selectFromBlobId h1 (Sel 0 5) mm


keys = toMap M.keys mmm
id0 = head keys
id1 = keys !! 1
id2 = keys !! 5 
id3 = keys !! 3
idl = [id0,id1,id2,id3]

gensels = genSels idl mmm (Sel 5 10)
ms = map (\(hash, s) -> selectFromBlobId hash (fromJust s) mmm) gensels
(m0, i0) = fromJust $ ms !! 0
(m1, i1) = fromJust $ ms !! 1
(m2, i2) = fromJust $ ms !! 2
(m3, i3) = fromJust $ ms !! 3
classArgs = map (\(h, mS) -> (fromJust mS, fromJust (len <$> (deref mmm h)))) gensels

inputSpace = (,,) <$> ["", "pre"] <*> ["", "sel"] <*> ["", "post"]
rets = map getSelIdx inputSpace
rs = rights rets
ls = lefts rets

checkRes (idx, xs) = (xs !! idx) == "sel"

-- | There exist three success cases:
-- 1. (none, some, some)
-- 2. (some, some, none)
-- 3. (some, some, some)
-- 
-- The remaining cases each have a custom error message that (hopefully)
-- clarifies the cause of the failure. See 'SelErr' src comments for examples
-- of failure cases.
getSelIdx :: forall c. (Monoid c, Eq c) 
          => (c, c, c) -> Either [SelErr] (Int, [c])
getSelIdx (pre, sel, post)
  | some pre && some sel && some post = Right (1, [pre,sel,post])
  | none pre && some sel && some post = Right (0, [sel,post])
  | some pre && none sel && some post = Left [EmptySelInMid]
  | some pre && some sel && none post = Right (1, [pre, sel])
  | some pre && none sel && none post = Left [EmptySelAtEnd]
  | none pre && none sel && some post = Left [EmptySelAtStart]
  | none pre && some sel && none post = Left [SelEqualsBG]
  | none pre && none sel && none post = Left [EmptyBG]
      where some :: c -> Bool
            some = not . none
            none = (mempty ==)

data SelErr = NoSelection
            | EmptySelAtStart -- e.g., ("", "", "hello world")
            | EmptySelInMid -- e.g., blob: "hello world", sel: 4 4, res: "hell","","o world"
            | EmptySelAtEnd -- e.g., ("hello world", "", "")
            | SelEqualsBG -- selection of bg blob and bg blob are the identical
            | EmptyBG -- the blob from which the selection was created is empty
    deriving (Eq, Show)

class Show e => NoteErr e where
    getErr :: e -> e


-- | Simple error class with useless stub function as a convenience for
-- functions that must call subroutines with divergent error types.
instance NoteErr SelErr where
    getErr _ = NoSelection


-- | Meant for use when generating id specific 'Selection's, 'pruneSel'
-- decrements the sIdx and eIdx vals of thhe given 'Selection' by the given
-- amount. If the subtraction results in an n<0 then n = 0.
pruneSel :: Selection -> Int -> Selection
pruneSel sel@(Sel s e) len = 
    let s' = nat (s - len)
        e' = nat (e - len)
        nat n = if n < 0 then 0 else n
     in Sel s' e'

-- Determines, given 'Selection' @s@ and an a 'Blob''s content @c@'s length @l@, what kind of selection 
-- @sel c s@ would generate.
classifySel :: Selection -> Int -> SelType
classifySel (Sel s e) l
  | l == 0 = EmptyBlob
  | lt (0, s, l) && lt (s, e, l) = Mid
  | s == 0 && lt (s, e, l) = Left'
  | s > l && s == e = EmptyPlus
  | s == e && e == 0 = EmptyLeft
  | s == e && s < l = EmptyMid
  | s == e && e == l = EmptyRight
  | s == 0 && e == l = WholeExact
  | s == 0 && e > l = WholePlus
  | lt (0, s, l) && e == l = RightExact
  | lt (0, s, l) && e > l = RightPlus
  | lt (s, e, 0) = Before
  | lt (l, s, e) = After

continue :: SelType -> Bool
continue RightPlus = True
continue WholePlus = True
continue After = True
continue _ = False

blobInSel :: SelType -> Bool
blobInSel Left' = True
blobInSel RightPlus = True
blobInSel RightExact = True
blobInSel WholeExact = True
blobInSel Mid = True
blobInSel _ = False

needsSelect :: SelType -> Bool
needsSelect Left' = True
needsSelect RightPlus = True
needsSelect RightExact = True
needsSelect Mid = True
needsSelect _ = False



sels = [Sel 0 0, Sel 3 3, Sel 5 5, Sel 7 7, Sel 0 5, Sel 0 7, Sel 3 5, Sel 3 7, Sel 0 3, Sel 0 8]
bLen = 5


lt :: Ord c => (c, c, c) -> Bool
lt (x, y, z) = x < y && y < z

gt :: Ord c => (c, c, c) -> Bool
gt (x, y, z) = x > y && y > z

lessThan :: Ord c => [c] -> Bool
lessthan [] = False
lessThan (x:xs) = go x xs
    where go _ [] = True
          go w (x:xs) = (w < x) && (go x xs)

greaterThan :: Ord c => [c] -> Bool
greaterThan [] = False
greaterThan (x:xs) = go x xs
    where go _ [] = True
          go w (x:xs) = (w > x) && (go x xs)

  
data SelType = Left'
             | EmptyBlob
             | RightPlus
             | RightExact
             | WholePlus
             | WholeExact
             | EmptyLeft
             | EmptyMid
             | EmptyRight
             | EmptyPlus
             | Mid
             | Before
             | After
             deriving (Eq, Show)
