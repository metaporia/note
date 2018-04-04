--{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Select where

import CMap
import Data.List (splitAt)
import Prelude hiding (lookup, insert)
import Data.Maybe (fromJust)
import qualified Data.Map as M

import Blob
import BlobId
import IdList
import IdListId
import Handle
import Content

import Helpers

data Selection = Sel { sIdx :: Int
                     , eIdx :: Int
                     } deriving (Eq, Show)


(pre, sel', post) = 
    (take (sIdx s) b, take (selLen) rest, drop (sIdx s + selLen) b)
    where rest = drop (sIdx s) b
          selLen = eIdx s - (sIdx s)

sel :: String -> Selection -> (String, String, String)
sel b (Sel s e) = (pre, sel, post)
    where (pre, rest) = splitAt s b
          (sel, post) = splitAt selLen rest
          selLen = e - s

-- returns the handle of the selection
splitBlob :: BlobId Id
          -> Selection 
          -> CMap String Id
          -> Maybe (CMap String Id, Handle Id)
-- | NB: the `selection` param represets starting and ending indices of the
--       derefed `Blob` reffed by `bid` *relative to bid's String`*!!
--       E.g., if bid points to "
--   > deref bid cMap ==  Just "hello world"
--   and,
--   > sel == Sel 30 4 -- because the sel was calculated from some different
--   global thread context, then splitBlob will fail.
splitBlob bid@(BlobId id) selection cMap = 
    ins >>= (\(m', bids) -> 
        replace bid bids m') >>= (\m'' -> 
            selHandle >>= (\handle -> return (m'', handle)))
    where s = derefBlobId bid cMap
          --ins :: Maybe (CMap String id, [BlobId id])
          --TODO: make vvv less fragile (what about toIdListHandle case?)
          selHandle = OfBlob <$> selHandle'
          selHandle' = (ins >>= return . (\(_, bids) -> bids !! handleIdx))
          handleIdx :: Int
          handleIdx = case (mPreSelPost >>= selPosn) of
                      Just idx -> idx
                      Nothing -> error "no sel handle idx"
          ins = parts >>= (\parts -> insertCs parts cMap)
          parts :: Maybe [String]
          parts = (filter isNotEmpty) <$> mPreSelPost
          mPreSelPost :: Maybe [String]
          mPreSelPost = fmap ( toList . (\s -> sel s selection)) s

-- since a Selection of one blob only ever produces, at a maximum, three 
-- sub-strings, detecting where the empty string(s) are placed should 
-- suffice to calculate the index of the selection handle after the 
-- removal of empty lists ([]).
selPosn :: Eq a => [[a]] -> Maybe Int
selPosn [] = Nothing
selPosn [x] = Nothing
selPosn [x,y] = Nothing
selPosn [x,y,z]
  | isNotEmpty x && isNotEmpty y = Just 1 -- (some, some, _)
  | x == [] && isNotEmpty y = Just 0 -- (none, some, _)
  | isNotEmpty x = Just 0 -- (some, _, _)
  | x == [] && y == [] && isNotEmpty z = Just 0 -- (none, none, some)
  | x == [] && isNotEmpty y && z == [] = Just 0 -- (none, some, none)
  | otherwise = Nothing

-- Nothing case represents the presence if non-BlobIds in list
insertCs :: forall c id. (Ord id, Num id)
         => [c] -> CMap c id -> Maybe (CMap c id, [BlobId id])
insertCs cs cMap = case sequence . snd $ tup of
                     Just bids -> Just (fst tup, bids)
                     Nothing -> Nothing
    where tup :: (CMap c id, [Maybe (BlobId id)])
          tup = foldr  (\c (m, bids) -> 
              let (m', handle) = (insert (toBlob c) m) 
               in (m', handleToBlobId handle : bids)) (cMap, []) cs

empty' = empty :: CMap String Id

-- | Deletes entry of bid, inserts the equivalent IdListId handle (using the 
-- same id #), and returns the modified map.
--
-- NB: (BlobId id) becomes (IdListId id), so any references to the bid become
--     invalid after this operation.
--     Ideally this operation is used along with a search and replace function
--     that replaces all instances of bid with idlid. yay?
--
replace :: (Ord id, Num id)
        => BlobId id -> [BlobId id] -> CMap c id -> Maybe (CMap c id)
replace (BlobId id) bids cMap = 
    Just $ CMap (M.delete handle (cmap cmap')) (nextId cmap')
    where cmap' = insertKV idListHandle idList cMap
          idListHandle = (toIdListHandle id) 
          idList = toIdList . unwrap $ (sequence bids)
          handle = toBlobHandle id

isNotEmpty :: [a] -> Bool
isNotEmpty [] = False
isNotEmpty _ = True

toList :: (a, a, a) -> [a]
toList (x, y, z) = x: y: z :[]

b = "hello world"
--   01234567890
s = Sel 0 11
(k, i) = initWith b

(l, j) = fromJust $ insertCs ["hello ", "world ", "this is the end."] empty'
(l', c) = fromJust $ splitBlob (BlobId 0) s (fst $ initWith "hello world")

