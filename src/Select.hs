--{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Select where

import CMap
import Data.List (splitAt)
import Prelude hiding (lookup, insert)
import Data.Maybe (fromJust)
import qualified Data.Map as M

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
        replace bid bids m') >>= (\m'' -> return (m'', handle))
    where s = derefBlobId bid cMap
          --ins :: Maybe (CMap String id, [BlobId id])
          handle = toIdListHandle id --rets whole idlist
          handle' = undefined
  -- TODO: return the correct handle, that of the selection
          ins = parts >>= (\parts -> insertCs parts cMap)
          parts :: Maybe [String]
          parts = (filter isNotEmpty) <$> mPreSelPost
          mPreSelPost :: Maybe [String]
          mPreSelPost = fmap ( toList . (\s -> sel s selection)) s

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
s = Sel 3 8
(k, i) = initWith b

(l, j) = fromJust $ insertCs ["hello ", "world ", "this is the end."] empty'
(l', c) = fromJust $ splitBlob (BlobId 0) s (fst $ initWith "hello world")

