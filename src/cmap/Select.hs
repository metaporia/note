{-|
Module : Select
Description : Interface for arbitrary reference/referent selection.

Further commentary, (with *bold*, and _italics_?)
-}
--{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Select where

import CMap ( CMap(CMap, cmap, nextId), initWith
            , insert, deref, derefIdListId, insertKV
            , toIdList, empty, lookup
            , derefBlobId, toBlob
             )
import Data.List (splitAt, foldl', scanl')
import Prelude hiding (lookup, insert)
import Data.Maybe (fromJust)
import qualified Data.Map as M
import Control.Monad (join)

import Blob
import BlobId
import IdList
import IdListId
import Handle ()
import Content

import Helpers

-- | Returns a tuple of, in order, the updated 'CMap' and the 'Handle id' of
-- the requested selection.

select :: forall id. (Ord id, Num id)
       => id -> Selection -> CMap String id -> (CMap String id, id)
select bid = undefined 

splitIdList :: forall id. (Ord id, Num id)
            => id
            -> Selection
            -> CMap String id
            -> (CMap String id, id)
splitIdList = undefined


-- | For each BlobId a contextualy appropriate Selection is generated/derived
-- from the given Selection--that is, the Selection parameter w.r.t. the entire
-- dereffed IdList.
--genSels :: --(Ord id, Num id) => 
--           IdList Id
--        -> Selection
--        -> CMap String Id
--        -> [(BlobId Id, Maybe Selection)]
--genSels idList sel cMap = (\(x,_,_) -> x) (foldr update base ids)
--    where 
--        ids :: [BlobId Id]
--        ids = toBIds idList
--        base = ([], Just sel, Just 0)
--        update :: BlobId Id 
--               -> ([(BlobId Id, Maybe Selection)], Maybe Selection, Maybe Int)
--               -> ([(BlobId Id, Maybe Selection)], Maybe Selection, Maybe Int)
--        update bid (retTups, currSel, lastsbG) = 
--                let bidLen = length <$> derefBlobId bid cMap :: Maybe Int
--                    nextSel = join $ (\l -> (updateSel l) <$> currSel) <$> sbG
--                    sbG = bidLen >>= (\bidL -> (bidL+) <$> lastsbG)
--                    curTup = (bid, currSel)
--                 in (curTup:retTups, nextSel, sbG)
--
b = "This is the first line.\n"
b1 = "This is the second line.\n"
b2 = "This is the third line.\n"
bs = b ++ b1 ++ b2


--testSel = Sel 10 25
--ids = fromJust $ handleToIdListId handle
--test' = genSels idList  testSel m'' ---success! with that solitary test case :(
--fetched = map derefFrom test'
--    where derefFrom (bid, mSel) = (derefBlobId bid m'', mSel)
--
--fetched' = foldr derefFrom ([], m'') test'

derefFrom (id, mSel) (ret, map) = (ret', map')
    where 
      (map', handle) = fromJust $ mSel >>= (\s-> splitBlob id s map)
      ret' = (id, handle):ret

--ma = snd fetched'
--ltup = fst fetched'


idList = (IdList bids)
(m', bids) = fromJust $ insertCs [b,b1,b2] empty'
(m'', handle) = insert (AnIdList idList) m'

-- given handle, and m''
-- TODO
-- □ fix fetched' so that, e.g., 
--   ▣  a) l0 -> [b4,b5] and not [b5,b4], and 
--   □  b) (b0 = l0) -> [b4, b5]. currently b0 has been removed, but not replaced.
--          - the key is that, b0 must be replaced with l0



data Selection = Sel { sIdx :: Int
                     , eIdx :: Int
                     } deriving (Eq, Show)

data Selection' = Sel' { sIdx' :: Idx, eIdx' :: Idx } deriving (Eq, Show)

data Idx = Start | Idx Int | End deriving (Eq, Show)

liftSel' :: (Int -> Int) -> Selection' -> Selection'
liftSel' f (Sel' (Idx s) (Idx e)) = Sel' (Idx $ f s) (Idx $ f e)
liftSel' f (Sel' Start (Idx e)) = Sel' Start (Idx $ f e)
liftSel' f (Sel' (Idx s) End) = Sel' (Idx $ f s) End

updateSel' :: Int -> Selection' -> Selection'
updateSel' idx = liftSel' ((-)idx) 

sel' :: String -> Selection' -> (String, String, String)
sel' s (Sel' Start End) = ("", s, "")
sel' s (Sel' Start (Idx e)) = ("", sel, post)
    where (sel, post) = splitAt e s
sel' s (Sel' (Idx idx) End) = (pre, sel, "")
    where (pre, sel) = splitAt idx s
sel' s' (Sel' (Idx s) (Idx e)) = (pre, sel, post)
    where (pre, rest) = splitAt s b
          (sel, post) = splitAt selLen rest
          selLen = e - s

sel :: String -> Selection -> (String, String, String)
sel b (Sel s e) = (pre, sel, post)
    where (pre, rest) = splitAt s b
          (sel, post) = splitAt selLen rest
          selLen = e - s

updateSel :: Int -> Selection -> Selection
updateSel idx (Sel s e) = Sel (subBy s idx) (subBy e idx)

subBy idx shift = if diff >= 0 then diff else 0
    where diff = idx - shift

-- returns the handle of the selection
splitBlob :: (Ord id, Num id) => BlobId id
          -> Selection 
          -> CMap String id
          -> Maybe (CMap String id, id)
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
          selHandle = selHandle'
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
          tup = foldl' (\(m, bids) c -> 
              let (m', handle) = (insert (toBlob c) m) 
               in (m', bids ++ [handleToBlobId handle])) (cMap, []) cs

-- rewrite without fold 
--insertCs' :: forall c id. (Ord id, Num id)
--         => [c] -> CMap c id -> Maybe (CMap c id, [BlobId id])

insertCs' cs cMap = tup
   -- case sequence . snd $ tup of
   --                  Just bids -> Just (fst tup, bids)
   --                  Nothing -> Nothing
     where --tup :: (CMap c id, [Maybe (BlobId id)])
          tup = scanl' (\(m, bids) c -> 
              let (m', handle) = (insert (toBlob c) m) 
               in (m', bids ++ [handleToBlobId handle])) (cMap, []) cs


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


--(pre, sel', post) = 
--    (take (sIdx s) b, take (selLen) rest, drop (sIdx s + selLen) b)
--    where rest = drop (sIdx s) b
--          selLen = eIdx s - (sIdx s)
--
empty' = empty :: CMap String Id

b' = "hello world"
--   01234567890
s = Sel 3 8
(k, i) = initWith b

(l, j) = fromJust $ insertCs ["hello ", "world ", "this is the end."] empty'
(l', c) = fromJust $ splitBlob (BlobId 0) s (fst $ initWith "hello world")
-- step through splitBlob to find point fo bid list reversal.

(mmap, integralHandle) = initWith "hello world"
cs = toList $ sel "hello world" (Sel 3 8)
(mmap', bidL) = fromJust $ insertCs cs mmap  -- reversed. fuck

qsort [] = []
qsort (a:as) = qsort left ++ [a] ++ qsort right
    where (left, right) = (filter (<=a) as, filter (>a) as)

main = print (qsort [8,4,0,3,1,23,11,18])
