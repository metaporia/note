--{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module CMap where

-- group exports related to CMap manipulation here. all auxiliary functionality
-- should be hidden.

import qualified Data.Map as M
import Prelude hiding (insert, lookup)
import Data.Maybe (fromJust)
import Data.Monoid (Monoid, (<>))
import Data.Char (isDigit, isAscii)

import Blob
import BlobId
import IdList
import IdListId
import Helpers
import Content
import Handle

data CMap c id = CMap { cmap :: (M.Map id (Content id c)) 
                      , nextId :: id} deriving (Eq, Show)

empty :: Num id => CMap c id
empty = CMap M.empty 0

insertKV :: (Num id, Ord id)
          => id 
          -> Content id c 
          -> CMap c id 
          -> CMap c id
insertKV handle content cMap =
       CMap (M.insert handle content (cmap cMap)) 
                        (nextId cMap)
     

insert :: (Num id, Ord id) 
       => Content id c -> CMap c id -> (CMap c id, id)
insert val map = 
    (CMap (M.insert currentId val (cmap map)) (nextId map + 1), currentId)
    where 
        currentId = nextId map

lookup :: (Num id, Ord id)
       => id -> CMap c id -> Maybe (Content id c)
lookup handle cMap = M.lookup handle map
    where map = cmap cMap


--derefBlobId' :: (Ord id, Num id) => id -> CMap c id -> Maybe c
--derefBlobId' bid map' = lookup id map' >>= blobFromContent


-- only call this if id is in fact a BlobId 
derefBlobId :: (Ord id, Num id) => id -> CMap c id -> Maybe c
derefBlobId id m = lookup id m >>= blobFromContent

derefIdListId :: forall id c.(Ord id, Num id, Monoid c)
              => id -> CMap c id -> Maybe c 
derefIdListId id map' = 
    lookup id map' >>= (fmap . fmap $ unwrap) . idsFromContent >>=
        (fmap mconcat) . sequence . (\ids -> 
            map (\id' -> derefBlobId id' map') ids)
    
deref :: (Num id, Ord id, Monoid c) => id -> CMap c id -> Maybe c
deref id cMap = case lookup id cMap of
                  Just (ABlob _) -> derefBlobId id cMap
                  Just (AnIdList _) -> derefIdListId id cMap
                  Nothing -> Nothing

initWithFile :: (Num id, Ord id)
             => FilePath -> IO (CMap String id, id)
initWithFile fp = readFile fp >>= return . initWith

initWith :: (Ord id, Num id) => c -> (CMap c id, id)
initWith c = insert (toBlob c) empty

blobFromContent :: Content id c -> Maybe c
blobFromContent (ABlob (Blob c)) = Just c
blobFromContent _ = Nothing

idsFromContent :: Content id c -> Maybe [BlobId id]
idsFromContent (AnIdList (IdList ids)) = Just ids
idsFromContent _ = Nothing


-- beware of cruft below

toBlob :: c -> Content id c
toBlob c = ABlob (Blob c)

toIdList :: Num id => [id] -> Content id c
toIdList ids = AnIdList $ IdList $ map BlobId ids

(m', i) = insert (toBlob "hello world") empty
(m'', i') = insert (AnIdList $ IdList bids) m'
    where 
        bids = [BlobId i]
--(m''', i'') = insert (AnIdList $ IdList [IdListId 0]) empty

-- allowed
b0 = Blob "hello world"
b0_id = BlobId 0 :: BlobId Id

idl0 = IdList [b0_id]
id0 = IdListId 0
-- not allowed
--id1 = IdList [IdListId 0]

m = M.empty :: M.Map (id) (Content id String)


n = m :: M.Map (Id) (Content Id String)

--n' = M.insert (OfBlob b0_id) (ABlob b0) n -- check
--n'' = M.insert (OfIdList (IdListId 0)) (AnIdList (IdList [b0_id])) n'

--l = insert (OfIdList (IdListId 0)) (AnIdList (IdList (IdListId 0))) 
-- prevented!

