module ContentMap  where

import qualified Data.Map as M
import Prelude hiding (lookup)
import System.IO (FilePath)
import Data.Maybe (fromMaybe)
import Data.Either (partitionEithers)
--import Data.Monoid ((<>))

import Content
import Handle (Handle (BlobId, IdListId))
import qualified Handle as H

type Id = Int

data ContentMap id = ContentMap (M.Map (Handle id) (Content id)) id deriving (Eq, Show)

--instance Monoid ContentMap where
--    mempty = empty
--    ContentMap m id `mappend` ContentMap m' id' = ContentMap (m <> m')
--    mempty `mappend` cm@(ContentMap _ _) = cm
-- TODO:
--  □  complete instance when hash id's have been added (so as to obviate the
--     union of maps with (potentiall) doubled keys
--  □  

initWithFile:: FilePath -> IO (ContentMap Id, Handle Id)
initWithFile fp = (fromFile fp) >>= return . initWith

type Selection = (Int, Int)
select :: (Ord id, Num id) =>
    Selection
    -> Handle id
    -> ContentMap id
    -> (ContentMap id, Handle id)
select (sIdx, eIdx) handle map = undefined

derefBlobId :: (Num id, Ord id)
            => ContentMap id -> H.BlobId id -> Either DerefError String
derefBlobId map id@(BlobId _) = 
    content >>= ((either (Left . contentToDerefError) (Right)) . fromBlob)
    where content = case lookup id map of
                  Just c -> Right c
                  Nothing -> Left NoValueAssocWithKey
derefIdListId :: (Num id, Ord id)
              => ContentMap id -> IdListId id ->  Either DerefError String
derefIdListId map id@(IdListId _) = 
    either (Left . contentToDerefError) ((fmap concat) . sequence) c
    where c = case fromIdList <$> lookup id map of
                  Just c -> (fmap . fmap) (derefHandle map) c
                  Nothing -> Right [Left NoValueAssocWithKey]

deref :: (Num id, Ord id, H.Handle' r)
      => ContentMap id -> r -> Either DerefError String
deref map handle
  | H.refersTo handle == H.Blob = undefined
  | H.refersTo handle == H.IdList = undefined
  | otherwise = undefined

--derefIdList :: (Num id, Ord id)
--            => ContentMap id -> BlobId id -> Either DerefError String
--derefIdLIst map id =
--    either (Left . contentToDerefError) ((fmap concat) . sequence) c
--        where c = case fromIdList 
--        
data DerefError = NoValueAssocWithKey | ContentErr ContentError deriving (Eq, Show)

contentToDerefError :: ContentError -> DerefError
contentToDerefError c = ContentErr c

initWith :: (Num id, Ord id) => Content id -> (ContentMap id, Handle id)
initWith content = insert content empty

empty :: (Num id, Ord id) => ContentMap id
empty = ContentMap M.empty 0

insert :: (Num id, Ord id) => Content id -> ContentMap id -> (ContentMap id, Handle id)
insert b (ContentMap map nextId) =
    ((ContentMap (M.insert id b map) $ nextId + 1 ), id)
        where id = case b of
                       Blob _ -> BlobId nextId
                       IdList _ -> IdListId nextId

lookup :: Ord id => Handle id -> ContentMap id -> Maybe (Content id)
lookup id (ContentMap map _) = M.lookup id map

insertContents :: (Ord id, Num id) 
               => [Content id] -> ContentMap id -> (ContentMap id, [(Handle id)])
insertContents bs map = foldr (\b (map', ids) -> let (map'', id) = insert b map' in
                                                  (map'', (id:ids))) (map, []) bs

