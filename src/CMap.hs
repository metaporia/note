--{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module CMap where

import qualified Data.Map as M
import Prelude hiding (insert, lookup)
import Data.Maybe (fromJust)
import Data.Monoid (Monoid, (<>))

--blobMap = M.empty :: M.Map (BlobId id) (Blob c)

newtype BlobId id = BlobId id deriving (Eq, Ord)

instance Show id => Show (BlobId id) where
    show (BlobId b) = 'b': show b

instance Functor BlobId where
    fmap f (BlobId id) = BlobId (f id)

instance Applicative BlobId where
    pure = BlobId
    BlobId f <*> BlobId a = BlobId (f a)

instance Monad BlobId where
    return = pure
    BlobId a >>= f = f a

class Unwrap f where
    unwrap :: f a -> a

instance Unwrap BlobId where
    unwrap (BlobId id) = id


newtype Blob c = Blob c deriving (Eq, Show)

--idListMap = M.empty :: M.Map (IdListId id) (IdList id)

newtype IdListId id = IdListId id deriving (Eq, Ord)

instance Show id => Show (IdListId id) where
    show (IdListId id) = 'l':show id

newtype IdList id = IdList [BlobId id] deriving (Eq)

instance Show id => Show (IdList id) where
    show (IdList ids) = show ids

data Handle id =  OfBlob (BlobId id) 
               | OfIdList (IdListId id)
               deriving (Eq, Ord)

instance Show id => Show (Handle id) where
    show (OfBlob id) = show id
    show (OfIdList id) = show id

data Content c id = ABlob (Blob c) 
                  | AnIdList (IdList id)
                  deriving (Eq)

instance (Show c, Show id) => Show (Content c id) where
    show (ABlob (Blob b)) = show b
    show (AnIdList (IdList ids)) = show ids

type Id = Int

data CMap c id = CMap { cmap :: (M.Map (Handle id) (Content c id)) 
                      , nextId :: id} deriving (Eq, Show)

--instance (Show id, Show c) => Show (CMap c id) where
empty :: Num id => CMap c id
empty = CMap M.empty 0

insertKV :: (Num id, Ord id)
          => Handle id 
          -> Content c id 
          -> CMap c id 
          -> CMap c id
insertKV handle content cMap =
       CMap (M.insert handle content (cmap cMap)) 
                        (nextId cMap)
     

insert :: (Num id, Ord id) 
       => Content c id -> CMap c id -> (CMap c id, Handle id)
insert val map = 
    (CMap (M.insert currentId val (cmap map)) (nextId map + 1), currentId)
    where 
        currentId = wrap $ nextId map
        wrap = case val of
                   ABlob _ -> (\id -> OfBlob (BlobId id))
                   AnIdList _ -> (\id -> OfIdList (IdListId id))

lookup :: (Num id, Ord id)
       => Handle id -> CMap c id -> Maybe (Content c id)
lookup handle cMap = M.lookup handle map
    where map = cmap cMap


derefBlobId :: forall id c. (Ord id, Num id) 
            =>  BlobId id -> CMap c id -> Maybe c
derefBlobId bid map = lookup handle map >>= blobFromContent
    where 
        handle :: Handle id
        handle = OfBlob bid

derefIdListId :: forall id c.(Ord id, Num id, Monoid c)
              => IdListId id -> CMap c id -> Maybe c 
derefIdListId id map' = 
    lookup handle map' >>= idsFromContent >>= 
        fmap mconcat . sequence . (\ids -> 
            map (\id -> derefBlobId  id map') ids)
        where handle :: Handle id
              handle = OfIdList id

deref :: (Num id, Ord id, Monoid c) => Handle id -> CMap c id -> Maybe c
deref (OfBlob bid) = derefBlobId bid
deref (OfIdList ids) = derefIdListId ids

initWithFile :: (Num id, Ord id)
             => FilePath -> IO (CMap String id, Handle id)
initWithFile fp = readFile fp >>= return . initWith

initWith :: (Ord id, Num id) => c -> (CMap c id, Handle id)
initWith c = insert (toBlob c) empty

blobFromContent :: Content c id -> Maybe c
blobFromContent (ABlob (Blob c)) = Just c
blobFromContent _ = Nothing

idsFromContent :: Content c id -> Maybe [BlobId id]
idsFromContent (AnIdList (IdList ids)) = Just ids
idsFromContent _ = Nothing


-- beware of cruft below

toBlob :: c -> Content c id
toBlob c = ABlob (Blob c)

toIdList :: Num id => [id] -> Content c id
toIdList ids = AnIdList $ IdList $ map BlobId ids

toBlobHandle :: Num id => id -> Handle id
toBlobHandle n = OfBlob (BlobId n)

toIdListHandle :: Num id => id -> Handle id
toIdListHandle n = OfIdList (IdListId n)

(m', i) = insert (toBlob "hello world") empty
(m'', i') = insert (AnIdList $ IdList bids) m'
    where 
        bids :: [BlobId Int]
        bids = [fromJust $ handleToBlobId i]
--(m''', i'') = insert (AnIdList $ IdList [IdListId 0]) empty
handleToBlobId :: Handle id -> Maybe (BlobId id)
handleToBlobId (OfBlob id@(BlobId _)) = Just id
handleToBlobId  _ = Nothing

-- allowed
b0 = Blob "hello world"
b0_id = BlobId 0 :: BlobId Id

idl0 = IdList [b0_id]
id0 = IdListId 0
-- not allowed
--id1 = IdList [IdListId 0]

m = M.empty :: M.Map (Handle id) (Content String id)


n = m :: M.Map (Handle Id) (Content String Id)

n' = M.insert (OfBlob b0_id) (ABlob b0) n -- check
n'' = M.insert (OfIdList (IdListId 0)) (AnIdList (IdList [b0_id])) n'

--l = insert (OfIdList (IdListId 0)) (AnIdList (IdList (IdListId 0))) 
-- prevented!

