module Handle () where

import BlobId
import IdListId
import Helpers

data Handle id =  OfBlob (BlobId id) 
               | OfIdList (IdListId id)
               deriving (Eq, Ord)

instance Read id => Read (Handle id) where
    readsPrec _ [] = []
    readsPrec _ s@('l':_) = [(OfIdList (read s), "")]
    readsPrec _ s@('b':_) = [(OfBlob (read s), "")]


instance Show id => Show (Handle id) where
    show (OfBlob id) = show id
    show (OfIdList id) = show id

instance Unwrap Handle where
    unwrap (OfBlob (BlobId id)) = id
    unwrap (OfIdList (IdListId id)) = id


handleToBlobId :: Handle id -> Maybe (BlobId id)
handleToBlobId (OfBlob id) = Just id
handleToBlobId  _ = Nothing

handleToIdListId :: Handle id -> Maybe (IdListId id)
handleToIdListId (OfIdList id) = Just id
handleToIdListId _ = Nothing

toBlobHandle :: Num id => id -> Handle id
toBlobHandle n = OfBlob (BlobId n)

toIdListHandle :: Num id => id -> Handle id
toIdListHandle n = OfIdList (IdListId n)










toStr :: (Ord a, Show a, Num a) => a -> a -> String
toStr n id
  | n > 1 = 'l':(show id)
  | otherwise = 'b':(show id)

to' n id = to $ toStr n id

tob id = to' 0 id
tol id = to' 2 id

to :: String -> Handle Id
to s = read s :: Handle Id
