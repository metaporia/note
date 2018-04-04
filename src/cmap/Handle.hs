module Handle where

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
