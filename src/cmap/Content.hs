module Content where

import IdList
import Blob

data Content id c = ABlob (Blob c) 
                  | AnIdList (IdList id)
                  deriving (Eq)

instance (Show c, Show id) => Show (Content id c) where
    show (ABlob (Blob b)) = show b
    show (AnIdList (IdList ids)) = show ids

instance Functor (Content id) where
    fmap _ (AnIdList l) = AnIdList l
    fmap f (ABlob (Blob c)) = ABlob $ Blob $ f c


