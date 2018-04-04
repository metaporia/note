module Content where

import IdList
import Blob

data Content c id = ABlob (Blob c) 
                  | AnIdList (IdList id)
                  deriving (Eq)

instance (Show c, Show id) => Show (Content c id) where
    show (ABlob (Blob b)) = show b
    show (AnIdList (IdList ids)) = show ids


