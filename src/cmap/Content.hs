{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Content where

import IdList
import Blob
import BlobId
import Crypto.Hash
import Data.ByteArray (convert)
import qualified Data.ByteString as B
import Data.ByteString.Conversion 
import Data.Maybe (fromJust)
import Data.List (intercalate)

data Content alg c = ABlob (Blob c) 
                  | AnIdList (IdList alg)
                  deriving (Eq)

instance forall c alg. (Show c, Show alg) => Show (Content alg c) where
    show (ABlob (Blob b)) = show b
    show (AnIdList (IdList hashes)) = show $ map showAbbrev hashes
        where 
            showAbbrev :: Digest alg -> String
            showAbbrev hash = take 7 $ show hash


instance Functor (Content alg) where
    fmap _ (AnIdList l) = AnIdList l
    fmap f (ABlob (Blob c)) = ABlob $ Blob $ f c


toBlob :: c -> Content id c
toBlob c = ABlob (Blob c)

toIdList :: HashAlgorithm alg
         => [Digest alg] -> Content alg c
toIdList ids = AnIdList $ IdList ids
