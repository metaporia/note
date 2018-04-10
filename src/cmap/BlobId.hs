module BlobId where
    -- DEPRECATED

import Data.Char (isDigit)
import Helpers

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

instance Read id => Read (BlobId id) where
    readsPrec _ ('b':digits) =
        if (all isDigit digits)
           then [(BlobId (read digits), "")]
        else []
    readsPrec _ _ = []

instance Unwrap BlobId where
    unwrap (BlobId id) = id
