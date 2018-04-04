module IdListId where

import Data.Char (isDigit)

newtype IdListId id = IdListId id deriving (Eq, Ord)


instance Show id => Show (IdListId id) where
    show (IdListId id) = 'l':show id

instance Read id => Read (IdListId id) where
    readsPrec _ ('l':digits) =
        if (all isDigit digits)
           then [(IdListId (read digits), "")]
        else []
    readsPrec _ _ = []


