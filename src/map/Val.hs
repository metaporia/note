{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Val where
-- new Content type for slice refactor

import Select (Selection, Splittable(..))
import Helpers (Key)
import Data.Monoid ((<>))

-- Ensure that it is difficult/impossible to insert a (k3, span k2 sel)
-- given that k2 points to a span.
data Val alg c = Blob Int c
               | Span (Key alg) Selection
               deriving Eq

-- | Construct 'Blob' variant of 'Val'.
mkBlob :: Splittable c => c -> Val alg c
mkBlob c = Blob (len c) c

-- | Construct 'Span' variant of 'Val'.
mkSpan :: Key alg -> Selection -> Val alg c
mkSpan key sel = Span key sel

instance forall c alg. (Show c, Show alg) => Show (Val alg c) where
    show (Blob l b) = show b
    show (Span key sel) = show (showAbbrev key) ++ show sel
        where 
            showAbbrev :: Key alg -> String
            showAbbrev hash = take 7 $ show hash

instance Functor (Val alg) where
    fmap _ (Span key sel) = Span key sel
    fmap f (Blob l c) = Blob l $ f c

