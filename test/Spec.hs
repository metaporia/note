module Spec where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

import Map (Handle(BlobId, IdListId))

main :: IO ()
main = test functor triggerHandle

-- Handle

instance Arbitrary id => Arbitrary (Handle id) where
    arbitrary = oneof [BlobId <$> arbitrary, IdListId <$> arbitrary]

instance Eq id => EqProp (Handle id) where (=-=) = eq

triggerHandle = undefined :: Handle (Int, Int, Int)

test testbatch trigger = quickBatch $ testbatch trigger

