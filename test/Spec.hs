import CMap
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

instance Arbitrary id => Arbitrary (BlobId id) where
    arbitrary = BlobId <$> arbitrary

instance Eq id => EqProp (BlobId id) where (=-=) = eq

triggerBlobId = undefined :: BlobId (Int, Int, Int)

test batch trigger = quickBatch $ batch trigger

main :: IO ()
main = do
    test functor triggerBlobId
    test applicative triggerBlobId
    test monad triggerBlobId


