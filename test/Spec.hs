
import CMap
import Select
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


instance Arbitrary Selection where
    arbitrary = uncurry Sel <$> (tups `suchThat` filterHiLo)
        where tups = arbitrary :: Gen (Int, Int)
              filterHiLo (a, b) = a >= 0 && a <=20 && b>=0 && b<=20

instance EqProp Selection where (=-=) = eq

sels = sample' (arbitrary :: Gen Selection)
strs = sample' (arbitrary :: Gen String )

args = (,) <$> strs <*> sels

ops = ["some", ""]
space = (,,) <$> ops <*> ops <*> ops

call = fmap (selPosn . toList) space

