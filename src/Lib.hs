module Lib
    ( someFunc
    ) where

import qualified Data.Map as M

import Prelude hiding (lookup)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

type Id = Int
type Blob = String

data ContentMap = ContentMap (M.Map (Handle Id) (Content Id)) (Handle Id) deriving (Eq, Show)

empty :: ContentMap
empty = ContentMap M.empty (BlobId 0)

insert :: Content Id -> ContentMap -> (ContentMap, Handle Id)
insert b (ContentMap map nextId) =
    ((ContentMap (M.insert nextId b map) $ succHandle nextId), nextId)

lookup :: Handle Id -> ContentMap -> Maybe (Content Id)
lookup id (ContentMap map _) = M.lookup id map

-- BlobId
insertContents :: [Content Id] -> ContentMap -> (ContentMap, [(Handle Id)])
insertContents bs map = foldr (\b (map', ids) -> let (map'', id) = insert b map' in
                                                  (map'', (id:ids))) (map, []) bs

b0 = "This is the first line\n"
b1 = "This is the second line\n"
b2 = "Guess which line this is: that which proceeds the second\n"

(map', almostIdList0) = insertContents (map Blob [b0,b1,b2]) empty

idList = IdList almostIdList0 :: Content Id

data Content id = Blob String | IdList [Handle id] deriving (Eq, Ord, Show)
data Handle id = BlobId id | IdListId id deriving (Eq, Ord, Show)
succHandle :: Num id => Handle id -> Handle id
succHandle (BlobId n) = BlobId (n + 1)
succHandle (IdListId n) = IdListId (n + 1)



