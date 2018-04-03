module Content where


import Handle (Handle)
import qualified Handle as H
import System.IO (readFile)
-- TODO: □ flatten Handle -> Content mapping.
data Content id = Blob String | IdList [H.BlobId id] deriving (Eq, Show)


data ContentError = IsIdList 
                  | IsBlob deriving (Eq, Show)

fromBlob :: Content id -> Either ContentError String
fromBlob (Blob s) = Right s
fromBlob (IdList _) = Left IsIdList

fromIdList :: Content id -> Either ContentError [H.BlobId id]
fromIdList (IdList ids) = Right ids
fromIdList (Blob _) = Left IsBlob

fromFile :: FilePath -> IO (Content id)
fromFile fp = Blob <$> readFile fp


