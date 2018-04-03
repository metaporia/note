module Lib
    ( someFunc
    ) where

import Prelude hiding (lookup)
import Content 
import Handle (Handle)
import ContentMap

import Data.Either

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- test code
b0 = "This is the first line\n"
b1 = "This is the second line\n"
b2 = "Guess which line this is: that which proceeds the second\n"

(map', almostIdList0) = insertContents (map Blob [b0,b1,b2]) empty

idList = IdList almostIdList0 :: Content Id

(map'', lid) = insert idList map'

(map''', llid) = insert (IdList [lid]) map''
