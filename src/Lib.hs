module Lib
    ( someFunc
    ) where

import qualified Data.Map as M

import Prelude hiding (lookup, insert)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

