module Helpers where

class Unwrap f where
    unwrap :: f a -> a

