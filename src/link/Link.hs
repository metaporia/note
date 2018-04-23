{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Link where

import Map
import Crypto.Hash (SHA1)
import qualified Data.Map as M
import qualified Helpers as H
-- TODO: 
-- ▣  getLinksTo :: Links -> Subj alg -> Obj alg 

-- | Unfortunately, since 'Map' is parametric over all @'HashAlg' alg => alg@
-- the @alg@ parameter is (unavoidably?) visible even here within the 'Link'
-- interface.
newtype Subj alg = Subj { getSubj :: H.Key alg} deriving (Eq, Ord, Show)
newtype Obj alg = Obj { getObj :: H.Key alg} deriving (Eq, Ord, Show)

-- | Like a bimap, but where instead of mapping @a -> b@, and @b -> a@
-- 'Links' maps @a -> [b]@, and @b -> [a]@. 
-- 
-- This is not for API consumers -- if
-- this seems like something you want, see the 'Linker' instance.
data Links alg = 
    Links {
            -- | links to 'Subj' from 'Obj'
            getTo :: M.Map (Subj alg) [Obj alg]
            -- | links from 'Obj' to 'Subj'
          , getFrom :: M.Map (Obj alg) [Subj alg]
          } deriving (Eq, Show)

type LinksToInternal alg = M.Map (Subj alg) [Obj alg]
type LinksFromInternal alg = M.Map (Obj alg) [Subj alg]

links :: Links SHA1
links = Links M.empty M.empty

linksTo' :: Links alg -> Subj alg -> [Obj alg]
linksTo' (Links to _) subj =
    case M.lookup subj to of
      Just objs -> objs
      Nothing -> []

-- |  (More) stable interface to insulate frontend API from changes to the backend linker
-- implementation.
--
-- This required /so/ many extensions.
class Linker (linker :: * -> *) alg where
    {-# MINIMAL linksTo, linksFrom, insertLinkTo, insertLinkFrom #-}
    type Subject alg
    type Object alg 

    linksTo :: linker alg -> Subject alg -> [Object alg]
    linksFrom :: linker alg -> Object alg -> [Subject alg]
    insertLinkTo :: linker alg 
                  -> Subject alg
                  -> Object alg
                  -> linker alg
    insertLinkFrom :: linker alg 
                  -> Object alg
                  -> Subject alg
                  -> linker alg


instance Linker Links alg where
    type Subject alg = Subj alg
    type Object alg = Obj alg

    linksTo :: Links alg -> Subject alg -> [Object alg]
    linksTo (Links to _) subj =
        case M.lookup subj to of
          Just objs -> objs
          Nothing -> []

    linksFrom :: Links alg -> Object alg -> [Subject alg]
    linksFrom (Links _ from) obj =
        case M.lookup obj from of
          Just subjs -> subjs
          Nothing -> []

    insertLinkTo (Links to from) subj obj =
        let to' = insertLink to from subj obj
                 in (Links to' from)

    insertLinkFrom (Links to from) obj subj =
        let from' = insertLink from to obj subj 
         in (Links to from')

-- | Helper to shorten implementations of 'insertLinkTo' and 'insertLinkFrom'.
insertLink :: Ord a => M.Map a [b] -> M.Map b [a] -> a -> b -> M.Map a [b]
insertLink as bs a b =
    case M.lookup a as of
      Just bs' ->  M.insert a (b:bs') as
      Nothing -> M.insert a [b] as

