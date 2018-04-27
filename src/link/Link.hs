{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Link where

import Crypto.Hash (SHA1)
import qualified Data.Map as M
import Data.Monoid ((<>))
import Helpers (Key, showAbbrev)
import Data.Bifunctor (bimap)
import Control.Monad.State
import Data.Functor.Identity
import VMap (HashAlg)
-- TODO: 
-- ▣  getLinksTo :: Links -> Subj alg -> Obj alg 

-- | Unfortunately, since 'Map' is parametric over all @'HashAlg' alg => alg@
-- the @alg@ parameter is (unavoidably?) visible even here within the 'Link'
-- interface.
newtype Subj alg = Subj { getSubj :: Key alg} deriving (Eq, Ord)

instance Show (Subj alg) where
    show (Subj k) = take 7 $ show k

newtype Obj alg = Obj { getObj :: Key alg} deriving (Eq, Ord)

instance Show (Obj alg) where
    show (Obj k) = take 7 $ show k


-- | Like a bimap, but instead of mapping @a -> b@, and @b -> a@
-- 'Links' maps @a -> [b]@, and @b -> [a]@. 
-- 
-- This is not for API consumers -- if
-- this seems like something you want, see the 'Linker' instance.
-- 
-- Naming of accessors is based upon the preposition preceding 'Subj',
-- instances of which are italicized below.
data Links alg = 
    Links { -- | links /from/ 'Subj' to 'Obj', i.e., subj -> obj
            getFrom :: M.Map (Subj alg) [Obj alg]
            -- | links from 'Obj' /to/ 'Subj', i.e., obj -> subj
          , getTo :: M.Map (Obj alg) [Subj alg]
          } deriving (Eq, Show)

pshow :: (HashAlg alg) => Links alg -> String
pshow (Links from to) = "*from* Subj to Obj:\n"
                      ++ (go $ M.toList from) 
                      ++ "\nfrom Obj *to* Subj:\n"
                      ++ (go $ M.toList to) 
    where go [] = []
          go ((k, ks):xs) = take 7 (show k) ++ " -> " 
                        ++ (show $ map (take 7 . show) ks)
                        ++ "\n"
                        ++ go xs



instance Monoid (Links alg) where
    mempty = Links mempty mempty
    -- THIS IS SUS AF
    Links to from `mappend` Links to' from' = Links (to <> to') (from <> from')

type LinksToInternal alg = M.Map (Subj alg) [Obj alg]
type LinksFromInternal alg = M.Map (Obj alg) [Subj alg]

emptySHA1 :: Links SHA1
emptySHA1 = empty

empty :: Links alg
empty = Links M.empty M.empty

-- |  (More) stable interface to insulate frontend API from changes to the backend linker
-- implementation.
--
-- This required /so/ many extensions.
class Linker (linker :: * -> *) alg where
    {-# MINIMAL linksTo, linksFrom, insertTo, insertFrom #-}
    type Subject alg
    type Object alg 
    -- | Lookup 'Object's that link to the given 'Subject'.
    linksTo :: linker alg -> Object alg -> [Subject alg]
    -- | Lookup 'Subject's that link to the given 'Object'.
    linksFrom :: linker alg -> Subject alg -> [Object alg]
    -- | Insert link /from/ 'Subject' to 'Object'.
    insertFrom :: linker alg 
                  -> Subject alg
                  -> Object alg
                  -> linker alg
    -- |Insert link from 'Object' /to/ 'Subject'.
    insertTo :: linker alg 
                  -> Object alg
                  -> Subject alg
                  -> linker alg
    -- | Insert biderectional link (which still preserves the "primary" direction;
    -- that is, that of 'Subject' to 'Object') from 'Subject' to 'Object'.
    insert :: Monoid (linker alg) 
           => Subject alg
           -> Object alg
           -> linker alg
           -> linker alg
    insert subj obj linker = 
        let lfrom = insertFrom linker subj obj
         in insertTo lfrom obj subj

    insert' :: Monoid (linker alg)
            => Subject alg
            -> Object alg
            -> StateT (linker alg) Identity ()
    insert' subj obj = state $ \s -> ((), insert subj obj s)

instance Linker Links alg where
    type Subject alg = Subj alg
    type Object alg = Obj alg

    linksFrom :: Links alg -> Subject alg -> [Object alg]
    linksFrom (Links to _) subj =
        case M.lookup subj to of
          Just objs -> objs
          Nothing -> []

    linksTo :: Links alg -> Object alg -> [Subject alg]
    linksTo (Links _ from) obj =
        case M.lookup obj from of
          Just subjs -> subjs
          Nothing -> []

    insertTo (Links to from) obj subj  =
        let to' = insertLink to from subj obj
                 in (Links to' from)

    insertFrom (Links to from) subj obj =
        let from' = insertLink from to obj subj 
         in (Links to from')

-- | Helper to shorten implementations of 'insertLinkTo' and 'insertLinkFrom'.
-- Inserts @a -> [b]@ and @b -> [a]@.
insertLink :: Ord a => M.Map a [b] -> M.Map b [a] -> a -> b -> M.Map a [b]
insertLink as bs a b =
    case M.lookup a as of
      Just bs' ->  M.insert a (b:bs') as
      Nothing -> M.insert a [b] as


