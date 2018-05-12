{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Lib where

import Prelude hiding (init, lookup, insert, span)

import qualified Data.Map as M

import Crypto.Hash --(SHA1, hash, hashWith)
import qualified Data.ByteString as B
import Data.ByteString.Char8 (pack)
import Data.ByteArray (ByteArrayAccess)
import Data.ByteString.Conversion
--import Content
import qualified Data.Text as T 
import qualified Data.Text.IO as TIO

import Text.RawString.QQ

import Helpers

import Control.Monad.State
import Control.Monad.State.Class
import Control.Monad.Trans.Maybe

import Control.Monad.Trans.Either
import Control.Monad.Except

import Control.Exception

import Data.Functor.Identity
import Data.Functor (void)

import Data.Maybe (isJust, fromJust, catMaybes)
import Data.Monoid ((<>))

import Note (Note(..), newNote, ls, lsvm, lslnk, lssm, lsabbr)
import qualified Note
import UI.Types (NoteS, runWith')
import Abbrev (alias')
import VMap



runWith = runWith'

run stateToRun state = runWith stateToRun state

go state = run (state *> ls') newNote
go' state = void $ (run (state *> ls') newNote)


              

ls' :: NoteS String ()
ls' = do n <- get
         liftIO $ sequence_ $ [lsvm, lslnk, lssm, lsabbr] <*> [n]
         put n
         return ()

two = loadf "specificity.md"  >>= aliasK "spec"



-- Internal API
aliasK :: T.Text -> Key SHA1 -> NoteS String ()
aliasK alias key = do
    n@(Note lnk vm abbr sm) <- get
    let abbr' = alias' abbr alias key
    put (Note lnk vm abbr' sm)
    return ()
{-
linkK :: ST -> ST -> NoteS String ST
linkK st st' = do
    n@(Note lnk vm abbr sm) <- get
    s <- liftEither $ Subj <$> getKey st
    o <- liftEither $ Obj <$> getKey st'
    let lnkr' = Link.insert s o lnk
    put (Note lnkr' vm abbr sm)
    return $ Msg "link successfuly registered"

link :: ST -> ST -> NoteS String ST
link st st' = do
    n@(Note lnk vm abbr sm) <- get
    s <- liftEither $ getAbbr st
    o <- liftEither $ getAbbr st'
    let lengthen' a = liftEither $ 
            case lengthen abbr a of 
              Just k -> Right k 
              Nothing -> Left "no key matching abbr found"
    st <- lengthen' s
    ot <- lengthen' o
    liftIO $ print s >> print "\n" >> print o
    liftIO $ print st >> print "\n" >> print ot
                

    let lnkr' = Link.insert (Subj st) (Obj ot) lnk
    put (Note lnkr' vm abbr sm)
    return . Msg $ s <> " successfuly linked to " <> o



derefAbbr :: ST -> NoteS String ST
derefAbbr st = do
    n@(Note lnk vm abbr sm) <- get
    alias' <- liftEither $ getAbbr st
    k <- liftEither $ case lengthen abbr alias' of
                       Just k -> Right k
                       Nothing -> Left "abbr not found in Abbrev"
    val <- liftEither $ 
        case deref vm k of
          Just b -> Right b 
          Nothing -> Left "could not deref key"
    put n
    return (Blob' val)
-}
loadf :: FilePath -> NoteS String (Key SHA1)
loadf fp = do
    eCont <- liftIO $ 
        (try (TIO.readFile fp) :: IO (Either IOError T.Text))
    contents <- liftEither $ convertException eCont
    n@(Note lnk vm abbr sm) <- get
    let (vm', mK) = insertRawBlob contents vm
    k <- liftEither $ case mK of
                        Just k' -> Right k'
                        Nothing -> Left "insertion failed."
    put (Note lnk vm' abbr sm)
    return k
{-
lsvm :: NoteS String ST
lsvm = do
    note <- get
    let s = show' $ getVMap note
    put note
    return . Ls $  T.pack s

lslnk :: NoteS String ST
lslnk = do
    note <- get
    let s = pshow $ getLinks note
    put note
    return . Ls $ T.pack s

lsabbr :: NoteS String ST
lsabbr = do
    note <- get
    let s = show $ getAbbrev note
    put note
    return . Ls $ T.pack s


runCmd :: Cmd -> Either String (NoteS String ST)
runCmd (Cmd cmd args) = 
    let f = M.lookup cmd ecmds
     in case f of 
          Just f' -> f' args
          Nothing -> Left "cmd not found."

abbr :: ST -> NoteS String ST
abbr st = do
    key <- liftEither $ getKey st
    n@(Note lnk vm abbr sm) <- get
    let (abr, abbr') = abbrev' abbr key 
    put (Note lnk vm abbr' sm)
    return (Abbr abr)

vmkeys :: NoteS String ST
vmkeys = do
    note <- get
    let x = appVM M.keys $ getVMap note
    put note
    return . List $ map (Key' . toAesonKey) x

abbrkeys :: NoteS String ST
abbrkeys = do
    note <- get
    let x = getAbbrKeys $ getAbbrev note
    put note
    return . List $ map Abbr x

-- abbrs
linksto :: ST -> NoteS String ST
linksto st = do
    note <- get
    abbr <- liftEither $ getAbbr st
    obj <- liftEither $ 
        case lengthen (getAbbrev note) abbr of
          Just k -> Right $ Obj k
          Nothing -> Left "could not find key matching abbr"
    let subjs =  linksTo (getLinks note) obj
    put note
    return . List $ map (\(Subj k) -> Abbr . take' 8 $ keyToText k) subjs

-- keys
linkstoK :: ST -> NoteS String ST
linkstoK st = do
    note <- get
    key <- liftEither $ getKey st
    let obj = Obj key
        subjs =  linksTo (getLinks note) obj
    put note
    return . List $ map (\(Subj k) -> Key' . toAesonKey $ k) subjs

            
linksfrom :: ST -> NoteS String ST
linksfrom st = do
    note <- get
    abbr <- liftEither $ getAbbr st
    subj <- liftEither $ 
        case lengthen (getAbbrev note) abbr of
          Just k -> Right $ Subj k
          Nothing -> Left "could not find key matching abbr"
    let objs =  linksFrom (getLinks note) subj
    put note
    return . List $ map (\(Obj k) -> Abbr 
                                   . T.pack 
                                   . take 8 $ show k) objs

linksfromK :: ST -> NoteS String ST
linksfromK st = do
    note <- get
    key <- liftEither $ getKey st
    let subj = Subj key
        objs =  linksFrom (getLinks note) subj
    put note
    return . List $ map (\(Obj k) -> Key' . toAesonKey $ k) objs

-- | Adds 'Span'' to VMap, SelMap
selectK :: ST -> NoteS String ST
selectK st = do
    (sourceKey, sel) <- liftEither $ getSpan st
    (Note lnk vm abbr sm) <- get
    (vm', spanKey) <- liftEither $ 
        case insertRawSpan sourceKey sel vm of
          Just (vm', spanKey) -> Right (vm', spanKey) 
          Nothing -> Left "unable to deref span's target key"
    let sm' = registerSpanInsertion spanKey sourceKey sel sm
    put (Note lnk vm' abbr sm')

    return (Key' $ toAesonKey spanKey)
-- 1.  insertRawSpan k s
-- 2. registerSpanInsertion  spankey (Span k s)
-- 3. return (key' spankey)

-- | 'selectK' for 'Abbr'
select :: ST -> ST -> NoteS String ST
select st st' = do
    (Note lnk vm abbr sm) <- get
    a <- liftEither $ getAbbr st
    sel <- liftEither $ getSel' st'
    sourceKey <- liftEither $
        case lengthen abbr a of
          Just k -> Right k
          Nothing -> Left "could not lengthen key"
    (vm', spanKey) <- liftEither $ 
        case insertRawSpan sourceKey sel vm of
          Just (vm', spanKey) -> Right (vm', spanKey) 
          Nothing -> Left "unable to deref span's target key"
    let sm' = registerSpanInsertion spanKey sourceKey sel sm
    put (Note lnk vm' abbr sm')
    return (Key' $ toAesonKey spanKey)
-} 
