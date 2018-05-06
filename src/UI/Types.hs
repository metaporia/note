{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module UI.Types where
import Prelude hiding (insert, lookup, init)

import Crypto.Hash
import Text.RawString.QQ

import Control.Monad.State
import Control.Monad.State.Class
import Control.Monad.Trans.Maybe

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Encoding (decodeUtf8,encodeUtf8)
import Data.String (IsString)
import Data.String.ToString (toString)

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LIO
import qualified Data.ByteString as B
import Data.ByteString.Conversion

import qualified Data.ByteString.Base64 as Base64

import Data.Functor.Identity
import Data.Maybe (catMaybes, fromJust, isJust)
import Data.Tuple (swap)

import Data.Aeson ( ToJSON(..), FromJSON(..), Value(..), encode, decode, genericToEncoding
                  , toEncoding, defaultOptions, object, (.=))

import Data.ByteArray (convert, ByteArrayAccess)

import qualified Data.Binary as BIN

import GHC.Generics

import Link
import qualified VMap as VM
import VMap
import Abbrev
import Helpers (Key, eitherToMaybe)
import Val
import Select
import UI.Vi
import Note


apply0 = const

apply :: (a -> b) -> [a] -> Maybe b
apply f xs = if not (null xs)
                then Just $ f (head xs)
                else Nothing

apply' :: (a -> b) -> [a] -> Either String b
apply' f xs = if not (null xs)
                then Right $ f (head xs)
                else Left "expected at least one argument"


apply2 :: (a -> a -> b) -> [a] -> Maybe b
apply2 f xs = do
    xs' <- hasLen xs 2
    return $ f (xs' !! 0) (xs' !! 1)

apply2' :: (a -> a -> b) -> [a] -> Either String b
apply2' f xs = do
    xs' <- hasLen' xs 2
    return $ f (xs' !! 0) (xs' !! 1)


apply3 f xs = do
    xs' <- hasLen xs 3
    return $ f (xs' !! 0) (xs' !! 1) (xs' !! 2)

hasLen :: [a] -> Int -> Maybe [a]
hasLen xs n = 
    let xs' = take n xs
     in if length xs >= n
           then Just xs'
           else Nothing

hasLen' :: [a] -> Int -> Either String [a]
hasLen' xs n = 
    let xs' = take n xs
     in if length xs >= n
           then Right xs'
           else Left $ "expected argument list of length " ++ show n
                       ++ "but instead received one of length " 
                       ++ show (length xs)


data ServiceTypes alg = Blob' T.Text
              | Span' (AesonKey alg) Selection
              | Key' (AesonKey alg)
              | Ls T.Text
              | Err T.Text -- expand to wrap err enum
              deriving (Show, Eq, Generic)

instance FromJSON (ServiceTypes alg)
instance ToJSON (ServiceTypes alg) where
    toEncoding = genericToEncoding defaultOptions

getKey :: HashAlg alg => ServiceTypes alg -> Maybe (Key alg)
getKey (Key' ak) = fromAesonKey ak
getKey _ = Nothing

getBlob :: ServiceTypes alg -> Maybe T.Text
getBlob (Blob' t) = Just t 
getBlob _ = Nothing

getSpan :: HashAlg alg => ServiceTypes alg -> Maybe (Key alg, Selection)
getSpan (Span' ak sel) = fromAesonKey ak >>= \k-> return (k, sel)
getSpan _ = Nothing

getLs :: HashAlg alg => ServiceTypes alg -> Maybe T.Text
getLs (Ls t) = Just t
getLs _ = Nothing

getErr :: HashAlg alg => ServiceTypes alg -> Maybe T.Text
getErr (Err e) = Just e
getErr _ = Nothing




ecmds :: (Ord k, IsString k) 
      =>  M.Map k ([ServiceTypes SHA1] -> Either String (NoteS SHA1 (Maybe (ServiceTypes SHA1))))
ecmds = M.fromList [ ("loadf", apply' loadf'')
                    , ("deref", apply' derefAbbr'')
                    , ("link", apply2' linkAbbr'')
                    , ("derefK", apply' derefKey'')
                    ] -- add number of arguments expected to tuple (threeple, rly).
                   



cmds'' :: (Ord k, IsString k) 
       =>  M.Map k ([ServiceTypes SHA1] -> Maybe (NoteS SHA1 (Maybe (ServiceTypes SHA1))))
cmds'' = M.fromList [ ("loadf", apply loadf'')
                    , ("deref", apply derefAbbr'')
                    , ("link", apply2 linkAbbr'')
                    , ("derefK", apply derefKey'')
                    ] -- add number of arguments expected to tuple (threeple, rly).


cmds' :: (Ord k, IsString k, HashAlg alg) 
      =>  M.Map k ([T.Text] -> Maybe (NoteS alg (Maybe L.ByteString)))
cmds' = M.fromList [ ("loadf", apply (loadf' . toString))
                   , ("deref", apply derefAbbr')
                   , ("link", apply2 linkAbbr')
                   ] -- add number of arguments expected to tuple (threeple, rly).


runCmd' :: Cmd -> Maybe (NoteS SHA1 (Maybe (ServiceTypes SHA1)))
runCmd' (Cmd cmd args) = 
    let f = M.lookup cmd cmds''
     in f >>=  \f' -> f' args

runeCmd :: Cmd -> Either String (NoteS SHA1 (Maybe (ServiceTypes SHA1)))
runeCmd (Cmd cmd args) = 
    let f = M.lookup cmd ecmds
     in case f of 
          Just f' -> f' args
          Nothing -> Left "cmd not found."





--runCmd :: HashAlg alg => Cmd -> Note alg T.Text -> Maybe (NoteS alg (Maybe L.ByteString))
--runCmd (Cmd cmd args) note = Just . NoteS $ \


newtype NoteS alg a = NoteS { getNoteS :: (StateT (Note alg T.Text) IO) a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadState (Note alg T.Text))


runNoteS :: NoteS SHA1 a -> Note SHA1 T.Text -> IO (a, Note SHA1 T.Text)
runNoteS = runStateT . getNoteS

emptyNoteS :: NoteS alg ()
emptyNoteS = put newNote

-- REMOTE API

-- | Load a file into state.
loadf' :: HashAlg alg => FilePath -> NoteS alg (Maybe L.ByteString)
loadf' fp = do
    f <- liftIO (TIO.readFile fp)
    (Note lnk vm abbr sm) <- get
    let (vm', mK) = insertRawBlob f vm
    put (Note lnk vm' abbr sm)
    let lbs = fmap (encode . toAesonKey) mK
    return lbs

-- | Load a file into state.
loadf'' :: HashAlg alg => ServiceTypes SHA1 -> NoteS alg (Maybe (ServiceTypes alg))
loadf'' st = do
    let f :: Maybe String
        f = T.unpack <$> (getBlob st) 
    mText <- liftIO $ sequence (f >>= return . TIO.readFile)
    n@(Note lnk vm abbr sm) <- get
    case mText of 
      Just contents -> do let (vm', mK) = insertRawBlob (fromJust mText) vm
                              lbs = fmap (Key' . toAesonKey) mK
                          put (Note lnk vm' abbr sm)
                          return lbs
      Nothing -> do put n
                    return Nothing


derefAbbr' :: HashAlg alg =>  T.Text -> NoteS alg (Maybe L.ByteString)
derefAbbr' t = do
    n@(Note lnk vm abbr sm) <- get
    let mK = lengthen abbr t
        bs = mK >>= fmap encode . deref vm 
    x <- liftIO $ putStrLn $ show bs
    put n
    return bs

derefAbbr'' :: ServiceTypes SHA1 -> NoteS SHA1 (Maybe (ServiceTypes SHA1))
derefAbbr'' st = do
    n@(Note lnk vm abbr sm) <- get
    let mK = getKey st
        bs = mK >>= fmap Blob' . deref vm 
    x <- liftIO $ putStrLn $ show bs
    put n
    return bs

derefKey'' :: ServiceTypes SHA1 -> NoteS SHA1 (Maybe (ServiceTypes SHA1))
derefKey'' st = do
    n@(Note lnk vm abbr sm) <- get
    let mK = getKey st
        bs = mK >>= fmap Blob' . deref vm 
    x <- liftIO $ putStrLn $ show bs
    put n
    return bs

linkAbbr' :: HashAlg alg => T.Text -> T.Text -> NoteS alg (Maybe L.ByteString)
linkAbbr' a a' = do
    n@(Note lnk vm abbr sm) <- get
    let mK = Subj <$> lengthen abbr a
        mK' = Obj <$> lengthen abbr a'
        lnkr' = case (mK >>= \k -> return (\o -> Link.insert k o lnk)) <*> mK' of
                  Just lnk' -> lnk'
                  Nothing -> lnk
    put (Note lnkr' vm abbr sm)
    return $ Just "Success"

linkAbbr'' :: ServiceTypes SHA1 -> ServiceTypes SHA1 ->  NoteS SHA1 (Maybe (ServiceTypes SHA1))
linkAbbr'' st st' = do
    n@(Note lnk vm abbr sm) <- get
    let mK = Subj <$> getKey st
        mK' = Obj <$> getKey st'
        lnkr' = case (mK >>= \k -> return (\o -> Link.insert k o lnk)) <*> mK' of
                  Just lnk' -> lnk'
                  Nothing -> lnk
    put (Note lnkr' vm abbr sm)
    return . Just $ Err "Success"

lsvm' :: (Show alg, HashAlg alg) => NoteS alg (Maybe L.ByteString)
lsvm' = do
    note <- get
    let s = show' $ getVMap note
    put note
    return . Just $ toByteString s

lsvm'' :: (Show alg, HashAlg alg) => NoteS alg (Maybe (ServiceTypes alg))
lsvm'' = do
    note <- get
    let s = show' $ getVMap note
    put note
    return . Just . Ls $  T.pack s



newtype Result a = Result a deriving (Eq, Generic, Show)

instance ToJSON a => ToJSON (Result a) where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON a => FromJSON (Result a)


--instance ToJSON JSONByteString where
--    toJSON bs = String $ Base64.encode (getJBS bs)

newtype AesonKey alg = AesonKey { getAesonKey :: T.Text } deriving (Eq, Show, Generic)


instance ToJSON (AesonKey alg) where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON (AesonKey alg)

keyToByteString :: Key alg -> B.ByteString
keyToByteString  = convert

byteStringToKey :: HashAlg alg => B.ByteString -> Maybe (Key alg)
byteStringToKey = digestFromByteString

keyToText :: Key alg -> T.Text
keyToText = decodeUtf8 . Base64.encode . keyToByteString

textToKey :: HashAlg alg => T.Text -> Maybe (Key alg)
textToKey =  join . fmap byteStringToKey . eitherToMaybe . Base64.decode . encodeUtf8

ak = toAesonKey k0

toAesonKey :: HashAlg alg => Key alg -> AesonKey alg
toAesonKey = AesonKey . keyToText

fromAesonKey :: HashAlg alg => AesonKey alg -> Maybe (Key alg)
fromAesonKey = textToKey . getAesonKey

data Cmd = Cmd T.Text [ServiceTypes SHA1] deriving (Eq, Generic, Show)

getCmd :: Cmd -> T.Text
getCmd (Cmd cmd _) = cmd

getArgs :: Cmd -> [ServiceTypes SHA1]
getArgs (Cmd _ args) = args


instance ToJSON Cmd where

    toJSON (Cmd cmd args) = object [ "type" .= String "command" 
                                   , "name" .= String cmd
                                   , "args" .= toJSON args
                                   ]

    toEncoding = genericToEncoding defaultOptions

instance FromJSON Cmd


