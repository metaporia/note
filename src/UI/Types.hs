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

import Control.Monad.Trans.Either
import Control.Monad.Trans.State (StateT, runStateT)
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Identity
import Control.Monad.Except 

import Control.Exception


import GHC.Generics

import Link
import qualified VMap as VM
import VMap
import Abbrev
import Helpers (Key, eitherToMaybe, maybeToEither, convertException)
import Val
import Select
import UI.Vi
import Note

type Note' = Note SHA1 T.Text
type ST = ServiceTypes SHA1
-- transformer ordering case one
newtype NoteS' err a = NoteS' { getNoteS' :: StateT Note' (EitherT err IO) a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadState (Note SHA1 T.Text), MonadError err)

emptyNoteS' :: NoteS' err ()
emptyNoteS' = put newNote 
 
runNoteS' :: NoteS' err a-> Note' -> IO (Either err (a, Note'))
runNoteS' = (runEitherT .) . runStateT . getNoteS'

runWith' m s = runNoteS' m s

 

derefKey :: ST -> NoteS' String (ServiceTypes SHA1)
derefKey st = do
    n@(Note lnk vm abbr sm) <- get
    k <- liftEither $ getKey st
    st' <- liftEither . fmap Blob' . deref' vm $ k
    put n
    return st'

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
              | Abbr T.Text
              | Ls T.Text
              | Err T.Text -- expand to wrap err enum
              | Msg T.Text
              deriving (Show, Eq, Generic)

instance FromJSON (ServiceTypes alg)
instance ToJSON (ServiceTypes alg) where
    toEncoding = genericToEncoding defaultOptions

getKey :: HashAlg alg => ServiceTypes alg -> Either String (Key alg)
getKey (Key' ak) = fromAesonKey ak
getKey _ = Left "Expected Key', found other variant."

getBlob :: ServiceTypes alg -> Either String T.Text
getBlob (Blob' t) = Right t 
getBlob _ = Left "Expected Blob', but found other variant."

getSpan :: HashAlg alg => ServiceTypes alg -> Either String (Key alg, Selection)
getSpan (Span' ak sel) = fromAesonKey ak >>= \k-> return (k, sel)
getSpan _ = Left "Expected Span', found other variant."

getLs :: HashAlg alg => ServiceTypes alg -> Either String T.Text
getLs (Ls t) = Right t
getLs _ = Left "Expected Ls, but found other variant."

getErr :: HashAlg alg => ServiceTypes alg -> Either String T.Text
getErr (Err e) = Right e
getErr _ = Left "Expected Err, but found other variant."

getAbbr :: HashAlg alg => ServiceTypes alg -> Either String T.Text
getAbbr (Abbr e) = Right e
getAbbr _ = Left "Expected Msg, but found other variant."

getMsg :: HashAlg alg => ServiceTypes alg -> Either String T.Text
getMsg (Msg e) = Right e
getMsg _ = Left "Expected Msg, but found other variant."



ecmds :: (Ord k, IsString k) 
      =>  M.Map k ([ST] -> Either String (NoteS' String ST))
ecmds = M.fromList [ ("derefK", apply' derefKey)
                   , ("loadf", apply' eloadf)
                   , ("deref", apply' ederefAbbr)
                   , ("abbrev",  apply' eabbr)
                   , ("lsvm",  const (return elsvm))
                   , ("lslnk", const (return elslnk))
                   , ("alias", apply2' ealias)
                   , ("link", apply2' elinkAbbr) ]
-- new "e*" API
ealias :: ST -> ST -> NoteS' String ST
ealias st st' = do
    alias <- liftEither $ getAbbr st
    key <- liftEither $ getKey st'
    n@(Note lnk vm abbr sm) <- get
    let abbr' = alias' abbr alias key
    put (Note lnk vm abbr' sm)
    return $ Msg "alias successfully registered"

elinkAbbr :: ST -> ST -> NoteS' String ST
elinkAbbr st st' = do
    n@(Note lnk vm abbr sm) <- get
    s <- liftEither $ Subj <$> getKey st
    o <- liftEither $ Obj <$> getKey st'
    let lnkr' = Link.insert s o lnk
    put (Note lnkr' vm abbr sm)
    return $ Msg "link successfuly registered"


ederefAbbr :: ST -> NoteS' String ST
ederefAbbr st = do
    n@(Note lnk vm abbr sm) <- get
    alias <- liftEither $ getAbbr st
    k <- liftEither $ case lengthen abbr alias of
                       Just k -> Right k
                       Nothing -> Left "abbr not found in Abbrev"
    val <- liftEither $ 
        case deref vm k of
          Just b -> Right b 
          Nothing -> Left "could not deref key"
    put n
    return (Blob' val)

eloadf :: ST -> NoteS' String ST
eloadf st = do
    fp <- liftEither . fmap T.unpack $ getBlob st
    eCont <- liftIO $ (try (TIO.readFile fp) :: IO (Either SomeException T.Text))
    contents <- liftEither $ convertException eCont
    n@(Note lnk vm abbr sm) <- get
    let (vm', mK) = insertRawBlob contents vm
        k = case mK of
              Just k' -> Right k'
              Nothing -> Left "insertion failed."
    st' <- liftEither $ (Key' . toAesonKey) <$> k
    put (Note lnk vm' abbr sm)
    return st'

elsvm :: NoteS' String ST
elsvm = do
    note <- get
    let s = show' $ getVMap note
    put note
    return . Ls $  T.pack s

elslnk :: NoteS' String ST
elslnk = do
    note <- get
    let s = pshow $ getLinks note
    put note
    return . Ls $ T.pack s


-- Deprecated RPC dispatcher
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

runeCmd :: Cmd -> Either String (NoteS' String ST)
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
loadf'' st =  undefined {- do
    fp <- liftEither $ T.unpack <$> getBlob st
    eContents <- liftIO $  TIO.readFile pf
    contents <- liftEither eContents
    n@(Note lnk vm abbr sm) <- get
    insertRawBlob contents vm
      lbs = fmap (Key' . toAesonKey) mK
                          put (Note lnk vm' abbr sm)
                          return lbs
                          -}
-- | Load a file into state.
data Error = Error String deriving (Eq, Show)
instance Exception Error


derefAbbr' :: HashAlg alg =>  T.Text -> NoteS alg (Maybe L.ByteString)
derefAbbr' t = do
    n@(Note lnk vm abbr sm) <- get
    let mK = lengthen abbr t -- err: abbr not found
        bs = mK >>= fmap encode . deref vm 
    x <- liftIO $ putStrLn $ show bs
    put n
    return bs

derefAbbr'' :: ServiceTypes SHA1 -> NoteS SHA1 (Maybe (ServiceTypes SHA1))
derefAbbr'' st = do
    n@(Note lnk vm abbr sm) <- get
    let mK = eitherToMaybe $ getKey st
        bs = mK >>= fmap Blob' . deref vm 
    x <- liftIO $ putStrLn $ show bs
    put n
    return bs

derefKey'' :: ServiceTypes SHA1 -> NoteS SHA1 (Maybe (ServiceTypes SHA1))
derefKey'' st = do
    n@(Note lnk vm abbr sm) <- get
    let mK = eitherToMaybe $ getKey st
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
    let mK = Subj <$> (eitherToMaybe $ getKey st)
        mK' = Obj <$> (eitherToMaybe $ getKey st')
        lnkr' = case (mK >>= \k -> return (\o -> Link.insert k o lnk)) <*> mK' of
                  Just lnk' -> lnk'
                  Nothing -> lnk
    put (Note lnkr' vm abbr sm)
    return . Just $ Msg "Success"

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

eabbr :: ST -> NoteS' String ST
eabbr st = do
    key <- liftEither $ getKey st
    n@(Note lnk vm abbr sm) <- get
    let (abr, abbr') = abbrev' abbr key 
    put (Note lnk vm abbr' sm)
    return (Abbr abr)






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

textToKey' :: HashAlg alg => T.Text -> Either String (Key alg)
textToKey' t =  (Base64.decode . encodeUtf8) t >>= maybeToEither . byteStringToKey  

ak = toAesonKey k0

toAesonKey :: HashAlg alg => Key alg -> AesonKey alg
toAesonKey = AesonKey . keyToText

fromAesonKey :: HashAlg alg => AesonKey alg -> Either String (Key alg)
fromAesonKey = textToKey' . getAesonKey

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


