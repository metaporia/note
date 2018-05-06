{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
module UI.Service (server, server', oas, module Types) where

import qualified Network.Socket as Sock
import Network.Socket.Options
import Network.Socket hiding (sendTo, send, recv)
import qualified Control.Exception as E
import qualified Network.Socket.ByteString.Lazy as NBL (send, sendAll, recv)
import Network.Socket.ByteString (send, sendAll, recv)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as BLC8
import Data.ByteString.Conversion
import Data.ByteString.Builder

import System.Posix.Signals


import Crypto.Hash 

import qualified Data.Text as T
import GHC.Generics

import System.IO

import System.Exit
import Control.Concurrent ( forkIO, myThreadId, ThreadId, threadDelay
                          , newEmptyMVar, putMVar)
import qualified Control.Exception as E
import Control.Monad (forever, foldM)


import Data.Binary hiding (get, put)
import Data.Bits

import Text.Printf

import Data.Int (Int64)
import Data.Aeson hiding (encode, decode, Result)
import qualified Data.Aeson as A

import Data.Maybe (fromJust, fromMaybe)
import Data.Either

import Control.Monad.State
import Data.Monoid ((<>))
import Control.Exception hiding (Handler)
import Control.Monad.Except


-- Internal
import Note hiding (runWith)

import UI.Types as Types
import VMap (HashAlg)
import Helpers


chunk_size = 32768 -- 2 ^ 15
recv_timeout = 1000000

encode' :: ToJSON a => a -> BL.ByteString
encode' = A.encode

decode' :: FromJSON a => BL.ByteString -> Maybe a
decode' = A.decode

server :: IO ()
server = undefined {-do
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bind sock (SockAddrInet 4242 iNADDR_ANY)
    listen sock 10
    forever $ do
        (client, _) <- accept sock
        --clientHandle <- socketToHandle client ReadMode 
        --hSetBuffering clientHandle NoBuffering
        setRecvTimeout client recv_timeout
        forkIO $ do
            (contents, len) <- recvAll client
            putStrLn $ "received payload of length " ++ show len
            case (A.decode contents :: Maybe Cmd) of
              Just cmd -> do BLC8.putStrLn contents 
                             print cmd
                             if getCmd cmd == "deref" 
                                then let s =  show . fst $ runState (derefAbbrNS (getArgs cmd !! 0)) note'
                                         arg0 = case null (getArgs cmd) of
                                                    False -> Just $ getArgs cmd !! 0
                                                    True -> Nothing
                                         t :: T.Text
                                         t = case lookupRun (getCmd cmd) (fromMaybe "" arg0) note' of
                                               Just x -> x
                                               Nothing -> "Lookup failed"
                                      in (print $ "abbr derefed. sending\n")
                                         *> NBL.send client (encode t) *> return ()
                                else return ()
              Nothing -> print contents
            bytes <- NBL.send client . encode $ "ack " `mappend` toByteString' len
            putStrLn ""
            close client
            --hClose clientHandle 
            -}

ex = do
    tid <- myThreadId
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bind sock (SockAddrInet 4242 iNADDR_ANY)
    listen sock 2

    installHandler sigINT (Catch (do E.throwTo tid E.UserInterrupt
                                     close sock)) Nothing
    threadDelay 5000000
    
server' :: IO ()
server' = do
    tid <- myThreadId
    --this <- newEmptyMVar
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bind sock (SockAddrInet 4242 iNADDR_ANY)
    listen sock 10
    -- (posix) system signal handling
    let loop n = do
            (client, _) <- accept sock
            --clientHandle <- socketToHandle client ReadMode 
            --hSetBuffering clientHandle NoBuffering
            ---
            
            setRecvTimeout client recv_timeout
            (contents, len) <- recvAll client
            putStrLn $ "received payload of length " ++ show len
            let mNote = do cmd <- (A.decode contents :: Maybe Cmd)
                           noteS <- runCmd' cmd
                           return noteS
                mRes = sequence $ (flip runWith n) <$> mNote

            mRes' <- mRes
            putStrLn $ show mRes'
            let ret = do (mST, nextState) <- mRes'
                         st <- mST
                         return (st, nextState)
            case ret of
              Just (st, nextState) -> do NBL.sendAll client . encode . A.encode $ st
                                         close client
                                         putStrLn "looping newstate"
                                         loop nextState
              Nothing -> if "exit" `BLC8.isPrefixOf` contents 
                            then do { close client; close sock }
                            else do { close client; putStrLn "looping" ; loop n }

    let intHandler :: Handler
        intHandler = Catch $ do 
                E.throwTo tid E.UserInterrupt
                close sock
                putStrLn "caught SIGINT; closed sock"
                --putMVar this ()


    installHandler sigINT intHandler Nothing

    loop note'

eserver :: IO ()
eserver = do
    tid <- myThreadId
    --this <- newEmptyMVar
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bind sock (SockAddrInet 4242 iNADDR_ANY)
    listen sock 10
    -- (posix) system signal handling
    let loop :: Note' -> IO ()
        loop n = do
            (client, _) <- accept sock
            --clientHandle <- socketToHandle client ReadMode 
            --hSetBuffering clientHandle NoBuffering
            ---
            
            setRecvTimeout client recv_timeout
            (contents, len) <- recvAll client
            putStrLn $ "received payload of length " ++ show len
            let eCmd = maybeToEither (A.decode contents :: Maybe Cmd)
                eNote = eCmd >>= runeCmd
                mRes = (flip runWith' n) <$> eNote
                embedded :: IO (Either String (ST, Note'))
                embedded = runWith' (handleCmd eCmd) n
            eTup <- embedded
            case eTup of
              -- 'apply' err
              Left err -> do print err
                             NBL.sendAll client . encode . A.encode . Err $ T.pack err
                             close client
                             putStrLn "looping newstate--lerr"
                             loop n
              -- success case
              Right (st, n') -> do NBL.sendAll client . encode . A.encode $ st
                                   close client
                                   putStrLn "looping newstate"
                                   loop n'

    let intHandler :: Handler
        intHandler = Catch $ do 
                E.throwTo tid E.UserInterrupt
                close sock
                putStrLn "caught SIGINT; closed sock"
                --putMVar this ()


    installHandler sigINT intHandler Nothing

    loop note'
    close sock


-- | Embed application of 'Cmd' inside 'NoteS'' state.
handleCmd :: Either String Cmd -> NoteS' String ST 
handleCmd eCmd = do
    n <- get
    cmd <- liftEither eCmd
    noteS <- liftEither $ runeCmd cmd
    e <- liftIO $ runWith' noteS n
    (st, n') <- liftEither e
    put n'
    return st

handleCmd' :: Socket -> Socket -> Note' -> IO ()
handleCmd' = undefined

    
    


showSock sock = do
    isc <- isConnected sock
    isb <- isBound sock
    isr <- isReadable sock
    isl <- isListening sock
    putStrLn $ show isc
    putStrLn $ show isb
    putStrLn $ show isr
    putStrLn $ show isl

contents = encode . A.encode $ Blob' "AOEuhaoeuaoeuaoeu"
cmd' = encode . A.encode $ Cmd "deref" [Blob' "spec"]

test :: IO (ServiceTypes SHA1, Note SHA1 T.Text)
test = do
    -- we're in the 'Maybe' monad here
    let mNote = do cmd <- (A.decode (decode cmd' :: BLC8.ByteString) :: Maybe (Cmd))
                   noteS <- (runCmd' cmd :: Maybe (NoteS SHA1 (Maybe (ServiceTypes SHA1))))
                   return noteS
        mRes = (runWith <$> mNote) <*> Just note'
--    putStrLn $ show mRes'
    let ret = 
            case mRes of 
              Nothing -> do putStrLn "arg error. see applyN"
                            putStrLn "nthing case"
                            return (Err "eaou", note')
              Just ioval -> do (mST, nextNote) <- ioval
                               putStrLn $ show mST
                               return (fromMaybe (Err "no ServiceType available") mST, nextNote)

    (sts, next) <- ret
    putStrLn $ "about to send: " ++ (show sts)
    return (sts, next)
       
runWith :: NoteS SHA1 (Maybe (ServiceTypes SHA1))
        -> Note SHA1 T.Text 
        -> IO (Maybe (ServiceTypes SHA1), Note SHA1 T.Text)
runWith noteS s = runNoteS noteS s


shutdownServer :: Socket -> IO ()
shutdownServer sock = NBL.sendAll sock (encode ("exit":: BL.ByteString) :: BL.ByteString)
        

-- client
openAndConn :: HostName -> String -> IO (Socket, SockAddr)
openAndConn host port = do
    -- host, port lookup. err or nonempty [AddrInfo].
    addrInfo <- getAddrInfo Nothing 
                            (Just host)
                            (Just port)
    let serverInfo = head addrInfo
        sockAddr = addrAddress serverInfo
    sock <- socket (addrFamily serverInfo)
                   (addrSocketType serverInfo)
                   (addrProtocol serverInfo)
    
    connect sock sockAddr
    return (sock, sockAddr)


send' :: (Socket, SockAddr) -> IO (Socket, SockAddr)
send' (sock, sockAddr) = do
--    putStrLn "please enter '>host port'"
--    putStr ">"
--    ws <- words <$> getLine
--    let host = ws !! 0
--        port = ws !! 1
    --     kjj
    msg' <- B.readFile "specificity.md"
    let sendAll' sock msg = 
            do bytes <- NBL.send sock msg
               if bytes < (BL.length msg)
                  then sendAll' sock (BL.drop bytes msg) 
                  else return ()
    sendAll' sock (pack' msg')
    putStrLn "sent!"
    return (sock, sockAddr)


sendCmd :: (Socket, SockAddr) -> Cmd -> IO (Maybe (ServiceTypes SHA1))
sendCmd (sock, _) cmd = do
    NBL.sendAll sock . encode $ A.encode cmd 
    putStrLn "sent!"
    (contents, _) <- recvAll sock
    BLC8.putStrLn contents
    close sock
    return (A.decode contents :: Maybe (ServiceTypes SHA1))

oas :: Cmd -> IO (Maybe (ServiceTypes SHA1))
oas cmd = do
    conn <- openAndConn "localhost" "4242"
    mST <- sendCmd conn cmd
    return mST


-- NB:
--
-- > (ntohl . decode . encode . htonl $ l) == l
--
pack' :: Binary a => a -> BL.ByteString
pack' payload = encode payload
    -- @ntohl . decode@ on the other side.


-- | A (hopefully) less moronic chunking strategy.
--
-- NB: 'mappend'ing 'Builder's has O(1) runtime!
recvAll :: Socket -> IO (BL.ByteString, Int64)
recvAll sock = do
    blen <- NBL.recv sock 8
    let len = fromIntegral (decode blen :: Int64)
    contents <- go sock len (lazyByteString "")
    return (contents, len)
    -- go sock len ""
    where go :: Socket -> Int64 -> Builder -> IO BL.ByteString
          go sock len buf = do
              chunk <- NBL.recv sock chunk_size
--             putStrLn "receiving..."
              let chunkLen = BL.length chunk
                  nextLen = len - chunkLen
                  buf' = buf <> (lazyByteString chunk)
              if nextLen > 0
                 then go sock nextLen buf' -- optimize?
                 else return (toLazyByteString buf')


unroll :: Word32 -> [Word8]
unroll x = map fromIntegral [ x .&. 0xff
                            , (x .&. 0xff00) `shiftR` 8
                            , (x .&. 0xff0000) `shiftR` 16 
                            , (x .&. 0xff000000) `shiftR` 24
                            ]
tuple :: [a] -> (a, a, a, a)
tuple as = (as !! 0, as!!1,as!!2,as!!3)

roll' = roll . tuple

roll :: (Word8, Word8, Word8, Word8) -> Word32
roll (w, x, y, z) = let ws :: [Word32]
                        ws =  [ fromIntegral w
                              , (fromIntegral x) `shiftL` 8
                              , (fromIntegral y) `shiftL` 16 
                              , (fromIntegral z) `shiftL` 24
                            ]
                         in foldr (.|.) 0x00000000 $ ws

pbits :: PrintfType r => r
pbits =  printf "%b\n"


encodingExample :: IO ()
encodingExample = do
    f <- B.readFile "specificity.md"
    let l = fromIntegral (B.length f) :: Word32 

    putStrLn $ "l = " ++ show l
    putStrLn $ "encode . htonl $ l = " ++ show (encode . htonl $ l)
    putStrLn $ "decode . encode . htonl $ l =" ++ show (decode . encode . htonl $ l :: Word32)
    putStrLn $ "ntohl . decode . encode . htonl $ l =" ++ show (ntohl . decode . encode . htonl $ l)

