{-# LANGUAGE OverloadedStrings #-}
module UI.Service where

import qualified Network.Socket as Sock
import Network.Socket hiding (sendTo, send, recv)
import qualified Control.Exception as E
import qualified Network.Socket.ByteString.Lazy as NBL (send, sendAll, recv)
import Network.Socket.ByteString (send, sendAll, recv)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Conversion
import System.IO

import Control.Concurrent (forkIO)
import Control.Monad (forever)

import Data.Binary
import Data.Bits

import Text.Printf

import Data.Int (Int64)

-- | `note` service: 
--
-- * listen for content requests of the form @load <key> <db>@ which should
-- respond with the result of @deref db key@
--
-- * listen for location/context requests of the form @locate <key> <db>@ which
-- should respond with a list of keys (those possible aliases for the conctent
-- around the current cursor position).
-- 
--
-- `note` client:
--
-- For use in e.g., [neo]vim plugin.
--
-- * yield keys of currently viewed blob -- name the buffer? keep a dict?
--
--      - this is kind of tricky as there may not be /one/ key for a given
--      stream. perhaps a safe default would be to return the key of the
--      longest val (that is, of the Blob variant)--that's it: deref Spans until
--      a blob is retrieved, then return teh key to that blob. Thus, /any/ key
--      that references a part of the current content stream should suffice to
--      identitify the context, right?
--
--      - some indication of the scope of each span may help users pick which
--      fits their query.
--      
--      - the key(s) of the smallest span around the query cursorPosn should be
--      returned first
--
-- * alias (abbrev?) keys
--
-- * link aliases, keys
--
-- * create new blobs
--
-- * load blob
--
-- * preview associated blobs (w abbrev keys)
--
--      - this will also be tricky, but more due to the UI limitations of vim
--      coupled with the potentially large number of associated 'Val's, than to
--      some intrinsic property of the data structures involved.
--

-- server
server' :: IO ()
server' = do
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bind sock (SockAddrInet 4242 iNADDR_ANY)
    listen sock 2
    mainLoop sock

-- server
server :: IO ()
server = do
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bind sock (SockAddrInet 4242 iNADDR_ANY)
    listen sock 10
    forever $ do
        (client, _) <- accept sock
        --clientHandle <- socketToHandle client ReadMode 
        --hSetBuffering clientHandle NoBuffering
        forkIO $ do
            blen <- NBL.recv client 8
            let len = (decode blen :: Int64)
                -- Why, if I use just 'recv', does the following result in an
                -- "out of memory" error, and crash?
                --
                --  * mixing strict 'n lazy 'ByteString' ? 
                --      - No--(reversion did not break it)
                --
                --  * arcane secret
                --      - not yet disproven
                --
            --contents <- NBL.recv client (fromIntegral len)
            contents <- recv client (fromIntegral len)
            putStrLn $ "received payload of length " ++ show len
            print contents
            close client
            --hClose clientHandle 
        

readAll :: Handle -> B.ByteString -> IO B.ByteString
readAll h buf = do
    eof <- hIsEOF h
    if eof
       then return buf
       else (B.hGetLine h)>>= (\bs -> (bs `mappend`) <$> (readAll h ""))


mainLoop :: Socket -> IO ()
mainLoop sock = do
    --  conn :: (Socket, SockAddr)
    conn <- accept sock
    forkIO (runConn conn)
    mainLoop sock

runConn :: (Socket, SockAddr) -> IO ()
runConn (sock, _) = do
    send sock "Hello\n"
    handle <- socketToHandle sock ReadMode 
    hSetBuffering handle NoBuffering

    msg <- readAll handle ""
    putStrLn $ show sock ++ " yielded: " ++ show msg
    putStrLn "handler-socket receiving shutdown"


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


-- NB:
--
-- > (ntohl . decode . encode . htonl $ l) == l
--
pack' :: Binary a => a -> BL.ByteString
pack' payload = encode payload
    -- @ntohl . decode@ on the other side.




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

