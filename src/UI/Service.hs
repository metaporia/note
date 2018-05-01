{-# LANGUAGE OverloadedStrings #-}
module UI.Service where

import qualified Network.Socket as Sock
import Network.Socket hiding (sendTo, send, recv)
import qualified Control.Exception as E
import Network.Socket.ByteString (send, sendAll, sendTo, recv)
import qualified Data.ByteString as B
import System.IO

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
server :: IO ()
server = do
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bind sock (SockAddrInet 4242 iNADDR_ANY)
    listen sock 2
    mainLoop sock

mainLoop :: Socket -> IO ()
mainLoop sock = do
    conn <- accept sock
    runConn conn
    mainLoop sock

runConn :: (Socket, SockAddr) -> IO ()
runConn (sock, _) = do
    send sock "Hello\n"
    handle <- socketToHandle sock ReadMode 
    hSetBuffering handle NoBuffering
    let readAll :: Handle -> B.ByteString -> IO B.ByteString
        readAll h buf = do
            eof <- hIsEOF h
            if eof
               then return buf
               else (B.hGetLine h)>>= (\bs -> (bs `mappend`) <$> (readAll h ""))
            
    msg <- readAll handle ""
    putStrLn $ show sock ++ " yielded: " ++ show msg

-- client
open :: HostName -> String -> IO (Socket, SockAddr)
open host port = do
    -- host, port lookup. err or nonempty [AddrInfo].
    addrInfo <- getAddrInfo Nothing 
                            (Just host)
                            (Just port)
    let serverInfo = head addrInfo
    sock <- socket (addrFamily serverInfo)
                   (addrSocketType serverInfo)
                   (addrProtocol serverInfo)
    

    return (sock, addrAddress serverInfo)


client :: IO ()
client = do
--    putStrLn "please enter '>host port'"
--    putStr ">"
--    ws <- words <$> getLine
--    let host = ws !! 0
--        port = ws !! 1

    let host = "127.0.0.1" 
        port =  "4242"
    (sock, sockAddr) <- open host port
    connect sock sockAddr
    putStrLn $"excellent, now enter the string you would like to send to " ++ host ++ " at "++ port ++"."
    msg <- B.readFile "specificity.md"
    bytesSent <- send sock msg
    putStrLn $ "sent " ++ (show bytesSent) ++  " bytes"
    return ()



