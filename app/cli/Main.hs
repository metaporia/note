{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

-- CLI: use structuredCLI or just "trifecta"? I'm leaning towards the latter.

import qualified Data.Text as T
import Text.RawString.QQ
import Text.Trifecta hiding (Err)
import qualified Data.ByteString as B
import Control.Applicative
import qualified Data.Map as M

import GHC.Generics
import Data.Aeson hiding (Result(..))

import Parse hiding (parseLink, parseAlias)
import UI.Service hiding (Result)

import Control.Monad
import Control.Monad.IO.Class

import System.Console.Haskeline
import Data.Maybe
import Data.List


parseCmd' :: Parser String
parseCmd'  = pad' (some letter) 

pad' p = skipMany space *> p <* skipMany space

parseArgAlpha :: Parser String
parseArgAlpha = pad' (some letter)

parseArgAlphaNum :: Parser String
parseArgAlphaNum = skipMany (char ' ') *> (some alphaNum)

parseArgNoneBut :: Parser String
parseArgNoneBut = skipMany (char ' ') *> (some (noneOf "\n "))

parseServiceType :: Parser (ServiceTypes alg)
parseServiceType = undefined

pBlob' :: Parser (ServiceTypes alg)
pBlob' = (Blob' . T.pack) <$> (skip (string "blob") 
                           *> (some (noneOf "\n ")))
pSpan' :: Parser (ServiceTypes alg)
pSpan' = undefined --Span' <$> 

-- a cmd is one of:
--
--  (1) derefK key
--  (2) loadf filepath
--  (3) abbrev key
--  (4) deref abbr
--  (5) lsvm 
--  (6) lslnk
--  (7) alias abbr key 
--  (8) link abbr abbr'

cons head tail = head : tail
pCmd :: Parser Cmd
pCmd = let ps = map try [parseAlias, parseLink, parseDeref, parseDerefK, parseLoadf, parseAbbr, parseLslnk, parseLsvm]
        in choice ps <* skipMany (oneOf "\n ")


parseAlias :: Parser Cmd
parseAlias = Cmd <$> cmd "alias" <* skip (char ' ')
                 <*> (cons <$> ((Abbr . T.pack) <$> some (noneOf "\n ") <* skip (char ' '))
                            <*> fmap (return . Key' . AesonKey . T.pack)  (some (noneOf "\n ")))
parseLink :: Parser Cmd
parseLink = Cmd <$> cmd "link" <* skip (char ' ')
                <*> fmap (return . Abbr . T.pack) (count 2 (noneOf "\n "))
parseDeref :: Parser Cmd
parseDeref = Cmd <$> (fmap T.pack $ (string "deref" <* skip (char ' '))) 
                 <*> (fmap (return . Abbr . T.pack) (some (noneOf "\n ")))


parseDerefK :: Parser Cmd
parseDerefK = Cmd <$> (fmap T.pack $ (string "derefK" <* skip (char ' '))) 
                  <*> (fmap (return . Key' . AesonKey . T.pack) (some (noneOf "\n ")))
                                         -- ^ 'toAesonKey' or 'AesonKey'
                                         -- likely the latter  ^^^

parseLoadf :: Parser Cmd
parseLoadf = Cmd <$> (fmap T.pack $ (string "loadf" <* skip (char ' ')))
                 <*> (fmap (return . Blob' . T.pack) (some (noneOf "\n ")))


cmd :: String -> Parser T.Text
cmd w = fmap T.pack (string w) 

parseAbbr :: Parser Cmd
parseAbbr = Cmd <$> cmd "abbr" <* skip (char ' ')
                <*> (fmap (return . Key' . AesonKey . T.pack) 
                          (some (noneOf "\n ")))
            
parseLslnk :: Parser Cmd
parseLslnk = Cmd <$> cmd "lslnk"
                <*> fmap (return . Err . T.pack. ("unused arg: "++)) (many (noneOf "\n "))

parseLsvm :: Parser Cmd
parseLsvm = Cmd <$> cmd "lsvm"
                <*> fmap (return . Err . T.pack. ("unused arg: "++)) (many (noneOf "\n "))









{-
parseCmd :: Parser Cmd
parseCmd = 
    Cmd <$> fmap T.pack parseCmd' 
        <*> (fmap .fmap) T.pack (many parseArgNoneBut)
        <* skipMany (char '\n')

parseQuoted :: Parser String
parseQuoted = skipMany (char ' ')
           *> skip (char '"')
           *> some (noneOf "\"")
           <* skip (char '"')

parseCustCmd' :: String -> Parser ([T.Text] -> Cmd)
parseCustCmd' s = Cmd <$> (fmap T.pack $ pad' (string s))

parseCusCmd :: String -> Int -> Parser Cmd
parseCusCmd cmdStr nargs =
    (parseCustCmd' cmdStr)
    <*>  (fmap . fmap) T.pack (count nargs parseArgAlphaNum)
    <* skipMany (char ' ')
    <* skip (char '\n')
    <* eof
-}
main :: IO ()
main = serve
{-
c = try (parseCusCmd "command" 2) <?> "command <arg> <arg>\\n" 
d = try (parseCusCmd "eal" 3) <?> "eal <arg> <arg> <arg>\\n"

t = c <|> d

-- | 'Command' enumerates the plugin API for note. This CLI is for testing
-- purposes, during which I will play both user and backend via the provided
-- hybrid interface.
data Command = WriteWithQuoted String -- | Write inline blob val.
             | WriteWithFile String -- | Create val from file contents.
             | Link String String  -- | Link two abbrev.
             | Alias String String --  | Bind alias to abbrev key.
             | Deref String  -- | Deref abbrev.
             | DerefA String -- | Deref Alias.
             | View String Int -- | Simulate user viewing of abbrev's blob at idx.
             deriving (Eq, Show, Generic)


cmds = M.empty :: M.Map String Command

cmd = Cmd "load" ["key0", "key1"]
-}
replEcho :: IO ()
replEcho  = do
    let loop = do putStr "> "
                  l <- getLine
                  case l of
                    "exit" -> return () 
                    _ -> case parse pCmd l of 
                           Success cmd -> do print cmd
                                             x <- oas cmd
                                             case x of
                                               Just x' -> print x'
                                               Nothing -> print "Nothing"
                                             loop
                           _ -> putStrLn l *> loop 
    loop
    
repl' = replEcho

parse :: Parser a -> String -> Result a
parse p s = parseString p mempty s

{-
resultToMaybe :: Result a -> Maybe a
resultToMaybe (Success v) = Just v
resultToMaybe (Failure _) = Nothing
-}

repl :: IO ()
repl = runInputT (Settings { complete = completeWord Nothing " \t" wordCompleter 
                           , historyFile = Nothing
                           , autoAddHistory = True
                           }) loop

    where 
        loop :: InputT IO ()
        loop = do
            minput <- getInputLine "> "
            case minput of 
              Nothing -> return ()
              Just "quit" -> return ()
              Just input -> do outputStrLn input
                               case parse pCmd input of
                                 Success cmd -> liftIO $ do x <- oas' cmd
                                                            case x of
                                                              Just x' -> print x'
                                                              Nothing -> print "Nothing"
                                 _ -> liftIO $ putStrLn input

                               loop


cmds = [ "deref"
       , "derefK"
       , "alias"
       , "abbr"
       , "lsvm"
       , "lslnk"
       , "loadf"
       ]
comp = ("??", map simpleCompletion cmds)
wordCompleter :: String -> IO [Completion]
wordCompleter s = do
    ks <- getKeys
    let matches = filter (isPrefixOf s) (cmds ++ ks)
    
    return $ map simpleCompletion matches


-- fetch a list of all keys in the current VMap
getKeys :: IO [String]
getKeys = do
    mST <- oas' (Cmd "vmkeys" [])
    let st = case mST of
                Just x  -> x
                Nothing -> Nil
        ret = case st of
                List ks -> ks
                Nil -> []
                
    return (map (T.unpack . (\(Key' (AesonKey k))-> k))  ret)



