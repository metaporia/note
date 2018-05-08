{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module UI.REPL where

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

import System.Process (system)

import System.Console.Haskeline
import Data.Maybe
import Data.List
import Data.Either (rights)
import Data.Char (digitToInt)

import Crypto.Hash

import Select (Selection(..))


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
pCmd = let ps = map try [ parseAlias
                        , parseLink
                        , parseDeref
                        , parseDerefK
                        , parseLoadf
                        , parseAbbr
                        , parseLslnk
                        , parseLsvm
                        , parseAbbrKeys
                        , parseVmKeys
                        , parseLinksFrom
                        , parseLinksTo
                        , parseLinksFromK
                        , parseLinksToK
                        , parseSelectK
                        , parseSelect
                        ]
        in choice ps <* skipMany (oneOf "\n ")

parseSelectK :: Parser Cmd
parseSelectK = Cmd <$> cmd "selectK" <* skip (char ' ')
                   <*> fmap return parseSpan 

parseSelect :: Parser Cmd
parseSelect = Cmd <$> cmd "select" <* skip (char ' ')
                 <*> (cons <$> ((Abbr . T.pack) <$> some (noneOf "\n ") <* skip (char ' '))
                           <*> fmap return parseSel)

parseSpan :: Parser ST
parseSpan = Span' <$> parseAesonKey <*> parseSelection

parseAesonKey :: Parser (AesonKey SHA1)
parseAesonKey = do x <- parseKey
                   case x of 
                     Key' ak -> return ak
                     _ -> unexpected "hex to key conversion failed"

parseSel :: Parser ST
parseSel = Sel' <$> (T.pack <$> some digit <* skip (char ' '))
                <*> (T.pack <$> some digit)

parseSelection :: Parser Selection
parseSelection = Sel <$> (read <$> some digit) <* skip (char ' ')
                     <*> (read <$> some digit)

parseLinksFromK :: Parser Cmd
parseLinksFromK = Cmd <$> cmd "linksfromK" <* skip (char ' ')
                     <*> fmap return parseKey

parseLinksToK :: Parser Cmd
parseLinksToK = Cmd <$> cmd "linkstoK" <* skip (char ' ')
                     <*> fmap return parseKey

parseKey :: Parser ST
parseKey = f <$> some (noneOf "\n ") 
    where f s = case hexStrToKey s of
                  Just k -> Key' $ toAesonKey k
                  Nothing -> Err "could not convert text to key"


parseLinksFrom :: Parser Cmd
parseLinksFrom = Cmd <$> cmd "linksfrom" <* skip (char ' ')
                     <*> fmap (return . Abbr . T.pack) (some (noneOf "\n "))

parseLinksTo :: Parser Cmd
parseLinksTo = Cmd <$> cmd "linksto" <* skip (char ' ')
                   <*> fmap (return . Abbr . T.pack) (some (noneOf "\n "))

parseAbbrKeys :: Parser Cmd
parseAbbrKeys = Cmd <$> cmd "abbrkeys" <* skipMany (char ' ')
                    <*> fmap (return . Err . T.pack. ("unused arg: "++)) (many (noneOf "\n "))

parseVmKeys :: Parser Cmd
parseVmKeys = Cmd <$> cmd "vmkeys" <* skipMany (char ' ')
                    <*> fmap (return . Err . T.pack. ("unused arg: "++)) (many (noneOf "\n "))

parseAlias :: Parser Cmd
parseAlias = Cmd <$> cmd "alias" <* skip (char ' ')
                 <*> (cons <$> ((Abbr . T.pack) <$> some (noneOf "\n ") <* skip (char ' '))
                            <*> fmap (return . Key' . AesonKey . T.pack)  (some (noneOf "\n ")))
parseLink :: Parser Cmd
parseLink = Cmd <$> cmd "link" <* skip (char ' ')
                <*> fmap (fmap $ Abbr . T.pack) (count 2 $ parseArgNoneBut)
parseDeref :: Parser Cmd
parseDeref = Cmd <$> (fmap T.pack $ (string "deref" <* skip (char ' '))) 
                 <*> (fmap (return . Abbr . T.pack) (some (noneOf "\n ")))


parseDerefK :: Parser Cmd
parseDerefK = Cmd <$> (fmap T.pack $ (string "derefK" <* skip (char ' '))) 
                  <*> fmap return parseKey
                                         -- ^ 'toAesonKey' or 'AesonKey'
                                         -- likely the latter  ^^^

parseLoadf :: Parser Cmd
parseLoadf = Cmd <$> (fmap T.pack $ (string "loadf" <* skip (char ' ')))
                 <*> (fmap (return . Blob' . T.pack) (some (noneOf "\n ")))


parseAbbr :: Parser Cmd
parseAbbr = Cmd <$> cmd "abbr" <* skip (char ' ')
                <*> fmap return parseKey

parseLslnk :: Parser Cmd
parseLslnk = Cmd <$> cmd "lslnk"
                <*> fmap (return . Err . T.pack. ("unused arg: "++)) (many (noneOf "\n "))

parseLsvm :: Parser Cmd
parseLsvm = Cmd <$> cmd "lsvm"
                <*> fmap (return . Err . T.pack. ("unused arg: "++)) (many (noneOf "\n "))


cmd :: String -> Parser T.Text
cmd w = fmap T.pack (string w) 











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
parse :: Parser a -> String -> Result a
parse p s = parseString p mempty s

{-
    resultToMaybe :: Result a -> Maybe a
resultToMaybe (Success v) = Just v
resultToMaybe (Failure _) = Nothing
-}

repl :: IO ()
repl = runInputT (Settings { complete = completeWord Nothing 
                                                     " \t" 
                                                     wordCompleter 
                           , historyFile = Just "repl.history"
                           , autoAddHistory = True }) loop

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
                                                              Just x' -> putStrLn (show x')
                                                              Nothing -> print "Nothing"
                                 _ -> if ":!" `isPrefixOf` input
                                         then liftIO $ system (drop 2 input) >> return ()
                                         else liftIO $ putStrLn input

                               loop

cmds = [ "deref"
       , "derefK"
       , "alias"
       , "abbr"
       , "loadf"
       , "link"
       , "lslnk"
       , "lsvm"
       , "linksto"
       , "linkstoK"
       , "linksfrom"
       , "linksfromK"
       , "vmkeys"
       , "abbrkeys"
       , "select" ]

parseHelp :: Parser String
parseHelp = skip (string "help ") *> some (noneOf " \n")

wordCompleter :: String -> IO [Completion]
wordCompleter s = do
    vm <- getVMKeys
    abbr <- getAbbrevKeys
    let matches = filter (isPrefixOf s) (cmds ++ vm ++ abbr)

    return $ map simpleCompletion matches


-- fetch a list of all keys in the current VMap
getVMKeys :: IO [String]
getVMKeys = do
    mST <- oas' (Cmd "vmkeys" [])
    let st = case mST of
                Just x  -> x
                Nothing -> Nil
        ret = case st of
                List ks -> ks
                Nil -> []
        ks = rights $ map (\(Key' ak) -> fromAesonKey ak) ret
    return $ map (show) ks

-- fetch a list of all keys in the current VMap
getAbbrevKeys :: IO [String]
getAbbrevKeys = do
    mST <- oas' (Cmd "abbrkeys" [])
    let st = case mST of
                Just x  -> x
                Nothing -> Nil
        ret = case st of
                List ks -> ks
                Nil -> []
    return (map (T.unpack . (\(Abbr k) -> k))  ret)





