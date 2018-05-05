{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

-- CLI: use structuredCLI or just "trifecta"? I'm leaning towards the latter.

import qualified Data.Text as T
import Text.RawString.QQ
import Text.Trifecta
import qualified Data.ByteString as B
import Control.Applicative
import qualified Data.Map as M

import GHC.Generics
import Data.Aeson hiding (Result(..))

import Parse
import UI.Service hiding (Result)

import Control.Monad
{-
parseCmd' :: Parser String
parseCmd'  = pad' (some letter) 

pad' p = skipMany space *> p <* skipMany space

parseArgAlpha :: Parser String
parseArgAlpha = pad' (some letter)

parseArgAlphaNum :: Parser String
parseArgAlphaNum = skipMany (char ' ') *> (some alphaNum)

parseArgNoneBut :: Parser String
parseArgNoneBut = skipMany (char ' ') *> (some (noneOf "\n "))


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
main = server'
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

replEcho :: IO ()
replEcho  = do
    let loop = do putStr "> "
                  l <- getLine
                  case l of
                    "exit" -> return () 
                    _ -> case parse parseCmd l of 
                           Success cmd -> do print cmd
                                             oas cmd
                                             loop
                           _ -> putStrLn l *> loop 
    loop
    
repl = replEcho

parse :: Parser a -> String -> Result a
parse p s = parseString p mempty s


resultToMaybe :: Result a -> Maybe a
resultToMaybe (Success v) = Just v
resultToMaybe (Failure _) = Nothing
-}
