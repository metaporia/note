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
import Data.Aeson

import Parse

parseCmd' :: Parser String
parseCmd'  = pad' (some letter) 

pad' p = skipMany space *> p <* skipMany space

parseArgAlpha :: Parser String
parseArgAlpha = pad' (some letter)

parseArgAlphaNum :: Parser String
parseArgAlphaNum = skipMany (char ' ') *> (some alphaNum)

data Cmd = Cmd T.Text [T.Text] deriving (Eq, Generic, Show)

parseCmd :: Parser Cmd
parseCmd = 
    Cmd <$> fmap T.pack parseCmd' 
        <*> (fmap .fmap) T.pack (many parseArgAlphaNum)
        <* skip (char '\n')

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

main :: IO ()
main = putStrLn "cli"

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


instance ToJSON Cmd where

    toJSON (Cmd cmd args) = object [ "type" .= String "command" 
                                   , "name" .= String cmd
                                   , "args" .= toJSON args
                                   ]

    toEncoding = genericToEncoding defaultOptions

instance FromJSON Cmd

cmds = M.empty :: M.Map String Command

cmd = Cmd "load" ["key0", "key1"]
