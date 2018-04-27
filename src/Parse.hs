{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Parse where

import qualified Data.Text as T
import Text.RawString.QQ
import Text.Trifecta
import Control.Applicative

-- '*.note' format: 
--
-- <alias> -> <filePath> | <String>  (quoted)
ex :: String
ex = [r|

[entries]

blob -> "hello world. is this the end? I should hope not. Shouldn't you? No, don't bother.
*I* certainly wouldn't."

specificity -> specificity.md

b3 -> "heol aeuao eurscaoeu aosceuh aoaoeu aoeucrhaoe ucaoeu rcsaoheu rcaoeh
urcaoheu rsacoehu rasocehu aorcehu asorcehu asroecuh aoesrc aoersch aorecuh
aoresch aorsceh aoresc aoresch aoresch aorescuh aorescuh aoresc arosec aroesc
aroesuhaoerscuh aorescu rcaoehu"

[links]

blob -> specificity
b3 -> specificity
|]

-- | takes in a 
--parseNote :: T.Text -> (T.Text, T.Text)
parseNote = T.splitOn " -> "


-- | If the format violates a (header -> body\nbody cont  \n body cont...)
-- format, 'getEntries' aborts and returns the entries already collected at 
-- the time of failure.
getEntries  = collectAll . T.lines

isStartOfEntry l = " -> " `T.isInfixOf` l
          --     rest | collected
          
collectTillNext [] = ([], [])
collectTillNext (x:xs) = if not (isStartOfEntry x) 
            then fmap (x:) (collectTillNext xs)
            else (x:xs, [])


-- | Takes a the lines of a .note file and collects them into a list of
-- entries.
collectAll :: [T.Text] -> [T.Text]
collectAll [] = []
collectAll (x:xs) = 
    let (rest, collected) = collectTillNext xs
        entry = T.concat (x:collected)
     in 
     if isStartOfEntry x 
        then entry : collectAll rest
        else []

-- An 'Entry' is an alias and a (FilePath | c), where c is as in Val alg c
data EntryVal c = File FilePath | RawBlob c deriving (Eq, Show)
data Entry c = Entry (Alias c) (EntryVal c) deriving (Eq, Show)

newtype Alias c = Alias c deriving (Eq, Show)

parseHeader :: String -> Parser String
parseHeader = string . ("[" ++) . (++ "]")

parseEntryHeader = pad $ parseHeader "entries"
parseLinkHeader = pad $ parseHeader "links"

pad :: Parser a -> Parser a
pad p = skip whiteSpace *> p <* skip whiteSpace

getAlias :: Entry c -> Alias c
getAlias (Entry a _) = a

getEntryVal (Entry _ ev) = ev

-- | An alias may be any alphanumeric sequence. Short is preferable.
parseAlias :: Parser (Alias String)
parseAlias = Alias <$> some alphaNum <* skip (string " -> ")

parseSndAlias :: Parser (Alias String)
parseSndAlias = Alias <$> some alphaNum <* skipMany (char '\n')

parseFilePath :: Parser (EntryVal String)
parseFilePath = File <$> some (noneOf " \n") <* skipMany (char '\n')

parseRawBlob :: Parser (EntryVal String)
parseRawBlob = 
    RawBlob <$> (skipQuote 
             *> some (noneOf "\"") 
             <* skipQuote 
             <* skipMany (char '\n'))
    where skipQuote = skip (string "\"") 

parseEntryVal :: Parser (EntryVal String)
parseEntryVal = try (parseRawBlob <|> parseFilePath)

parseEntry :: Parser (Entry String)
parseEntry = Entry <$> parseAlias <*> parseEntryVal

parseEntries :: Parser [Entry String]
parseEntries = (skip parseEntryHeader) *> some (pad parseEntry)

skip :: Parser a -> Parser ()
skip p = () <$ p

fromSuccess (Success a) = a

newtype AliasLink c = AliasLink (Alias c , Alias c) deriving (Eq, Show)

parseLink :: Parser (AliasLink String)
parseLink = (AliasLink .) . (,) <$> parseAlias <*> parseSndAlias
