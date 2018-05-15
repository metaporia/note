{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Parse where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.RawString.QQ
import qualified Data.Text.Lazy as TL
import Text.Trifecta hiding (rendered, render, Rendering)
import Control.Applicative
import Data.Monoid ((<>))
import Data.Bifunctor

import Control.Monad.Except

import Lib
import Select (Selection(..))

import Helpers
import Crypto.Hash
import Val
import Select
import UI.Types (NoteS(..))


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

-- to/from  note markup format.
--
-- desideratum: max 'MetaData' (term) constructor name length of, say, 17?
--              (for consistent left-hand side padding)
--
-- | In the case of the lines marked 'p`' (at offsets +7, +9, respectively):
--  
--  * some lookahead ahead is required,
--  * multiple passes are required, or
--  * something else that parses multi-line partial spans
--
--
--  The following must be renderable:
--      
--  * one partial span (per line)
--  * (?: future) multiple partial spans (one identifier, e.g., p^)
--  * multiple partial spans, identifiers
--  * multi-line spans
--  * multi-line spans, sandwiched by optional partial spans
--
rendered :: T.Text
rendered = [r|
  p^| From a humble $13.50, to a gargantuan $750, the price of a life saving drug
    |        ^^^^^^              ^^^^^^^^^^
  p~| skyrocketed overnight [1].  Daraprim, as a drug that treats toxoplasmosis,
    |                       ~~~             '''''''''
  p`| sparked enormous controversy over the price hike.  People, outraged, argued
    |                                                                      ``````
  p`| there is ethical obligation to lower drug prices.  Companies in return
    | `````
  p<| disavowed any moral obligation to lower prices, and claim R&D efforts will
    |                                                     <<<<<
    | result in more life saving medications [2]
    |
    |
   &|     And here is another line of unremittingly brilliant lexical ejaculate--and
   &| get this, it even spans multiple lines!
    |
   #|     And here is another line of unremittingly brilliant lexical ejaculate--and
   #| get this, it even spans multiple lines!--
  p#| and even some more.
    | #############
|]

-- target 0: one partial span per line
r0 = [ ( "sparked enormous controversy over the price hike.  People, outraged, argued", "p^")
     , ( "                                                                     ``````", "")
     , ( "disavowed any moral obligation to lower prices, and claim R&D efforts will", "p<")
     , ( "                                                    <<<<<", "")
     ]

-- this intermediate format which can be directly rendered, serialized (to
-- JSON?) or loaded into program state, viz. 'NoteS'.
r1 = [ ( "sparked enormous controversy over the price hike.  People, outraged, argued", [Partial 69 75 '`'])
     , ( "disavowed any moral obligation to lower prices, and claim R&D efforts will",  [Partial 52 57 '<'])]

sr = fst $ r0 !! 0
sr' = fst  $ r0 !! 2

r3 = loadK sr  >>= (\k -> selectK k (Sel 69 75)) 
     -- >> loadK sr' >>= (\k -> selectK k (Sel 52 57)) 
     >>= lookupK
     >>= liftEither . fromSpan
     >>= (\(k, (Sel s e)) -> return $  (k, [Partial s e '^']))


data LineTag = 
     Partial Int Int Char -- ^ @Partial sIdx eIxd c@ describes a single partial span (1 ln).
   | Whole Char -- ^ a whole line selection, which may have fellows ('LineTag's w the same 'Char')
   | RenderedLine
   deriving (Eq, Show)

rend :: T.Text
rend = [r|
hello world | PartialSel '^'
      ^^^^^
|]


leftPadding = 17

-- there
conv0 :: [(T.Text, T.Text)]
conv0 = [ ("hello world", "p^" )
        , ("      ^^^^^", "") ]

conv1 :: [(T.Text, [LineTag])]
conv1 = [ ("hello world", [Partial 6 11 '^'])]

conv2 = loadK "hello world" >>= \k -> selectK k (Sel 6 11)
-- and back again
undoConv2 = conv2 
        >>= lookupK 
        >>= liftEither . fromSpan 
        >>= (\(k, (Sel s e)) -> return $  (k, [Partial s e '^']))
        >>= (\(k, ps) -> do val <- derefK k
                            return (val, ps))

newtype Rendering = Rendering { getRend :: [(T.Text, [LineTag])] } deriving (Eq, Show)

-- | Takes a 'Key' to a 'Blob' and converts the assoc'd 'Blob' to a Rendering.
intoRendering :: Key SHA1 -> NoteS String Rendering
intoRendering = undefined

-- | Apply 'Selection' to 'Rendering'.
applySelection :: Selection -> Rendering -> Rendering
applySelection = undefined


