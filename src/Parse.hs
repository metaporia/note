{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Parse where

import Prelude hiding (lookup)

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.RawString.QQ
import qualified Data.Text.Lazy as TL
import Text.Trifecta hiding (rendered, render, Rendering, Span)
import Control.Applicative
import Data.Monoid ((<>))
import Data.Bifunctor

import Control.Monad.Except
import Control.Monad.State.Class
import Lib

import Data.Maybe (maybe)

import Text.Show.Pretty

import Helpers
import Crypto.Hash
import Val
import Select
import UI.Types (NoteS(..))
import Select (Selection(..))


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
r1 = [ ( "sparked enormous controversy over the price hike.  People, outraged, \
         \argued", [Partial 69 75 '`'])

     , ( "disavowed any moral obligation to lower prices, and claim R&D efforts\
         \ will",  [Partial 52 57 '<'])]

sr = fst $ r0 !! 0
sr' = fst  $ r0 !! 2

r3 = loadK sr  >>= selectK (Sel 69 75)
     -- >> loadK sr' >>= (\k -> selectK k (Sel 52 57)) 
     >>= lookupK
     >>= liftEither . fromSpan
     >>= (\(k, (Sel s e)) -> return $  (k, [Partial s e '^']))


data LineTag = 
     Partial Int Int Char -- ^ @Partial sIdx eIxd c@ describes a single partial span (1 ln).
   | Whole Char -- ^ a whole line selection, which may have fellows ('LineTag's w the same 'Char')
   | None
--   | RenderedLine -- ^ marks line as visual metadata
   deriving (Eq, Show)

rend :: T.Text
rend = [r|
hello world | PartialSel '^'
      ^^^^^
|]


leftPadding = 5

-- there
conv0 :: [(T.Text, T.Text)]
conv0 = [ ("hello world", "p^" )
        , ("      ^^^^^", "") ]

conv1 :: [(T.Text, [LineTag])]
conv1 = [ ("hello world", [Partial 6 11 '^'])]

conv2 = loadK "hello world" >>= selectK (Sel 6 11)
-- and back again
undoConv2 = conv2 
        >>= lookupK 
        >>= liftEither . fromSpan 
        >>= (\(k, (Sel s e)) -> return $  (k, [Partial s e '^']))
        >>= (\(k, ps) -> do val <- derefK k
                            return (val, ps))

newtype Rendering 
  = Rendering { getRend :: [(T.Text, [LineTag])] } deriving (Eq, Show)

newtype BareRendering
  = BareRendering { getBareRend :: [T.Text] } deriving (Eq, Show)

-- | Add empty list for 'LineTag's to each line of 'Text'.
fromBareRendering :: BareRendering -> Rendering
fromBareRendering = Rendering 
                  . flip zip (repeat []) 
                  . getBareRend

-- | Convert 'Key' into 'Rendering'.
intoRendering :: Key SHA1 -> NoteS String Rendering
intoRendering = fmap fromBareRendering . intoBareRendering 

-- | Takes a 'Key' to a 'Blob' and converts the assoc'd 'Blob' to a 
-- BareRendering.
intoBareRendering :: Key SHA1 -> NoteS String BareRendering
intoBareRendering = fmap bareRenderBlob . derefK

bareRenderBlob :: T.Text -> BareRendering
bareRenderBlob = BareRendering . T.lines

blobRender :: T.Text -> Rendering
blobRender = fromBareRendering . bareRenderBlob


--- WTF!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! TODO: fix
-- | NB: still only attempting to render single linetags
tagLines :: Rendering -> Selection -> IO Rendering
tagLines (Rendering r) = fmap Rendering . go r
    where go :: [(T.Text, [LineTag])] -> Selection -> IO [(T.Text, [LineTag])]
          go [] _ = return []
          go ((t, tags):rest) s@(Sel q e)  = 
              do let s' = (pruneSel s (len t))
                 pPrint (s, len t)
                 -- TODO remove use of min, which makes this all so brittle.
                 let classified = flip classifySel (len t) . Sel q . min e $ len t
                 putStrLn $ ppShow (classified, s, len t)
                 cur <- case classified of
                               Mid -> return (t, Partial q e '^':tags)
                               Left' -> return (t, Partial q e '#':tags)
                               WholeExact -> print "here" >> return (t, Whole '#':tags)
                               _   -> return (t, tags)
                 ([cur] <>) <$> go rest s'



          -- this case is unused, as it is only ever called on bareRenderings
          --go ((t, (ln:lns)):rest) s = do let s' = (pruneSel s (len t))
          --                               r <- go rest  s'
          --                               return $ (t, [ln]) : r


blob = blobRender s
mlSel = Sel 76 151
huh = tagLines blob mlSel >>= pPrint

-- Determines, given 'Selection' @s@ and an a 'Blob''s content @c@'s length @l@, what kind of selection 
-- @sel c s@ would generate.
--
-- NB: classifies a 'Selection' of a blob whose length is /greater/ than
-- @'eIdx' sel@.
classifySel :: Selection 
            -> Int -- ^ Blob length
            -> SelType
classifySel (Sel s e) l
  | l == 0 = EmptyBlob
  | lt (0, s, l) && lt (s, e, l) = Mid
  | s == 0 && lt (s, e, l) = Left'
  | s > l && s == e = EmptyPlus
  | s == e && e == 0 = EmptyLeft
  | s == e && s < l = EmptyMid
  | s == e && e == l = EmptyRight
  | s == 0 && e == l = WholeExact
  | s == 0 && e > l = WholePlus
  | lt (0, s, l) && e == l = RightExact
  | lt (0, s, l) && e > l = RightPlus
  | lt (s, e, 0) = Before
  | lt (l, s, e) = After
  | otherwise = Unclassified

-- TODO: replace 'classifySel' w 'classifySelOfLine' in 'tagLines'
-- classify a sel on a blob wrt to a given line
classifyBlobSelWRTLine :: Selection -- ^ sel wrt to bg blob
                       -> Int -- ^ length of line
                       -> SelType -- ^ type of sel /for the line/
classifyBlobSelWRTLine = undefined

lt :: Ord c => (c, c, c) -> Bool
lt (x, y, z) = x < y && y < z

gt :: Ord c => (c, c, c) -> Bool
gt (x, y, z) = x > y && y > z


data SelType = Left'
             | EmptyBlob
             | RightPlus
             | RightExact
             | WholePlus
             | WholeExact
             | EmptyLeft
             | EmptyMid
             | EmptyRight
             | EmptyPlus
             | Mid
             | Before
             | After
             | Unclassified
             deriving (Eq, Show)
-- | Meant for use when generating id specific 'Selection's, 'pruneSel'
-- decrements the sIdx and eIdx vals of thhe given 'Selection' by the given
-- amount. If the subtraction results in an n<0 then n = 0.
pruneSel :: Selection 
         -> Int -- ^ length of last chunk
         -> Selection
pruneSel sel@(Sel s e) len = 
    let s' = nat (s - len)
        e' = nat (e - len)
        nat n = if n < 0 then 0 else n
     in Sel s' e'


s :: T.Text
s = [r|From a humble $13.50, to a gargantuan $750, the price of a life saving drug
skyrocketed overnight [1].  Daraprim, as a drug that treats toxoplasmosis,
sparked enormous controversy over the price hike.  People, outraged, argued
there is ethical obligation to lower drug prices.  Companies in return disavowed 
any moral obligation to lower prices, and claim R&D efforts will result in more 
life saving medications [2].
|]

s1 = Sel 7 13  

xx = loadK s                  >>= aliasK "bg"
 >>= select (Sel 7 13)        >>= aliasK "humble"
 >> select (Sel 0 4) "bg"     >>= aliasK "new"
 >> select (Sel 27 37) "bg"   >>= aliasK "gargantuan"
 >> select (Sel 75 170) "bg"  >>= aliasK "price"
 >> loadK "second commentary" >>= aliasK "c2"
 >> loadK "my commentary"     >>= aliasK "c1"

 >> link "c1" "humble"
 >> link "c2" "gargantuan"
--  >> link "c1" "price"
 >> link "humble" "new"
 >> lengthen' "bg"

xy = loadf "mock/specificity.md" >>= aliasK "spec"
  >> loadK "eureka!"             >>= aliasK "resp"
  >> select (Sel 50 1000) "spec" >>= aliasK "selection"
  >> link "resp" "selection"
  >> select (Sel 105 130) "spec"   >>= aliasK "selection2"
  >> link "resp" "selection2"
  >> lengthen' "spec"

ret' n = do e <- go n
            let (r, n) = fromRight e
            pPrint r
            --T.putStrLn $ render r

ret n=do e <- go n
         let (k, n') = fromRight e
         e' <- go $ do r <- n >>= intoRendering
                       sels <- getSelections k
                       liftIO $ pPrint sels
                       r <-  renderSpansOf' sels r
                       liftIO $ pPrint r
                       return r
         let (r, n'') = fromRight e'
         putStrLn ""
         T.putStrLn $ render r

sels' = do e <- go $ xx >>= getSelections
           let (ss, n) = fromRight e
           e <- go $ xx >>= \k -> fg k ss
           pPrint . fst $ fromRight e
           return ()

            

rend2 = fromBareRendering $ bareRenderBlob s


maybeE :: err ->  Maybe a -> Either err a
maybeE  _   (Just a) = Right a
maybeE  err _        = Left err

liftE = liftEither

renderFirstSpanOf :: Key SHA1 -> NoteS String Rendering
renderFirstSpanOf k = do
    ks <- getSpansOfK k 
    -- fetch first span whose sourcekey is k
    k' <- liftE $ case null ks of
                        True -> Left "no comments on k"
                        False -> Right $ ks !! 0
    val <- lookupK k' -- is such a partial patten safe?
    (_, s) <- liftE . fromSpan $ val                
    r <- intoRendering k  
    tagLines' r s

-- | Lookup all the selections that have the given 'Key' as a sourcekey.
getSelections :: Key SHA1 -> NoteS String [Selection]
getSelections = getSpansOfK 
       >=> sequence 
         . map (lookupK >=> liftE 
                          . fmap snd 
                          . fromSpan)

-- | Rendere all spans which address the given 'Key'.
renderSpansOf :: Key SHA1 -> NoteS String Rendering
renderSpansOf k = do n <- get
                     sels <- getSelections k
                     foldr (\s n -> n >>= flip tagLines' s)
                           (intoRendering k)
                           sels
   
-- | Rendere all spans which address the given 'Key'.
renderSpansOf' :: [Selection] -> Rendering -> NoteS String Rendering
renderSpansOf' [] r = return r
renderSpansOf' (x:xs) r = tagLines' r  x >>= renderSpansOf' xs 
   
fg k sels = foldr (\s n -> n >>= flip tagLines' s) (intoRendering k) sels

tagLines' :: Rendering -> Selection -> NoteS String Rendering
tagLines' = (liftIO .) . tagLines

-- | this just extracts the original derefed content. add higlight line
-- rendering.
render :: Rendering -> T.Text
render  = T.unlines . map (uncurry renderLineTags) . getRend

-- | Text args should be one line each.
padLeft :: Int -- ^ how much padding?
        -> T.Text -- ^ text to the left of '|'
        -> T.Text -- ^ tetx to the right of '|'
        -> T.Text
padLeft n l r = makeOfLengthN n l <> "| " <> r
    where makeOfLengthN n t 
            | lt >= n = T.take n t
            | lt < n  = (T.replicate (n - lt) " ") <> l
            where lt = len t



-- | Only renders /first/ 'LineTag'.
renderLineTags :: T.Text -> [LineTag] -> T.Text
renderLineTags t [] = padLeft leftPadding "" t
renderLineTags t (x:xs) = 
    let lp = leftPadding
        checkHiLine h = if not $ T.null h 
                          then "\n" <>  padLeft lp "" h
                          else ""
        cat = (\(ln, hi, signs) -> padLeft lp signs ln <> checkHiLine hi) 
        base = renderLineTag' t x
     in cat $ foldl renderLineTag base xs

text' :: T.Text
text' =  "From a humble $13.50, to a gargantuan $750, the price of a life saving drug"
linetags =  [Partial 27 37 '^' , Partial 7 13 '#' ]

-- 'ware: Helpers below

-- | Performs all but the first 'LineTag' application.
renderLineTag :: (T.Text, T.Text, T.Text) -- ^ in order: (content, higlights, gutter signs)
              -> LineTag -> (T.Text, T.Text, T.Text) 
renderLineTag (c, hi, signs) (Partial s e chr) =
    let l = len c
        (x, y, z) = sel hi (Sel s e)
        hi' = x
           <> T.map (const chr) y
           <> z
        in (c, hi', signs <> T.pack [chr])
renderLineTag (c, hi, signs) (Whole chr) = (c, hi, signs <> T.pack [chr])
        


-- | Preforms first 'LineTag' application.
renderLineTag' :: T.Text-> LineTag -> (T.Text, T.Text, T.Text)
renderLineTag' content (Partial s e c) = 
    let lt = T.length content
        (x, y, z) = sel (T.replicate lt " ") (Sel s e)
        highlight' = x 
                  <> T.map (const  c) y 
                  <> z 
     in (content, highlight', T.pack [c])
renderLineTag' t (Whole chr) = (t, T.empty, T.pack [chr])
renderLineTag' t _ = (t, T.empty, T.empty)
