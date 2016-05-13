------------------------------------------------------------
-- |
-- Module      : Koncat.Permissions
-- License     : WTFPL Version 2
-- Maintainer  : Georg Grab, <grab@thereisnobreak.net>
-- Stability   : Unstable
-- Portability : Linux
--
-- "Koncat.PrettyPrint" is providing a few option functions that make Bot-Output more aesthetically pleasing for the
-- end user. Note that Syntax Highlighting is done in a different Module, that being "Koncat.Language.SyntaxHighlighter"
------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}
 
module Koncat.PrettyPrint (
--   drawTable
   drawTable'  
 , fExpct
 , fExpct'
 , argHL
 , (*+)
 , createRReq
) where

import qualified Data.List as L
import qualified Data.ByteString.Char8 as B
import           Data.Char(chr)

{- | Align a Multi-Dimensional Array

>>> putStrLn $ drawTable [["thisIsRow1","AnotherElemInRow1","alsoInRow1"],["thisIsRow2","moreText","EvenMoreText"]] " | "
thisIsRow1        | thisIsRow2   | 
AnotherElemInRow1 | moreText     | 
alsoInRow1        | EvenMoreText | 

However, it may be more convenient for you to input text per column:

>>> putStrLn $ drawTable (transpose [["column_one","column_two","column_three"],["alsoFirstcolumn","AndTheresTheSecondcol", "...."]]) " | "
column_one      | column_two            | column_three | 
alsoFirstcolumn | AndTheresTheSecondcol | ....         | 

where transpose is from "Data.List"
 -}
--drawTable :: [[B.ByteString]] -- ^ Input Array. You might want to call Data.List.transpose on it (see examples)
--          -> B.ByteString     -- ^ Delimiter of Columns. Usually this is something like @\" | \"@
--          -> B.ByteString     -- ^ Output String
--drawTable [] _ = B.empty
--drawTable i sp = B.concat $ B.intercalate "\n" $ B.transpose $ 
--             map (\x -> toSpaces sp x) $ putTags i

{- | See @drawTable@. The final result isn't concatentated.
 -}

(*+) :: B.ByteString -> B.ByteString -> B.ByteString
(*+) = B.append

drawTable' :: [[B.ByteString]] -> B.ByteString -> [B.ByteString]
drawTable' [] _ = [B.empty]
drawTable' i sp = map B.concat $ L.transpose $ map (\x -> toSpaces sp x) $ putTags i

-- Tag every Element of the Array with the spaces needed in order
-- to gain an equal length in relation to the longest String in the Array
putTags :: [[B.ByteString]] -> [[(Int, B.ByteString)]] 
{-# INLINE putTags #-}
putTags [] = []
putTags i = [numSpaces $ head i] ++ (putTags $ tail i)
  where
    numSpaces :: [B.ByteString] -> [(Int, B.ByteString)]   
    numSpaces m = zip (map (\x -> (maximum $ map B.length m) - B.length x) m) m 

-- Take the Delimiter and the Array created via putTags and reduce it to Rows of
-- equal length
toSpaces :: B.ByteString -> [(Int, B.ByteString)] -> [B.ByteString]
{-# INLINE toSpaces #-}
toSpaces _ []  = []
toSpaces isc i =
    (addSpaces isc $ head i) : (toSpaces isc $ tail i)
  where
    addSpaces :: B.ByteString -> (Int,B.ByteString) -> B.ByteString
    addSpaces isc' (n,s) = s `B.append` B.replicate n ' ' `B.append` isc'

{- | Build the syntax of a function call with the arguments it requires.
>>> fExpct "add" ["Int","Int"]
\#add(Int,Int)
 -}
fExpct :: String -> [B.ByteString] -> B.ByteString
fExpct a b = "#" *+ B.pack a *+ "(" *+ B.intercalate "," b *+ ")"

{- | Build the highlighted syntax of a function call with the arguments it requires.
 -}
fExpct' :: String -> [B.ByteString] -> B.ByteString
fExpct' a b = "#" *+ B.pack a *+ "(" *+ B.intercalate "," (argHL b) *+ ")"

{- | Add (IRC) highlighting to a list of ByteStrings (usually arguments).
Currently the output will be highlighted in light blue.
 -}
argHL :: [B.ByteString] -> [B.ByteString]
argHL x = map (wrapHL "12") x
    
wrapHL :: B.ByteString -> B.ByteString -> B.ByteString
wrapHL ascii bs = chr 3 `B.cons` ascii *+ (bs `B.snoc` chr 15)

createRReq :: String -> Integer -> B.ByteString
createRReq desc cnt =
    '(' `B.cons`   B.pack(show cnt)
        *+ ")> " 
        *+ wrapHL "12" "Input Request" 
        *+ ": " 
        *+ B.pack desc
