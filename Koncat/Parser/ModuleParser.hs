{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-unused-do-bind #-}

module Koncat.Parser.ModuleParser(
    parseHandshake
  , defaultValues  
  , parseAPICall
  , getTime
) where
 
import           Data.Maybe 
import qualified Data.ByteString.Char8 as B

import           Text.Parsec 
import           Text.Parsec.ByteString

import           Koncat.Types.Module
import           Koncat.Parser.FuncParser
import           Koncat.Parser.IRCParser(quotedContents)


{- | parseHandshake gets the raw text as recieved when asking a module
     for a handshake, parses it and stuffs it in the Handshake Data Type.
 -}
parseHandshake :: B.ByteString -> Handshake
parseHandshake s = Handshake 
                   { desc     = putDesc       t 
                   , funcs    = runFuncParser (putFunc t)
                   , author   = putAuthor     t
                   }
 where
   t = tail $ splitH s 
                   
 
{- Figure 1 displays how such a message should look like.

  |----------------------------------------------------------------------------------|
  | $F={helloworld:[A Hello World Function](*String)%E=5s%P=Admin;other_func:...     |
  |  \1 \2          \3                      \4      \5   \6       \7                 |
  |\                                                                                 |
  || 1) A capital "F" (ASCII Value 0x46) initiates the Function segment              |
  || 2) The Function Name. This value will later be put in the modules STDIN         |
  ||    in order to call the function.                                               |
  || 3) The description of the Function. This value will later be used in            |
  ||    :help and :list core functions. Note that this value is non-optional!        |
  || 4) Arguments. Semantics for this will be elaborated in "Module.hs" later on.    |
  || 5) Optional Execution time of the function.                                     |
  ||    Valid units are {s,sec,min,h,d}.                                             |
  ||    If not set, the maximum execution time is set to 60 seconds.                 |
  ||    If unsure about the execution time of the function, for example because of   |
  ||    doing an Network activity, you can use the -daemonize switch in IRC to       |
  ||    disable Execution time checking.                                             |
  || 6) Permissions. If not set this will be ANY, making the function available to   |
  ||    everyone. Possible units are {ANY,Admin,Registered,Privileged}               |
  || 7) Functions are delimeted with a semicolon (ASCII Value 0x3B)                  |
  |---------------------------------FIG 1.-------------------------------------------|
 -}


{- | Hardcoded lookup-table that describes default values for variables.
     They will be inserted in the SQL Database if the user didn't set it!
 -}
defaultValues :: [(Char, String)]
defaultValues = [ ('E', "6000000")
                , ('P', "ANY") ]

{-
  |----------------------------------------------------------------------------|
  | :tsv!tsv@localhost PRIVMSG #main :This is the actual message               |
  | \1   \2  \3                \4     \5                                       |
  |\                                                                           |
  || 1) Nickname (GenericIRC {UserInfo{name}})                                 |
  || 2) Realname (GenericIRC {UserInfo{rName}})                                |
  || 3) vhost    (GenericIRC {UserInfo{vhost}})                                |
  || 4) Channel  (GenericIRC {orig})                                           |
  || 5) Message  (GenericIRC {msg})                                            |
  |-----------------------------------FIG 2.-----------------------------------|
-}


parseAPICall :: B.ByteString -> Either ParseError ModAPICall
parseAPICall = parse apiParse "(module-stdin)" 
  where
    apiParse :: Parser ModAPICall
    apiParse = do
        char '\\'                   <?> "initial backslash"
        name <- (many $ noneOf "{") <?> "name of API function"
        char '{'                    <?> "beginning of arguments ({)"
        cont <- sepEndBy (quotedContents <|> many(noneOf ",}")) (char ',') <?> "parse of arguments"
        char '}'                    <?> "end of arguments (})"
        return $ ModAPICall name (if cont == [[]] then Nothing else Just cont)

-- ----------

splitH :: B.ByteString -> [B.ByteString]
splitH = B.split '$'

-- ----------

-- Identify a variable, ex $E=Some Value -> Just "Some Value"
identPart :: Char -> [B.ByteString] -> Maybe B.ByteString
identPart c x = case filter (\y -> B.head y == c) x of
    [a] -> Just (B.drop 2 a)
    _   -> Nothing

-- Get the description.
-- The description is required, so throw an error if there is no description.
putDesc :: [B.ByteString] -> B.ByteString
putDesc a = fromJust $ identPart 'D' a

{- | Get Microseconds from @int{s|sec|min|h|d}@, as required for setting a timeout using System.Timout

>>> getTime "10s"
10000000

>>> getTime "2min"
120000000

 -}
getTime :: B.ByteString -> Int
getTime t = million $ case unit of
    Nothing -> 60 
    Just a  -> case snd a of
        "s"   -> fst a
        "sec" -> fst a
        "min" -> fst a * 60
        "h"   -> fst a * 3600
        "d"   -> fst a * 86400
        _     -> 60          
 where
   unit = B.readInt t
   million num = num * 1000000
               
putAuthor :: [B.ByteString] -> B.ByteString
putAuthor a = case identPart 'A' a of
    Nothing -> "Unknown"
    Just b  -> b

putFunc :: [B.ByteString] -> B.ByteString
putFunc a = fromJust $ identPart 'F' a


