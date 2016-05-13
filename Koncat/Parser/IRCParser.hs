{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-unused-do-bind #-}

module Koncat.Parser.IRCParser(
    parseIRC
  , parseIRCFunctionCall  
  , parseAlias
  , quotedContents
) where

import qualified Data.ByteString.Char8 as B

import           Text.Parsec 
import           Text.Parsec.ByteString

import           Koncat.Types.Common
 
{- |
parseIRC parses everything except the actual message
the Bot recieves from IRC, and stuffs it in the GenericIRC type.

>>> parseIRC ":tsv!tsv@localhost PRIVMSG #main :This is the actual message "
GenericIRC {user = UserInfo {name = "tsv", rName = "tsv", vhost = "localhost"}, orig = "#main", msg = ["This","is","the","actual","message"]}

* See Figure 2 (Source Code) for further reference on implementation

* See RFC 1459, <http://www.faqs.org/rfcs/rfc1459.html> for further reference on the IRC specification.
 -}
parseIRC :: B.ByteString -- ^ Raw Input String
         -> B.ByteString -- ^ Bot Nickname
         -> GenericIRC   -- ^ Parsed IRC Message
parseIRC s nick = GenericIRC { user = UserInfo { name  = fromTo ':' '!' user'
                                          , rName = fromTo '!' '@' user'
                                          , vhost = B.drop 1 $ B.dropWhile (/= '@') user'
                                          }
                        , orig = target
                        , msg  = args'
                        } 
  where
    splitted = B.words s
    user'    = splitted !! 0 
    target   = if (splitted !! 2) == nick
                   then fromTo ':' '!' user'
                   else splitted !! 2
    args'    = [B.drop 1 . head $ drop 3 splitted] ++ drop 4 splitted


{-
  -----------------------------------------------------------------------------
  | <+nick> :util.testytest #dothingsheh(Arg1,Arg2) -flag -flag2=SomeArgument |
  |          \1              \2          \3          \4                       | 
  |\                                                                          |
  || 1) Module Name   (IRCCommand{qModule})                                   |
  || 2) Function Name (IRCCommand{qFunc})                                     |
  || 3) Arguments     (IRCCommand{qArgs})                                     |
  || 4) Flags         (IRCCommand{qFlags})                                    |
  | NOTE: Arguments and Flags are optional (Implementation via Maybe)         |
  --------------------------------FIG 3.---------------------------------------
-}

{- |
parseIRCFunctionCall is  parsing an IRC message which is directed to the Bot and referring to a Module.

>>> parseIRCFunctionCall ":util.testytest #dothingsheh(Arg1,Arg2) -flag -flag2=SomeArgument"
Right (IRCCommand {qModule = "util.testytest", qFunc = "dothingsheh", qArgs = Just ["Arg1","Arg2"], qFlags = Just [("flag",Nothing),("flag2",Just "SomeArgument")]})

 -}
parseIRCFunctionCall :: B.ByteString -> Either ParseError IRCCommand
parseIRCFunctionCall input = parse functionCall "(ircin)" input

funcIllegal = "#(); "

functionCall = do { 
    char ':' 
  ; modName  <- many (noneOf funcIllegal)
  ; spaces
  ; fName    <- funcInfo
  ; flagsSet <- getFlags
  ; return IRCCommand { qModule = modName
                      , qFunc   = fst fName
                      , qArgs   = isEmpty $ snd fName
                      , qFlags  = fIsEmpty flagsSet
                      }
  }
  where
    isEmpty s = case s of
        [[]]  -> Nothing
        a     -> Just a

    fIsEmpty s = case s of
        [] -> Nothing
        a  -> Just a

funcInfo = 
    do { try (char '#')
       ; fName <- many (noneOf "( ")
       ; char '('
       ; fArgs <- getFuncArgs
       ; char ')'
       ; return (fName, fArgs)
       } 
    <?> "function (#func(..))"   
  
getFlags :: Parser [(String, Maybe String)]
getFlags = tFlags `sepBy` spaces

tFlags = 
    do { spaces 
       ; skipMany1 $ char '-'
       ; cnt <- many (noneOf " =")
       ; skipMany (space <|> char '=')
       ; val <- optionMaybe (quotedContents <|> (many1 $ noneOf " -"))
       ; spaces
       ; return (cnt, val)
       }
getFuncArgs = sepBy (many(noneOf ",)")) separator

separator = skipMany1 (space <|> char ',')

quotedContents = do
    char '\'' <|> char '"'
    cnt <- (many $ noneOf "'\\\"") 
    char '\'' <|> char '"'
    return cnt

{- | Extract 2 quoted Strings from one String.

>>> parseAlias "\"First String\" \"Second String\""
("First String", "Second String")
 -}

parseAlias :: B.ByteString -> (String,String)
parseAlias s = case parse aliasParse "(ircin)" s of
    Left e  -> ("E", show e)
    Right v -> v

aliasParse :: Parser (String,String)
aliasParse = do
    spaces
    tFst <- quotedContents
    spaces
    tSnd <- quotedContents
    spaces
    return (tFst, tSnd)

-- ----------
-- | Extract a part of a string
fromTo :: Char -> Char -> B.ByteString -> B.ByteString
fromTo a b c = B.drop 1 . B.takeWhile (/= b) $ B.dropWhile (/= a) c
