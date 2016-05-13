{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind#-}

module Koncat.Parser.ConfigParser(
    parseConfiguration
) where

import qualified Data.ByteString.Char8 as B
import           Text.Parsec 
import           Text.Parsec.ByteString
import           Koncat.Types.Config
import           Koncat.Parser.IRCParser(quotedContents)

parseConfiguration :: FilePath -> IO (Either ParseError [Entry])
parseConfiguration fp = do
    file <- B.readFile fp
    return $
        parse allDirectives ("file=" ++ fp) file

allDirectives :: Parser [Entry]
allDirectives = do
    skipMany comment
    directive `sepEndBy1` (spaces >> skipMany comment >> spaces)
 
directive :: Parser Entry
directive = do {
        spaces;
        name <- many1 (alphaNum <|> char '-');
        spaces;
        do {
            char '{';
            spaces;
            skipMany comment;
            entries <- directive `sepEndBy1` (skipMany comment >> delimiter);-- (skipMany space >> (newline <|> char ';'));
            spaces;
            char '}';
            return $ Directive (name,entries);
        } <|> do {
            char '=';
            spaces;
            val <- quotedContents <|> many (alphaNum <|> char ':' <|> char '-');
            spaces;
            return $ Alloc (name, val);
        }
    }
 
comment :: Parser ()
comment = string "#" >> manyTill anyChar newline >> spaces >> return ()

 {- skipMany space
    name <- many1 alphaNum
    skipMany space
    char '{'
    values <- (sepEndBy1 entries delimiter)
    char '}'
    return (name,values)
  where
    entries :: Parser (String,String)
    entries = do
        skipMany space
        nm <- many alphaNum
        skipMany space
        char '='
        skipMany space
        val <- quotedContents <|> many (alphaNum <|> char ':')
        skipMany space
        return (nm,val)
-}
delimiter :: Parser String
delimiter = many $ oneOf "\v\f\t\r\n;"

--parseConfiguration :: FilePath -> IO (Either ParseError Configuration)
--parseConfiguration fp = readFile fp >>=
--    parse parseConfiguration ("<file=" ++ fp ++ ">") 

--getDirectives :: Parser (String, String)
--getDirectives = do
--    startWord <- many (noneOf " ") 
--    char ' '
--    char '{'
--    cnt <- many (noneOf "}")
--    char '}'
--    return (startWord,cnt)

{-parseConfiguration = do
   return  ServerSetting{
       address = address'
     , useSSL  = useSSL'
     , initCh  = chans'
     , useCPrivmsg = useCPrivmsg'
     , commandPrefix = commandPrefix'
     , debug = debug'
     , debugChannel = debugChannel'
     , botMeta = BotSettings
           { bNickname = bNickname'
           , bUsername = bUsername' 
           , bRealname = bRealname'
           , bIdent    = bIdent'
           }
     , security = SecuritySettings 
           { superadmin = superadmin'
           
           }
   }
-}
