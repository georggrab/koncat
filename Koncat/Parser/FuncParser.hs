{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-unused-do-bind #-}

module Koncat.Parser.FuncParser (
    runFuncParser
) where
 
import qualified Data.ByteString.Char8 as B

import           Text.Parsec 
import           Text.Parsec.ByteString


{- |
runFuncParser will parse the function part of the Handshake.
See @Figure 1@ (-> Source Code) for further elaboration.

>>> runFuncParser "{helloworld:[A Hello World Function](*String)%E=5s%P=Admin}"
[("helloworld","A Hello World Function","*String",Just [('E',"5s"),('P',"Admin")])]

 -}
runFuncParser :: B.ByteString -> [(String, String, String, Maybe [(Char, String)])]
runFuncParser s = case parse parseFunc "(mod-stdin)" s of
    Left e  -> [("E","E", show e, Nothing)] 
    Right a -> a

--parseFunc :: Parsec B.ByteString () [(String, String, String, Maybe [(Char, String)])]
parseFunc = do
    char '{'
    x <- (do{try(oneFunc)}) `sepEndBy1` char ';'
    char '}'
    return x

--oneFunc :: Parser (String, String, String, Maybe [(Char, String)])    
oneFunc = do
    name <- getName
    desc <- getDesc
    args <- getArgs
    let hasArgs = if args == ""
            then "Void"
            else args
    modifierVars <- getVariables
    return (name, desc, hasArgs, modifierVars)

getName :: Parsec B.ByteString () String
getName = do {
      content <- many $ noneOf ": "
    ; char ':'
    ; return content
    }
  <?> "Function Name"

getDesc :: Parser String
getDesc = do {
      char '['
    ; content <- many1 $ noneOf "]"
    ; char ']'
    ; return content
    }
  <?> "Description"

getArgs :: Parser String
getArgs = do {
      char '(' 
    ; content <- many $ noneOf ")"
    ; char ')'
    ; return content
    }
  <?> "Arguments"

getVariables :: Parser (Maybe [(Char, String)])
getVariables = optionMaybe (char '%' >> sepBy oneVariable (char '%'))

oneVariable :: Parser ((Char,String))
oneVariable = do
    c <- anyChar
    char '='
    cnt <- many $ (noneOf "%;")
    return (c,cnt)
