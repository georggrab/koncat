{-# LANGUAGE OverloadedStrings #-}
import           Koncat.Parser.IRCParser(parseIRCFunctionCall)
import qualified Data.ByteString.Char8 as B
import           Text.Parsec
import           Text.Parsec.ByteString
import           System.Environment
import           System.IO
import           Control.Monad

{-
TODO
  *parsing:
     - sanitize [DONE]
     - Syntax Def.
        - Syntax Analysis
     - Error?
     - Bytecode?
  *impl:
     - functions
     - if
     - variables
     - exec :: KoncatSource -> IO ()
-}

-- MST = Main Source Tree
data KoncatSource =
    MST KoncatSource
  | STMTFor KoncatSource 
  | STMTIf KoncatSource
  | FUNC Directive KoncatSource

-- Annotations before function def
data Directive = 
    -- Type Annotation, [A| Int -> Int]
    Type T
    -- CRET Type Annotation, [CRT| Int -> Int]
    CRetType T


main :: IO ()
main = do
    b <- getArgs
    a <- B.readFile $ head b
    print $ sanitizer $ splitStatements a
--
--interpretSource :: ByteString -> IO ()
--interpretSource bs =
--    let t  = determineStatementType line
--        ln = splitStatements bs

splitStatements :: B.ByteString -> [B.ByteString]
splitStatements bs = case parse statement "<f:?>" bs of
    Left parseError -> [] --STUB
    Right bs        -> map B.pack bs

-- Sanitize Comments, blank lines etc
sanitizer :: [B.ByteString] -> [B.ByteString]
sanitizer bs = filter f bs
  where
    f x = if isBlank x then False
      else if isComment x then
          False
      else True 
    isBlank bs = B.empty == B.filter (/=' ') bs
    isComment x = "//" == (B.take 2  (B.dropWhile (==' ') x))
       

statement = many (noneOf ";\n") `sepEndBy` (char ';' <|> char '\n')
    

