------------------------------------------------------------
-- |
-- Module      : Koncat.Parsers
-- License     : WTFPL Version 2
-- Maintainer  : Georg Grab, <grab@thereisnobreak.net>
-- Stability   : Unstable
-- Portability : Linux
--
-- @Koncat.Parser@ exports various parsers neccessary to efficiently handle user queries.
-- It is using the Parsec Text Parsing library internally.
--
-- Todo:
--
-- * Configuration File instead of defaultValues
--
-- * Search for cases where parseIRC might fail, rewrite
--
------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}
 
module Koncat.Parser(
    module Koncat.Parser.FuncParser
  , module Koncat.Parser.IRCParser
  , module Koncat.Parser.ModuleParser
  , module Koncat.Parser.ConfigParser
) where

import Koncat.Parser.FuncParser
import Koncat.Parser.IRCParser
import Koncat.Parser.ModuleParser
import Koncat.Parser.ConfigParser
