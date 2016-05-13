------------------------------------------------------------
-- |
-- Module      : Koncat.Core
-- License     : WTFPL Version 2
-- Maintainer  : Georg Grab, <grab@thereisnobreak.net>
-- Stability   : Unstable
-- Portability : Linux
--
-- @Koncat.Core.Native@ contains definitions for builtin functions.
-- That is, they are directly executed by the Koncat runtime and are
-- not accessing foreign modules.
--
------------------------------------------------------------

{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}

module Koncat.Core.Native (
    isNative
) where


import           System.Exit
import           Control.Exception
import qualified Data.List as L
import qualified Data.Map  as Map
import qualified Data.ByteString.Char8 as B

import           Koncat.Core.API
import           Koncat.Core.Native.Permissions
import           Koncat.Types.Common
import           Koncat.Parser.IRCParser
import           Koncat.Source
import           Koncat.PrettyPrint(drawTable')

-- | Determine whether an IRC Message is referring to a native function.
-- Return the native function on success, or Nothing.
isNative :: GenericIRC -> Maybe (GenericIRC -> IRC ())
isNative g = Map.lookup (head $ msg g) mapper

mapper :: Map.Map B.ByteString (GenericIRC -> IRC ())
mapper = Map.fromList $
         [ (":echo"         , echo          )
         , (":quit"         , quit          )
         , (":reload"       , reload        )
         , (":alias"        , alias         )
         , (":expose-state" , exposeS       )
         , (":list"         , listModules   )
         , (":funcs"        , listFunctions ) 
         , (":register"     , handleRegister)
         , (":identify"     , updateUserDB_identQuery)
         , (":testget"      , testGet       )
         ]

-- Disconnect from IRC and exit the program.
-- @TODO permissions
quit :: GenericIRC -> IRC ()
quit _ = r_write "QUIT :Bye" >> io (exitWith ExitSuccess)

-- This is a debug function
echo :: GenericIRC -> IRC ()
echo g = privmsg (orig g) $ B.unwords $ drop 1 $ msg g

-- Reload the SQLite Database
-- @TODO permissions
reload :: GenericIRC -> IRC ()
reload g = io(rebuild) >> privmsg (orig g) "RELOAD OK"

-- Set an Alias
-- @TODO user specific, unalias
alias :: GenericIRC -> IRC ()
alias g = case parseAlias input of
    ("E", e) -> privmsg (orig g) $ B.pack $ replace e "\n" " "
    v        -> do 
        st <- get
        db <- gets aliasDB
        put $ st {aliasDB = (B.pack $ fst v, B.pack $ snd v):db}
        privmsg (orig g) "Alias OK"
  where
    input = B.unwords $ tail $ msg g

-- Expose the underlying State. This is a debug function.
-- @TODO permissions | remove
exposeS :: GenericIRC -> IRC ()
exposeS g = get >>= \a -> privmsg (orig g) (B.pack $ show a)

-- List all Modules
-- @TODO restrict certain Modules, permissions
listModules :: GenericIRC -> IRC ()
listModules g = 
    io(fetchModules) >>= \a -> privmsg (orig g) $ B.intercalate ", " a
    
-- List all Functions of a Module
-- @TODO restrict certain Functions, permissions
listFunctions :: GenericIRC -> IRC ()
listFunctions g = do
    metaInfo <- io (exposeMeta $ B.unpack $ (msg g) !! 1)
    case metaInfo of
        Nothing -> privmsg (orig g) "Cannot call funcs on that: Module not found"
        Just a  -> mapM_ (privmsg (orig g)) (drawTable' (L.transpose a) " | ")

testGet :: GenericIRC -> IRC ()
testGet g = do
    str <- cGetString query
    privmsg (orig g) $ B.pack str
  where
    query = B.unpack $ B.unwords $ drop 1 $ msg g
