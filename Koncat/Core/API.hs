------------------------------------------------------------
-- |
-- Module      : Koncat.Core.API
-- License     : WTFPL Version 2
-- Maintainer  : Georg Grab, <grab@thereisnobreak.net>
-- Stability   : Unstable
-- Portability : Linux
--
-- @Koncat.Core.API@ exports functions that increase the abstraction level of IRC,
-- making working with it more comfortable.
--
-- Currently, this Module is also temporarily acting as a configuration file.
-- Configuration will get its own File in the near future. 
------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
 
module Koncat.Core.API(
    module Control.Monad.State
  , module Control.Monad.IO.Class  
 
    -- * Low-level Interfacing
  , r_write
  , r_write'

    -- * Events
  , namely
  , onServerCode
  , onUserAction

    -- * API Functions
  , joinChannel
  , partChannel
  , joinAll
  , retPing
  , recvArgs
  , privmsg
  , privmsg'
  , incCounter
  , toDebugChan
  
    -- * Configuration
  , cGetString
  , cGetString'
  , cAddr
  , cDbg

    -- * Convenience
  , io

    -- * Miscellaneous
  , hasFlag
  , getFlag
  , isPrivateMsg
  , replace
  , printf'

) where

import Control.Monad.State
import Control.Monad.IO.Class
import Text.Printf
--import System.IO
import Data.List
import Data.Char(toLower)
import System.IO(Handle)
import qualified Data.ByteString.Char8 as B

import Koncat.Types.Common
import Koncat.Types.Config

-- | Write a raw String to the Server, also logging to stdout
r_write :: B.ByteString -> IRC ()
{-# INLINE r_write #-}
r_write s = do
    h <- gets iSock
    io (B.hPutStr h (s `B.append` eol))
    io (B.putStrLn $ ">> " `B.append` s)
  where
    eol = "\r\n"

-- | Same than "r_write", except for the function not being inside the IRC Monad
-- thus requiring the Handle to be passed as an argument.
r_write' :: Handle -> B.ByteString -> IO ()
{-# INLINE r_write' #-}
r_write' s bs = do
    B.hPutStr s (bs `B.append` "\r\n") 
    B.putStrLn $ ">> " `B.append` bs

-- | Event based on a match of (the beginning of) a Line coming from the Server
namely :: B.ByteString -> (B.ByteString -> Bool)
{-# INLINE namely #-}
namely s = B.isPrefixOf s

-- | Event based on a Control Code coming from the Server
onServerCode :: B.ByteString -> (B.ByteString -> Bool)
onServerCode num = \x -> num == (B.words x) !! 1

-- | Event based on Action coming from a user
onUserAction :: B.ByteString -> (B.ByteString -> Bool)
onUserAction = onServerCode

-- | Join a Channel
joinChannel :: Channel -> B.ByteString -> IRC ()
joinChannel c _ = trackJoin c 
               >> r_write ("JOIN " `B.append` c)
  where
    trackJoin :: Channel -> IRC ()
    trackJoin chan = do
        bot <- get
        cDB <- gets channelDB
        put $ bot {channelDB=(chan:cDB)}

-- | Leave a Channel
partChannel :: Channel -> IRC ()
partChannel c = trackPart c
             >> r_write ("PART " `B.append` c)
  where
    trackPart :: Channel -> IRC ()
    trackPart chan = do
        bot <- get
        cDB <- gets channelDB
        put $ bot {channelDB=(delete chan cDB)}

-- | Join multiple channels
joinAll :: B.ByteString -> IRC ()
joinAll _ = do
    dbg  <- cGetString "debug-channel"
    chan <- cGetList   "channel"
    forM_  (map B.pack (dbg:chan)) $ \x -> joinChannel x B.empty

-- | Respond to a Ping
retPing :: B.ByteString -> IRC ()          
retPing s = r_write $ "PONG :" `B.append` recvArgs s

-- | Get the actual message from a Client, or the Server
recvArgs :: B.ByteString -> B.ByteString
recvArgs = B.drop 1.B.dropWhile (/= ':').B.drop 1

-- | Send a message to the channel
privmsg :: Channel -> B.ByteString -> IRC ()
privmsg c s = r_write $ "PRIVMSG " `B.append` c `B.append` " :" `B.append` s

-- | See "r_write'"
privmsg' :: Handle -> Channel -> B.ByteString -> IO ()
privmsg' h c s = r_write' h $ "PRIVMSG " `B.append` c `B.append` " :" `B.append` s

-- | Lift IO in the IRC Monad
io :: IO a -> IRC a
io = liftIO

-- | Increment the Counter the State is Carrying
incCounter :: IRC ()
incCounter = do
    bot  <- get
    numC <- gets modCnt
    put (bot { modCnt = numC+1 })

-- | Check if a flag is set, discarding eventual values bound to it.
hasFlag :: String -> Flags -> Bool
hasFlag _ (Just []) = False
hasFlag s flags     = case flags of
    Just ((a,_):xs) -> if a == s then True
                       else hasFlag s (Just xs)
    Nothing         -> False

-- | Check if a flag is set and return the value of it if set.
getFlag :: String -> Flags -> Maybe String
getFlag _ (Just []) = Nothing
getFlag s flags     = case flags of
    Just ((a,b):xs) -> if a == s then b
                       else getFlag s (Just xs)    
    Nothing         -> Nothing                   

-- | Check if a message someone is sending is private.
isPrivateMsg :: GenericIRC -> Bool
isPrivateMsg gIRC =
    -- If someone is privmsg'ing us, the following equation will evaluate to true
    -- for further reference on that, check the parseIRC section of Koncat/Parsers.hs.
    (orig gIRC) == (name . user $ gIRC)

-- | Find/Replace something
replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace [] _ _ = []
replace s fnd repl =
    if take (length fnd) s == fnd
        then repl ++ (replace (drop (length fnd) s) fnd repl)
        else [head s] ++ (replace (tail s) fnd repl)

-- | Log something to the Debug Channel
toDebugChan :: B.ByteString -> IRC ()
toDebugChan logMsg = cGetString "debug-channel" >>= \x -> privmsg (B.pack x) logMsg

-- | printf a ByteString
printf' :: PrintfType r => B.ByteString -> r
printf' = printf . B.unpack

cSearchH :: String -> IConf -> IConf
cSearchH s = filter (\(Alloc(k,_)) -> s == k)

cGetString :: String -> IRC String
cGetString s = do
    ic <- gets iConf
    case cSearchH s ic of
        [Alloc (_,a)] -> return a
        _             -> error "No such Configuration switch"

cGetString' :: String -> IConf -> String
cGetString' s ic = case cSearchH s ic of
    [Alloc (_,a)] -> a
    _             -> error "No such Configuration switch"

cAddr :: IConf -> (String, String)
cAddr ic = case cSearchH "address" ic of
    [Alloc (_,a)] -> let (ok,d1) = span (/=':') a
                     in (ok, drop 1 d1)
    _             -> error "No such Configuration switch"

cDbg :: IRC Bool
cDbg = gets iConf >>= \x -> case map (\(Alloc(_,v)) -> map toLower v) $ cSearchH "debug" x of
    ["true"]  -> return True
    ["false"] -> return False
    _       -> error "Configuration field debug: must be either true or false"

cGetList :: String -> IRC [String]
cGetList s = do
    s0 <- cGetString s
    return $ tl s0 []
  where
    tl [] buf  = buf
    tl str buf = let (one,rst) = span (/=',') str 
                 in tl (dropWhile (==' ') $ drop 1 rst) $ one:buf
