------------------------------------------------------------
-- |
-- Module      : Koncat.Core
-- License     : WTFPL Version 2
-- Maintainer  : Georg Grab, <grab@thereisnobreak.net>
-- Stability   : Unstable
-- Portability : Linux
--
-- @Koncat.Core@ exports the bootstrap function, which is used to start the Bot.
-- Internally, @Koncat.Core@ handles connecting to servers and dispatching server queries.
--
------------------------------------------------------------

{-# LANGUAGE OverloadedStrings, CPP #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Koncat.Core (
    -- * Starting the Bot
    bootstrap
) where
  
import           Data.Maybe
import           System.IO
import           System.Time
import           System.Exit
import           Control.Exception     hiding (evaluate)
import           Control.Concurrent
import           Control.Monad.State
import qualified Data.Map              as Map
import qualified Data.ByteString.Char8 as B

import           Control.Concurrent.STM
import           Text.Printf
import           Network.Socket hiding (recv)

import           Koncat.Core.API
import           Koncat.Core.Native
import           Koncat.Core.Callback
import           Koncat.Core.Config
import           Koncat.Source(initModuleChangeHandler, expose, funcExists)
import           Koncat.Types.Common
import           Koncat.Parser.IRCParser
import           Koncat.Module
import           Koncat.Permissions
import           Koncat.Language.Arguments

import Debug.Trace

 
-- | Set up Exception handling, start off the conversation with the server and start providing IRC Bot functionalities.
bootstrap :: IO ()
bootstrap = do 
    forkIO repl
    initModuleChangeHandler
    bracket iConnect disconnect dServe
  where
    disconnect s = hClose $ iSock s
    dServe s   = catch (evalStateT handshake s) (\(SomeException e) -> putStrLn $ show e)

repl :: IO ()
repl = forever $ do
    getLine >>= \x -> case x of
        "ohai" -> putStrLn "bhai"
        _      -> return ()

iConnect :: IO Bot
iConnect = do
    -- Only Parse one Server Configuration for testing
    conf     <- botsFromConfiguration "koncat.conf"
    case conf of
        Left e  -> do 
            hPutStrLn stderr (show e)
            exitWith $ ExitFailure 255
            undefined
        Right c ->
            let testConfig   = head c
                (host, port) = cAddr testConfig
            in notify testConfig $ do
            s        <- socket AF_INET Stream defaultProtocol
            addrInf  <- getAddrInfo Nothing (Just host) (Just port)
            connect s $ addrAddress $ addrInf !! 3

            hdl <- socketToHandle s ReadWriteMode

            -- Get the current UNIX Time for uptime tracking
            t <- getClockTime

            cDB <- atomically $ newTVar (Map.empty)

            -- Put Everything in the Monad and return the Bots initial state
            return $ Bot hdl testConfig t 0 [] Map.empty [] cDB
  where
    notify c a = bracket_
        (printf   "Connecting to %s ... " (cGetString' "address" c) >> hFlush stdout)
        (putStrLn "ok")
        a

-- Start off the Conversation with the server by handshaking it
handshake :: IRC ()
handshake = do
    nick <- liftM B.pack $ cGetString "nick"
    --let nick' = B.pack nick
    r_write $ "NICK " `B.append` nick
    r_write $ "USER " `B.append` nick `B.append` " Bot Bot aCoolBot" 
    gets iSock >>= serve

-- This is the main function, dispatching user queries, handling Ping, etc.
serve :: Handle -> IRC ()
serve s = forever $ do
    h <- io(B.hGetLine s)
    let 
        -- Strip the <CR> from the response
        proper = B.init h

        -- Check if the server is sending Data
        -- our Bot should respond to
        doLookup = mkIdent_Lookup proper mkIdent

        -- The server sent something the bot will not respond to
        noMatch  = isNothing doLookup

    -- Print the Servers response for logging purposes    
    io (B.putStrLn $ "<< " `B.append` proper)

    -- Check if we should respond to the Message from the server
    if noMatch then return ()   
               else fromJust doLookup h

-- Based on a line of Server Response, return a function
-- that matches in mkIdent
mkIdent_Lookup :: B.ByteString
               -> [(B.ByteString -> Bool, B.ByteString -> IRC ())] 
               -> Maybe (B.ByteString -> IRC ())
mkIdent_Lookup _ [] = Nothing
mkIdent_Lookup s ((x,y):r)
    | x s           = Just y
    | otherwise     = mkIdent_Lookup s r

-- Mkident is creating events based on Messages coming from the server.
mkIdent :: [(B.ByteString -> Bool, B.ByteString -> IRC ())]    
mkIdent = [ ( namely       "PING :"    , retPing               )
          , ( onServerCode "422"       , joinAll               )
          , ( onUserAction "PRIVMSG"   , directedTowardsMe     ) 
          ]

-- Check if message is directed towards the bot
directedTowardsMe :: B.ByteString -> IRC ()
directedTowardsMe x = do
    nick <- liftM B.pack $ cGetString "nick"
    let irc = parseIRC x nick
    gets callBackDB >>= \db -> io(tryExec db irc)
    if (B.head $ head $ msg irc) == ':' 
      -- Messages starting with ':' are seen as a message
      -- towards the Bot and thus evaluated
      then evaluate irc
      else do
          -- The Message did not start with ':' so check if
          -- the message referred to an alias a user made
          db <- gets aliasDB
          case lookup (head $ msg irc) db of
              Just a  -> modCall irc{msg=(B.words a) ++ (tail $ msg irc)}

              -- otherwise, do nothing.
              Nothing -> return () 

-- Check if the user issued a command towards me and respond accordingly
evaluate :: GenericIRC -> IRC ()
evaluate s = case isNative s of

                 -- First, check if the call issued is referring to a native (builtin) function
                 Just a  -> nativeSafetyLayer'' $ a s

                 -- Otherwise check if the call is an alias, then check
                 -- if the call is referring to a module.
                 Nothing -> initCheck s
    
  where
    initCheck :: GenericIRC -> IRC ()
    initCheck gIRC = do
        db <- gets aliasDB

        case lookup (head $ msg gIRC) db of
            Just b  -> modCall gIRC{msg = (B.words b) ++ (tail $ msg gIRC)}
            Nothing -> modCall gIRC

    nativeSafetyLayer :: IRC () -> IRC ()
    nativeSafetyLayer g = io (catch (return g) $ \(SomeException _) -> return $ io(putStrLn "test")) >> return ()

    --nativeSafetyLayer' :: IRC () -> IRC ()
    --nativeSafetyLayer' g = StateT (catch
    --    (evalStateT g () >> return ())
    --    (\e -> putStrLn "Error" >> return()))
    nativeSafetyLayer'' g = catchStateT g (\(SomeException e) -> io(putStrLn "error"))   

catchStateT :: Exception e
            => StateT s IO a
            -> (e -> StateT s IO a)
            -> StateT s IO a
catchStateT a onE = do
    s1 <- get
    (result, s2) <- liftIO $ runStateT a s1 `catch` \e ->
        runStateT (onE e) s1
    put s2
    return result

modCall :: GenericIRC -> IRC ()
modCall s = do
    case parseIRCFunctionCall (B.unwords $ msg s) of
        -- There was an Error parsing the command, so send the detailed
        -- Error Message to IRC
        Left e  -> privmsg (orig s) $ B.pack $ (replace (show e) "\n" " ")

        -- Message was parsed successfully
        Right v -> 
            io (expose $ qModule v)
               >>= \exposed -> case exposed of

               -- No Module was found
               Nothing -> privmsg (orig s) "No such Module"

               -- Module was found
               Just a  -> do

                   -- Now that the Module was found, check if the Function exists
                   unlessFuncExists v (privmsg (orig s) "No such Func") $ do

                   -- Now, check if the user has enough Privilege to call that Module.
                   -- The Privilege required by the Module
                   unlessHasPrivilege (name . user $ s) v
                       (privmsg (orig s) "Permission denied")
                       (continue s v a)

  where
    continue :: GenericIRC -> IRCCommand -> String -> IRC ()
    {-# INLINE continue #-}
    continue gIRC v a = do
        h    <- gets iSock
        json <- io(craftJSON v h (orig gIRC))
        dbg  <- cDbg
        dbgC <- liftM B.pack $ cGetString "debug-channel"
        case json of
            Just j  -> execMod j (dbgC, dbg) $ constructModuleCall (gIRC,v) a
            Nothing -> return ()

unlessFuncExists :: IRCCommand -> IRC () -> IRC () -> IRC ()
unlessFuncExists v i1 i2 = 
    if hasFlag "force" $ qFlags v
        then i2
        else do
            x <- io (funcExists v)
            if x then i2
                 else i1

