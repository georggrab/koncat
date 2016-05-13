------------------------------------------------------------
-- |
-- Module      : Koncat.Module
-- License     : WTFPL Version 2
-- Maintainer  : Georg Grab, <Georg Grab, <grab@thereisnobreak.net>>
-- Stability   : Unstable
-- Portability : Linux
--
-- @Koncat.Module@ is responsible for starting and stopping Modules, checking if the
-- Module isn't annoying the IRC Server (for example flooding it, which could also lead into the server killing our bot)
-- and providing the Modules with the Koncat API
--
------------------------------------------------------------

{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}

module Koncat.Module (
    execMod
  , constructModuleCall  
--  , readMod  
) where


import System.Process
import System.Posix.IO
import System.Posix.Terminal
import System.Timeout
import Control.Concurrent
import Control.Exception
import Koncat.Core.API
import System.IO
import Text.Printf

import qualified Data.ByteString.Char8 as B

import Koncat.Source
import Koncat.Types.Common
import Koncat.Types.Module
import Koncat.Parser.ModuleParser(getTime)
import Koncat.Language.ModuleAPI

-- | Execute a Module, streaming its output into IRC.
execMod :: B.ByteString -> (Channel, Bool) -> ModuleCall -> IRC ()
execMod json dbg m = do
    bot <- get
    
    -- Increments the counter the State is carrying
    -- for future modules to be called.
    incCounter

    -- Start the Module
    io ((forkIO $ initModule json appl dbg bot) >> return ())
  where
    -- Change the destination of the ModuleCall if the -to flag
    -- has been toggled
    appl = applyModuleFlags m

initModule :: B.ByteString -> ModuleCall -> (Channel, Bool) -> Bot -> IO ()
initModule json m (dbgChan, debug) bot = do
    when debug $ putStrLn $ "Starting Module: " ++ show m

    -- Using a Pseudoterminal is neccessary because Modules are likely
    -- not to flush stdout regularily. This would result in the data
    -- the module sends not being available until execution stops.
    (master, slave) <- openPseudoTerminal
    hSlave          <- fdToHandle slave
    hMaster         <- fdToHandle master
    (_,_,_, pid)    <- 
        createProcess (proc (path m) []) { std_out = UseHandle hSlave 
                                         , std_in  = UseHandle hSlave
                                         , std_err = UseHandle hSlave }

    -- Put the Function name in the Modules STDIN.
    hPutStrLn hMaster $ func m

    -- Next, put the Args into the Modules STDIN.
    B.hPutStrLn hMaster json

    -- Throw away 2 Lines from the Modules STDOUT.
    -- This is neccessary because otherwise the two lines we put in the modules
    -- STDIN previously would be sent to IRC. We don't want that!
    _ <- hGetLine hMaster
    _ <- hGetLine hMaster

    -- React accordingly if the -maxexec flag is set. Otherwise retrieve
    -- the Modules Maximum Execution time from the SQL Database.
    maxExec <- decideMaxExec (getFlag "maxexec" (flags m)) 
                             (drop 4 $ path m) (func m)

    -- Enter the loop, wrapping the computation in the timeout function.
    -- Note that we are currently not in the IRC Monad, so all the sockets
    -- have to be carried over manually.
    t <- timeout maxExec $ loop (hasFlag "nocnt"     $ flags m) 
                                (hasFlag "nomodstop" $ flags m) 
                                hMaster bot dbgChan (dest m)
    case t of

        -- The Module has exceeded the Maximum Execution time.
        -- Unless the -nokill flag is set, send an Error Message to IRC
        -- and kill the process.
        Nothing -> 
            unless (hasFlag "nokill" (flags m)) $ do {privmsg' (iSock bot) (dest m) $ 
                B.pack $ printf "Module violating Maximum Execution time of %d : %s" maxExec (show m);
                terminateProcess pid}

        -- Do nothing if everything went fine.      
        Just _  -> return ()   

fetchLine :: Handle -> IO B.ByteString
fetchLine h = flip catch (\(_::IOException) ->
        -- GHC thinks a Hardware fault occured when the Process
        -- inside the Pseudoterminal ends. Let's catch it and
        -- tell loop that there are no more lines coming.
        -- Note that the "\EOI" message makes the Bot think it stopped,
        -- the Module may continue executing.
        -- However, once the maximum execution time is passed the Module will be killed
        -- anyways.
        return "\\EOI"
    ) $ B.hGetLine h

loop :: 
     -- Indicator that the -nocnt switch has been flipped,
     -- thus resulting in the Bot not writing the Counter 
     -- in front of every Message.
        Bool 

     -- Indicator that the -nostop switch has been flipped. 
     -- This results in the Bot not sending a "Module stopped" message 
     -> Bool 

     -- The Counter which is displayed in front of every message
     -- -> Integer 

     -- The master connected to the Pseudoterminal, acting as
     -- the STDIN and STDOUT of the Module.
     -> Handle 

     -- The IRC Network Socket
     -- -> Handle
    
     -- A copy of Bot containing the IRC Network Socket and Counter.
     -> Bot

     -- The Debug Channel
     -> Channel

     -- The Channel/Nick we should send the response to.
     -> Channel 

     -> IO ()
loop nocnt nostop h bot dbgChan c = do
    line <- fetchLine h
    case line of 
        "\\EOI" -> unless nostop $ privmsg' (iSock bot) c (sqBW  `B.append` "Module Stopped." )
        a       -> if B.head a == '\\'
            then checkAPICall a h bot dbgChan c >>= \x -> B.hPutStrLn h x
              >> loop nocnt nostop h bot dbgChan c
            else privmsg' (iSock bot) c (sqBW `B.append` a) >> loop nocnt nostop h bot dbgChan c
  where
    sqBW :: B.ByteString
    sqBW = if nocnt then
               B.empty
           else '(' `B.cons` (B.pack $ show (modCnt bot)) `B.append` ") "

applyModuleFlags :: ModuleCall -> ModuleCall
applyModuleFlags m = case getFlag "to" (flags m) of
    Nothing -> m
    Just a  -> m {dest = B.pack a}

decideMaxExec :: Maybe String -> String -> String -> IO Int     
decideMaxExec flag mPath mFunc = case flag of
    Nothing -> getMaxExecTime mPath mFunc
    Just a  -> return $ getTime $ B.pack a 

--checkUserPrivileges :: IRCCommand -> IO Bool    
--checkUserPrivileges i = case 

-- | Collect Information neccessary to start a Module.
constructModuleCall :: (GenericIRC, IRCCommand) -- ^ Information about the user, information about the command the user issued
                    -> String                  -- ^ the modulePath, as retrieved via Koncat.Source.expose
                    -> ModuleCall
constructModuleCall (a, b) mPath = 
    ModuleCall { path   = mPath
               , func   = qFunc b
               , args   = qArgs b
               , dest   = orig a
               , flags  = qFlags b
               , caller = name (user a) }
