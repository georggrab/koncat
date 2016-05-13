------------------------------------------------------------
-- |
-- Module      : Koncat.Language.ModuleAPI
-- License     : WTFPL Version 2
-- Maintainer  : Georg Grab, <grab@thereisnobreak.net>
-- Stability   : Unstable
-- Portability : Linux
--
-- @Koncat.Language.ModuleAPI@ is providing an API to Modules that they can use
-- to do various tasks like asking someone in an IRC Channel for some more input
-- required to continue the Module or listing what users are joined to Channels relevant to the Bot.
--
-- Examples of API Calls coming from a Modules STDOUT
--
-- > \askforinput{Please enter an additional Number:}
--
-- Koncat will automatically infer most arguments supplied to the API, thus
-- quotation marks are not needed for strings.
--
-- > \askforinput{"Please, enter an additional Number", 60}
--
-- A lot of API Calls are overloaded, hence multiple arguments may be supplied to them.
-- For example the above version of askforinput will emit a signal to the module stating
-- that nobody answered the demand for input within 60 seconds. 
--
-- Also note how we explicitly used quotation marks in the example above, even though it
-- was stated previously that types are inferred. However, note that we used a comma in that string; it would follow that
-- if quotation marks were to be emitted the comma would be interpreted as an extra argument
-- to the API which is not what we want.
--
-- Refer to the ModuleAPI.README or to the source code of this File to get a complete list
-- of API Calls, their parameters and overloadings.
--
-- Also check docs/implementLang.README for notes on what things your API should comply with if you're implementing
-- an API for Koncat in your favorite (not-supported) programming language.
------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module Koncat.Language.ModuleAPI (
    checkAPICall
) where


import           System.IO(Handle)
import qualified Data.ByteString.Char8 as B

import           Control.Concurrent.STM

import           Koncat.Parser.ModuleParser
import           Koncat.Core.API
import           Koncat.PrettyPrint((*+), createRReq)
import           Koncat.Types.All
import           Koncat.Core.Callback

-- unsure whether master-handle is neccessary?
checkAPICall :: B.ByteString 
             -> Handle 
             -> Bot
             -> Channel
             -> Channel
             -> IO B.ByteString
checkAPICall input _ bot dbgChan from = case parseAPICall input of
    Left _  -> return "\\PFAIL"
    Right v -> case getMFunc v of
        "askforinput" -> askforinput bot from dbgChan $ getMArgs v
        "joinchannel" -> joinchannel bot      $ getMArgs v
--        "listusers"   -> listusers   bot      $ getMArgs v
--        "privmsg"     -> privmsgMod  bot      $ getMArgs v
        _             -> return "\\PNOFUNC"


askforinput :: Bot -> Channel -> Channel -> Maybe [String] -> IO B.ByteString
askforinput bot from dbgChan apiArgs = case apiArgs of
    Nothing  -> return "\\ENOSUCHOVERLOAD"
    Just [a] -> do
        callId <- registerCallback (CallBack (ResponseRequest $ createRReq a $ modCnt bot)  (\gIRC -> 
            if (head $ msg gIRC) == '(' `B.cons` B.pack (show $ modCnt bot) *+ ")>"
                then Just $ B.concat $ tail $ msg gIRC
                else Nothing  
                  ) Nothing ) bot from dbgChan
        b <- atomically $ waitForSatiation (callBackDB bot) callId 
        atomically $ deleteCallback (callBackDB bot) callId
        case b of
            Nothing -> return "\\NOSATIATION"
            Just c  -> return c

    {-Just [a,t] -> do
        callId <- registerCallBack ResponseRequest ("Input Request: " ++ a)
        b      <- waitForSatiation callId (read t :: Int)
        deleteCallBack callId
        case b of
            Nothing -> return "\\NOSATIATION"
            Just a  -> return a
-}
    _     -> return "\\ENOSUCHOVERLOAD"

joinchannel :: Bot -> Maybe [String] -> IO B.ByteString
joinchannel bot apiArgs = case apiArgs of
    Nothing  -> return "\\ENOSUCHOVERLOAD"
    Just [a] -> do
        r_write' (iSock bot) $ "JOIN " *+ B.pack a
     >> return "\\OK"   
    Just _   -> return "\\ENOSUCHOVERLOAD"
