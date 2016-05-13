------------------------------------------------------------
-- |
-- Module      : Koncat.Core.Native.Permissions
-- License     : WTFPL Version 2
-- Maintainer  : Georg Grab, <grab@thereisnobreak.net>
-- Stability   : Unstable
-- Portability : Linux
--
-- This Module implements the Permission-System of the bot.
--
-- Maintenance of User Information is done in "Koncat.Permissions".
-- "Koncat.Core.Native.Permissions" is providing the actual bindings to IRC that
-- users can use to login, @logout@ and register.
--
-- Todo:
--
-- * grantUserPermissions function
--
-- * Logout user when IRC is quit
--
-- * Cross-Check with Permission-System of the IRC Server in order to confirm
--   the identity of a user.
--
------------------------------------------------------------
  
{-# LANGUAGE OverloadedStrings #-}

module Koncat.Core.Native.Permissions (
    updateUserDB_userQuit
  , updateUserDB_identQuery
  , handleRegister
--  , loginUser
--  , logoutUser
--  , deleteUser
--  , grantUserPermissions
) where

import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as B

import           Koncat.Core.API
import           Koncat.Permissions
import           Koncat.Types.Common
import           Koncat.Types.Module
import           Koncat.Parser.IRCParser

-- | The user has quit, so log them out from the system.
updateUserDB_userQuit :: B.ByteString -> IRC ()
updateUserDB_userQuit u = do
    bot  <- get
    db   <- gets userDB
    nick <- cGetString "nick"
    let who = user $ parseIRC (B.pack nick) u
    put $ bot{userDB = Map.delete (name who) db}

-- | A user has requested identification.
--
-- Verify their credentials and log them in, or display
-- an error message if the credentials are wrong.
updateUserDB_identQuery :: GenericIRC -> IRC ()
updateUserDB_identQuery gIRC = 
    if isPrivateMsg gIRC    
        then do
            -- Identify <the-rest-is-the-password>
            let creds = B.unwords . tail $ msg gIRC

            login <- io(loginUser (orig gIRC) creds)

            case login of 

                -- The user typed the wrong password.
                Nothing -> privmsg (orig gIRC) "Authentification failed."

                -- No record in the SQLite Database was found.
                Just UnRegistered -> privmsg (orig gIRC) "You are not registered. Use \":register <password>\" to register."

                -- The password matched, modify the State in order to grant newly gained permissions to the user.
                Just a  -> do
                    bot <- get
                    db  <- gets userDB
                    put bot{userDB = Map.insert (orig gIRC) a db}
                    privmsg (orig gIRC) $ "Authentification successful, Privileges elevated to: " `B.append` (B.pack $ show a)
                    
        else privmsg (orig gIRC) "Please privmsg me for that"

-- | A user wants to register.
handleRegister :: GenericIRC -> IRC ()
handleRegister gIRC =
    if isPrivateMsg gIRC
        then do
            exists <- io(userExists (name . user $ gIRC))
            if exists    
                then privmsg u "You are already registered."
                else io(registerUser u creds) >> privmsg u ("OK, Registered with: " `B.append` u `B.append` " : " `B.append` creds)

        else privmsg u "Please privmsg me for that"
  where
    u = orig gIRC
    creds = B.unwords . tail $ msg gIRC
