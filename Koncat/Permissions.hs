------------------------------------------------------------
-- |
-- Module      : Koncat.Permissions
-- License     : WTFPL Version 2
-- Maintainer  : Georg Grab, <grab@thereisnobreak.net>
-- Stability   : Unstable
-- Portability : Linux
--
-- Various functions neccessary to control Module Execution
-- by implementing Permissions.
--
------------------------------------------------------------

{-# LANGUAGE OverloadedStrings, CPP #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
 
module Koncat.Permissions (
    registerUser
  , rebuildPermDB  
  , loginUser
  , toPermission
  , userExists
  , getPermission
  , unlessHasPrivilege
  , delUser
  , wipePermDB
) where

import           Prelude hiding (lookup)
import qualified Data.ByteString.Char8 as B
import           Data.Char(toLower)
import           Data.Map(lookup)
import           Data.Maybe(fromMaybe)
import           System.Directory

import           Database.HDBC
import           Database.HDBC.Sqlite3   
import           Crypto.PasswordStore

import           Koncat.Core.API
import           Koncat.Types.Common
import           Koncat.Types.Module

-- | Register a user 
registerUser :: B.ByteString -- ^ Username
             -> B.ByteString -- ^ Password
             -> IO ()
registerUser u s = do
    c  <- connectSqlite3 ".koncat.user.db"
    pw <- makePassword s 12
            
    quickQuery' c "INSERT INTO users VALUES (?,?,?)" [ toSql u
                                                     , toSql $ pw
                                                     , toSql ("Registered"::B.ByteString)]
    commit c
    disconnect c

-- | Login a user
loginUser :: B.ByteString -> B.ByteString -> IO (Maybe Permission)
loginUser u s = do
    c   <- connectSqlite3 ".koncat.user.db"
    res <- quickQuery' c "SELECT password,level FROM users WHERE username=?" [toSql u]

    case res of
        [] -> return $ Just UnRegistered
        _  -> if verifyPassword s (fromSql (head . head $ res)::B.ByteString)
                then return $ Just $ toPermission  (fromSql (last . head $ res)::String)
                else return Nothing

-- | Check if a user exists in the Database
userExists :: B.ByteString -> IO Bool
userExists u = do
    c   <- connectSqlite3 ".koncat.user.db"
    res <- quickQuery' c "SELECT 1 FROM users WHERE username=?" [toSql u]

    case res of
        []  -> return False
        _   -> return True

-- | Delete a user from the database
delUser :: String -> IO () 
delUser u = do
    -- Note that input validation is done in Koncat.Core.Native.Permissions
    c   <- connectSqlite3 ".koncat.user.db"
    quickQuery' c "DELETE FROM users WHERE username=?" [toSql u]

    commit c
    disconnect c

-- | Set up the database structure
rebuildPermDB :: IO ()
rebuildPermDB = do
     c <- connectSqlite3 ".koncat.user.db"
     quickQuery' c "CREATE TABLE users ( username CHAR(500), password CHAR(500), level CHAR(50) );" []
     commit c
     disconnect c

-- | Completely remove the database and rebuild the database structure.
wipePermDB :: IO ()     
wipePermDB = do
    exists <- doesFileExist ".koncat.user.db"
    if exists 
        then removeFile ".koncat.user.db" >> rebuildPermDB
        else rebuildPermDB

-- | If user has enough privileges, do something. Otherwise, do something else.
unlessHasPrivilege :: Nick       -- The Nick that is querying us 
                   -> IRCCommand -- The IRCCommand the nick is sending along
                   -> IRC ()     -- The user doesn't have enough permissions 
                   -> IRC ()     -- The user has enough permissions 
                   -> IRC ()
unlessHasPrivilege who iCmd i0 i1 = do
    modPrivilegeRequired <- io(getPermission iCmd)

    -- The Privilege the user has
    userPrivilege        <- gets userDB

    case modPrivilegeRequired of

       -- No special Permissions are required for this Plugin, so continue
       ANY -> i1

       -- Otherwise compare userPrivilege and modPrivilege.
       -- If the user is not in the userDB (not identified) 
       -- the privilege will default to UnRegistered.
       b   -> if b >= fromMaybe UnRegistered 
                          (lookup who userPrivilege)

                  -- Permissions of the User are larger or equal; continue execution
                  then i1

                  -- Fail with a Message to orig. (Maybe privmsg the User?)
                  else i0

-- | Query the database for a Permission of a user
getPermission :: IRCCommand -> IO Permission
getPermission i = do
    c <- connectSqlite3 ".koncat.cache.db"

    res <- quickQuery' c ("SELECT perms FROM [mod_" ++ qModule i ++ "] WHERE fname=?") [toSql $ qFunc i]
    print $ qModule i
    print $ qFunc i
    print $ res

    return $ toPermission (fromSql (head $ head $ res)::String)

-- | Convert a String to a Permission 
--
-- TODO:
--
-- * Simply make an instance of Read
toPermission :: String -> Permission
toPermission a =
    case map toLower a of
        "admin"         -> Admin
        "administrator" -> Admin
        "registered"    -> Registered
        "privileged"    -> Privileged
        "any"           -> ANY
        _               -> Admin
        
