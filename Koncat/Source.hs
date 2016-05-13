------------------------------------------------------------
-- |
-- Module      : Koncat.Source
-- License     : WTFPL Version 2
-- Maintainer  : Georg Grab, <grab@thereisnobreak.net>
-- Stability   : Unstable
-- Portability : Linux
--
-- "Koncat.Source" will source child directories of mod\/ for modules.
-- It will also expose a few functions for working with the .koncat.cache.db,
-- a database where temporary Module Information is stored and constantly rebuilt.
--
-- Plugins will be available to the user by calling it with its (mod-)path from IRC.
--
-- Implementation:
--
-- * Every Plugin, no matter the language it is written in, has to implement
--   a handshake, exposing functions, description for functions and various other
--   metadata to the Bot.
--
--   For elaboration on the Syntax of handshakes, check "Koncat.Parsers" 
--
-- * This Module proceeds to store the information provided by the modules in a SQLite database.
--
-- * The user then is able to call a Function of a Module from IRC using the Syntax described above.
--
-- Notes:
--
-- * Functions of modules are called by printing their name in the modules STDIN.
--
-- * A Directory may be excluded from sourcing by putting a file named .dont.src in it.
--
------------------------------------------------------------

{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Koncat.Source ( 
     -- * General functions
     rebuild
   , initModuleChangeHandler  
   , rmDB  
     -- * Operations on modules
   , update
   , expose
   , exposeMeta
   , dropMod
   , getMaxExecTime
   , fetchModules
   , fetchArguments
   , safeMod
     -- * True/False queries on the database
   , doesTableExist
   , funcExists
   , doesModuleExist
     -- * Other, not yet used operations on the database
   , fetchStr
   , toString
 
) where


import System.Directory
import System.Process
import System.IO
import System.Posix.Files
import Control.Monad

import System.INotify
import System.FilePath.Posix
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as B
import Database.HDBC
import Database.HDBC.Sqlite3   

import Koncat.Parser.ModuleParser
import Koncat.Types.Common
import Koncat.Types.Module
import Koncat.Core.API (replace)

-- Files and Directories to be excluded from sourcing.
-- Note: the sourcing takes place in the mod/ Directory
noMod :: [String]
noMod = ["..", "."]

-- | Rebuild the database
rebuild :: IO ()
rebuild = do
    notNecessary <- checkSizeEntry
    if notNecessary
        then putStrLn "Supressing Rebuild"
        else initRebuild
  where
    initRebuild = do
        putStr "Rebuilding Database.. "

        -- Remove the current Database
        rmDB
        c    <- connectSqlite3 ".koncat.cache.db"
        setCurrentDirectory "mod/"
        dDat <- deepRecurse "."

        -- We don't need the "./" in front of the filenames
        let proper = map (drop 2) dDat
        setCurrentDirectory ".."

        -- Create the Main Table Structure
        quickQuery' c "CREATE TABLE mod ( modname CHAR(500), cpath CHAR(500), \
                                        \ author CHAR(50) )" []
         
        -- This is the information we get just by searching the Directory tree
        stmtInit <- prepare c "INSERT INTO mod VALUES (? , ?, NULL)"

        -- Populate mod database
        forM_ proper $ \dir -> do
            execute stmtInit [toSql (getModName dir), toSql dir]

        -- Select our newly created entries in order to create mod_ tables
        result <- quickQuery' c "SELECT modname, cpath FROM mod" []

        let modnames = map (\x -> fromSql $ (x !! 0)::String) result

        -- Create additional tables
        forM_ modnames $ \funcn -> do
            quickQuery' c (replace mod_stmt "%s" funcn) []
            
        -- Finally, handshake the modules and fill our newly created tables
        -- with information
        forM_ result $ \sBL -> do
           handshake c (fromSql $ (sBL !! 1)::String) 

        commit c
        disconnect c
        writeDBSize
        putStrLn "done"    

    mod_stmt = "CREATE TABLE [mod_%s] (  \
                    \  fname   CHAR(50)  \
                    \, fdesc   CHAR(8000)\
                    \, args    CHAR(500) \
                    \, execT   INTEGER   \
                    \, perms   CHAR(50)  \
                \);"

-- Perform a handshake on a Module
handshake :: Connection -> FilePath -> IO ()
handshake c s = do
    hs <- E.catch (readProcess ("mod" </> s) [] "HELO") $ \(e::E.SomeException) -> do
        putStrLn $ "Encountered Exception while parsing handshake of " ++ show s ++ " : " ++ show e
        return []

    case hs of
       [] -> return ()
       _  -> dispatchHandshake c (B.pack hs) s
        

-- Handshake the module and put the results in the SQLite Database.
dispatchHandshake :: Connection -> B.ByteString -> FilePath -> IO ()
dispatchHandshake conn hs s = do

    -- Parse the Handshake
    let parsed = parseHandshake hs

    -- I decided to keep Parser.hs pure and raise the Error here.
    -- [("E","E","E")] is representing the Error
    if quad1 ( head ( funcs parsed)) == "E" 
        then putStrLn $ quad3 $ head $ funcs parsed
        else return ()    

    -- This statement will insert the Author in the mod Database.
    quickQuery' conn "UPDATE mod SET author = ? WHERE cpath = ?" [ toSql $ author parsed, toSql s ]

    
    -- This statement will insert the Handshake into the appropriate Database.
    stmtIns <- prepare conn $ "INSERT INTO [mod_" ++ getModName s ++ "] VALUES (?, ?, ?, ?, ?)"

    forM_ (funcs parsed) $ \funcName -> do
        execute stmtIns [ toSql (quad1 funcName)
                        , toSql (quad2 funcName)
                        , toSql (quad3 funcName)
                        , toSql $ getTime $ B.pack $ (valueOrDefault 'E') (quad4 funcName) 
                        , toSql $ (valueOrDefault 'P') (quad4 funcName)
                        ]
  where
    getVar :: Char -> [(Char, String)] -> String
    getVar c []           = getVar c defaultValues
    getVar c ((v,val):xs) = if v == c
        then val
        else getVar c xs
        
    valueOrDefault :: Char -> Maybe [(Char, String)] -> String
    valueOrDefault var varContainer = case varContainer of
        Nothing -> getVar var defaultValues
        Just a  -> getVar var a

-- | Remove the Database
rmDB :: IO ()
rmDB = do
    exists <- doesFileExist ".koncat.cache.db"
    if exists 
        then removeFile ".koncat.cache.db"
        else return ()    

initModuleChangeHandler :: IO ()
initModuleChangeHandler = do
    inotify <- initINotify
    addWatch inotify [Modify, Move, MoveIn, MoveOut, Delete, Create] "mod/" watcher
    return ()
  where
    watcher _ = putStrLn "Database Update triggered"
             >> rebuild

writeDBSize :: IO ()
writeDBSize = do
    exists <- doesFileExist ".koncat.cache.db"
    if exists
        then do
            status <- getFileStatus ".koncat.cache.db"
            withFile ".koncat.tmp" WriteMode $ \h ->
                hPutStr h . show $ fileSize status
        else return ()

checkSizeEntry :: IO Bool
checkSizeEntry = do
    exists <- doesFileExist ".koncat.tmp"
    if exists
        then do
            sizeRecorded <- readFile ".koncat.tmp"
            sizeReal     <- liftM fileSize $ getFileStatus ".koncat.cache.db"
            if sizeRecorded == show sizeReal
                then return True
                else return False
        else return False

-- | This is the name IRC users will call a module with.
-- If a Path is util/test.py the modName will be util.test
getModName :: FilePath -> String
getModName s = dropExtension $ replace s "/" "." 

-- ----------
-- Functions for working with the tripleton in `handshake`
quad1 :: (a, b, c, d) -> a
quad1 (a, _, _, _) = a

quad2 :: (a, b, c, d) -> b
quad2 (_, b, _, _) = b

quad3 :: (a, b, c, d) -> c
quad3 (_, _, c, _) = c

quad4 :: (a, b, c, d) -> d
quad4 (_, _, _, d) = d
-- ----------

-- | Get Files in Subdirectories
deepRecurse :: FilePath -> IO [FilePath]
deepRecurse top = do
    scnt <- getDirectoryContents top 

    -- Filter files starting with a dot and files listed in noMod
    let proper = filter nope scnt

    -- Apply deepRecurse to every Directory
    paths <- forM proper $ \nm -> do
        let curPath = top </> nm
        isDir <- doesDirectoryExist curPath
        if isDir 
            then do

            -- If a .dont.src file is in the directory, don't do anything
            dont <- doesFileExist (curPath </> ".dont.src")

            if not dont 
               then deepRecurse curPath
               else return []    

            -- If it's a file, put it in the list    
            else do
                -- But first check if it is executable
                exec <- getPermissions curPath
                if executable exec 
                    then return [curPath]
                    else putStrLn ("Warning: Non-Executable file \""++curPath++"\" in mod Directory, ignoring.")
                      >> return []

    -- Concatentate the results.  
    return (concat paths)    
  where
    nope x = x `notElem` noMod && head x /= '.'

fetchStr :: String -> [SqlValue] -> IO [[String]]
fetchStr q s = do
    c <- connectSqlite3 ".koncat.cache.db"
    v <- quickQuery' c q s
    return $ map (\innerList -> 
        map (\values -> fromSql values::String) innerList) v

fetchBStr :: String -> [SqlValue] -> IO [[B.ByteString]]
fetchBStr q s= do
    c <- connectSqlite3 ".koncat.cache.db"
    v <- quickQuery' c q s
    return $ map (\innerList -> 
        map (\values -> fromSql values::B.ByteString) innerList) v

toString :: [[SqlValue]] -> [[String]]
toString v = map (\inner ->
    map (\val -> fromSql val::String) inner) v

toBString :: [[SqlValue]] -> [[B.ByteString]]
toBString v = map (\inner ->
    map (\val -> fromSql val::B.ByteString) inner) v

{- | When working with [mod_some.module] tables, doing something like

> SELECT * FROM [mod_" ++ getModName someModule ++ "]"

will result in a SQL Injection vulnerability.

This function will safely execute queries referring to such tables.

>>> safeMod "util.test" "SELECT * FROM {DB}" []
Just [[SqlByteString "helloworld",SqlByteString ....

>>> safeMod "inputContainingSqli" "SELECT * FROM {DB}" []
Nothing
 -}
safeMod :: String     -- ^ Modname, for example \"util.test\"
        -> String     -- ^ Query, for example \"SELECT * FROM {DB}\". {DB} is a placeholder for the modname.
        -> [SqlValue] -- ^ Additional values to be inserted in the database.
        -> IO (Maybe [[SqlValue]])
safeMod mName q v = do
    c <- connectSqlite3 ".koncat.cache.db"
    r <- quickQuery' c "SELECT 1 FROM mod WHERE modname=?" [toSql mName]
    case r of
        [] -> disconnect c >> return Nothing
        _  -> do
            x <- disP (replace q "{DB}" $ "[mod_"++mName++"]") v
            return $ Just x
  where
    disP :: String -> [SqlValue] -> IO [[SqlValue]]
    disP query v' = do
        c <- connectSqlite3 ".koncat.cache.db"
        r <- quickQuery' c query v'
        commit c
        disconnect c
        return r

-- | Rebuild the cache of a specific module.
-- This function uses the filename, not the modname.
update :: FilePath -> IO ()
update what = do
    -- This Function is likely to be called apart from other 
    -- functions in this file, so (re)establish the connection
    c <- connectSqlite3 ".koncat.cache.db"

    -- Delete everything from the Table
    safeMod (getModName what) "DELETE FROM {DB}" []

    -- `handshake` will repopulate the Table
    handshake c what

    commit c
    disconnect c

-- | Remove a module from the database.
-- This function uses the filename, not the modname.
dropMod :: FilePath -> IO ()   
dropMod what = do
    c <- connectSqlite3 ".koncat.cache.db"

    -- Delete the Entry in the metatable, mod 
    quickQuery' c "DELETE FROM mod WHERE cpath=?" [toSql what]

    commit c
    disconnect c

    -- Drop the modules own table, mod_<modules modname>
    safeMod (getModName what) "DROP TABLE {DB}" []

    return ()


-- | Search the database if there is a module matching a modname.
-- Returns the Path to the module, or Nothing.
expose :: String -> IO (Maybe FilePath)
expose modname = do
    c <- connectSqlite3 ".koncat.cache.db" 

    result <- quickQuery' c "SELECT cpath FROM mod WHERE modname=?" [toSql modname]

    case result of
        [a]   -> return $ Just ("mod" </> fromSql (a !! 0)::FilePath)
        _     -> return Nothing

-- | Search the database and return the (name,args, permissions, desc) of the functions.
exposeMeta :: String -> IO (Maybe [[B.ByteString]])
exposeMeta what = do
    result <- safeMod what "SELECT fname, args, perms, fdesc FROM {DB}" []
    case result of
        Nothing -> return Nothing
        Just a  -> return $ Just $ toBString a

-- | List modules that are available.
fetchModules :: IO [B.ByteString]    
fetchModules = fetchBStr "SELECT modname FROM mod" [] >>= return . concat 

-- | List Arguments a function requires.
fetchArguments :: String -> String -> IO [B.ByteString]
fetchArguments module_ func_ = do
    res <- safeMod module_ "SELECT args FROM {DB} WHERE fname=?" [toSql func_]
    case res of
        Nothing -> return []
        Just a  -> case B.words $ B.concat $ concat $ toBString a of
            ["Void"] -> return []
            b        -> return b

-- | Check if a module exists
doesModuleExist :: String -> IO Bool
doesModuleExist module_ = do
    modN <- expose module_
    case modN of
        Nothing -> return False
        Just _  -> return True

-- | Return the maximum execution time of a Function in microseconds, given the filepath and the function
getMaxExecTime :: FilePath -> String -> IO Int
getMaxExecTime s f = do
    res <- safeMod (getModName s) "SELECT execT FROM {DB} WHERE fname=?" [toSql f]
    case res of
        Nothing -> return 0
        Just a  -> return (fromSql (head $ head $ a)::Int)

-- | Check if the functioncall of a module is vaild.
funcExists :: IRCCommand -> IO Bool 
funcExists i = do
    res <- safeMod (qModule i) "SELECT fname FROM {DB} WHERE fname=?" [toSql $ qFunc i]
    case res of
        Nothing -> return False
        Just [] -> return False
        Just _  -> return True

-- | Check if a table in the database exists.
doesTableExist :: String -> IO Bool
doesTableExist table = do
    c <- connectSqlite3 ".koncat.cache.db"
    t <- getTables c
    case filter (==table) t of
        [] -> return False
        _  -> return True

