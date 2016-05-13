------------------------------------------------------------
-- |
-- Module      : Koncat.Core.Callback
-- License     : WTFPL Version 2
-- Maintainer  : Georg Grab, <grab@thereisnobreak.net>
-- Stability   : Unstable
-- Portability : Linux
--
-- Implementation of Callbacks, that modules can register in order
-- to perform I/O with users or request other information from IRC.
--
------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module Koncat.Core.Callback(
    registerCallback
  , waitForSatiation  
  , deleteCallback
  , tryExec
) where
 
import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as B
import           System.Random
import           Control.Concurrent.STM
import           Koncat.Types.Common
import           Koncat.Types.Callback
import           Koncat.Core.API
import           Koncat.PrettyPrint((*+))

-- | Generate a random Integer while also provisioning for the extremely theoretical case
-- of the number already being used in the Callback-Database. If this is the case, increment
-- the random number by 1 and retry.
getClFree :: PtrCDB -> IO Int
getClFree cDB = do
    intGen <- randomIO :: IO Int
    atomically $ checkCDB intGen cDB
  where
    checkCDB :: Int -> PtrCDB -> STM Int
    checkCDB intGen db = 
      readTVar db >>= \x ->
        case Map.lookup intGen x of
            Just _  -> checkCDB (intGen+1) db
            Nothing -> return intGen

waitForSatiation :: PtrCDB -> Int -> STM (Maybe B.ByteString)
waitForSatiation cDB callID = do
        callDB <- readTVar cDB
        case Map.lookup callID callDB of
            Nothing -> return Nothing
            Just a  -> case mCarry a of
                Nothing -> waitForSatiation cDB callID
                x       -> return x
 
registerCallback :: CallBack -> Bot -> Channel -> Channel -> IO Int
registerCallback callback bot dbgChan from = do
    callID <- getClFree cDB
    case getIntent callback of
        ResponseRequest a -> privmsg' (iSock bot) from a >> continue callback cDB callID
        Stream c          -> (privmsg' (iSock bot) dbgChan $ "MODID["*+B.pack (show (modCnt bot))*+"]: started streaming from: " *+ c)
                          >> continue callback cDB callID
        VoidRequest       -> continue callback cDB callID
  where
    continue :: CallBack -> PtrCDB -> Int -> IO Int
    continue callb db callID = atomically $ do
        bot' <- readTVar db
        writeTVar db (Map.insert callID callb bot')
        return callID

    cDB :: PtrCDB
    cDB = callBackDB bot
    
deleteCallback :: PtrCDB -> Int -> STM ()
deleteCallback cDB callID = do
    bot <- readTVar cDB
    writeTVar cDB  (Map.delete callID bot)

tryExec :: PtrCDB -> GenericIRC -> IO ()
tryExec cDB gIRC = atomically $ do
   readTVar cDB >>= \x -> 
        writeTVar cDB $ Map.mapWithKey (\_ a ->
            case constraint a $ gIRC of
                Nothing -> a
                Just y  -> a{mCarry = Just y}
        ) x
