------------------------------------------------------------
-- |
-- Module      : Koncat.Language.Arguments
-- License     : WTFPL Version 2
-- Maintainer  : Georg Grab, <grab@thereisnobreak.net>
-- Stability   : Unstable
-- Portability : Linux
--
-- Allow functions to request a specific set of arguments in order to make them do something useful.
--
-- The arguments that a function requires to have has to be in the module handshake.
-- 
-- Arguments in the handshake may be of one of the types supported by JSON:
--   * String
--   * Number
--   * Array
--   * Bool
-- 
-- Special Arguments include:
--     void (empty brackets) -> No Arguments
--     web  -> Spawns a web interface for inserting Arguments (not to be implemented anytime soon)
-- 
-- The Types are merely for parsing and comfort-purposes, meaning that they will still be inserted
-- into the Module the same way they did before, by putting them into stdin line-by-line.
-- 
-- This results in, in case there's no API for the Language you're writing the Module in,
-- the Arguments being ultimately presented to you as strings (that you will be able to convert easily).
------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module Koncat.Language.Arguments where

import           System.IO(Handle)
import           Data.Either
import           Data.Maybe
import           Data.Char(toLower)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy  as BL

import           Data.Aeson
import           Data.Text.Encoding

import           Koncat.PrettyPrint(fExpct', (*+))
import           Koncat.Source(fetchArguments)
import           Koncat.Core.API
import           Koncat.Types.Common

newtype ArgE = ArgE { getEArgs :: [Value] } deriving (Show)

instance ToJSON ArgE where
    toJSON (ArgE values) = object ["args" .= values]

craftJSON :: IRCCommand -> Handle -> Channel -> IO (Maybe B.ByteString)
craftJSON iCmd hdl origC = do
    argsReq <- fetchArguments (qModule iCmd) (qFunc iCmd)
    if (length argsReq) /= (length . fromMaybe [] $ qArgs iCmd)
        then privmsg' hdl origC "Call failed: Argument mismatch"
          >> return Nothing
        else do
            let converted = convertAll iCmd argsReq
            case lefts converted of
                [] -> return $ Just $ BL.toStrict $ encode (ArgE (rights converted)) 
                _  -> privmsg' hdl origC
                        ("Call failed: Invalid Argument (Expected: " *+ fExpct' (qFunc iCmd) argsReq *+ ")")
                   >> return Nothing
    
convertAll :: IRCCommand -> [B.ByteString] -> [Either () Value]
convertAll iCmd argsReq = do
    case qArgs iCmd of
        Nothing -> []
        Just a  -> flip map (zip argsReq (map B.pack a)) $ \e ->
            case fst e of
                "Int"    -> rawInt $ snd e

                "Bool"   -> rawBool $ snd e

                -- It's always possible to parse a string, so set the result to Right
                "String" -> Right $ rawStr $ snd e

                -- The case of a wrong argument supplied is taken care of earlier already,
                -- provide a catch-all case alternative anyways, just in case.
                _        -> Left ()

--convertArg :: B.ByteString -> B.ByteString -> Either (IRC ()) Value
--convertArg iCmd what = 

rawInt :: B.ByteString -> Either () Value
rawInt b = case B.readInt b of
    Nothing       -> Left ()
    Just (int,bs) -> if B.empty == bs
        then Right (Number $ fromIntegral int)
        else Left  ()

rawStr :: B.ByteString -> Value
rawStr = String . decodeUtf8 

rawBool :: B.ByteString -> Either () Value
rawBool b = case B.map toLower b of
    "true"  -> Right $ Bool True
    "false" -> Right $ Bool False
    _       -> Left ()


--argsMatch :: Either SArgs [Value] -> IRCCommand -> IRC Bool
--argsMatch (Left a)  iCmd = case a of
--    Varidic -> return True
--    Void    -> if qArgs iCmd == [] then return True else return False
--argsMatch (Right b) iCmd = 

--argsMatch :: [String] -> IRCCommand -> IRC Bool
--argsMatch ["Varidic"]
--argsMatch args iCmd = 

--argEncode :: IRCCommand -> IO (Maybe [(ArgT, a)])
--argEncode =
    

