------------------------------------------------------------
-- |
-- Module      : Koncat.Core.Config
-- License     : WTFPL Version 2
-- Maintainer  : Georg Grab, <grab@thereisnobreak.net>
-- Stability   : Unstable
-- Portability : Linux
--
-- Koncat.Core.Config is the logic part of Koncats Configuration Infrastructure.
-- It creates the Data Structure that will be available in the IRC Monad later on from
-- the configuration file, contains a hard-coded table of default configuration allocations and
-- provides functions for working with configuration entries within the IRC Monad.
--
------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-do-bind -fno-warn-incomplete-uni-patterns -fno-warn-incomplete-patterns #-}

module Koncat.Core.Config(
  botsFromConfiguration
) where

import Text.Parsec(ParseError)
import Koncat.Parser.ConfigParser
import Koncat.Types.Config

data Polarity = North | South deriving Eq

botsFromConfiguration :: FilePath -> IO (Either ParseError [[Entry]])
botsFromConfiguration fp = do
    config <- parseConfiguration fp
    case config of
        Left parseError -> return $ Left parseError
        Right entries   -> do
            let overridden  = globalOverride $ getAllocations $
                    filterDirective entries North
                serverconfs = map (\(Directive (_,a)) -> withST (getAllocations a) overridden) $
                    (filterDirective entries South)
            return $ Right serverconfs


searchOverride :: Entry -> [Entry] -> [Entry]
searchOverride entry allE = go entry allE []
  where
    go _ [] skipped = skipped
    go oa@(Alloc (pk, _)) (relv@(Alloc (dk, _)):r) skipped =
        if pk == dk
            then skipped ++ (oa:r)
            else go oa r (relv:skipped)

withST :: [Entry] -> [Entry] -> [Entry]
withST [] state = state
withST (a:r) state = let state' = searchOverride a state
                     in withST r state'

{- | Given Allocations to be overridden, return all default allocations, overriding given Configuration switches
 -}
globalOverride :: [Entry] -> [Entry]
globalOverride etr = withST etr iterDefaults 

{- | If the Polarity is North, all Server Directives will be returned.
     If the Polarity is South, everything but the Server Directives will be returned.
 -}
filterDirective :: [Entry] -> Polarity -> [Entry]
filterDirective entries pl = flip filter entries $ \entry -> case entry of
    Directive ("server", _) -> if pl == North then True else False
    _                       -> if pl == South then True else False

{- | Top level should be a Directive; Get all Child Allocations of a (Server) Directive
 -}
getAllocations :: [Entry] -> [Entry]
getAllocations [] = []
getAllocations (x:xs) = case x of
    a@(Alloc (_,_)) -> a : getAllocations xs       
    Directive (_,e) -> getAllocations e ++ getAllocations xs

iterDefaults :: [Entry]
iterDefaults = 
    [ Alloc ("nick", "Koncat")
    , Alloc ("user", "Bot")
    , Alloc ("realname", "Koncat v0.0.0.12")   
    , Alloc ("address", "")
    , Alloc ("channel", "")
    , Alloc ("use-cprivmsg-if-chanop","false")
    , Alloc ("command-prefix", ":")
    , Alloc ("debug","true")
    , Alloc ("debug-channel","")
    , Alloc ("superadmin", "verify-via-cli")
    , Alloc ("allow-root", "false")
    , Alloc ("max-exec-time", "60s")
    , Alloc ("permission", "ANY")
    ]

