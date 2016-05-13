module Koncat.Types.Module (
    Permission(..), Handshake(..)
  , ModuleCall(..), ModAPICall(..)
) where

import qualified Data.ByteString.Char8 as B
import           Koncat.Types.IRCT

{- | Represents the Permission that is neccessary to call a Module 
i.e the permission the Module is requesting the user to have.

Note that this data differs from the levels of permission someone is able to assume.
 -}
data Permission 
    = Admin        -- ^ Having the highest possible priority, Admins are always able to execute a Module.
    | ANY          -- ^ Next is ANY, i.e the Module is not requesting the user to have any special permissions.
    | Privileged   -- ^ Only privileged users and above are allowed to execute
    | Registered   -- ^ Only registered users and above are allowed to execute
    | UnRegistered -- ^ Having the lowest possible priority, UnRegistered is only able to execute if ANY or UnRegistered is set.
    deriving (Eq,Show,Ord)

{- | Information a Module sends over during the HELO-Handshake 
 -}
data Handshake = Handshake
    { desc      :: B.ByteString -- ^ The Description of the Module
    , funcs     :: [( String
                    , String
                    , String
                    , Maybe [(Char, String)]
                   )]           -- ^ Functions the Module sends over (see "Koncat.Parsers")
    , author    :: B.ByteString -- ^ The Auther of the Module.
    } deriving (Show)

{- | Information that is neccessary in order to start a module 
 -}
data ModuleCall = ModuleCall 
    { path   :: FilePath       -- ^ The path of the Module
    , func   :: String         -- ^ The function of the Module to be called
    , args   :: Maybe [String] -- ^ Arguments the function should be called with
    , dest   :: Channel        -- ^ Where output should be redirected to (may also be a user)
    , caller :: Nick           -- ^ The Nick who called the Module (TODO: make it UserInfo)
    , flags  :: Flags          -- ^ Flags
    } deriving (Show, Eq)

{- | An API Call made by the Module 
 -}
data ModAPICall = ModAPICall 
    { getMFunc :: String
    , getMArgs :: Maybe [String]
    } deriving (Show)
