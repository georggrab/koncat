module Koncat.Types.Common (
    module Koncat.Types.IRCT
  , IRC, IConf, Bot(..)
) where

import qualified Data.Map              as Map
import qualified Data.ByteString.Char8 as B
import           Control.Monad.State
import           System.Time
import           System.IO
 
import           Koncat.Types.Callback
import           Koncat.Types.IRCT
import           Koncat.Types.Config
import           Koncat.Types.Module(Permission)

{- | Bot is carrying all the data we need in order to dispatch queries.
 -}
data Bot = Bot 
    { iSock      :: Handle          -- ^ The Socket that's connected to the server
    , iConf      :: IConf           -- ^ Configuration associated with the Bot instance
    , starttime  :: !ClockTime      -- ^ The starttime, for uptime tracking 
    , modCnt     :: !Integer        -- ^ A counter containing the number of Modules executed
    , aliasDB    :: [( B.ByteString
                     , B.ByteString
                    )]              -- ^ Aliases that are set
    , userDB     :: Map.Map Nick Permission -- ^ Users that have Registered
    , channelDB  :: ![Channel]      -- ^ Channels the Bot joined
    , callBackDB :: PtrCDB          -- ^ Database containing the Callbacks
    } deriving(Show)

{- | The IRC Monad 
 -}
type IRC = StateT Bot IO
