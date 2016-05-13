module Koncat.Types.IRCT where

import qualified Data.ByteString.Char8 as B

{- | The Nickname of a user 
 -}
type Nick = B.ByteString

{- | An IRC channel 
 -}
type Channel = B.ByteString

{- | Flags the user supplies
 -}
type Flags = Maybe [(String, Maybe String)]

{- | A message coming from the user directed to the Bot (not referring to a native call) 
 -}
data IRCCommand = IRCCommand 
    { qModule :: String         -- ^ the Module the user attempts to call
    , qFunc   :: String         -- ^ the Function the user attempts to call
    , qArgs   :: Maybe [String] -- ^ Arguments the user is delivering to the function
    , qFlags  :: Flags          -- ^ Flags the user is transmitting to the Bot
    } deriving (Show)

{- | GenericIRC holds information contained in a single line of input coming from the server. (see "parseIRC") 
 -}
data GenericIRC = GenericIRC
    { user :: UserInfo        -- ^ Information about the User
    , orig :: Channel         -- ^ Where the Message is coming from. Note this may also be a User, not only a Channel.
    , msg  :: [B.ByteString]  -- ^ The actual Message.
    } deriving (Show)

{- | A minimal amount of Information about the User a Message is coming from 
 -}
data UserInfo = UserInfo
    { name  :: Nick         -- ^ The Nickname of the user
    , rName :: Nick         -- ^ the RealName of the user
    , vhost :: B.ByteString -- ^ the vhost of the user 
    } deriving (Show, Eq)

