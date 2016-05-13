{-# LANGUAGE FlexibleInstances #-}

module Koncat.Types.Callback where

import qualified Data.ByteString.Char8 as B
import qualified Data.Map              as Map
import           Control.Concurrent.STM(TVar)
import           Koncat.Types.IRCT
 
type PtrCDB = TVar (Map.Map Int CallBack)

{- | Intent of a Callback, for example the Intent to conduct I/O with users, 
     gather some information from the IRC Server... 
 -}
data Intent 
    = ResponseRequest B.ByteString
    | Stream Channel
    | VoidRequest
    deriving (Eq, Show)

{- | A Callback that is defined in "Koncat.Core.Callback", gets registered in
     the global Callback Database and will deliver some information to a module some time.

     In order to do that, the @constraint@ function the Callback carries will execute
     every time new data is coming from the server. Once the constraint is met (it returns "Just ..."),
     the result of the constraint is copied to the @mCarry@ field the value of which is then delivered
     back to the module that called it.

     Once the constraint is met, it will be deleted from the Callback Database (exception: Stream Intent)
 -}
data CallBack = CallBack
    { getIntent  :: Intent             -- ^ What does the Callback do?
    , constraint :: GenericIRC         -- ^ When will the Callback trigger something?
                 -> Maybe B.ByteString
    , mCarry     :: Maybe B.ByteString -- ^ Result of constraint (if it's @Just ...@)
    }

instance Show CallBack where
    show _ = "<anonymous:callback>"
    
instance Show (TVar (Map.Map Int CallBack)) where
    show _ = "<var>"
