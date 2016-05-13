module Koncat.Types.Config where

data Entry = Alloc (String,String) | Directive (String,[Entry]) deriving (Show,Eq)
type IConf = [Entry]
