module CML.Utils ( receiveSync
                 , sendSync
                 , selectSync
                 ) where

import Control.Concurrent.CML

dontcare :: a -> Bool
dontcare _ = True

receiveSync :: Channel a -> IO a
receiveSync ch = sync $ receive ch dontcare

sendSync :: Channel a -> a -> IO ()
sendSync ch v = sync $ transmit ch v

selectSync :: [Event a] -> IO a
selectSync = sync . choose
