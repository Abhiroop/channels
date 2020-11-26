module CML.Utils ( dontcare -- not a CML util but frequently used with select
                 , forever
                 , receiveSync
                 , sendSync
                 , selectSync
                 , spawnNoTID
                 ) where

import Control.Concurrent.CML
import Control.Monad(void)

dontcare :: a -> Bool
dontcare _ = True

receiveSync :: Channel a -> IO a
receiveSync ch = sync $ receive ch dontcare

sendSync :: Channel a -> a -> IO ()
sendSync ch v = sync $ transmit ch v

selectSync :: [Event a] -> IO a
selectSync = sync . choose

forever :: IO a -> IO b
forever action = do
  action
  forever action

spawnNoTID :: IO () -> IO ()
spawnNoTID = void . spawn
