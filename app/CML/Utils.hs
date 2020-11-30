module CML.Utils ( dontcare -- not a CML util but frequently used with select
                 , forever
                 , receiveSync
                 , sendSync
                 , selectSync
                 , spawnNoTID
                 , timedsend
                 ) where

import Control.Concurrent (threadDelay)
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

type Timeout = Int --seconds

data Message = NotReceived

timedsend :: Timeout
          -> Channel a
          -> a     -- value to send
          -> IO () -- timeout action
          -> IO ()
timedsend timeout ch v timeoutaction = do
  internalCh <- channel
  spawnNoTID $ do
    threadDelay timeout
    sendSync internalCh NotReceived
  selectSync [ wrap (transmit ch v) return
             , wrap (receive internalCh dontcare) timeouthandler]
  where
    timeouthandler _ = timeoutaction





-- timedsend :: Timeout -> Channel a -> a -> a -> IO ()
-- timedsend timeout ch v1 v2 = do
--   spawnNoTID $ do
--     threadDelay timeout
--     _ <- receiveSync ch -- receive v1
--     sendSync ch v2
--   sync $ wrapabort aborthandler (transmit ch v1)
--   where
--     aborthandler = do
--       sendSync ch v1
--       _ <- receiveSync ch -- receive v2 and allow termination
--       return ()
