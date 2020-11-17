module CML.Core where

import Control.Concurrent.BoundedChan
import Control.Concurrent.MVar

data Channel a = Channel (Event a) (BoundedChan a) (Event a)
type Event a = MVar a

channel :: IO (Channel a)
channel = do
  i <- newEmptyMVar
  o <- newEmptyMVar
  ch <- newBoundedChan 1
  return $ Channel i ch o

recvEvt :: Channel a -> Event a
recvEvt (Channel _ _ o) = o

sendEvt :: Channel a -> a -> Event ()
sendEvt (Channel i ch o) val = undefined

sync :: Event a -> IO a
sync = takeMVar
