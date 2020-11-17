module Fib where

import Control.Concurrent (forkIO)
import Control.Concurrent.CML
import Control.Monad (void)
import CML.Utils
import Data.IORef

forever :: IO a -> IO b
forever action = do
  action
  forever action


spawnNoTID = void . spawn

add :: Channel Int -> Channel Int -> Channel Int -> IO ()
add inCh1 inCh2 outCh = forever $ do
  v1 <- receiveSync inCh1
  v2 <- receiveSync inCh2
  sendSync outCh (v1 + v2)


delay :: Maybe Int
       -> Channel Int
       -> Channel Int
       -> IO ()
delay init inCh outCh = forever $ do
  case init of
    Nothing -> do
      v <- receiveSync inCh
      delay (Just v) inCh outCh
    Just v -> do
      sendSync outCh v
      delay Nothing inCh outCh

copy :: Channel Int -> Channel Int -> Channel Int -> IO ()
copy inCh outCh1 outCh2 = forever $ do
  v <- receiveSync inCh
  sendSync outCh1 v
  sendSync outCh2 v

mkFibNetwork :: IO (Channel Int)
mkFibNetwork = do
  outCh <- channel
  c1 <- channel
  c2 <- channel
  c3 <- channel
  c4 <- channel
  c5 <- channel
  spawn $ delay (Just 0) c4 c5
  spawn $ copy c2 c3 c4
  spawn $ add c3 c5 c1
  spawn $ copy c1 c2 outCh
  spawn $ sendSync c1 1
  return outCh

test = do
  outCh <- mkFibNetwork
  foo outCh 1
  where
    foo outCh i = do
      x <- receiveSync outCh
      putStrLn $ "Fibonacci Term  " ++ (show i) ++ " - "++ show x
      foo outCh (i + 1)




-- Unit Tests

-- testing add
r = do
  c1 <- channel
  c2 <- channel
  c3 <- channel
  spawn $ add c1 c2 c3
  foo c1 c2 c3 5 10
  where
    foo c1 c2 c3 i1 i2 = do
      spawn $ sendSync c1 i1
      spawn $ sendSync c2 i2
      val <- receiveSync c3
      putStrLn $ "Adding  " ++ (show i1) ++ " and " ++ (show i2) ++ " = " ++ show val
      foo c1 c2 c3 (i1 + 1) (i2 + 2)

-- testing delay
m = do
  c1 <- channel
  c2 <- channel
  spawn $ delay (Just 0) c1 c2 -- Why moving spawnNoTID to delay gives non deterministic results
  foo c1 c2 1
  where
    foo c1 c2 i = do
      spawn $ sendSync c1 i
      r <- receiveSync c2
      putStrLn $ "Step - " ++ show i ++ "; Value - " ++ show r
      foo c1 c2 (i + 1)

-- testing copy
z = do
  c1 <- channel
  c2 <- channel
  c3 <- channel
  spawn $ copy c1 c2 c3
  foo c1 c2 c3 1
  where
    foo c1 c2 c3 i = do
      spawn $ sendSync c1 i
      a <- receiveSync c2
      b <- receiveSync c3
      putStrLn $ "Received " ++ show a ++ " and " ++ show b
      foo c1 c2 c3 (i + 1)





-------------------------------------------------------------------------



-- SML/NJ API

forever' :: IO a -> (a -> IO a) -> IO ()
forever' init f =
  let loop s = loop (s >>= f)
   in void $ spawn $ void (loop init)

add' :: Channel Int -> Channel Int -> Channel Int -> IO ()
add' inCh1 inCh2 outCh = forever' (return ()) $ (\() -> do
  v1 <- receiveSync inCh1
  v2 <- receiveSync inCh2
  sendSync outCh (v1 + v2))

delay' :: Maybe Int
       -> Channel Int
       -> Channel Int
       -> IO ()
delay' init inCh outCh =
  forever' (return init) (\v -> case v of
                                  Nothing -> do
                                    val <- receiveSync inCh
                                    return $ Just val
                                  Just val -> do
                                    sendSync outCh val
                                    return Nothing
                         )

copy' :: Channel Int -> Channel Int -> Channel Int -> IO ()
copy' inCh outCh1 outCh2 = forever' (return ()) $ (\() -> do
  v <- receiveSync inCh
  sendSync outCh1 v
  sendSync outCh2 v)

mkFibNetwork' :: IO (Channel Int)
mkFibNetwork' = do
  outCh <- channel
  c1 <- channel
  c2 <- channel
  c3 <- channel
  c4 <- channel
  c5 <- channel
  delay' (Just 0) c4 c5
  copy' c2 c3 c4
  add' c3 c5 c1
  copy' c1 c2 outCh
  sendSync c1 1
  return outCh

-- XXX: Leaks memory
-- test = do
--   outputChannel <- mkFibNetwork
--   val <- receiveSync outputChannel
--   putStrLn $ "abhi " ++ show val

------------------------------------------------------------------------

foo = do
  c1 <- channel
  c2 <- channel
  delay' (Just 0) c1 c2
  return (c1, c2)

baz = do
  (c1, c2) <- foo
  val <- receiveSync c2
  putStrLn $ "abhi" ++ (show val)

