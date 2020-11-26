module Fibselect where

import Control.Concurrent.CML
import Control.Monad (void)
import CML.Utils
import Data.Maybe


add :: Channel Int -> Channel Int -> Channel Int -> IO ()
add inCh1 inCh2 outCh = forever $ do
  (a,b) <- selectSync [ wrap (receive inCh1 dontcare) (handler1 inCh2)
                      , wrap (receive inCh2 dontcare) (handler2 inCh1)
                      ]
  sendSync outCh (a + b)
  where
    handler1 channel m1 = do
      m2 <- receiveSync channel
      return (m1, m2)
    handler2 channel m2 = do
      m1 <- receiveSync channel
      return (m1, m2)


-- In delay the size of the state or "buffer" (in Lustre speak) is
-- bounded by the size of the Maybe Int. So either Nothing or Just Int
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
  selectSync [ wrap (transmit outCh1 v) (\_ -> sendSync outCh2 v)
             , wrap (transmit outCh2 v) (\_ -> sendSync outCh1 v)
             ]

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



------------------------------------------------------------------------------------




----------------------------Incorrect delay implementations-------------------------
-- push based delay
-- doesn't work; throws exception
-- Incorrect; we shouldn't select based on actions here
-- we should select based on the value of init
delay' :: Maybe Int
       -> Channel Int
       -> Channel Int
       -> IO ()
delay' init inCh outCh = forever $ do
  let val = fromMaybe (error "error value doesn't exist") init
  selectSync [ wrap (receive inCh dontcare) (\v -> delay' (Just v) inCh outCh)
             , wrap (transmit outCh val) (\_ -> delay' Nothing inCh outCh)
             ]





-- gives strange results
delay'' :: Int
        -> Channel Int
        -> Channel Int
        -> IO ()
delay'' init inCh outCh = forever $ do
  selectSync [ wrap (receive inCh dontcare) (\v -> delay'' v inCh outCh)
             , wrap (transmit outCh init) (\_ -> delay'' init inCh outCh)
             ]

m = do
  c1 <- channel
  c2 <- channel
  spawn $ delay(Just 0) c1 c2 -- Why moving spawnNoTID to delay gives non deterministic results
  foo c1 c2 1
  where
    foo c1 c2 i = do
      spawn $ sendSync c1 i
      r <- receiveSync c2
      putStrLn $ "Step - " ++ show i ++ "; Value - " ++ show r
      foo c1 c2 (i + 1)
