{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Swapchans where


import CML.Utils
import Control.Concurrent.CML

type SwapChan a = Channel a

swapChannel :: IO (SwapChan a)
swapChannel = do
  c <- channel
  return c

swap :: SwapChan a -> a -> Event a
swap c val = do
  choose [ wrap (transmit c val) sendhandler
         , wrap (receive c dontcare) recvhandler
         ]
    where
      recvhandler m = do
        sendSync c val
        pure m
      sendhandler _ = do
        m <- receiveSync c
        pure m


type SwapChan' a = Channel (a, Channel a)

swapChannel' = do
  c <- channel
  return c

swap' :: SwapChan' a -> a -> Event a
swap' c msgOut = do
  guard $ do
    inCh <- channel
    pure $
      choose [ wrap (receive c dontcare) recvhandler
             , wrap (transmit c (msgOut, inCh)) (sendhandler inCh)
             ]
    where
      recvhandler (msgIn, outCh) = do
        sendSync outCh msgOut
        pure msgIn

      sendhandler ich _ = do
        msgIn <- receiveSync ich
        pure msgIn

foo = do
  c1 <- channel
  c2 <- channel
  sc <- swapChannel'
  spawnNoTID (process1 sc c1 5)
  spawnNoTID (process2 sc c2 3)
  v1 <- receiveSync c1
  v2 <- receiveSync c2
  putStrLn $ show (v1,v2)

process1 sc ch v = do
  sv <- sync $ swap' sc v
  sendSync ch sv

process2 sc ch v = do
  sv <- sync $ swap' sc v
  sendSync ch sv
