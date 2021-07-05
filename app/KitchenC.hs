{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module KitchenC where


import CML.Utils
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.BoundedChan
import Control.Concurrent.CML
import Control.Concurrent.MVar
import System.CPUTime
{- Robot Kitchen

An automated kitchen operated by a robot and equipped with sensors.

Task
----
You get orders in a queue. All orders have a designated expire time. After that expire time
you do not get any points for that order. If responded within the designated expire time you
get 10 points otherwise you get -10. Your goal is to maximize the number of points while
using the least number of robots (we start with 1).

A robot is capable of
1. Taking rice
2. Putting rice on a cooker
3. Taking the rice out when it is cooked
4. Fetching plates
5. Put dirty plates in plate washer
6. Taking plate(s) out of washer once it is cleaned
7. Taking fish
8. Cutting it
9. Taking the cut fish from the board(and restoring the board)
10. Putting {cooked rice}(Step 3) and {cut fish} (Step 9) and put on a {plate}(Step - 4/6).
11. Serve a plate with cut fish and cooked rice to the customer


The resources with limited availability are:

1. The fish chopping board = 2
2. The number of plates    = 5
3. The number of rice cookers = 2

Resources with unlimited availability (To simplify things)
1. Fish
2. Rice
3. Soap and water

Timed activities
1. A batch of rice has a *designated cooking time*.
   It then allows a small buffer period after which it catches fire.
   The kitchen should never catch fire.
2. Orders have wait times after which they expire and you don't get points.

There are sensors located on
1. Rice cookers
2. Plate washer
3. Chopping Boards
4. Dirty plates counter (queue)
5. Order queue

Constraints:
- Currently we have one robot which maps to a single processor.

- The edges of the system should have buffers but bounded buffers which
  block after it reaches capacity


** Not covered **
- Parallelism challenges
  What happens when we have 2 robots or 2 processors or more? How
  much does our program change? Can we write one program and have
  it deploy to two separate robots?

** Simplifications **
1. all sensors never drop messages
2. all connections are never broken.
3. no retry, connection timeouts etc
4. real world APIs are written in C; and always callback based; a wrapper
   is needed which translates the program's model to whatever protocol the
   C function on the sensor expects like BLE etc. This is a challenge of
   the IO interface.
5. if rice is half cooked and taken out of the cooker and then put back
   again, cooking will start from scratch
6. There is no conept of a kitchen table or having a limited number of items
   you can put in your hands. You can simply store things in variables in
   a given cycle. Once an order is done you can store in the BoundedChan as
   well but you should stall and wait if the BoundedChan is full and that is
   intentional. We don't want memory growth to be unbounded.
-}

type Points = Int
-- Limited Resources
newtype Plate =
  Plate { plateState :: PlateState }
  deriving (Show, Ord, Eq)

data PlateState = DIRTY | CLEAN deriving (Show, Ord, Eq)


newtype ChoppingBoard =
  ChoppingBoard {boardState :: BoardState}
  deriving (Show, Ord, Eq)

data BoardState = EMPTY | CUTDONE deriving (Show, Ord, Eq)


newtype RiceCooker =
  RiceCooker {cookerState :: CookerState}
  deriving (Show, Ord, Eq)

data CookerState = VACANT | COOKDONE | FIRE deriving (Show, Ord, Eq)
--                             ^
--                             |
--                    if the cooker is in this state
--                    for more than `cookerBufferTime`
--                    it catches fire

newtype PlateWasher =
  PlateWasher { platesInside :: [Plate] }
  deriving (Show, Ord, Eq)

-- Unlimited Resources
newtype Fish =
  Fish { fishState :: FishState }
  deriving (Show, Ord, Eq)

data FishState = CUT | WHOLE deriving (Show, Ord, Eq)


newtype Rice =
  Rice { riceState :: RiceState } -- denotes 1 serving
  deriving (Show, Ord, Eq)

data RiceState = UNCOOKED | COOKED | BURNT deriving (Show, Ord, Eq)


-- constants
riceCookingTime  = 15 -- seconds
cookerBufferTime = 10 -- seconds
plateCleanTime   =  8 -- seconds
fishCuttingTime  =  3 -- seconds
onesec = 1000000 -- microseconds
-- Other actions are considered instantenous


-- Input and output to system
-- Dirty plates are also inputted to the system

data Order =
  Order { waitTime :: Double
        , orderId  :: Int
        } deriving (Ord, Show, Eq)

data Serve =
  Serve { servingPlate :: Plate
        , servedRice :: Rice
        , servedFish :: Fish
        , serviceId  :: Int -- should match order id
        } deriving (Ord, Show, Eq)

verifyServe :: Serve -> Bool
verifyServe s =
  case  (riceStatus,fishStatus) of
    (COOKED, CUT) -> True
    _             -> False
  where
    (riceStatus,fishStatus) = (riceState $ servedRice s, fishState $ servedFish s)

-- global unconstrained resources
fishes :: [Fish]
fishes = repeat (Fish WHOLE)

rice :: [Rice]
rice = repeat (Rice UNCOOKED)

bandwidth :: Int
bandwidth = 5
--                    Bounded, buffered channels
--                              | | |
--                              | | |
--                              | | |
cook :: BoundedChan Order  -- <-  | |
     -> BoundedChan Plate  -- <---  |
     -> BoundedChan Serve  -- <-----
     -> Points
     -> IO ()
cook orderQ plateQ serviceQ points = do
  dirtyPlateChan <- channel
  mvars <- sequence $ replicate bandwidth newEmptyMVar
  ps <- cook' mvars dirtyPlateChan []
  cook orderQ plateQ serviceQ (points + sum ps)
  where
    cook' [] _ waitlist = mapM takeMVar waitlist
    cook' (mvar:restmvars) dpChan waitlist = do
      order <- readChan orderQ -- intentional blocking
      forkIO (dirtyplatehandler dpChan)
      forkIO (orderhandler order dpChan mvar)
      cook' restmvars dpChan (mvar : waitlist)
    dirtyplatehandler dpChan = do
      dirtyplate <- readChan plateQ
      sendSync dpChan dirtyplate
      dirtyplatehandler dpChan
    orderhandler order dpChan mvar = do
      serve <- timedOrder order dpChan
      total <-
        case serve of
          Nothing -> return $ points - 10
          Just s  ->
            if verifyServe s
            then do
              writeChan serviceQ s -- deliberate blocking on main thread
              return $ points + 10
            else return $ points - 10
      putMVar mvar total


timedOrder :: Order
           -> Channel Plate
           -> IO (Maybe Serve)
timedOrder o@(Order waittime _) dpChan = do
  start <- getCPUTime
  v <- wholeOrder o dpChan
  end   <- getCPUTime
  let diff = (fromIntegral (end - start)) / (10^12) :: Double
  if diff > waittime
  then return Nothing
  else return $ Just v

wholeOrder :: Order
           -> Channel Plate
           -> IO Serve
wholeOrder (Order _ orderid) dirtyPlateChan = do
  let uncookedrice = head rice
  cookerInChan  <- channel
  cookerOutChan <- channel
  asyncSpawn $ cooker initCookers cookerInChan cookerOutChan
  cookStateChan <- channel
  asyncSpawn $ cookrice uncookedrice cookerOutChan cookStateChan
  -- ^ might silently throw error if Rice state is not UNCOOKED
  -- and cause deadlock but practically shouldn't happen
  cookedriceChan <- channel
  asyncSpawn $ riceReady cookStateChan cookerInChan cookedriceChan


  platesInChan <- channel
  plateOutChan <- channel
  asyncSpawn $ plates initPlates platesInChan plateOutChan
  asyncSpawn $ washer initWasher dirtyPlateChan platesInChan


  let uncutfish = head fishes
  boardInChan  <- channel
  boardOutChan <- channel
  asyncSpawn $ choppingBoard initBoards boardInChan boardOutChan
  boardStateChan <- channel
  asyncSpawn $ cutfish uncutfish boardOutChan boardStateChan
  -- ^ might silently throw exception if fish is not WHOLE
  -- and cause deadlock but practically shouldn't happen
  cutfishChan <- channel
  asyncSpawn $ fishready boardStateChan boardInChan cutfishChan

  -- channels needed for order
  cookedrice   <- receiveSync cookedriceChan -- Priority 1; might cause FIRE
  servingplate <- receiveSync plateOutChan
  cutfish <- receiveSync cutfishChan

  return $ Serve servingplate cookedrice cutfish orderid
  where
    initCookers = replicate 2 (RiceCooker VACANT)
    initPlates  = replicate 5 (Plate CLEAN)
    initBoards  = replicate 2 (ChoppingBoard EMPTY)
    initWasher  = PlateWasher []


cooker :: [RiceCooker]
        -> Channel RiceCooker
        -> Channel RiceCooker
        -> IO ()
cooker cs inCh outCh = forever $ do
  if length cs == 0
  then do
    c <- receiveSync inCh
    cooker [c] inCh outCh
  else do
    selectSync [ wrap (receive inCh dontcare) recvhandler
               , wrap (transmit outCh (head cs)) sendhandler
               ]
  where
    recvhandler mr = cooker (mr : cs) inCh outCh
    sendhandler _  = cooker (tail cs) inCh outCh

cookrice :: Rice
         -> Channel RiceCooker
         -> Channel CookerState
         -> IO ()
cookrice (Rice UNCOOKED) cookerChan cookStateChan
  = forever $ do
  _ <- receiveSync cookerChan -- blocks
  riceCookBurnCycle cookStateChan
    where
      riceCookBurnCycle c = do
        threadDelay (riceCookingTime * onesec) -- blocks
        timedsend timeout c COOKDONE timeoutaction
        where
          timeout = cookerBufferTime * onesec
          timeoutaction = sendSync c FIRE

      -- riceCookBurnCycle c = do
      --   threadDelay (riceCookingTime * onesec) -- blocks
      --   sendSync c COOKDONE
      --   threadDelay (cookerBufferTime * onesec)
      --   sendSync c FIRE

      -- riceCookBurnCycle c = do
      --   threadDelay (riceCookingTime * onesec) -- blocks
      --   asyncSpawn $ do
      --     threadDelay (cookerBufferTime * onesec)
      --     sendSync c FIRE
      --   sendSync c COOKDONE

cookrice _ _ _ = return () -- anything apart from uncooked rice


riceReady :: Channel CookerState
          -> Channel RiceCooker
          -> Channel Rice
          -> IO ()
riceReady inCh outCh1 outCh2 = forever $ do
  cs <- receiveSync inCh
  case cs of
    VACANT   -> return () -- none sends this message
    COOKDONE ->
      selectSync [ wrap (transmit outCh1 (RiceCooker VACANT)) sendhandler1
                 , wrap (transmit outCh2 (Rice COOKED)) sendvacant
                 ]
    FIRE ->
      selectSync [ wrap (transmit outCh1 (RiceCooker VACANT)) sendhandler2
                 , wrap (transmit outCh2 (Rice BURNT)) sendvacant
                 ]
  where
    sendhandler1 _ = sendSync outCh2 (Rice COOKED)
    sendhandler2 _ = sendSync outCh2 (Rice BURNT)
    sendvacant _   = sendSync outCh1 (RiceCooker VACANT)


plates :: [Plate]
       -> Channel [Plate]
       -> Channel Plate
       -> IO ()
plates ps inCh outCh = forever $ do
  if length ps == 0
  then do
    pWasher <- receiveSync inCh
    plates pWasher inCh outCh
  else do
    sendSync outCh (head ps)
    plates (tail ps) inCh outCh

-- plates :: Maybe [Plate]
--        -> Channel [Plate]
--        -> Channel Plate
--        -> IO ()
-- plates ps inCh outCh = forever $ do
--   case ps of
--     Nothing -> do
--       pWasher <- receiveSync inCh
--       plates (Just pWasher) inCh outCh
--     Just p -> do
--       selectSync [ wrap (receive inCh dontcare) recvhandler
--                  , wrap (transmit outCh (head p)) sendhandler
--                  ]
--   where
--     recvhandler mr = do
--       case ps of
--         Nothing -> error "Impossible case"
--         Just _ -> plates (Just mr) inCh outCh
--     sendhandler _ = do
--       case ps of
--         Nothing -> error "Impossible case"
--         Just p -> case length p of
--                     0 -> plates Nothing inCh outCh
--                     _ -> plates (Just $ tail p) inCh outCh


-- starts only when all 5 plates are loaded
washer :: PlateWasher
       -> Channel Plate -- DIRTY plates
       -> Channel [Plate] -- clean plates
       -> IO ()
washer (PlateWasher ps) inCh outCh = forever $ do
  if length ps < 5
  then do
    p <- receiveSync inCh
    washer (PlateWasher (p:ps)) inCh outCh
  else do
    threadDelay (plateCleanTime * onesec)
    sendSync outCh (replicate 5 (Plate CLEAN))



choppingBoard :: [ChoppingBoard]
              -> Channel ChoppingBoard
              -> Channel ChoppingBoard
              -> IO ()
choppingBoard boards inCh outCh = forever $ do
  if length boards == 0
  then do
    b <- receiveSync inCh
    choppingBoard [b] inCh outCh
  else do
    selectSync [ wrap (receive inCh dontcare) recvhandler
               , wrap (transmit outCh (head boards)) sendhandler
               ]
  where
    recvhandler mr = choppingBoard (mr : boards) inCh outCh
    sendhandler _  = choppingBoard (tail boards) inCh outCh

cutfish :: Fish
        -> Channel ChoppingBoard
        -> Channel BoardState
        -> IO ()
cutfish (Fish WHOLE) inCh outCh = forever $ do
  _ <- receiveSync inCh
  fishCutCycle outCh
    where
      fishCutCycle c = do
        threadDelay (fishCuttingTime * onesec)
        sendSync c CUTDONE
cutfish _ _ _ = return ()

fishready :: Channel BoardState
          -> Channel ChoppingBoard
          -> Channel Fish
          -> IO ()
fishready inCh outCh1 outCh2 = do
  bs <- receiveSync inCh
  case bs of
    EMPTY   -> return () -- none sends this message
    CUTDONE ->
      selectSync [ wrap (transmit outCh1 (ChoppingBoard EMPTY)) sendhandler1
                 , wrap (transmit outCh2 (Fish CUT)) sendhandler2
                 ]
  where
    sendhandler1 _ = sendSync outCh2 (Fish CUT)
    sendhandler2 _ = sendSync outCh1 (ChoppingBoard EMPTY)



asyncSpawn = spawnNoTID

test :: IO ()
test = do
  f <- newBoundedChan 1
  g <- newBoundedChan 1
  h <- newBoundedChan 1
  forkIO $ cook f g h 0
  forkIO $ writeChan f (Order 25.0 1)
  s <- readChan h
  putStrLn $ "abhi " ++ show s
