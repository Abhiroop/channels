{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Kitchen where


import CML.Utils
import Control.Concurrent (threadDelay)
import Control.Concurrent.BoundedChan
import Control.Concurrent.Chan
import Control.Concurrent.CML
import Control.Monad.Trans.State.Strict -- transformers

import qualified Control.Monad.State.Strict as S -- mtl

{- Robot Kitchen

An automated kitchen operated by a robot and equipped with sensors.

Task
----
You get orders in a queue. All orders have a designated expire time. After that expire time
you do not get any points for that order. If responded within the designated expire time you
get 10 points. Your goal is to maximize the number of points while using the least number of
robots (we start with 1).

A robot is capable of
1. Taking rice
2. Putting rice on a cooker
3. Taking the rice out when it is cooked
4. Fetching plates
5. Clean dirty plates
6. Putting cooked rice on a plate.
7. Taking fish
8. Cutting it
9. Put a cut fish on the plate (no need to cook it)
10. Serve a plate with cut fish and cooked rice to the customer


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
3. Dirty plates counter (queue)
4. Order queue

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
-}

-- Limited Resources
newtype Plate =
  Plate { plateState :: PlateState }
  deriving (Show, Ord, Eq)

data PlateState = DIRTY | CLEAN deriving (Show, Ord, Eq)


newtype ChoppingBoard =
  ChoppingBoard {boardState :: BoardState}
  deriving (Show, Ord, Eq)

data BoardState = EMPTY | CUTTING | CUTDONE deriving (Show, Ord, Eq)


newtype RiceCooker =
  RiceCooker {cookerState :: CookerState}
  deriving (Show, Ord, Eq)

data CookerState = VACANT | COOKING | COOKDONE | FIRE deriving (Show, Ord, Eq)
--                                       ^
--                                       |
--                           if the cooker is in this state
--                           for more than `cookerBufferTime`
--                           it catches fire

data PlateWasher =
  PlateWasher { washerState :: WasherState
              , platesInside :: [Plate]
              }
  deriving (Show, Ord, Eq)

data WasherState = STOPPED | INUSE | DONE deriving (Show, Ord, Eq)
-- We have simplified the plate washer
-- state considerably. It is large enough
-- that it can handle all 5 plates. Also
-- if it is loaded with few plates and you
-- put a new one it isn't an inconsistent state.



-- Unlimited Resources
newtype Fish =
  Fish { fishState :: FishState }
  deriving (Show, Ord, Eq)

data FishState = CUT | WHOLE deriving (Show, Ord, Eq)


newtype Rice =
  Rice { riceState :: RiceState } -- denotes 1 serving
  deriving (Show, Ord, Eq)

data RiceState = UNCOOKED | COOKED | BURNT deriving (Show, Ord, Eq)

data SoapWater = SoapWater deriving (Show, Ord, Eq) -- indicates 1 plate worth soap water

-- constants
riceCookingTime  = 15 -- seconds
cookerBufferTime = 10 -- seconds
plateCleanTime   =  8 -- seconds
fishCuttingTime  =  3 -- seconds
onesec = 1000000 -- microseconds
-- Other actions are considered instantenous

data KitchenState = KitchenState { freshplates :: [Plate] -- 5 plates
                                 , boards  :: [ChoppingBoard]
                                 , cookers :: [RiceCooker]
                                 , fishes  :: [Fish]
                                 , rice :: [Rice]
                                 , soap :: [SoapWater]
                                 , plateWasher :: PlateWasher
                                 }

initKitchen :: KitchenState
initKitchen =
  KitchenState { freshplates = replicate 5 (Plate CLEAN)
               , boards  = replicate 2 (ChoppingBoard EMPTY)
               , cookers = replicate 2 (RiceCooker VACANT)
               , fishes  = repeat (Fish WHOLE)
               , rice    = repeat (Rice UNCOOKED)
               , soap    = repeat SoapWater
               , plateWasher = PlateWasher STOPPED []
               }

newtype Kitchen a =
  Kitchen
    { runKitchen :: StateT KitchenState IO a
    }
  deriving (Functor, Applicative, Monad, S.MonadIO, S.MonadState KitchenState)


-- Input and output to system
-- Dirty plates are also inputted to the system

data Order =
  Order { waitTime :: Int
        , orderId  :: Int
        } deriving (Ord, Show, Eq)

data Serve =
  Serve { servedRice :: Rice
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


--                    Bounded, buffered channels
--                              | | |
--                              | | |
--                              | | |
cook :: BoundedChan Order  -- <-  | |
     -> BoundedChan Plate  -- <---  |
     -> BoundedChan Serve  -- <-----
     -> Kitchen ()
cook orderQ plateQ serviceQ = do
  cook orderQ plateQ serviceQ

wholeOrder :: Kitchen Serve
wholeOrder = do
  r <- takeRice -- Step 1

  -- Step 2.1
  -- choose empty pressure cooker - takeCooker
  -- check rice state, cooker state
  -- then call riceToCooker
  -- if busy handle other tasks

  -- Step 2.2
  -- receiveSync on channel from Step 2.1
  -- but receive asynchronously

  -- Step 3
  -- approach the cooker from Step 2.1
  -- call takeOutRice and hopefully get
  -- cooked rice;(cooker in KitchenState
  -- is update inside takeOutRice)
  -- If rice is uncooked take goto Step 2.1

  -- Step 4
  -- Call takeFreshPlate if success goto step 6
  -- else goto step 5(clean dirty plates).
  -- if both 5 and 6 unsuccessful proceed to
  -- fish cutting Step 7

  -- Step 5
  -- clean dirty plates (fresh plates exhausted)
  -- check if PlateWasher is INUSE if not send
  -- a dirty plate and current state of the washer
  -- to `cleanPlate`

  undefined

takeRice :: Kitchen Rice
takeRice = do
  r <- S.gets rice
  updateRice (tail r)
  return (head r)

takeCooker :: Kitchen (Maybe RiceCooker)
takeCooker = do
  cookers <- S.gets cookers
  case length cookers of
    0 -> return Nothing
    _ -> do -- cooker available; take one and modify state
      updateCookers (tail cookers)
      return $ Just (head cookers)

riceToCooker :: Rice
             -> RiceCooker
             -> Kitchen (Maybe (Channel CookerState))
riceToCooker (Rice UNCOOKED) (RiceCooker VACANT) = do
  ch <- S.liftIO channel
  S.liftIO $ spawnNoTID (riceCookBurnCycle ch)
  return (Just ch)
    where
      riceCookBurnCycle c = do
        threadDelay (riceCookingTime * onesec)
        sendSync c COOKDONE
        threadDelay (cookerBufferTime * onesec)
        sendSync c FIRE
riceToCooker _ _ = return Nothing -- all other states have
                                  -- something wrong with
                                  -- the rice or the cooker

takeOutRice :: RiceCooker -> Kitchen (Maybe Rice)
takeOutRice rc =
  case cookerState rc of
    VACANT  -> return Nothing
    COOKING -> do
      putVacantCookerBack
      return $ Just (Rice UNCOOKED)
    COOKDONE -> do
      putVacantCookerBack
      return $ Just (Rice COOKED)
    FIRE -> return Nothing

takeFreshPlate :: Kitchen (Maybe Plate)
takeFreshPlate = do
  ps <- S.gets freshplates
  case length ps of
    0 -> return Nothing
    _ -> do -- plate available; take one and modify state
      updatePlates (tail ps)
      return $ Just (head ps)

cleanPlate :: Plate
           -> PlateWasher
           -> Kitchen (Maybe (Channel WasherState))
cleanPlate p@(Plate DIRTY) (PlateWasher status ps)
  | status == STOPPED || status == DONE = do
      c <- S.liftIO channel
      S.liftIO $ spawnNoTID (startCleaning c) -- this call should instantly return
      updatePlateWasher (PlateWasher INUSE (p:ps))
      return (Just c)
  | otherwise = return Nothing -- status == INUSE
  where
    startCleaning ch = do
      threadDelay (plateCleanTime * onesec)
      sendSync ch DONE

cleanPlate (Plate CLEAN)  _ = return Nothing


-- Helpers --
putVacantCookerBack :: Kitchen ()
putVacantCookerBack = do
  cookers <- S.gets cookers
  updateCookers (RiceCooker VACANT : cookers)

updatePlates :: [Plate] -> Kitchen ()
updatePlates p =
  S.modify $ \s -> s { freshplates = p }

updateBoards :: [ChoppingBoard] -> Kitchen ()
updateBoards b =
  S.modify $ \s -> s { boards = b }

updateCookers :: [RiceCooker] -> Kitchen ()
updateCookers c =
  S.modify $ \s -> s { cookers = c }

updateFishes :: [Fish] -> Kitchen ()
updateFishes f =
  S.modify $ \s -> s { fishes = f }

updateRice :: [Rice] -> Kitchen ()
updateRice r =
  S.modify $ \s -> s { rice = r }

updateSoapWater :: [SoapWater] -> Kitchen ()
updateSoapWater sw =
  S.modify $ \s -> s { soap = sw }

updatePlateWasher :: PlateWasher -> Kitchen ()
updatePlateWasher pw =
  S.modify $ \s -> s { plateWasher = pw }
