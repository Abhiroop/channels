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

-- Step 1 of asynchrony
-- Do multiple tasks asynchronously
-- But one process. It will select and block
-- in certain places
-- If we introduce multiple processes there
-- are hidden race condtions in takeCookerC
-- to begin with plus all other constrained
-- resources will have races.
wholeOrder :: Kitchen Serve
wholeOrder = do
  uncookedrice <- takeRice
  cookerChan <- S.liftIO channel
  takeCookerC cookerChan -- async?
  cookStateChan <- S.liftIO channel
  riceToCookerC uncookedrice cookerChan cookStateChan -- async
  -- ^ may silently throw error if Rice state anything
  -- apart from UNCOOKED

  -- Step 1 takeRice

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
  -- Call takeFreshPlate if success goto step 7
  -- else goto step 5(put dirty plates in washer).
  -- Also check if washer has clean plates already
  -- if washer INUSE plus `takeFreshPlate`

  -- Step 5
  -- clean dirty plates (fresh plates exhausted)
  -- check if PlateWasher is INUSE if not send
  -- a dirty plate and current state of the washer
  -- to `cleanPlate`

  -- Step 6
  -- take plate out of washer - "takeOutPlates"
  -- if you get plates out; take one plate and update
  -- the state and proceed to Step 7.
  -- If washer in use plus no fresh plates goto step 8

  -- Step 7
  -- `takeFish`

  -- Step 8.1
  -- Try taking a chopping board `takeChoppingBoard`

  -- Step 8.2
  -- Take fish from step 7 and chopping board from step8
  -- and start chopping. listen asynchronous for the end
  -- of chop time. if you didn't succeed might need to wait.

  -- Step 9
  -- Approach the board and take the fish out. and hopefully
  -- get cut fish. If it is whole go back to Step 8.1

  -- Step 10
  -- Take output of Step 3,6,9; Combined they constitute an order

  -- return
  undefined

takeRice :: Kitchen Rice
takeRice = do
  r <- S.gets rice
  updateRice (tail r)
  return (head r)

-- this is strange because we are accessing a global state
-- without using channels
takeCookerC :: Channel RiceCooker -> Kitchen ()
takeCookerC cookerChan = foreverK $ do
  cookers <- S.gets cookers -- separate get and update will lead to races
  case length cookers of
    0 -> return ()
    _ -> do
      updateCookers (tail cookers)
      asyncSpawn $ sendSync cookerChan (head cookers)


takeCooker :: Kitchen (Maybe RiceCooker)
takeCooker = do
  cookers <- S.gets cookers
  case length cookers of
    0 -> return Nothing
    _ -> do -- cooker available; take one and modify state
      updateCookers (tail cookers)
      return $ Just (head cookers)



riceToCookerC :: Rice
              -> Channel RiceCooker
              -> Channel CookerState
              -> Kitchen ()
riceToCookerC (Rice UNCOOKED) cookerChan cookStateChan
  = foreverK $ asyncSpawn $ do
  _ <- S.liftIO $ receiveSync cookerChan
  S.liftIO (riceCookBurnCycle cookStateChan)
    where
      riceCookBurnCycle c = do
        threadDelay (riceCookingTime * onesec)
        sendSync c COOKDONE
        threadDelay (cookerBufferTime * onesec)
        sendSync c FIRE
riceToCookerC _ _ _ = return () -- anything apart from uncooked rice




riceToCooker :: Rice
             -> RiceCooker
             -> Kitchen (Maybe (Channel CookerState))
riceToCooker (Rice UNCOOKED) (RiceCooker VACANT) = do
  ch <- S.liftIO channel
  asyncSpawn (riceCookBurnCycle ch)
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



-- takeOutRice :: RiceCooker -> Kitchen (Maybe Rice)
-- takeOutRice rc =
--   case cookerState rc of
--     VACANT  -> return Nothing
--     COOKING -> do
--       putVacantCookerBack
--       return $ Just (Rice UNCOOKED)
--     COOKDONE -> do
--       putVacantCookerBack
--       return $ Just (Rice COOKED)
--     FIRE -> return Nothing

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
      asyncSpawn (startCleaning c) -- this call should instantly return
      updatePlateWasher (PlateWasher INUSE (p:ps))
      return (Just c)
  | otherwise = return Nothing -- status == INUSE
  where
    startCleaning ch = do
      threadDelay (plateCleanTime * onesec)
      sendSync ch DONE

cleanPlate (Plate CLEAN)  _ = return Nothing


takeOutPlates :: PlateWasher -> Kitchen (Maybe [Plate])
takeOutPlates (PlateWasher STOPPED ps) = do
  emptyWasher
  return $ Just ps
takeOutPlates (PlateWasher DONE ps) = do
  emptyWasher
  return $ Just ps
takeOutPlates (PlateWasher INUSE _) = return Nothing

takeFish :: Kitchen Fish
takeFish = do
  fs <- S.gets fishes
  updateFishes (tail fs)
  return (head fs)

takeChoppingBoardC :: Kitchen (Channel ChoppingBoard)
takeChoppingBoardC = do
  c <- S.liftIO channel
  bs <- S.gets boards
  case length bs of
    0 -> return c
    _ -> do
      updateBoards (tail bs)
      asyncSpawn $ sendSync c (head bs)
      return c

takeChoppingBoard :: Kitchen (Maybe ChoppingBoard)
takeChoppingBoard = do
  bs <- S.gets boards
  case length bs of
    0 -> return Nothing
    _ -> do -- board available; take one and modify state
      updateBoards (tail bs)
      return $ Just (head bs)

fishToBoard :: Fish
            -> ChoppingBoard
            -> Kitchen (Maybe (Channel BoardState))
fishToBoard (Fish WHOLE) (ChoppingBoard EMPTY) = do
  ch <- S.liftIO channel
  asyncSpawn (fishCutCycle ch)
  return (Just ch)
    where
      fishCutCycle c = do
        threadDelay (fishCuttingTime * onesec)
        sendSync c CUTDONE
fishToBoard _ _ = return Nothing -- all other states won't
                                  -- result in a channel

takeCutFish :: ChoppingBoard -> Kitchen (Maybe Fish)
takeCutFish cb =
  case boardState cb of
    EMPTY  -> return Nothing
    CUTTING -> do
      putBoardBack
      return $ Just (Fish WHOLE)
    CUTDONE -> do
      putBoardBack
      return $ Just (Fish CUT)



-- Helpers --

asyncSpawn :: IO () -> Kitchen ()
asyncSpawn = S.liftIO . spawnNoTID

foreverK :: Kitchen () -> Kitchen ()
foreverK action = do
  action
  foreverK action


putVacantCookerBack :: Kitchen ()
putVacantCookerBack = do
  cookers <- S.gets cookers
  updateCookers (RiceCooker VACANT : cookers)

putBoardBack :: Kitchen ()
putBoardBack = do
  bs <- S.gets boards
  updateBoards (ChoppingBoard EMPTY : bs)

emptyWasher :: Kitchen ()
emptyWasher =
  updatePlateWasher (PlateWasher STOPPED [])

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
