{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
module Kitchen where



import CML.Utils
import Control.Concurrent.CML
import Control.Monad.Trans.State.Strict
import Prelude hiding (init)

import qualified Control.Monad.State.Strict as S

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
5. Putting cooked rice on a plate.
6. Taking fish
7. Cutting it
8. Put a cut fish on the plate (no need to cook it)
9. Serve a plate with cut fish and cooked rice to the customer
10. Clean dirty plates

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

There sensors located on
1. Rice cookers
2. Dirty plates counter
3. Order queue (although mostly you would have to poll it all the time)

Currently we have one robot which maps to a single processor.


** Not covered **
- Parallelism challenges
  What happens when we have 2 robots or 2 processors or more? How
  much does our program change? Can we write one program and have
  it deploy to two separate robots?

** Simplifications **
1. all sensor never drop messages
2. all connections are never broken.
3. no retry, timeouts
4. real world APIs are written in C; and always callback based; a wrapper
   is needed which translates the program's model to whatever protocol the
   C function on the sensor expects like BLE etc. This is a challenge of
   the IO interface.
-}

-- Limited Resources
newtype Plate =
  Plate { plateState :: PlateState }
  deriving (Show, Ord, Eq)

data PlateState = DIRTY | CLEAN deriving (Show, Ord, Eq)


newtype ChoppingBoard =
  ChoppingBoard {boardState :: BoardState}
  deriving (Show, Ord, Eq)

data BoardState = EMPTY | INUSE deriving (Show, Ord, Eq)


newtype RiceCooker =
  RiceCooker {cookerState :: CookerState}
  deriving (Show, Ord, Eq)

data CookerState = VACANT | COOKING | COOKDONE | FIRE deriving (Show, Ord, Eq)
--                                       ^
--                                       |
--                               if the cooker is in this state
--                               for more than `cookerBufferTime`
--                               it catches fire


-- Unlimited Resources
newtype Fish =
  Fish {fishState :: FishState}
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
plateCleanTime   = 10 -- seconds
sinkToCookerWalkTime = 3 -- seconds

-- Other actions are considered instantenous

data KitchenState = KitchenState { freshplates :: [Plate] -- 5 plates
                                 , boards  :: [ChoppingBoard]
                                 , cookers :: [RiceCooker]
                                 , fishes  :: [Fish]
                                 , rice :: [Rice]
                                 , soap :: [SoapWater]
                                 }

init :: KitchenState
init = KitchenState { freshplates = replicate 5 (Plate CLEAN)
                    , boards  = replicate 2 (ChoppingBoard EMPTY)
                    , cookers = replicate 2 (RiceCooker VACANT)
                    , fishes  = repeat (Fish WHOLE)
                    , rice    = repeat (Rice UNCOOKED)
                    , soap    = repeat SoapWater
                    }

newtype Kitchen a =
  Kitchen
    { runKitchen :: StateT KitchenState IO a
    }
  deriving (Functor, Applicative, Monad, S.MonadIO, S.MonadState KitchenState)


-- Input and output to system

newtype Order =
  Order {waitTime :: Int}
  deriving (Ord, Show, Eq)

data Serve = Serve deriving (Ord, Show, Eq)

cook :: Kitchen ()
cook = do
  f <- S.get
  S.liftIO $ putStrLn "Hello!"
