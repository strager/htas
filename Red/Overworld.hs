module Red.Overworld where

import Control.Monad (unless, when, replicateM_)
import Data.Bits ((.&.), testBit)
import Data.IORef
import Data.Word
import HTas.Low
import HTas.Direct (GB)

wFlags_D733 = 0xD732

wYCoord = 0xD360
wXCoord = 0xD361

wCurMap = 0xD35D

wWalkCounter = 0xCFC4

wIsInBattle = 0xD056

hJoyPressed = 0xFFB3

data Location = Location
    { locMap :: Word8
    , locX :: Word8
    , locY :: Word8
    } deriving (Eq, Show, Ord)

getLocation :: GB -> IO Location
getLocation gb = do
    map <- cpuRead gb wCurMap
    x <- cpuRead gb wXCoord
    y <- cpuRead gb wYCoord
    pure $ Location
        { locMap = map
        , locX = x
        , locY = y
        }

trainerEncounter :: GB -> IO Bool
trainerEncounter gb = do
    flags <- cpuRead gb wFlags_D733
    return (testBit flags 3)

bufferedStep :: GB -> IORef Input -> Input -> IO ()
bufferedStep gb inputRef input = bufferedWalk gb inputRef [input]

-- Walk with buffered inputs
-- Aborts if a battle starts
bufferedWalk :: GB -> IORef Input -> [Input] -> IO ()
bufferedWalk gb inRef inps =
    case inps of
        [] -> pure ()
        d:ds -> do
            inBattle <- cpuRead gb wIsInBattle
            if inBattle /= 0
            then pure ()
            else do
                -- TODO(strager): Stop if we open a menu
                -- (e.g. by talking to a sign).
                writeIORef inRef d
                waitForWalkStart gb d
                waitForStep gb d
                bufferedWalk gb inRef ds
    where
    waitForWalkStart gb input = do
        count <- cpuRead gb wWalkCounter
        inBattle <- cpuRead gb wIsInBattle
        bonked <- (== 0xB4) <$> cpuRead gb 0xC02A
        trainer <- trainerEncounter gb
        if count == 7 || inBattle /= 0 || bonked || trainer
        then pure ()
        else do
            advanceFrame gb
            waitForWalkStart gb input
    waitForStep gb input = do
        count <- cpuRead gb wWalkCounter
        inBattle <- cpuRead gb wIsInBattle
        bonked <- (== 0xB4) <$> cpuRead gb 0xC02A
        trainer <- trainerEncounter gb
        {-
        when (count == 0 && (input `hasAllInput` i_A)) $ do
          -- HACK(strager)
          writeIORef inRef mempty
          advanceFrame gb
          advanceFrame gb -}
        if count == 0 || inBattle /= 0 || bonked || trainer
        then pure ()
        else do
            advanceFrame gb
            waitForStep gb input

waitForItemJingle :: GB -> IO ()
waitForItemJingle gb = advanceUntil gb $ do
    jinglePlaying <- (== 0x86) <$> cpuRead gb 0xC02A
    inBattle <- cpuRead gb wIsInBattle
    pure $ jinglePlaying || inBattle /= 0
