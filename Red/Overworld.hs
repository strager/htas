module Red.Overworld where

import Control.Monad (unless, when, replicateM_)
import Data.Bits ((.&.))
import Data.IORef
import Data.Word
import HTas.Low
import HTas.Direct (GB)

wYCoord = 0xD361
wXCoord = 0xD362

wCurMap = 0xD35E

wWalkCounter = 0xCFC5

wIsInBattle = 0xD057

hJoyPressed = 0xFFB2

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
        if count == 7 || inBattle /= 0 || bonked
        then pure ()
        else do
            advanceFrame gb
            waitForWalkStart gb input
    waitForStep gb input = do
        count <- cpuRead gb wWalkCounter
        inBattle <- cpuRead gb wIsInBattle
        bonked <- (== 0xB4) <$> cpuRead gb 0xC02A
        {-
        when (count == 0 && (input `hasAllInput` i_A)) $ do
          -- HACK(strager)
          writeIORef inRef mempty
          advanceFrame gb
          advanceFrame gb -}
        if count == 0 || inBattle /= 0 || bonked
        then pure ()
        else do
            advanceFrame gb
            waitForStep gb input

waitForItemJingle :: GB -> IO ()
waitForItemJingle gb = advanceUntil gb $ do
    jinglePlaying <- (== 0x86) <$> cpuRead gb 0xC02A
    inBattle <- cpuRead gb wIsInBattle
    pure $ jinglePlaying || inBattle /= 0
