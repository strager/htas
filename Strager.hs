{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Data.Heap as Heap
import Data.Heap (MinPrioHeap)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Prelude hiding (log)
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Foldable (asum)
import Control.Applicative (Alternative(..))
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Foreign
import Foreign.C.Types
import System.IO
import Text.Printf
import Control.Monad.Trans.State (evalState, state)
import Data.List (nub, permutations)
import Control.Concurrent.STM (STM, atomically, retry)
import Control.Concurrent.STM.TChan (TChan, newTChan, readTChan, writeTChan)
import Control.Concurrent.STM.TQueue (TQueue, newTQueueIO, readTQueue, writeTQueue)
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Concurrent.STM.TVar (TVar, modifyTVar, newTVarIO, readTVar, readTVarIO, writeTVar)
import Control.Exception (bracket)
import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.Async (asyncOn, wait)
import Control.Concurrent.STM.Stats (dumpSTMStats, trackNamedSTM)

import HTas.Direct
import HTas.Low

import Red.Battle
import Red.Intro
import Red.Overworld
import Red.Save
--import Search
--import MoonManip

import Data.ByteString (ByteString)
import Data.IORef
import Data.Maybe
import Data.Monoid
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import System.Random
import Text.Printf

import Red.Battle
--import Search
import HTas.Direct
import HTas.Low

import Red.Overworld

import Control.Concurrent
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Foldable
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Foreign
import Foreign.C.Types
import System.IO
import Text.Printf

import HTas.Direct
import HTas.Low

import Red.Battle
import Red.Intro
import Red.Overworld
import Red.Save
--import Search
--import MoonManip
import System.Exit (exitFailure)

for :: [a] -> (a -> b) -> [b]
for = flip map

main :: IO ()
main = do
    baseSave <- BS.readFile "pokered_dugtrio3.sav"

    runSearch newGB checkpointer $ do
        _frame <- asum $ for [0] $ \frame -> do
            gb <- getGameboy
            liftIO $ loadSaveData gb (setSaveFrames frame baseSave)
            liftIO $ reset gb
            liftIO $ doOptimalIntro gb
            checkpoint
        dugtrio
    return ()

    where
    newGB :: IO GB 
    newGB = do
        gb <- create
        loadRomFile gb "pokered.gbc"
        return gb

    checkpointer :: GB -> IO RedCheckpoint
    checkpointer gb = do
        encountered <- (/= 0) <$> cpuRead gb wIsInBattle
        encounter <- if encountered
            then Just <$> readEncounter gb
            else return Nothing
        location <- getLocation gb
        rngState <- readRNGState gb
        return RedCheckpoint
            { checkpointEncounter = encounter
            , checkpointLocation = location
            , checkpointRNGState = rngState
            }

data RedCheckpoint = RedCheckpoint
    { checkpointEncounter :: Maybe Encounter
    , checkpointLocation :: Location
    , checkpointRNGState :: (Word8, Word8, Word8)
    -- TODO(strager): Checkpoint object timers.
    -- TODO(strager): Checkpoint the direction the player is facing.
    -- TODO(strager): Checkpoint the in-game timer.
    }
    deriving (Eq, Ord, Show)

prune :: Search a
prune = empty

expectMap :: Word8 -> Search ()
expectMap map = do
    gb <- getGameboy
    location <- liftIO $ getLocation gb
    when (locMap location /= map) $ do
        -- log $ printf "expected %d got %d\n" map (locMap location)
        prune

viridianCityMap :: Word8
viridianCityMap = 5

route11Map :: Word8
route11Map = 22

diglettCaveEntranceBMap :: Word8
diglettCaveEntranceBMap = 85

diglettCaveMap :: Word8
diglettCaveMap = 197

dugtrio :: Search ()
dugtrio = do
    -- TODO(strager): Prune if we're not at the expected
    -- location.

    let tryStep input = do
            inputRef <- liftIO $ newIORef mempty
            gb <- getGameboy
            liftIO $ setInputGetter gb (readIORef inputRef)
            liftIO $ bufferedStep gb inputRef input
            return input

    let tryStepA input = tryStep input <|> tryStep (input <> i_A)

    let tryStepsPressingAArbitrarily [] _aPressesAllowed = return []
        tryStepsPressingAArbitrarily (input : remainingInputs) aPressesAllowed
            | aPressesAllowed == 0 = tryStepWithoutA <* maybeCheckpoint
            | otherwise = tryStepWithoutA <|> tryStepWithA <* maybeCheckpoint
            where
            maybeCheckpoint
                | length remainingInputs `mod` 4 == 0 = checkpoint
                | otherwise = return ()
            tryStepWithoutA = ((:) <$> tryStep input <*> tryStepsPressingAArbitrarily remainingInputs aPressesAllowed)
            tryStepWithA = ((:) <$> tryStep (input <> i_A) <*> tryStepsPressingAArbitrarily remainingInputs (aPressesAllowed - 1))

    segment1Path <- tryStepsPressingAArbitrarily (replicate 21 i_Right) 2
    expectMap route11Map
    let segment2Path = []
    segment3Step <- tryStepA i_Up <* expectMap diglettCaveEntranceBMap
    let segment3Path = [segment3Step]

    segment4Path <- do
        s1 <- tryStepA i_Up <* expectMap diglettCaveEntranceBMap
        s2 <- tryStepA i_Up <* expectMap diglettCaveEntranceBMap
        s3 <- tryStepA i_Right <* expectMap diglettCaveEntranceBMap
        s4 <- tryStepA i_Up <* expectMap diglettCaveEntranceBMap
        s5 <- tryStepA i_Right <* expectMap diglettCaveMap
        return [s1, s2, s3, s4, s5]
    checkpoint

{-
    let segment2Paths = do
            basePath <- concat
              [ map (i_Up :) $ nub $ permutations [i_Up, i_Up, i_Right, i_Right]
              , map (\path -> i_Right : i_Up : path) $ nub $ permutations [i_Up, i_Up, i_Right]
              ]
            let aPaths = pressAArbitrarily basePath
            -- Don't press A if we would interact with the
            -- dude.
            let aPaths' = filter (\path -> not $
                    (path !! 0) `hasAllInput` i_Up &&
                    (path !! 1) `hasAllInput` i_Up &&
                    (path !! 2) `hasAllInput` i_Up &&
                    (path !! 3) `hasAllInput` i_A) aPaths
            aPaths'
            -}

    (segment5Path, encounter) <- do
        let loop depth = do
                when (depth > 7) prune
                step <- tryStepA i_Up <|> tryStepA i_Down <|> tryStepA i_Left
                expectMap diglettCaveMap
                gb <- getGameboy
                encountered <- liftIO $ (/= 0) <$> cpuRead gb wIsInBattle
                if encountered
                then do
                    encounter <- liftIO $ readEncounter gb
                    -- log $ printf "%s\n" (show encounter)
                    unless (species encounter == 118 && level encounter == 31) prune
                    checkpoint
                    return ([step], encounter)
                else do
                    --checkpoint -- FIXME(strager): Why doesn't this work?
                    (path, encounter) <- loop (depth + 1)
                    return (step : path, encounter)
        loop 0

    let paths = [segment1Path, segment2Path, segment3Path, segment4Path, segment5Path]
    log $ printf "Found encounter: %s\n%s" (show encounter)
        $ concat (map (\path -> printf " - %s\n" (show path) :: String) paths)

data Search a where
    Alternative :: Search a -> Search a -> (a -> Search b) -> Search b
    Checkpoint :: Search a -> Search a
    Empty :: Search a
    GetGameboy :: (GB -> Search a) -> Search a
    LiftIO :: IO a -> (a -> Search b) -> Search b
    Log :: String -> Search a -> Search a
    Pure :: a -> Search a

instance Functor Search where
    fmap f x = x >>= pure . f

instance Applicative Search where
    pure x = Pure x
    x <*> y = x >>= (\f -> y >>= pure . f)

instance Monad Search where
    Alternative x y continue >>= continue' = Alternative x y (\r -> continue r >>= continue')
    Checkpoint continue >>= continue' = Checkpoint (continue >>= continue')
    Empty >>= _ = Empty
    GetGameboy continue >>= continue' = GetGameboy (\gb -> continue gb >>= continue')
    LiftIO io continue >>= continue' = LiftIO io (\r -> continue r >>= continue')
    Log message continue >>= continue' = Log message (continue >>= continue')
    Pure x >>= continue' = continue' x

    return x = Pure x

instance MonadIO Search where
    liftIO io = LiftIO io (\r -> Pure r)

instance Alternative Search where
    empty = Empty
    x <|> y = Alternative x y (\r -> Pure r)

-- TODO(strager)
type Cost = Integer

type GBState = ByteString

type Checkpointer c = GB -> IO c

runSearch :: forall a c. (Show c, Eq c, Ord c) => IO GB -> Checkpointer c -> Search a -> IO ()
runSearch newGB checkpointer initialSearch = do
    logMutex <- newMVar ()
    checkpointsVar <- newTVarIO (Map.empty :: Map c Cost)
    -- The queue of work to do.
    -- TODO(strager): Search the graph in breadth-first
    -- order so edges can be discovered (thus searched
    -- concurrently) as soon as possible.
    workQueue <- newTQueueIO :: IO (TQueue (Search a))
    trackNamedSTM "set up queue" $ writeTQueue workQueue initialSearch

    let enqueue :: GBState -> Search a -> STM ()
        enqueue state continue = writeTQueue workQueue $ do
            gb <- getGameboy
            liftIO $ loadState gb state
            continue

    let runSearch' :: GB -> Search a -> IO ()
        runSearch' gb (Alternative x y continue) = do
            -- Put one side (x) into the queue so we can
            -- execute both sides concurrently.
            state <- saveState gb
            -- FIXME(strager): We also need to save the
            -- input getter.
            trackNamedSTM "queue alternative" $ enqueue state (x >>= continue)
            runSearch' gb (y >>= continue)
        runSearch' gb (Checkpoint continue) = do
            checkpoint <- checkpointer gb
            checkpoints <- readTVarIO checkpointsVar
            let isCheckpointNew = case Map.lookup checkpoint checkpoints of
                    Nothing -> True
                    Just _cost -> False -- TODO(strager): Pick the solution with the lower cost.
            if isCheckpointNew
            then do
                cost <- getCost gb
                isCheckpointStillNew <- trackNamedSTM "store checkpoint" $ do
                    checkpoints <- readTVar checkpointsVar
                    let isCheckpointStillNew = case Map.lookup checkpoint checkpoints of
                            Nothing -> True
                            Just _cost -> False -- TODO(strager): Pick the solution with the lower cost.
                    when isCheckpointStillNew
                        $ writeTVar checkpointsVar $ Map.insert checkpoint cost checkpoints
                    return isCheckpointStillNew
                -- TODO(strager): Instead of
                -- invoking continue, add it to the
                -- queue so we can run lower-cost
                -- work if necessary. Would that
                -- cause too much context switching?
                when isCheckpointStillNew $ runSearch' gb continue
            else return () -- Prune.
        runSearch' _gb Empty = return ()
        runSearch' gb (GetGameboy continue) = runSearch' gb (continue gb)
        runSearch' gb (LiftIO io continue) = do
            x <- io
            runSearch' gb (continue x)
        runSearch' gb (Log message continue) = do
            withMVar logMutex $ \() -> hPutStr stderr message
            runSearch' gb continue
        runSearch' _gb (Pure _) = return ()

    let processQueue :: GB -> IO ()
        processQueue gb = do
            let acquireWork :: IO (Search a)
                acquireWork = trackNamedSTM "acquireWork"
                    $ readTQueue workQueue
            let releaseWork :: Search a -> IO ()
                releaseWork _work = return ()
            let handleWork :: Search a -> IO ()
                handleWork work = runSearch' gb work
            bracket acquireWork releaseWork handleWork
            -- TODO(strager): Terminate this loop!
            processQueue gb

    numCapabilities <- getNumCapabilities
    threads <- forM [1..numCapabilities] $ \cap -> asyncOn cap $ do
        gb <- newGB
        processQueue gb

    -- DEBUGGING
    forM_ [1..30] $ \_ -> do
        threadDelay (1000 * 1000)
        checkpoints <- readTVarIO checkpointsVar
        printf "%3d checkpoints; ??? queued jobs\n" (Map.size checkpoints)
    exitFailure

    mapM_ wait threads

    where
    -- FIXME(strager): Is this a good cost function?
    getCost :: GB -> IO Cost
    getCost gb = getCycleCount gb

checkpoint :: Search ()
checkpoint = Checkpoint (Pure ())

getGameboy :: Search GB
getGameboy = GetGameboy (\gb -> Pure gb)

log :: String -> Search ()
log message = Log message (Pure ())

setSaveFrames :: Word8 -> ByteString -> ByteString
setSaveFrames f dat =
    let dat' = editByte saveTimeFrames f dat
        checksum = computeChecksum saveMainDataStart saveMainDataEnd dat'
        dat'' = editByte saveMainDataChecksum checksum dat'
    in
    dat''

data Encounter = Encounter
    { species :: Word8
    , level :: Word8
    , attackDV :: Word8
    , defenseDV :: Word8
    , speedDV :: Word8
    , specialDV :: Word8
    }
    deriving (Eq, Ord, Show)

readEncounter :: GB -> IO Encounter
readEncounter gb = do
    advanceUntil gb ((/= 0) <$> cpuRead gb wIsInBattle)
    species <- cpuRead gb wEnemyMonSpecies
    level <- cpuRead gb wEnemyMonLevel
    dv1 <- cpuRead gb wEnemyMonAtkDefDV
    dv2 <- cpuRead gb wEnemyMonSpdSpcDV
    return Encounter
        { species = species
        , level = level
        , attackDV = (dv1 `shiftR` 4) .&. 0xF
        , defenseDV = dv1 .&. 0xF
        , speedDV = (dv2 `shiftR` 4) .&. 0xF
        , specialDV = dv2 .&. 0xF
        }

readRNGState :: GB -> IO (Word8, Word8, Word8)
readRNGState gb = do
    add <- cpuRead gb 0xFFD3
    sub <- cpuRead gb 0xFFD4
    div <- cpuRead gb 0xFF04
    return (add, sub, div)
