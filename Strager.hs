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
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
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
import qualified Data.Foldable (foldr1)

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
    baseSave <- BS.readFile "pokeyellow_mt_moon.sav"

    runSearch newGB redCheckpointer $ do
        setups <- do
            gb <- getGameboy
            forM [0x00, 0x03, 0x10, 0x18, 0x20, 0x21, 0x22, 0x30] $ \frame -> liftIO $ do
                loadSaveData gb (setSaveFrames frame baseSave)
                reset gb
                doOptimalIntro gb
                saveState gb
        log "Made setups\n"

        -- Search with an arbitrary setup.
        do
            gb <- getGameboy
            liftIO $ loadState gb (head setups)

        path <- mtMoon

        workingSetups <- flip filterM setups $ \setup -> do
            gb <- getGameboy

            liftIO $ do
                loadState gb setup
                inputRef <- newIORef mempty
                setInputGetter gb (readIORef inputRef)
                bufferedWalk gb inputRef path
                writeIORef inputRef mempty
                loc <- getLocation gb
                let waitForEncounter c
                        | c == 0 = return Nothing
                        | otherwise = do
                            encounter <- (/= 0) <$> cpuRead gb wIsInBattle
                            if encounter
                            then Just <$> readEncounter gb
                            else do
                                advanceFrame gb
                                waitForEncounter (c - 1)
                enc <- waitForEncounter 120
                case enc of
                    Nothing -> return True
                    Just _ -> return False

        when (length workingSetups < 2) prune

        log $ printf "Found path (%d/%d IGT) (%d A-s): %s\n"
            (length workingSetups)
            (length setups)
            (length $ filter (\i -> i `hasAllInput` i_A) path)
            (show path)

    return ()

    where
    newGB :: IO GB 
    newGB = do
        gb <- create
        loadRomFile gb "pokeyellow.gbc"
        return gb

redCheckpointer :: GB -> IO RedCheckpoint
redCheckpointer gb = do
    encountered <- (/= 0) <$> cpuRead gb wIsInBattle
    if encountered
    then RedCheckpointEncounter <$> readEncounter gb
    else do
        location <- getLocation gb
        rngState <- readRNGState gb
        spritesBytes <- forM [0] $ \spriteIndex
            -> forM [0..9] $ \i
                -> cpuRead gb $ 0xC100 + (spriteIndex * 0x10) + i
        return RedCheckpointOverworld
            { checkpointLocation = location
            , checkpointRNGState = rngState
            , checkpointSprites = concat spritesBytes
            }

data RedCheckpoint
    = RedCheckpointOverworld
    { checkpointLocation :: Location
    , checkpointRNGState :: [Word8]
    , checkpointSprites :: [Word8]
    -- TODO(strager): Checkpoint object timers.
    }
    | RedCheckpointEncounter Encounter
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

expectLocation :: Location -> Search ()
expectLocation location = do
    gb <- getGameboy
    actualLocation <- liftIO $ getLocation gb
    when (location /= actualLocation) $ do
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

data Paths = Paths (Map Input Paths)

data Pos = Pos !Int !Int

unionPaths :: Paths -> Paths -> Paths
unionPaths (Paths xs) (Paths ys)
    = Paths $ Map.unionWith unionPaths xs ys

consIf :: Bool -> a -> [a] -> [a]
consIf True x xs = x : xs
consIf False _x xs = xs

pathsBetween :: (Pos -> Input -> Bool) -> Pos -> Pos -> Paths
pathsBetween canStep from to = Paths $ Map.fromList $ do
    (input, pos') <- steps from to
    unless (canStep from input) []
    return (input, pathsBetween canStep pos' to)
    where
    steps :: Pos -> Pos -> [(Input, Pos)]
    steps (Pos fromX fromY) (Pos toX toY)
        = consIf (fromX < toX) (i_Right, Pos (fromX + 1) fromY)
        $ consIf (fromY < toY) (i_Down, Pos fromX (fromY + 1))
        $ consIf (fromX > toX) (i_Left, Pos (fromX - 1) fromY)
        $ consIf (fromY > toY) (i_Up, Pos fromX (fromY - 1))
        []

appendPaths :: Paths -> Paths -> Paths
appendPaths (Paths paths) nextPaths
    | Map.null paths = nextPaths
    | otherwise = Paths $ Map.map (\xs -> appendPaths xs nextPaths) paths

pressAArbitrarily :: Int -> Paths -> Paths
pressAArbitrarily maximumAPresses paths@(Paths xs)
    | maximumAPresses == 0 = paths
    | otherwise = Paths (Map.map (pressAArbitrarily maximumAPresses) xs) `unionPaths` aPaths
    where
    aPaths = Paths $ Map.map (pressAArbitrarily (maximumAPresses - 1)) $ Map.mapKeysWith unionPaths (<> i_A) xs

mtMoon :: Search [Input]
mtMoon = do
    -- TODO(strager): Prune if we're not at the expected
    -- location.

    let tryStep input = do
            inputRef <- liftIO $ newIORef mempty
            gb <- getGameboy
            liftIO $ setInputGetter gb (readIORef inputRef)
            liftIO $ bufferedStep gb inputRef input
            bonked <- liftIO $ (== 0xB4) <$> cpuRead gb 0xC02A
            when bonked prune
            trainer <- liftIO $ trainerEncounter gb
            when trainer prune
            encountered <- liftIO $ (/= 0) <$> cpuRead gb wIsInBattle
            when encountered prune
            unless (input `hasAllInput` i_A) checkpoint
            return input

    -- HACK(strager): We let bonk pruning abort bad paths.
    let canStep _pos _input = True

    let runPath :: Input -> Paths -> Search [Input]
        runPath input nextPaths = (:) <$> tryStep input <*> runPaths nextPaths
        runPaths :: Paths -> Search [Input]
        runPaths (Paths paths) = if Map.null paths
            then return []
            else Data.Foldable.foldr1 (<|>) $ Map.mapWithKey runPath paths

    segment1Path <- runPaths $
        pressAArbitrarily 1 (pathsBetween canStep (Pos 14 35) (Pos 14 22) `appendPaths` pathsBetween canStep (Pos 14 22) (Pos 20 22))
    expectLocation $ Location 59 20 22
    segment2Path <- runPaths $
        pressAArbitrarily 1 (pathsBetween canStep (Pos 20 22) (Pos 31 10))
    expectLocation $ Location 59 31 10
    segment3Path <- runPaths $
        pressAArbitrarily 1 (pathsBetween canStep (Pos 31 10) (Pos 31 3) `appendPaths` pathsBetween canStep (Pos 31 3) (Pos 16 8))
    expectLocation $ Location 59 16 8
    segment4Path <- runPaths $
        pressAArbitrarily 1 (pathsBetween canStep (Pos 16 8) (Pos 12 17) `appendPaths` pathsBetween canStep (Pos 12 17) (Pos 5 5))
    expectLocation $ Location 60 5 5
    segment5Path <- runPaths $
        pressAArbitrarily 1 (pathsBetween canStep (Pos 5 5) (Pos 21 17))
    expectLocation $ Location 61 21 17
    segment6Path <- runPaths $
        pressAArbitrarily 1 (pathsBetween canStep (Pos 21 17) (Pos 26 14) `appendPaths` pathsBetween canStep (Pos 26 14) (Pos 32 16) `appendPaths` pathsBetween canStep (Pos 32 16) (Pos 36 14))
    expectLocation $ Location 61 36 14
    segment7Path <- runPaths $
        -- TODO(strager): Check other possible paths.
        pressAArbitrarily 1 (pathsBetween canStep (Pos 36 14) (Pos 34 24) `appendPaths` pathsBetween canStep (Pos 34 24) (Pos 11 31))
    expectLocation $ Location 61 11 31
    segment8Path <- runPaths $
        pressAArbitrarily 1 (pathsBetween canStep (Pos 11 31) (Pos 12 9))
    expectLocation $ Location 61 12 9

    let waitForEncounter c
            | c == 0 = return ()
            | otherwise = do
                gb <- getGameboy
                encounter <- liftIO $ (/= 0) <$> cpuRead gb wIsInBattle
                unless encounter $ do
                    liftIO $ advanceFrame gb
                    waitForEncounter (c - 1)
    waitForEncounter 120

    let paths = [segment1Path, segment2Path, segment3Path, segment4Path, segment5Path, segment6Path, segment7Path, segment8Path]
    return $ concat paths
    {-
    log $ printf "Found path (%d A-s):\n%s" (sum $ map (length . filter (\p -> p `hasAllInput` i_A)) paths)
        $ concat (map (\path -> printf " - %s\n" (show path) :: String) paths)
    -}

data Search a where
    Alternative :: Search a -> Search a -> (a -> Search b) -> Search b
    Checkpoint :: Search a -> Search a
    Empty :: Search a
    GetGameboy :: (GB -> Search a) -> Search a
    IsRedundant :: (Bool -> Search a) -> Search a
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
    IsRedundant continue >>= continue' = IsRedundant (\is -> continue is >>= continue')
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
type Cost = ()

type GBState = ByteString

type Checkpointer c = GB -> IO c

-- | The set of work to be performed by multiple threads
-- during runSearch.
--
-- A WorkQueue is actually a stack. FILO order is better
-- than FIFO order because performing work in FILO order has
-- several important benefits, including:
--
-- * Killing search branches as soon as possible, therefore
--   using significantly less memory (important!)
-- * Utilizing CPU caches better (unmeasured)
newtype WorkQueue a = WorkQueue (TVar [a])

newWorkQueueIO :: IO (WorkQueue a)
newWorkQueueIO = WorkQueue <$> newTVarIO []

readWorkQueue :: WorkQueue a -> STM a
readWorkQueue (WorkQueue var) = do
    xs <- readTVar var
    case xs of
        [] -> retry
        (x:xs) -> do
            writeTVar var xs
            return x

writeWorkQueue :: WorkQueue a -> a -> STM ()
writeWorkQueue (WorkQueue var) x = modifyTVar var (x :)

runSearch :: forall a c. (Show c, Eq c, Ord c) => IO GB -> Checkpointer c -> Search a -> IO ()
runSearch newGB checkpointer initialSearch = do
    logMutex <- newMVar ()
    let log message = do
            tid <- myThreadId
            withMVar logMutex $ \() -> hPutStr stderr $ printf "[%s] %s" (show tid) message

    checkpointsVar <- newTVarIO (Map.empty :: Map c Cost)

    -- Some counters for debugging.
    enqueuedWorkCountRef <- newIORef (0 :: Int)
    startedWorkCountRef <- newIORef (0 :: Int)
    completedSearchCountRef <- newIORef (0 :: Int)
    prunedSearchCountRef <- newIORef (0 :: Int)
    abortedSearchCountRef <- newIORef (0 :: Int)
    savedStateCountRef <- newIORef (0 :: Int)

    let incCounter counterRef = atomicModifyIORef' counterRef (\x -> (x + 1, ()))

    -- The queue of work to do.
    workQueue <- newWorkQueueIO :: IO (WorkQueue (Search a))
    trackNamedSTM "set up queue" $ writeWorkQueue workQueue initialSearch

    let enqueue :: GBState -> Search a -> STM ()
        enqueue state continue = writeWorkQueue workQueue $ do
            gb <- getGameboy
            liftIO $ loadState gb state
            continue

    let runSearch' :: GB -> Search a -> IO ()
        runSearch' gb (Alternative x y continue) = do
            -- Put one side (x) into the queue so we can
            -- execute both sides concurrently.
            -- FIXME(strager): This eats up memory!
            state <- saveState gb
            -- FIXME(strager): We also need to save the
            -- input getter.
            incCounter savedStateCountRef
            incCounter enqueuedWorkCountRef
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
                if isCheckpointStillNew
                then runSearch' gb continue
                else do
                    --log "aborted\n"
                    incCounter abortedSearchCountRef
            else do
                --log "aborted\n"
                incCounter abortedSearchCountRef
        runSearch' _gb Empty = do
            incCounter prunedSearchCountRef
            return ()
        runSearch' gb (GetGameboy continue) = runSearch' gb (continue gb)
        runSearch' gb (IsRedundant continue) = do
            checkpoint <- checkpointer gb
            checkpoints <- readTVarIO checkpointsVar
            let isCheckpointNew = case Map.lookup checkpoint checkpoints of
                    Nothing -> True
                    Just _cost -> False -- TODO(strager): Pick the solution with the lower cost.
            runSearch' gb $ continue (not isCheckpointNew)
        runSearch' gb (LiftIO io continue) = do
            x <- io
            runSearch' gb (continue x)
        runSearch' gb (Log message continue) = do
            log message
            runSearch' gb continue
        runSearch' _gb (Pure _) = do
            incCounter completedSearchCountRef
            return ()

    let processQueue :: GB -> IO ()
        processQueue gb = do
            let acquireWork :: IO (Search a)
                acquireWork = trackNamedSTM "acquireWork"
                    $ readWorkQueue workQueue
            let releaseWork :: Search a -> IO ()
                releaseWork _work = return ()
            let handleWork :: Search a -> IO ()
                handleWork work = do
                    incCounter startedWorkCountRef
                    runSearch' gb work
            bracket acquireWork releaseWork handleWork
            -- TODO(strager): Terminate this loop!
            processQueue gb

    numCapabilities <- getNumCapabilities
    threads <- forM [1..numCapabilities] $ \cap -> asyncOn cap $ do
        gb <- newGB
        processQueue gb

    -- DEBUGGING
    forever $ do
        threadDelay (1000 * 1000)
        checkpoints <- readTVarIO checkpointsVar
        enqueuedWorkCount <- atomicModifyIORef' enqueuedWorkCountRef (\x -> (x, x))
        startedWorkCount <- atomicModifyIORef' startedWorkCountRef (\x -> (x, x))
        completedSearchCount <- atomicModifyIORef' completedSearchCountRef (\x -> (x, x))
        prunedSearchCount <- atomicModifyIORef' prunedSearchCountRef (\x -> (x, x))
        abortedSearchCount <- atomicModifyIORef' abortedSearchCountRef (\x -> (x, x))
        savedStateCount <- atomicModifyIORef' savedStateCountRef (\x -> (x, x))
        log $ printf "%6d checkpoints; %6d enqueued works; %6d started works; %6d completed searches; %6d pruned searches; %6d aborted searches; %6d state saves\n" (Map.size checkpoints) enqueuedWorkCount startedWorkCount completedSearchCount prunedSearchCount abortedSearchCount savedStateCount

    mapM_ wait threads

    where
    getCost :: GB -> IO Cost
    getCost _gb = return ()

checkpoint :: Search ()
checkpoint = Checkpoint (Pure ())

getGameboy :: Search GB
getGameboy = GetGameboy (\gb -> Pure gb)

isRedundant :: Search Bool
isRedundant = IsRedundant (\is -> Pure is)

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

readRNGState :: GB -> IO [Word8]
readRNGState gb = do
    add <- cpuRead gb 0xFFD3
    sub <- cpuRead gb 0xFFD4
    div <- cpuRead gb 0xFF04
    timers <- forM [0xDA40..0xDA44] $ cpuRead gb
    return $ [add, sub, div] ++ timers
