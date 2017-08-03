{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module Main where

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
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan (TChan, newTChan, readTChan, writeTChan)
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Concurrent.STM.TVar (TVar, newTVar, readTVar, readTVarIO, writeTVar)
import Control.Exception (bracket)
import Control.Concurrent.Async (forConcurrently)

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

for :: [a] -> (a -> b) -> [b]
for = flip map

main :: IO ()
main = do
    baseSave <- BS.readFile "pokered_dugtrio3_2stepscloser.sav"

{-
    _paths <- runSearch newGB $ do
        (frame, _) <- segment "IGT frame" [0] $ \gb _inputRef frame -> do
            loadSaveData gb (setSaveFrames frame baseSave)
            reset gb
            doOptimalIntro gb
            return $ Just ()
        dugtrio
    return ()
-}

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
    deriving (Eq, Ord)

prune :: Search a
prune = empty

expectMap :: Word8 -> Search ()
expectMap map = do
    gb <- getGameboy
    location <- liftIO $ getLocation gb
    when (locMap location /= map) prune

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
    gb <- getGameboy
    -- TODO(strager): Prune if we're not at the expected
    -- location.

    inputRef <- liftIO $ newIORef mempty
    liftIO $ setInputGetter gb (readIORef inputRef)

    let tryStep input = do
            liftIO $ bufferedStep gb inputRef input
            checkpoint
            return input
    let tryStepA input = tryStep input <|> tryStep (input <> i_A)

    segment1Path <- replicateM 14 $ tryStepA i_Right <* expectMap viridianCityMap
    segment2Path <- replicateM 5 $ tryStepA i_Right <* expectMap route11Map
    segment3Step <- tryStepA i_Up <* expectMap diglettCaveEntranceBMap
    let segment3Path = [segment3Step]

    segment4Path <- do
        s1 <- tryStepA i_Up <* expectMap diglettCaveEntranceBMap
        s2 <- tryStepA i_Up <* expectMap diglettCaveEntranceBMap
        s3 <- tryStepA i_Right <* expectMap diglettCaveEntranceBMap
        s4 <- tryStepA i_Up <* expectMap diglettCaveEntranceBMap
        s5 <- tryStepA i_Right <* expectMap diglettCaveMap
        return [s1, s2, s3, s4, s5]

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
                encountered <- liftIO $ (/= 0) <$> cpuRead gb wIsInBattle
                if encountered
                then do
                    encounter <- liftIO $ readEncounter gb
                    liftIO $ print encounter
                    return ([step], encounter)
                else do
                    (path, encounter) <- loop (depth + 1)
                    return (step : path, encounter)
        loop 0

    let paths = [segment1Path, segment2Path, segment3Path, segment4Path, segment5Path]
    log $ printf "Found encounter: %s\n%s" (show encounter)
        $ concat (map (\path -> printf " - %s\n" (show path) :: String) paths)

{-
data SearchDSL f where
    Alternative :: f -> f -> (a -> f) -> SearchDSL f
    Checkpoint :: f -> SearchDSL f
    GetGameboy :: (GB -> f) -> SearchDSL f
    Empty :: SearchDSL f
    LiftIO :: IO a -> (a -> f) -> SearchDSL f
    Log :: String -> f -> SearchDSL f

instance Functor SearchDSL where
    fmap f (Alternative x y continue) = Alternative (f x) (f y) (\r -> f (continue r))
    fmap f (Checkpoint continue) = Checkpoint (f continue)
    fmap f (GetGameboy continue) = GetGameboy (\gb -> f (continue gb))
    fmap f Empty = Empty
    fmap f (LiftIO io continue) = LiftIO io (\x -> f (continue x))
    fmap f (Log message continue) = Log message (f continue)
-}

data Search a where
    Alternative :: Search a -> Search a -> (a -> Search b) -> Search b
    Checkpoint :: Search a -> Search a
    Empty :: Search a
    GetGameboy :: (GB -> Search a) -> Search a
    LiftIO :: IO a -> (a -> Search b) -> Search b
    Log :: String -> Search a -> Search a
    Pure :: a -> Search a

--instance Functor Search where
    --fmap f search = Bind search (\x -> Pure (f x))
    {-
    fmap f (Alternative x y) = Alternative (fmap f x) (fmap f y)
    fmap f (Bind search continue) = Bind (fmap f search) (\x -> fmap search continue)
    -}

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

{-
instance {-# OVERLAPPING #-} Alternative (Free SearchDSL) where
    empty = liftF Empty
    x <|> y = liftF $ Alternative x y id
-- -}
{-
instance Alternative SearchDSL where
    empty = Empty
    x <|> y = Alternative x y id
-- -}

{-
instance MonadIO (Free SearchDSL) where
    liftIO io = liftF $ LiftIO io id
-}

--type Search = Free SearchDSL

-- TODO(strager)
type Cost = ()

type GBState = ByteString

type Checkpointer c = GB -> IO c

runSearch :: (Eq c, Ord c) => IO GB -> Checkpointer c -> Search a -> IO [a]
runSearch newGB checkpointer search = do
    gb <- newGB
    checkpointsRef <- newIORef (Map.empty :: Map c (Cost, GBState))
    let runSearch' :: Search a -> IO [a]
        runSearch' (Alternative x y continue) = do
            -- FIXME(strager): We also need to save the
            -- input getter.
            beforeState <- saveState gb
            xs <- runSearch' (x >>= \r -> continue r)
            loadState gb beforeState
            ys <- runSearch' (y >>= \r -> continue r)
            return (xs ++ ys)
        runSearch' (Checkpoint continue) = do
            checkpoint <- checkpointer gb
            checkpoints <- readIORef checkpointsRef
            let isCheckpointNew = case Map.lookup checkpoint checkpoints of
                    Nothing -> True
                    Just (_cost, _state) -> False -- TODO(strager): Pick the solution with the lower cost.
            if isCheckpointNew
            then do
                let cost = () -- TODO(strager)
                state <- saveState gb
                writeIORef checkpointsRef $ Map.insert checkpoint (cost, state) checkpoints
                -- TODO(strager): Breadth-first search.
                runSearch' continue
            else return [] -- Prune.
        runSearch' Empty = return []
        runSearch' (GetGameboy continue) = runSearch' (continue gb)
        runSearch' (LiftIO io continue) = do
            x <- io
            runSearch' (continue x)
        runSearch' (Log message continue) = do
            hPutStr stderr message
            runSearch' continue
        runSearch' (Pure x) = return [x]
    runSearch' search

checkpoint :: Search ()
checkpoint = Checkpoint (Pure ())

getGameboy :: Search GB
getGameboy = GetGameboy (\gb -> Pure gb)

log :: String -> Search ()
log message = Log message (Pure ())

{-
data SearchDSL f where
    Log :: String -> f -> SearchDSL f
    Segment :: (Ord s) => String -> [a] -> (GB -> IORef Input -> a -> IO (Maybe s)) -> ((a, s) -> f) -> SearchDSL f

instance Functor SearchDSL where
    fmap f (Log message continue) = Log message (f continue)
    fmap f (Segment name paths apply continue) = Segment name paths apply (f . continue)

type Search = Free SearchDSL

runSearch :: IO GB -> Search a -> IO [a]
runSearch newGB search = do
    gbs <- replicateM 8 newGB
    gbChan <- atomically $ do
        gbChan <- newTChan
        mapM_ (writeTChan gbChan) gbs
        return gbChan
    runCountVar <- atomically $ newTVar (0 :: Int)
    logMutex <- newMVar ()
    -- TODO(strager): Clean up this thread.
    forkIO $ forever $ do
        threadDelay (3 * 1000 * 1000)
        count <- readTVarIO runCountVar
        log logMutex $ printf "%d runs executed\n" count
    runSearch' gbChan runCountVar logMutex search

    where
    log :: MVar () -> String -> IO ()
    log logMutex message = withMVar logMutex $ \_ -> hPutStr stderr message

    runSearch' :: TChan GB -> TVar Int -> MVar () -> Search a -> IO [a]
    runSearch' gbChan runCountVar logMutex search
        = runSearch'' Nothing search

        where
        runSearch'' :: Maybe ByteString -> Search a -> IO [a]
        runSearch'' maybeBeginState search = case search of
            Free (Log message continue) -> do
                log logMutex message
                runSearch'' maybeBeginState continue
            Free (Segment name paths apply continue) -> do
                -- log logMutex $ printf "%s: Begin\n" name
                goodSegments <- runSegmentPaths maybeBeginState paths apply
                log logMutex $ printf "%s: Found %d unique successful paths (from %d input paths)\n" name (Map.size goodSegments) (length paths)
                outs <- forConcurrently (Map.assocs goodSegments) $ \(result, (path, endState))
                    -> runSearch'' (Just endState) $ continue (path, result)
                -- log logMutex $ printf "%s: End\n" name
                return $ concat outs
            Pure x -> return [x]

        runSegmentPaths :: (Ord s) => Maybe ByteString -> [a] -> (GB -> IORef Input -> a -> IO (Maybe s)) -> IO (Map s (a, ByteString))
        runSegmentPaths maybeBeginState paths apply = do
            resultsVar <- atomically $ newTVar Map.empty
            gen <- newStdGen
            forConcurrently paths $ \path -> bracket
                (atomically $ readTChan gbChan)
                (\gb -> atomically $ writeTChan gbChan gb)
                $ \gb -> do
                    inputRef <- newIORef mempty
                    setInputGetter gb (readIORef inputRef)
                    case maybeBeginState of
                        Just beginState -> loadState gb beginState
                        Nothing -> return ()
                    maybeResult <- apply gb inputRef path
                    atomically $ do
                        count <- readTVar runCountVar
                        writeTVar runCountVar (count + 1)
                    case maybeResult of
                        Just result -> do
                            results <- readTVarIO resultsVar
                            if result `Map.member` results
                                -- TODO(strager): Evaluate cost and pick the cheaper version.
                                then return ()
                                else do
                                    endState <- saveState gb
                                    atomically $ do
                                        results <- readTVar resultsVar
                                        -- TODO(strager): Evaluate cost and pick the cheaper version.
                                        writeTVar resultsVar $ Map.insert result (path, endState) results
                        Nothing -> return ()
            readTVarIO resultsVar

segment :: (Ord s) => String -> [a] -> (GB -> IORef Input -> a -> IO (Maybe s)) -> Search (a, s)
segment name paths apply = liftF $ Segment name paths apply id

log :: String -> Search ()
log message = liftF $ Log message ()

combinations :: [a] -> Int -> [[a]]
combinations xs n = mapM (\_ -> xs) [1..n]

takeRandom :: (RandomGen g) => Int -> [a] -> g -> [a]
takeRandom n xs gen = take n $ shuffle xs gen

dugtrio :: Search [[Input]]
dugtrio = do
    let segment1Paths = do
            let basePath = replicate 19 i_Right
            let basePath' = basePath ++ [i_Up]
            let aPaths = pressAArbitrarily basePath'
            let aPaths' = aPaths
            aPaths'
    (segment1Path, _) <- segment "Enter Diglett Cave" segment1Paths $ \gb inputRef path -> do
        bufferedWalk gb inputRef path
        rngState <- readRNGState gb
        return $ Just (last path, rngState)

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
    (segment2Path, _) <- segment "Climb ladder" segment2Paths $ \gb inputRef path -> do
        bufferedWalk gb inputRef path
        rngState <- readRNGState gb
        return $ Just (last path, rngState)

    let segment3Paths = flip map [0 :: Int .. 100 :: Int] $ \seed ->
            i_Left : (flip evalState (mkStdGen seed) $ do
                forM [0..8] $ \_ ->
                    state $ randomOf [i_Up, i_Down, i_Left, i_Up <> i_A, i_Down <> i_A, i_Left <> i_A])
    (segment3Path, encounter) <- segment "Encounter Dugtrio" segment3Paths $ \gb inputRef path -> do
        bufferedWalk gb inputRef path
        encountered <- (/= 0) <$> cpuRead gb wIsInBattle
        if encountered
        then do
            encounter <- readEncounter gb
            return $ if species encounter == 118 && level encounter == 31
                then Just encounter
                else Nothing
        else return Nothing
    log $ printf "Found encounter: %s\n - %s\n - %s\n - %s\n" (show encounter) (show segment1Path) (show segment2Path) (show segment3Path)

    -- TODO(strager): Return something useful.
    return []
-}

randomOf :: (RandomGen g) => [a] -> g -> (a, g)
randomOf xs gen = (xs !! index, gen')
    where
    (index, gen') = randomR (0, length xs - 1) gen

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

pressAArbitrarily :: [Input] -> [[Input]]
pressAArbitrarily path = map
    (\aInputs -> zipWith (<>) path aInputs)
    $ map (\i -> shuffle (replicate i i_A ++ replicate (length path - i) (Input 0)) (mkStdGen 0)) [0..(length path `div` 2)]
    -- Brute force method: $ combinations [Input 0, i_A] (length path)

-- https://stackoverflow.com/a/29054603/39992
shuffle :: (RandomGen g) => [a] -> g -> [a]
shuffle [] _gen = []
shuffle xs gen =
    let (randomPosition, gen') = randomR (0, length xs - 1) gen
        (left, (a:right)) = splitAt randomPosition xs
    in a : shuffle (left ++ right) gen'

readRNGState :: GB -> IO (Word8, Word8, Word8)
readRNGState gb = do
    add <- cpuRead gb 0xFFD3
    sub <- cpuRead gb 0xFFD4
    div <- cpuRead gb 0xFF04
    return (add, sub, div)
