{-# LANGUAGE GADTs #-}

module Main where

import Prelude hiding (log)
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Foldable
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid
import Data.Traversable
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Foreign
import Foreign.C.Types
import System.IO
import Text.Printf
import Control.Monad.Free (Free(Free, Pure), liftF)
import Control.Monad.Trans.State (evalState, state)
import Data.List (permutations)

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
import Data.Traversable
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
import Data.Traversable
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

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering

    baseSave <- BS.readFile "pokered_dugtrio2.sav"

    gb <- create
    loadRomFile gb "pokered.gbc"

    beginState <- saveState gb
    _paths <- runSearch gb beginState $ do
        (frame, _) <- segment "IGT frame" [0] $ \gb _inputRef frame -> do
            loadSaveData gb (setSaveFrames frame baseSave)
            reset gb
            doOptimalIntro gb
            return $ Just ()
        dugtrio
    return ()

data SearchDSL f where
    Log :: String -> f -> SearchDSL f
    Segment :: (Ord s) => String -> [a] -> (GB -> IORef Input -> a -> IO (Maybe s)) -> ((a, s) -> f) -> SearchDSL f

instance Functor SearchDSL where
    fmap f (Log message continue) = Log message (f continue)
    fmap f (Segment name paths apply continue) = Segment name paths apply (f . continue)

type Search = Free SearchDSL

runSearch :: GB -> ByteString -> Search a -> IO [a]
runSearch gb beginState search = case search of
    Free (Log message continue) -> do
        putStr message
        runSearch gb beginState continue
    Free (Segment name paths apply continue) -> do
        printf "%s: Begin\n" name
        goodSegments <- runSegmentPaths paths apply
        printf "%s: Found %d unique successful paths (from %d input paths)\n" name (Map.size goodSegments) (length paths)
        outs <- forM (Map.assocs goodSegments) $ \(result, (path, endState))
            -> runSearch gb endState $ continue (path, result)
        printf "%s: End\n" name
        return $ concat outs
    Pure x -> return [x]

    where
    runSegmentPaths :: (Ord s) => [a] -> (GB -> IORef Input -> a -> IO (Maybe s)) -> IO (Map s (a, ByteString))
    runSegmentPaths paths apply = do
        resultToPathAndStateRef <- newIORef Map.empty
        forM_ paths $ \path -> do
            inputRef <- newIORef mempty
            setInputGetter gb (readIORef inputRef)
            loadState gb beginState
            printf "."
            maybeResult <- apply gb inputRef path
            case maybeResult of
                Just result -> do
                    s <- readIORef resultToPathAndStateRef
                    -- TODO(strager): Evaluate cost and pick
                    -- the cheaper version.
                    let shouldInsert = result `Map.notMember` s
                    when shouldInsert $ do
                        endState <- saveState gb
                        writeIORef resultToPathAndStateRef (Map.insert result (path, endState) s)
                    return ()
                Nothing -> return ()
        printf "\n"
        readIORef resultToPathAndStateRef

segment :: (Ord s) => String -> [a] -> (GB -> IORef Input -> a -> IO (Maybe s)) -> Search (a, s)
segment name paths apply = liftF $ Segment name paths apply id

log :: String -> Search ()
log message = liftF $ Log message ()

readRNGState :: GB -> IO (Word8, Word8, Word8)
readRNGState gb = do
    add <- cpuRead gb 0xFFD3
    sub <- cpuRead gb 0xFFD4
    div <- cpuRead gb 0xFF04
    return (add, sub, div)

combinations :: [a] -> Int -> [[a]]
combinations xs n = mapM (\_ -> xs) [1..n]

pressAArbitrarily :: [Input] -> [[Input]]
pressAArbitrarily path = map
    (\aInputs -> zipWith (<>) path aInputs)
    $ combinations [Input 0, i_A] (length path)

dugtrio :: Search [[Input]]
dugtrio = do
    let segment1Paths = do
            basePath <- permutations [i_Right, i_Right, i_Right, i_Up]
            let basePath' = basePath ++ [i_Up]
            let aPaths = pressAArbitrarily basePath'
            -- Don't press A if we would interact with the
            -- sign.
            let aPaths' = filter (\path -> not $ (path !! 0) `hasAllInput` i_Up && (path !! 1) `hasAllInput` i_A) aPaths
            aPaths'
    (segment1Path, _) <- segment "Enter Diglett Cave" segment1Paths $ \gb inputRef path -> do
        bufferedWalk gb inputRef path
        rngState <- readRNGState gb
        return $ Just (last path, rngState)

    let segment2Paths = do
            basePath <- permutations [i_Up, i_Up, i_Right, i_Right]
            let basePath' = i_Up : basePath
            let aPaths = pressAArbitrarily basePath'
            -- Don't press A if we would interact with the
            -- dude.
            let aPaths' = filter (\path -> not $ (path !! 0) `hasAllInput` i_Up && (path !! 1) `hasAllInput` i_Up && (path !! 2) `hasAllInput` i_A) aPaths
            aPaths'
    (segment2Path, _) <- segment "Climb ladder" segment2Paths $ \gb inputRef path -> do
        bufferedWalk gb inputRef path
        rngState <- readRNGState gb
        return $ Just (last path, rngState)

    let segment3Paths = flip map [0 :: Int .. 100 :: Int] $ \seed ->
            i_Left : (flip evalState (mkStdGen seed) $ do
                forM [0..20] $ \_ ->
                    state $ randomOf [i_Up, i_Down, i_Left, i_Up <> i_A, i_Down <> i_A, i_Left <> i_A])
    (segment3Path, encounter) <- segment "Encounter Dugtrio" segment3Paths $ \gb inputRef path -> do
        bufferedWalk gb inputRef path
        encountered <- (/= 0) <$> cpuRead gb wIsInBattle
        if encountered
        then do
            advanceUntil gb ((/= 0) <$> cpuRead gb wIsInBattle)
            species <- cpuRead gb wEnemyMonSpecies
            level <- cpuRead gb wEnemyMonLevel
            dv1 <- cpuRead gb wEnemyMonAtkDefDV
            dv2 <- cpuRead gb wEnemyMonSpdSpcDV
            --print (species, level, dv1, dv2)
            {-
            if species == 118 && level == 31
            then return $ Just (dv1, dv2)
            else return Nothing
            -}
            return $ Just (species, level, dv1, dv2)
        else do
            return Nothing
    log $ printf "Found encounter: %s\n - %s\n - %s\n - %s\n" (show encounter) (show segment1Path) (show segment2Path) (show segment3Path)

    -- TODO(strager): Return something useful.
    return []

randomOf :: (RandomGen g) => [a] -> g -> (a, g)
randomOf xs gen = (xs !! index, gen')
    where
    (index, gen') = randomR (0, length xs - 1) gen

{-
dugtrioSegments :: [GB -> IORef Input -> Segment StateGroup [Input]]
dugtrioSegments =
    [ \gb inputRef ->
      let
        paths = Vector.map (++ [i_Up]) $ enumeratePaths i_Right i_Up 0 4 [(0, 4)]
      in Segment
        { generate = selectRandom paths
        , apply = \path stateGroup -> do
            --printf "Segment 1: %s to %d states\n" (show path) (length stateGroup)
            resultStates <- for stateGroup $ \state -> do
                loadState gb state
                bufferedWalk gb inputRef path
                saveState gb
            pure (resultStates, fromIntegral (length resultStates))
        }
    , \gb inputRef ->
      let
        paths = enumeratePaths i_Up i_Right 0 5 [(1, 5)]
      in Segment
        { generate = selectRandom paths
        , apply = \path stateGroup -> do
            --printf "Segment 2: %s to %d states\n" (show path) (length stateGroup)
            resultStates <- for stateGroup $ \state -> do
                loadState gb state
                bufferedWalk gb inputRef path
                saveState gb
            pure (resultStates, fromIntegral (length resultStates))
        }
    , \gb inputRef -> Segment
        { generate = do
            selectRandom paths
        , apply = \path stateGroup -> do
            printf "Segment 3: %s to %d states\n" (show path) (length stateGroup)
            resultMaybeStates <- for stateGroup $ \state -> do
                loadState gb state

                bufferedWalk gb inputRef path
                encountered <- (/= 0) <$> cpuRead gb wIsInBattle
                if encountered
                then do
                    advanceUntil gb ((/= 0) <$> cpuRead gb wIsInBattle)
                    species <- cpuRead gb wEnemyMonSpecies
                    level <- cpuRead gb wEnemyMonLevel
                    print (species, level)
                    if species == 118 && level == 31
                    then Just <$> saveState gb
                    else pure Nothing
                else pure Nothing
            let resultStates = catMaybes resultMaybeStates
            pure $ (resultStates, fromIntegral (length resultStates))
        }
    ]

launchSearch :: [GB -> IORef Input -> Segment StateGroup [Input]] -> StateGroup -> IO ()
launchSearch segs initialStates = do
    initialIORef <- newIORef (Just (Checkpoint
        { revPaths = []
        , currentState = initialStates
        , value = 60
        }))
    lock <- newMVar ()
    launchLoop segs initialIORef lock
    where
    launchLoop segs sourceRef lock = do
        case segs of
            [] -> pure ()
            s:ss -> do
                targetRef <- newIORef Nothing

                gb <- create
                loadRomFile gb "pokered.gbc"
                inputRef <- newIORef mempty
                setInputGetter gb (readIORef inputRef)

                forkIO . forever $ do
                    segmentStep (s gb inputRef) sourceRef targetRef $ \check -> do
                        withMVar lock $ \_ -> do
                            pure ()
                            --printf "Segment %d\tValue %f\n" (length (revPaths check)) (value check)
                        {-
                        when (length ss == 1 && value check > 20) $ do
                            let description = printf "Value %f\t%s\n" (value check) (show . reverse $ revPaths check)
                            appendFile "almost_paths.txt" description
                        -}
                        when (length ss == 0 {- && value check > 20 -}) $ do
                            let description = printf "Value %f\t%s\n" (value check) (show . reverse $ revPaths check)
                            appendFile "complete_paths.txt" description
                    threadDelay 10000
                launchLoop ss targetRef lock
-}

setSaveFrames :: Word8 -> ByteString -> ByteString
setSaveFrames f dat =
    let dat' = editByte saveTimeFrames f dat
        checksum = computeChecksum saveMainDataStart saveMainDataEnd dat'
        dat'' = editByte saveMainDataChecksum checksum dat'
    in
    dat''

getEncounterData :: GB -> IO (Word8, Word8, Word8, Word8)
getEncounterData gb = do
    advanceUntil gb ((/= 0) <$> cpuRead gb wIsInBattle)
    species <- cpuRead gb wEnemyMonSpecies
    level <- cpuRead gb wEnemyMonLevel
    dv1 <- cpuRead gb wEnemyMonAtkDefDV
    dv2 <- cpuRead gb wEnemyMonSpdSpcDV
    pure $ (species, level, dv1, dv2)
