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

import HTas.Direct
import HTas.Low

import Red.Battle
import Red.Intro
import Red.Overworld
import Red.Save
import Search
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
import Search
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
import Search
--import MoonManip

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering

    baseSave <- BS.readFile "pokered_dugtrio2.sav"

    gb <- create
    loadRomFile gb "pokered.gbc"

    beginState <- saveState gb
    _paths <- runSearch gb beginState $ do
        (frame, _) <- runSegment [0] $ \gb _inputRef frame -> do
            loadSaveData gb (setSaveFrames frame baseSave)
            reset gb
            doOptimalIntro gb
            return $ Just ()
        dugtrio
    return ()

data SearchDSL f where
    Log :: String -> f -> SearchDSL f
    RunSegment :: (Eq s) => [a] -> (GB -> IORef Input -> a -> IO (Maybe s)) -> ((a, s) -> f) -> SearchDSL f

instance Functor SearchDSL where
    fmap f (Log message continue) = Log message (f continue)
    fmap f (RunSegment queue apply continue) = RunSegment queue apply (f . continue)

type Search = Free SearchDSL

runSearch :: GB -> ByteString -> Search a -> IO [a]
runSearch gb beginState search = case search of
    Free (Log message continue) -> do
        putStr message
        runSearch gb beginState continue
    Free (RunSegment queue apply continue) -> do
        ass <- forM queue $ \path -> do
            inputRef <- newIORef mempty
            setInputGetter gb (readIORef inputRef)
            loadSaveData gb beginState
            maybeResult <- apply gb inputRef path
            case maybeResult of
                -- TODO(strager): Dedupe paths based on result.
                -- TODO(strager): Breadth-first search.
                Just result -> do
                    endState <- saveState gb
                    runSearch gb endState $ continue (path, result)
                Nothing -> return []
        return $ concat ass
    Pure x -> return [x]

runSegment :: (Eq s) => [a] -> (GB -> IORef Input -> a -> IO (Maybe s)) -> Search (a, s)
runSegment queue apply = liftF $ RunSegment queue apply id

log :: String -> Search ()
log message = liftF $ Log message ()

dugtrio :: Search [[Input]]
dugtrio = do
    let segment1Paths = [[i_Right, i_Right, i_Right, i_Up, i_Up]]
    (segment1Path, _) <- runSegment segment1Paths $ \gb inputRef path -> do
        bufferedWalk gb inputRef path
        -- TODO(strager)
        return $ Just (0 :: Word8, 0 :: Word8)
    log $ printf "Segment 1: %s\n" (show segment1Path)

    let segment2Paths = [[i_Up, i_Up, i_Up, i_Right, i_Right]]
    (segment2Path, _) <- runSegment segment2Paths $ \gb inputRef path -> do
        bufferedWalk gb inputRef path
        -- TODO(strager)
        return $ Just (0 :: Word8, 0 :: Word8)
    log $ printf "Segment 2: %s\n" (show segment2Path)

    let segment3Paths = flip map [0 :: Int .. 100 :: Int] $ \seed ->
            flip evalState (mkStdGen seed) $ do
                forM [0..20] $ \_ ->
                    state $ randomOf [i_Up, i_Down, i_Left, i_Right]
    (segment3Path, _) <- runSegment segment3Paths $ \gb inputRef path -> do
        print path
        bufferedWalk gb inputRef path
        encountered <- (/= 0) <$> cpuRead gb wIsInBattle
        if encountered
        then do
            advanceUntil gb ((/= 0) <$> cpuRead gb wIsInBattle)
            species <- cpuRead gb wEnemyMonSpecies
            level <- cpuRead gb wEnemyMonLevel
            print (species, level)
            if species == 118 && level == 31
            then return $ Just ()
            else return Nothing
        else do
            putStrLn "no encounter"
            return Nothing
    log $ printf "Segment 3: %s\n" (show segment3Path)

    return [segment1Path, segment2Path, segment3Path]

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
