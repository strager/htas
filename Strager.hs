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
import Data.List (nub, permutations)

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

    baseSave <- BS.readFile "pokered_dugtrio3.sav"

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
        -- printf "%s: Begin\n" name
        goodSegments <- runSegmentPaths paths apply
        -- printf "%s: Found %d unique successful paths (from %d input paths)\n" name (Map.size goodSegments) (length paths)
        outs <- forM (Map.assocs goodSegments) $ \(result, (path, endState))
            -> runSearch gb endState $ continue (path, result)
        -- printf "%s: End\n" name
        return $ concat outs
    Pure x -> return [x]

    where
    runSegmentPaths :: (Ord s) => [a] -> (GB -> IORef Input -> a -> IO (Maybe s)) -> IO (Map s (a, ByteString))
    runSegmentPaths paths apply = do
        resultToPathAndStateRef <- newIORef Map.empty
        gen <- newStdGen
        -- HACK(strager): Make things faster.
        let paths' = takeRandom 20 paths gen
        forM_ paths' $ \path -> do
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
    $ map (\i -> shuffle (replicate i i_A ++ replicate (length path - i) (Input 0)) (mkStdGen 0)) [0..length path - 1]
    -- Brute force method: $ combinations [Input 0, i_A] (length path)

-- https://stackoverflow.com/a/29054603/39992
shuffle :: (RandomGen g) => [a] -> g -> [a]
shuffle [] _gen = []
shuffle xs gen =
    let (randomPosition, gen') = randomR (0, length xs - 1) gen
        (left, (a:right)) = splitAt randomPosition xs
    in a : shuffle (left ++ right) gen'

takeRandom :: (RandomGen g) => Int -> [a] -> g -> [a]
takeRandom n xs gen = take n $ shuffle xs gen

dugtrio :: Search [[Input]]
dugtrio = do
    let segment1Paths = do
            let basePath = replicate 21 i_Right
            let basePath' = basePath ++ [i_Up]
            let aPaths = pressAArbitrarily basePath'
            let aPaths' = aPaths
            aPaths'
    (segment1Path, _) <- segment "Enter Diglett Cave" segment1Paths $ \gb inputRef path -> do
        bufferedWalk gb inputRef path
        rngState <- readRNGState gb
        return $ Just (last path, rngState)

    let segment2Paths = do
            basePath <- nub $ permutations [i_Up, i_Up, i_Right, i_Right]
            let basePath' = i_Up : basePath
            let aPaths = pressAArbitrarily basePath'
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
                forM [0..20] $ \_ ->
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
