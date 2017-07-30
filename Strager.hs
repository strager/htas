module Main where

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
import MoonManip

import MoonManip (enumeratePaths)

main :: IO ()
main = do
    gb <- create
    loadRomFile gb "pokered.gbc"
    dat <- BS.readFile "pokered_dugtrio.sav"

    inputRef <- newIORef mempty

    for_ [0..59] $ \frame -> do
        loadSaveData gb (setSaveFrames frame dat)
        (loc, encData) <- dugtrioManip gb inputRef
        printf "IGT0: %2d\t%s\t" frame (show loc)
        case encData of
            Nothing -> do
                printf "No encounter\n"
            Just (species, level, dv1, dv2) -> do
                printf "Species: %d\tLevel: %d\tDVs: %02x%02x\n" species level dv1 dv2

dugtrioManip :: GB -> IORef Input -> IO (Location, Maybe (Word8, Word8, Word8, Word8))
dugtrioManip gb inputRef = do
    reset gb
    doOptimalIntro gb
    setInputGetter gb (readIORef inputRef)
    clearTraceCallback gb

    bufferedWalk gb inputRef $ cycle [i_Up, i_Down]
    loc <- getLocation gb
    encData <- getEncounterData gb
    pure (loc, Just encData)

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
