module Search where

import Control.Concurrent
import Data.IORef
import System.Random
import Text.Printf

data Segment a b = Segment
    { generate :: IO b
    , apply :: b -> a -> IO (a, Double)
    }

data Checkpoint a b = Checkpoint
    { revPaths :: [b]
    , currentState :: a
    , value :: Double
    }

replaceProbability :: Checkpoint a b -> Checkpoint a b -> Double
replaceProbability oldCheck newCheck =
    let val1 = value oldCheck
        val2 = value newCheck
    in
    if val2 == 0 then 0
    else min 1 $ exp (-val1/val2)

coin :: Double -> IO Bool
coin p = (< p) <$> randomRIO (0, 1)

iterateM :: Int -> (a -> IO a) -> a -> IO a
iterateM n f a =
    if n == 0
    then pure a
    else f a >>= iterateM (n-1) f

segmentStep :: Segment a b -> IORef (Maybe (Checkpoint a b)) -> IORef (Maybe (Checkpoint a b)) -> (Checkpoint a b -> IO ()) -> IO ()
segmentStep seg sourceRef targetRef cb = do
    sourceDat <- readIORef sourceRef
    case sourceDat of
        Nothing -> threadDelay (10^6)
        Just sourceCheck -> do
            path <- generate seg
            (newState, newVal) <- apply seg path (currentState sourceCheck)
            let newCheck = Checkpoint
                    { revPaths = path:revPaths sourceCheck
                    , currentState = newState
                    , value = newVal
                    }
            if newVal > 0
            then do
                cb newCheck
                targetDat <- readIORef targetRef
                case targetDat of
                    Nothing -> do
                        atomicWriteIORef targetRef (Just newCheck)
                    Just targetCheck -> do
                        shouldReplace <- coin (replaceProbability targetCheck newCheck)
                        if shouldReplace
                        then atomicWriteIORef targetRef (Just newCheck)
                        else pure ()
            else
                pure ()
