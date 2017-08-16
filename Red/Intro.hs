module Red.Intro (doOptimalIntro) where

import Control.Monad
import Data.IORef
import Data.Monoid
import Text.Printf

import HTas.Low
import HTas.Direct

data IntroState
    = Start
    | Credits
    | IntroScene
    | TitleScreen
    | MainMenu
    | Done
    deriving (Eq, Show, Ord)

doOptimalIntro :: GB -> IO ()
doOptimalIntro gb = do
    introState <- newIORef Start
    frameCounter <- newIORef (0 :: Integer)

    setInputGetter gb $ do
        s <- readIORef introState
        pure $ case s of
            Start -> mempty
            -- Red: Credits -> i_Up <> i_B <> i_Select
            Credits -> i_Start
            IntroScene -> i_A
            TitleScreen -> i_Start
            MainMenu -> i_A
            Done -> mempty

    setTraceCallback gb $ \dat -> do
        let addr = trace_PC dat
        frame <- readIORef frameCounter
        curState <- readIORef introState
        case curState of
            Start -> do
                -- Red: 0x589D = PlayShootingStar + 0x13 (call DelayFrames)
                -- Yellow: 0x5A18 = PlayShootingStar + 0x16 (call DelayFrames)
                when (addr == 0x5A18) $ do
                    writeIORef introState Credits
            Credits -> do
                -- Red: 0x42DD = DisplayTitleScreen
                -- Yellow: 0x4147 = DisplayTitleScreen
                when (addr == 0x4147) $ do
                    writeIORef introState TitleScreen
                -- Yellow: 0x582D = PlayIntroScene
                when (addr == 0x582D) $ do
                    writeIORef introState IntroScene
            IntroScene -> do
                -- Red: 0x42DD = DisplayTitleScreen
                -- Yellow: 0x4171 = DisplayTitleScreen
                when (addr == 0x4171) $ do
                    writeIORef introState TitleScreen
            TitleScreen -> do
                -- Red: 0x5AF2 = MainMenu
                -- Yellow: 0x5BA6 = MainMenu
                when (addr == 0x5BA6) $ do
                    writeIORef introState MainMenu
            MainMenu -> do
                -- Red: 0x5D52 = StartNewGame
                -- Yellow: 0x5CD2 = StartNewGame
                when (addr == 0x5CD2) $ do
                    writeIORef introState Done
                -- Red: 0x5BD1 = MainMenu.pressedA = MainMenu + 0xdf (call GBPalWhiteOutWithDelay3)
                -- Yellow: 0x5C83 = MainMenu.pressedA = MainMenu + 0xdd (call GBPalWhiteOutWithDelay3)
                when (addr == 0x5C83) $ do
                    writeIORef introState Done
            Done -> do
                pure ()

    advanceUntilDone gb introState frameCounter
    clearTraceCallback gb

advanceUntilDone :: GB -> IORef IntroState -> IORef Integer -> IO ()
advanceUntilDone gb stateRef frameCounter = do
    s <- readIORef stateRef
    if s == Done
    then pure ()
    else do
        advanceFrame gb
        modifyIORef' frameCounter (+1)
        advanceUntilDone gb stateRef frameCounter
