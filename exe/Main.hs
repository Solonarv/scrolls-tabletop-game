{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Main (main) where

import System.Exit (exitFailure, exitSuccess)
import SDL3 hiding (sin, cos)
import Data.IORef (newIORef, readIORef, IORef, writeIORef)
import Foreign.C (CFloat)
import Control.Monad
import Data.Function

main :: IO ()
main = do
  unfailing "set metadata" $ sdlSetAppMetadata "Scrollslike VTT" "1.0.0" ""
  unfailing "initialize SDL3" $ sdlInit [SDL_INIT_VIDEO, SDL_INIT_EVENTS]

  sdlLog "Initialized subsystems:"
  sdlLog . show =<< sdlWasInit []

  window_ <- sdlCreateWindow "Scrollslike VTT" 800 600 []
  window <- case window_ of
    Nothing -> do
      sdlLog "failed to create window"
      sdlQuit; exitFailure
    Just w -> pure w

  sdlCreateRenderer window Nothing >>= \case
    Nothing -> do
      sdlLog "failed to create default renderer, error:"
      sdlLog . show =<< sdlGetError
      sdlDestroyWindow window
      sdlQuit
      exitFailure
    Just rdr -> do
      name <- sdlGetRendererName rdr
      sdlLog $ "created renderer named: " <> show name
      runApp window rdr

  sdlLog "shutting down..."
  sdlQuit
  sdlLog "shut down."
  exitSuccess

runApp :: SDLWindow -> SDLRenderer -> IO ()
runApp _win rdr = do
  shouldQuitRef <- newIORef False
  freq <- sdlGetPerformanceFrequency
  fix \loop -> do
    tickTimeNow <- sdlGetPerformanceCounter
    let timeNow = fromIntegral tickTimeNow / fromIntegral freq
    sdlPumpEvents
    shouldQuit <- processEvents shouldQuitRef
    unless shouldQuit do
      renderFrame rdr timeNow
      loop


processEvents :: IORef Bool -> IO Bool
processEvents shouldQuitRef = do
  fix \loop -> sdlPollEvent >>= \case
    Nothing -> pure ()
    Just ev
      | SDLEventQuit{} <- ev -> writeIORef shouldQuitRef True
      | SDLEventKeyboard ke <- ev
      , SDL_SCANCODE_Q <- ke.sdlKeyboardScancode
      , ke.sdlKeyboardDown -> writeIORef shouldQuitRef True
      | otherwise -> do
        done <- readIORef shouldQuitRef
        unless done loop
  readIORef shouldQuitRef

renderFrame :: SDLRenderer -> CFloat -> IO ()
renderFrame rdr t = do
  let rect = SDLFRect (100 * cos t + 400) (100 * sin t + 300) 50 50

  sdlSetRenderDrawColor rdr 32 32 64 255
  sdlRenderClear rdr
  sdlSetRenderDrawColor rdr 255 87 65 255

  sdlRenderFillRect rdr (Just rect)

  void $ sdlRenderPresent rdr


unfailing :: String -> IO Bool -> IO ()
unfailing msg act = do
  success <- act
  unless success $ do
    sdlLog ("failed to " <> msg)
    exitFailure
