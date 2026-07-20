{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Main (main) where

import System.Exit (exitFailure, exitSuccess)
import SDL qualified
import Data.IORef (newIORef, readIORef, IORef, writeIORef)
import Foreign.C (CFloat)
import Control.Monad
import Data.Function
import Data.StateVar
import Linear
import Linear.Affine
import qualified SDL.Raw.Timer as SDL
import SDL (KeyboardEventData(keyboardEventKeysym))

main :: IO ()
main = do
  SDL.initialize [SDL.InitVideo, SDL.InitEvents]

  window <- SDL.createWindow "Scrollslike VTT" SDL.defaultWindow
    { SDL.windowInitialSize = V2 800 600
    }


  rdr <- SDL.createRenderer window 0 SDL.defaultRenderer
  runApp window rdr

  putStrLn "shutting down..."
  SDL.quit
  putStrLn "shut down."
  exitSuccess

runApp :: SDL.Window -> SDL.Renderer -> IO ()
runApp _win rdr = do
  shouldQuitRef <- newIORef False
  freq <- SDL.getPerformanceFrequency
  fix \loop -> do
    tickTimeNow <- SDL.getPerformanceCounter
    let timeNow = fromIntegral tickTimeNow / fromIntegral freq
    SDL.pumpEvents
    shouldQuit <- processEvents shouldQuitRef
    unless shouldQuit do
      renderFrame rdr timeNow
      loop


processEvents :: IORef Bool -> IO Bool
processEvents shouldQuitRef = do
  fix \loop -> SDL.pollEvent >>= \case
    Nothing -> pure ()
    Just SDL.Event{eventPayload=ev}
      | SDL.QuitEvent{} <- ev -> writeIORef shouldQuitRef True
      | SDL.KeyboardEvent ke <- ev
      , SDL.ScancodeQ <- ke.keyboardEventKeysym.keysymScancode
      , SDL.Pressed <- ke.keyboardEventKeyMotion -> writeIORef shouldQuitRef True
      | otherwise -> do
        done <- readIORef shouldQuitRef
        unless done loop
  readIORef shouldQuitRef

renderFrame :: SDL.Renderer -> CFloat -> IO ()
renderFrame rdr t = do
  let rect = SDL.Rectangle (P (V2 (100 * cos t + 400) (100 * sin t + 300)))  (V2 50 50)

  SDL.rendererDrawColor rdr $= V4 32 32 64 255
  SDL.clear rdr
  SDL.rendererDrawColor rdr $= V4 255 87 65 255

  SDL.fillRectF rdr rect

  SDL.present rdr
