module NESDL (sdlInit, draw, getKeys, quitHandler) where

import Graphics.UI.SDL as SDL
import Control.Monad (forM_, forever)
import Data.Word
import System.IO.Unsafe (unsafeInterleaveIO)
import Types hiding (Pixel)
import Prelude hiding (Left, Right)

-- Initialize the system.
sdlInit :: IO ()
sdlInit = do
    SDL.init [InitVideo,InitEventthread]
    enableKeyRepeat 500 20
    setVideoMode 256 240 24 [DoubleBuf]
    return ()

-- listens for quit messages
quitHandler :: IO ()
quitHandler = do
    e <- waitEvent
    case e of
        Quit -> quit
        _    -> quitHandler

-- Called once every frame
draw :: [Word32] -> IO ()
draw ps = draw' $ zipWith (\p (x,y) -> (Pixel p,x,y)) ps 
                --  [ (x,y) | y <- [0,2..478], x <- [0,2..510] ] 
                  [ (x,y) | y <- [0..239], x <- [0..255] ] 
  where
    draw' :: [(Pixel, Int, Int)] -> IO ()
    draw' pxs = do
        sur <- getVideoSurface
        forM_ pxs $ \(p,x,y) -> fillRect sur (Just $ Rect x y 1 1) p
        SDL.flip sur

-- Lazy list of inputs
getKeys :: IO [Maybe Input]
getKeys = unsafeInterleaveIO $ do
    ev <- pollEvent
    case ev of
        KeyDown key -> do
            let result = checkKey KeyPress key
            ks <- getKeys
            return (result : ks)
        KeyUp key -> do
            let result = checkKey KeyRelease key
            ks <- getKeys
            return (result : ks)
        _ -> do
            ks <- getKeys
            return (Nothing : ks)

-- Translates a keypress to a maybe input
checkKey t (Keysym k _ _) = case k of
    SDLK_z      -> Just $ Input t P1 A
    SDLK_x      -> Just $ Input t P1 B
    SDLK_c      -> Just $ Input t P1 Start
    SDLK_v      -> Just $ Input t P1 Select
    SDLK_LEFT   -> Just $ Input t P1 Left
    SDLK_UP     -> Just $ Input t P1 Up
    SDLK_DOWN   -> Just $ Input t P1 Down
    SDLK_RIGHT  -> Just $ Input t P1 Right
    SDLK_q      -> Just $ Input t P2 A
    SDLK_e      -> Just $ Input t P2 B
    SDLK_1      -> Just $ Input t P2 Start
    SDLK_2      -> Just $ Input t P2 Select
    SDLK_a      -> Just $ Input t P2 Left
    SDLK_w      -> Just $ Input t P2 Up
    SDLK_s      -> Just $ Input t P2 Down
    SDLK_d      -> Just $ Input t P2 Right
    _           -> Nothing 

