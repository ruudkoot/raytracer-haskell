module Renderer.Renderer (renderScene) where

import Data.Colour (Colour(..))
import Data.Vector (Vector4D(..))

import Output.Output (toSize)
import Output.PPM (toPPM)

import Renderer.Intersections (hit')
import Renderer.Scene (World(..), Ray(..), RenderOptions(..), Object(..))

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan 
import Control.Concurrent.MVar
import Control.Exception (finally)


-- | The number of threads to use in 
-- the rendering process.
--
type Threads = Int

-- | A RayMaker produces a Ray when given 
-- its pixel coordinates.
--
type RayMaker = Int -> Int -> Ray


type WorkLoad = Chan (Int, Int)
type RenderResult = Chan (Int, Int, Colour Int)

-- | Used to wait for worker threads. 
-- See the Control.Concurrent documentation
-- for more details. 
--
type Children = MVar [MVar ()]


-- | Renders the World, but uses no threads.
--
renderScene :: World -> IO ()
renderScene threads world = maybe bad save $ toPPM (toSize w) (toSize h) pixels
  where raymaker = getRayMaker world w h
        (w,h) = (roWidth (wOptions world), roHeight (wOptions world))
        bad = error "Error: didn't produce a valid PPM image."
        save = writeFile $ roFile (wOptions world)
        pixels = [renderPixel i j raymaker (wObject world) | 
                  i <- [0..h-1],
                  j <- [0..w-1]]


-- | Renders the World using the given 
-- number of threads and saves the PPM 
-- output.
--
renderSceneConc :: Threads -> World -> IO ()
renderSceneConc threads world = 
  do work <- newChan :: IO WorkLoad
     result <- newChan :: IO RenderResult
     children <- newMVar [] :: IO (MVar [MVar ()])
     writeList2Chan work [(i, j) | i <- [0..h-1], 
                                   j <- [0..w-1]]
     runThreads threads children work result world


runThreads :: Threads -> Children -> WorkLoad -> RenderResult -> World -> IO ()
runThreads 0 children work res world = waitForChildren children res world
runThreads n children work res world = 
  do mvar <- newEmptyMVar :: IO (MVar ())
     childs <- takeMVar children 
     putMVar children (mvar:childs)
     forkIO (renderThread work res `finally` putMVar mvar ())
     runThreads (n -1) children work res world


waitForChildren :: Children -> RenderResult -> World -> IO ()
waitForChildren children res world = do 
  cs <- takeMVar children 
  case cs of 
    []   -> saveResult res world
    m:ms -> do 
      putMVar children ms 
      takeMVar m 
      waitForChildren children res world
 

saveResult :: RenderResult -> World -> IO ()
saveResult res world = undefined
  

renderThread :: WorkLoad -> RenderResult -> IO()
renderThread work res = return ()

renderPixel :: Int -> Int -> RayMaker -> Object -> Colour Int
renderPixel x y ray object = if hit' (ray x y) object then white else black 
  where (white, black) = (Colour (255,255,255), Colour(0,0,0))


getRayMaker :: World -> Int -> Int -> RayMaker 
getRayMaker world w h = mkRayMaker x y delta
  where delta = 2 * x / fromIntegral w
        (x,y) = (tan(0.5 * fov), x * fromIntegral h / fromIntegral w)
        fov = roFov (wOptions world)


mkRayMaker :: Double -> Double -> Double -> RayMaker 
mkRayMaker x y delta i j = Ray eye dir
  where eye = Vector4D (0, 0, -1, 1)
        dir = Vector4D (x - (fromIntegral j + 0.5) * delta,
                        y - (fromIntegral i + 0.5) * delta, 1, 1)

