module Renderer.Renderer (render) where

import Data.Colour (Colour(..), Colours)
import Data.Vector (Vector4D(..))

import Output.Output (toSize)
import Output.PPM (toPPM)

import Renderer.Intersections (hit')
import Renderer.Scene 


import Control.Concurrent (forkIO, yield)
import Control.Concurrent.Chan 
import Control.Concurrent.MVar
import Control.Exception (finally)
import Control.Monad (unless)
import Data.List (sort)
import System.IO.Unsafe


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

{-# NOINLINE work #-}
work :: WorkLoad 
work = unsafePerformIO newChan

{-# NOINLINE result #-}
result :: RenderResult 
result = unsafePerformIO newChan

{-# NOINLINE children #-}
children :: Children 
children = unsafePerformIO $ newMVar []


render :: Threads -> World -> IO () 
render 0 = renderScene 
render 1 = renderScene 
render n = renderSceneConc n


-- | Renders the World without using threads.
--
renderScene :: World -> IO ()
renderScene world = saveRendering world pixels
  where raymaker = getRayMaker world
        (w,h) = getDimensions world
        pixels = [renderPixel i j raymaker (wObject world) | 
                  i <- [0..h-1],
                  j <- [0..w-1]]


saveRendering :: World -> Colours Int -> IO ()
saveRendering world pixels = maybe bad save $ toPPM (toSize w) (toSize h) pixels
  where bad = error "Error: didn't produce a valid PPM image."
        save = writeFile $ roFile (wOptions world)
        (w,h) = getDimensions world

-- | Renders the World using the given 
-- number of threads and saves the PPM 
-- output.
--
renderSceneConc :: Threads -> World -> IO ()
renderSceneConc threads world = 
  do writeList2Chan work [(i, j) | i <- [0..h-1], j <- [0..w-1]] 
     runThreads threads world `finally` waitForChildren world
  where (w,h) = getDimensions world


runThreads :: Threads -> World -> IO ()
runThreads 0 _     = return () 
runThreads n world = 
  do mvar <- newEmptyMVar
     modifyMVar_ children (\cs -> return $ mvar:cs)
     forkIO (renderThread (getRayMaker world) (wObject world) 
              `finally` putMVar mvar ())
     runThreads (n - 1) world


waitForChildren :: World -> IO ()
waitForChildren world = do 
  cs <- takeMVar children 
  case cs of 
    []   -> saveResult world
    m:ms -> do putMVar children ms 
               takeMVar m 
               waitForChildren world


saveResult :: World -> IO ()
saveResult world = do 
  res <- mkList []
  saveRendering world (map (\(_,_,c) -> c) $ sort res)
  where mkList r = do empty <- isEmptyChan result
                      if empty then return r 
                               else do c <- readChan result
                                       mkList (c:r)
                                 
  

renderThread :: RayMaker -> Object -> IO ()
renderThread raymaker obj = do 
  empty <- isEmptyChan work 
  unless empty $ 
    do (i,j) <- readChan work -- blocks indefinitely, if other thread made chan empty
       colour <- return $ renderPixel i j raymaker obj
       writeChan result (i, j, colour)
       renderThread raymaker obj 


renderPixel :: Int -> Int -> RayMaker -> Object -> Colour Int
renderPixel x y ray object = if hit' (ray x y) object then white else black 
  where (white, black) = (Colour (255,255,255), Colour(0,0,0))


getRayMaker :: World -> RayMaker 
getRayMaker world = mkRayMaker x y delta
  where (w,h) = getDimensions world
        (x,y) = (tan(0.5 * fov), x * fromIntegral h / fromIntegral w)
        delta = 2 * x / fromIntegral w
        fov = roFov (wOptions world)


mkRayMaker :: Double -> Double -> Double -> RayMaker 
mkRayMaker x y delta i j = Ray eye dir
  where eye = Vector4D (0, 0, -1, 1)
        dir = Vector4D (x - (fromIntegral j + 0.5) * delta,
                        y - (fromIntegral i + 0.5) * delta, 1, 1)

