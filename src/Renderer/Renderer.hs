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


type RenderResult = Chan (Int, Int, Colour Int)

-- | Used to wait for worker threads. 
-- See the Control.Concurrent documentation
-- for more details. 
--
type Children = MVar [MVar ()]

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
  do  runThreads threads work world `finally` waitForChildren world
  where (w,h) = getDimensions world
        work = [(i, j) | i <- [0..h-1], j <- [0..w-1]]


runThreads :: Threads -> [(Int, Int)] -> World -> IO ()
runThreads n work world = runThreads' n work 
  where runThreads' 0 _ = return () 
        runThreads' 1 w = thread w
        runThreads' i w = do let (wt, w') = splitAt part w
                             thread wt
                             runThreads' (i - 1) w'
        part = (length work) `div` n
        raymaker = getRayMaker world 
        object = wObject world
        thread wrk = do mvar <- newEmptyMVar
                        modifyMVar_ children (\cs -> return $ mvar:cs)
                        forkIO (renderThread raymaker object wrk
                            `finally` putMVar mvar ())
                        return ()
        

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
                                 
  

renderThread :: RayMaker -> Object -> [(Int, Int)] -> IO ()
renderThread _ _ [] = return ()
renderThread raymaker obj ((i,j):work) = 
  do let colour = renderPixel i j raymaker obj
     writeChan result (i, j, colour)
     renderThread raymaker obj work


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

