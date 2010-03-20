module Renderer.Renderer (render) where

import Data.Colour (Colour(..), Colours, toRGB, colour)
import Data.Vector (Vector4D(..), normalize)
import Data.Radians

import Output.Output (toSize)
import Output.PPM (toPPM)

import Base.Light
import Base.Shader

import Renderer.Intersections2
import Renderer.Scene 
import Renderer.Shaders
import Renderer.Lightning


import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Exception (finally)
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


-- | The RenderResult is a list of triples: 
-- [(pixelY, pixelX, colour)]
-- 
type RenderResult = MVar [(Int, Int, Colour Int)]

-- | Used to wait for worker threads. 
-- See the Control.Concurrent documentation
-- for more details. 
--
type Children = MVar [MVar ()]

{-# NOINLINE result #-}
result :: RenderResult 
result = unsafePerformIO $ newMVar []

{-# NOINLINE children #-}
children :: Children 
children = unsafePerformIO $ newMVar []


-- | High level render function that uses renderScene 
-- when Thread is zero or one; and renderSceneConc otherwise.  
-- This is useful because renderSceneConc has a greater overhead.
--
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
        pixels = [renderPixel i j raymaker (wObject world) (wLights world) | 
                  i <- [0..h-1],
                  j <- [0..w-1]]


-- | Calculates the colour for a single pixel position.
--
renderPixel :: Int -> Int -> RayMaker -> Object -> [RenderLight] -> Colour Int
renderPixel x y raymaker object lights
  = let ray  = raymaker x y
        info = intersect ray object
    in if isHit info
       then let texturecoord = textureCoord info
                lalaShader   = getShader object
                surface      = runShader uvShader texturecoord
             in toRGB $ localLightning info
                                       lights   -- visible lights
                                       (runShader lalaShader texturecoord)
                                       -- surface
                                       ray
                                       
       else colour 255 255 0
       -- else if even (x+y)
       --      then colour   0   0   0
       --      else colour 255   0 255

-- | Saves the calculated colours to a PPM file (the 
-- location of which is specified in the GML)
--
saveRendering :: World -> Colours Int -> IO ()
saveRendering world pixels = maybe bad save $ toPPM (toSize w) (toSize h) pixels
  where bad = error "Error: didn't produce a valid PPM image."
        save = writeFile $ roFile (wOptions world)
        (w,h) = getDimensions world


-- | Creates the RayMaker for the given world.
--
getRayMaker :: World -> RayMaker 
getRayMaker world = mkRayMaker x y delta 
  where (w,h) = getDimensions world
        (x,y) = (tan(0.5 * radians fov), x * fromIntegral h / fromIntegral w)
        delta = 2 * x / fromIntegral w
        fov = roFov (wOptions world)


-- | A RayMakerMaker, if you will; but you won't.
--
mkRayMaker :: Double -> Double -> Double -> RayMaker 
mkRayMaker x y delta i j = Ray eye dir
  where eye = Vector4D (0, 0, -5, 0)
        dir = normalize $
              Vector4D (x - (fromIntegral j + 0.5) * delta,
                        y - (fromIntegral i + 0.5) * delta, 1, 1)




-- * Concurrent Rendering
--


-- | Renders the World using the given 
-- number of threads and saves the PPM 
-- output.
--
renderSceneConc :: Threads -> World -> IO ()
renderSceneConc threads world = 
  do modifyMVar_ result (\_ -> return [])
     runThreads threads work world `finally` waitForChildren world
  where (w,h) = getDimensions world
        work = [(i, j) | i <- [0..h-1], j <- [0..w-1]]


-- | Divides the workload (a list of pixel coordinates) 
-- between a given number of threads.
--
runThreads :: Threads -> [(Int, Int)] -> World -> IO ()
runThreads n work world = runThreads' n work 
  where partSize        = length work `div` n
        runThreads' 1 w = newThread world w
        runThreads' i w = do let (wt, w') = splitAt partSize w
                             newThread world wt
                             runThreads' (i - 1) w'


-- | Creates a new lock and spawns a new thread 
-- for the given workload.
--
newThread :: World -> [(Int, Int)] -> IO ()
newThread world work = 
  do let (raymaker, object, lights) = (getRayMaker world, wObject world, wLights world)
     mvar <- newEmptyMVar
     modifyMVar_ children (\cs -> return $ mvar:cs)
     forkIO (renderThread raymaker object lights work `finally` putMVar mvar ())
     return ()
        

-- | This function takes a workload of pixel locations 
-- and calculates their colour; pushing it to the result MVar.
--
renderThread :: RayMaker -> Object -> [RenderLight] -> [(Int, Int)] -> IO ()
renderThread _ _ _ [] = return ()
renderThread raymaker obj lights ((i,j):work) = 
  do let col = renderPixel i j raymaker obj lights
     modifyMVar_ result (\cs -> return $ (i, j, col) : cs)
     renderThread raymaker obj lights work


-- | Waits for the threads to finish, 
-- and then saves the result.
--
waitForChildren :: World -> IO ()
waitForChildren world = do 
  cs <- takeMVar children 
  case cs of 
    []   -> saveResult world
    m:ms -> do putMVar children ms 
               takeMVar m 
               waitForChildren world
-- | Takes the result and saves it.
-- The result MVar is not emptied here, 
-- and instead gets reset when a new image gets 
-- rendered (in renderSceneConc)
--
saveResult :: World -> IO ()
saveResult world = do 
  res <- readMVar result
  saveRendering world (map (\(_,_,c) -> c) $ sort res)

