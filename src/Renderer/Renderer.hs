module Renderer.Renderer (render) where
import Data.Maybe

import Data.Colour (Colour(..), Colours, toRGB, fromColour)
import Data.Vector (Vector4D(..), normalize, toVec3D, dropW, addW, (!.!),Pt3D)
import Data.Radians

import Output.Output (toSize)
import Output.PPM (toPPM)

import Base.Light
import Base.Shader

import Renderer.Intersections2
import Renderer.Scene 
import Renderer.Shaders
import Renderer.Lighting
import Renderer.IntersectionInfo

import Control.Parallel            --(par)
import Control.Parallel.Strategies --(rnf, using, parListChunk, rdeepseq)
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Exception (finally)
import Data.List (sort)
import System.IO (hPutStr, stderr)
import System.IO.Unsafe

import Debug.Trace
 

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


-- | Render a progress bar on the screen. Only redraw if necessary.
--
dot i j w h expr = if numberOfDots > previousNumberOfDots
                   then unsafePerformIO $ do putDotsLine; return expr
                   else expr
  where numberOfDots = ((100 * (i * h + j)) `div` (w * h))
        previousNumberOfDots = ((100 * (i * h + j - 1)) `div` (w * h))
        putDotsLine = hPutStr stderr ("\r" ++ (replicate numberOfDots '.') ++
                                                " " ++ show numberOfDots ++ "%")

-- | Renders the World without using threads.
--
renderScene :: World -> IO ()
renderScene world = saveRendering world pixels
  where raymaker = getRayMaker world
        (w,h) = getDimensions world
        depth = (roDepth.wOptions) world
        pixels = map  (\(i,j) -> dot i j h w $ renderPixel depth i j raymaker world)
                      [(i,j) | i <- [0..h-1], j <- [0..w-1]]
                      `using` parListChunk (w*h `div` 4) rdeepseq


-- | Calculates the colour for a single pixel position 
-- by recursively shooting a ray into the World.
--
renderPixel :: Int -> Int -> Int -> RayMaker -> World -> Colour Int
renderPixel depth x y raymaker world = toRGB . Colour $ renderPixel' depth (raymaker x y) id
  where 
    object = wObject world
    lights = wLights world
    ambient = fromColour $ (roAmbience.wOptions) world
    renderPixel' depth ray f = 
      case intersect ray object of 
        Nothing -> f $ toVec3D 0 0 0 -- No intersections. Intensity=0
        Just info ->                 -- An intersection. Find out intensity:
          let surface      = runShader (shader info) $ textureCoord info
              n            = normal info
              reflDir      = fmap (2 * n !.! dropW (rDirection ray) *) n
              reflected    = let origin    = addW (location info) 1
                                 direction = negate (rDirection ray) + addW reflDir 0
                                 clearasil = origin + 0.01 * direction -- cures acne
                              in Ray { rOrigin    = clearasil
                                     , rDirection = direction } -- should this ray be transformed?
              lightsv = filter (not.shadowed (location info) object) lights
          in if depth == 0 -- Are we at the bottom of the recursion?
               then f $ toVec3D 0 0 0 -- Yes, return intensity 0 ; or should that be 1???
               else renderPixel' (depth - 1) reflected -- No, shoot another ray..
                      (f . localLighting ambient info lightsv surface ray) -- ..and build 
                      -- up the result in continuation passing style (lighter on memory)
                                    
shadowed::Pt3D -> Object -> RenderLight -> Bool
shadowed p o (DirectLight l _)     = let p4 = addW p 1.0 
                                     in not.isJust.intersect (Ray p4 (addW (l-p) 0.0)) $ o
shadowed p o (PointLight l _)      = let p4 = addW p 1.0 
                                     in hit (Ray p4 (addW (l-p) 0.0)) $ o
shadowed p o (SpotLight l _ _ _ _) = let p4 = addW p 1.0 
                                     in hit (Ray p4 (addW (l-p) 0.0)) $ o

-- | Saves the calculated colours to a PPM file (the 
-- location of which is specified in the GML)
--
saveRendering :: World -> Colours Int -> IO ()
saveRendering world pixels = maybe bad save $ toPPM (toSize w) (toSize h) pixels
  where bad = error "Error: didn't produce a valid PPM image."
        save = trace ("writing result to " ++ roFile (wOptions world)) writeFile $ roFile (wOptions world)
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
  where eye = Vector4D (0, 0, -1, 1)
        dir = Vector4D (x - (fromIntegral j + 0.5) * delta,
                        y - (fromIntegral i + 0.5) * delta, 1, 0)




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
     forkIO (renderThread raymaker world work `finally` putMVar mvar ())
     return ()
        

-- | This function takes a workload of pixel locations 
-- and calculates their colour; pushing it to the result MVar.
--
renderThread :: RayMaker -> World -> [(Int, Int)] -> IO ()
renderThread _ _  [] = return ()
renderThread raymaker world ((i,j):work) = 
  do let col = renderPixel ((roDepth.wOptions) world) i j raymaker world
     modifyMVar_ result (\cs -> return $ (i, j, col) : cs)
     renderThread raymaker world work


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

