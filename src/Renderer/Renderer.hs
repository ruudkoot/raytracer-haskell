module Renderer.Renderer (render) where
import Data.Maybe

import Data.Colour (Colour(..), Colours, fromColour, toRGB, toColour)
import Data.Vector (Vector3D(..), normalize, toVec3D, (!.!), Ray(..), rDirection, rOrigin, vector3D, mkRay, vmap)
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

-- | High level render function that uses renderScene 
-- when Thread is zero or one; and renderSceneConc otherwise.  
-- This is useful because renderSceneConc has a greater overhead.
--
render :: Threads -> World -> IO () 
render 0 = renderScene 
render 1 = renderScene 
--render n = renderSceneConc n


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
        pixels = map  (\(i,j) -> renderPixel depth i j raymaker world)
                      [(i,j) | i <- [0..h-1], j <- [0..w-1]]
                      `using` parListChunk (w*h `div` 4) rdeepseq


-- | Calculates the colour for a single pixel position 
-- by recursively shooting a ray into the World.
--
renderPixel :: Int -> Int -> Int -> RayMaker -> World -> Colour Int
renderPixel depth x y raymaker world = toRGB $ toColour $ renderPixel' depth (raymaker x y) id
  where 
    object = wObject world
    lights = wLights world
    ambient = fromColour $ (roAmbience.wOptions) world
    renderPixel' :: Int -> Ray -> (Vector3D -> Vector3D) -> Vector3D
    renderPixel' depth ray f = 
      case intersect ray object of 
        Nothing -> f $ toVec3D 0 0 0 -- No intersections. Intensity=0
        Just info ->                 -- An intersection. Find out intensity:
          let surface      = runShader (shader info) $ textureCoord info
              n            = normal info
              reflDir      = vmap (2 * n !.! (rDirection ray) *) n
              reflected    = let origin    = location info
                                 direction = negate (rDirection ray) + reflDir
                                 clearasil = origin + 0.01 * direction -- cures acne
                              in mkRay clearasil direction -- should this ray be transformed?
              lightsv = filter (not.shadowed (location info) object) lights
          in if depth == 0 -- Are we at the bottom of the recursion?
               then f $ toVec3D 0 0 0 -- Yes, return intensity 0 ; or should that be 1???
               else renderPixel' (depth - 1) reflected -- No, shoot another ray..
                      (f . localLighting ambient info lightsv surface ray) -- ..and build 
                      -- up the result in continuation passing style (lighter on memory)
                                    
shadowed:: Vector3D -> Object -> RenderLight -> Bool
shadowed p o (DirectLight l _)     = isJust.intersect (mkShadowRay p l) $ o
shadowed p o (PointLight l _)      = hit (mkShadowRay p l) o
shadowed p o (SpotLight l _ _ _ _) = hit (mkShadowRay p l) o

mkShadowRay::Vector3D->Vector3D->Ray
mkShadowRay p l = let direction = l-p
                      p' = p + 0.01 * direction
                  in mkRay p' direction

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
mkRayMaker x y delta i j = mkRay eye dir
  where eye = vector3D (0, 0, -1)
        dir = vector3D (x - (fromIntegral j + 0.5) * delta,
                        y - (fromIntegral i + 0.5) * delta, 1)
