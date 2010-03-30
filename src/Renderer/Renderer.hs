module Renderer.Renderer (renderScene) where

import Base.Shader   (runShader)

import Data.Colour   (Colour(..), Colours, toRGB, toColour)
import Data.Radians  (radians)
import Data.Vector   (toVec3D, (!.!), Ray, rDirection, vector3D, mkRay, vmap)


import Output.Output (toSize)
import Output.PPM    (toPPM)

import Renderer.IntersectionInfo (IntersectionInfo(..),nearest)
import Renderer.Intersections    (intersect)
import Renderer.Lighting         (localLighting)
import Renderer.Scene            (World(..), RenderOptions(..), getDimensions)

import Control.Parallel.Strategies (parListChunk, using, rdeepseq, rpar) 




-- | A RayMaker produces a Ray when given 
-- its pixel coordinates.
--
type RayMaker = Int -> Int -> Ray


-- | Renders the World without using threads.
--
renderScene :: World -> IO ()
renderScene world = saveRendering world pixels
  where raymaker = getRayMaker world
        (w,h) = getDimensions world
        depth = (roDepth.wOptions) world
        pixels = [renderPixel depth i (h-j) raymaker world |  j <- [0..h-1], i <- [0..w-1]]
                 `using` parListChunk (w*h `div` 2) rdeepseq


-- | Calculates the colour for a single pixel position 
-- by recursively shooting a ray into the World.
--
renderPixel :: Int -> Int -> Int -> RayMaker -> World -> Colour Int
renderPixel depth x y raymaker world = toRGB . toColour $ renderPixel' depth (raymaker x y) id
  where 
    renderPixel' depth ray k = 
      case intersect ray (wObject world) of 
        []   -> k $ toVec3D 0 0 0 -- No intersections. Intensity=0
        rs ->                   -- An intersection. Find out intensity:
          let info = nearest rs
              surface   = runShader (shader info) $ textureCoord info
              n         = normal info
              reflDir   = vmap (2 * n !.! (rDirection ray) *) n
              reflected = let origin    = location info
                              direction = negate (rDirection ray) + reflDir
                              clearasil = origin + 0.01 * direction -- cures acne
                          in mkRay clearasil direction -- should this ray be transformed?

          -- Build result up in continuation passing style.
          -- 
          in if depth == 0 
               then k $ toVec3D 0 0 0
               else renderPixel' (depth - 1) reflected 
                      (k . localLighting info world surface ray) 
                                    

-- | Saves the calculated colours to a PPM file (the 
-- location of which is specified in the GML)
--
saveRendering :: World -> Colours Int -> IO ()
saveRendering world pixels = maybe bad save $ toPPM (toSize w) (toSize h) pixels
  where bad = error "Error: didn't produce a valid PPM image."
        save p = putStrLn ("writing result to " ++ roFile (wOptions world)) >> (writeFile (roFile (wOptions world)) p)
        (w,h) = getDimensions world


-- | Creates the RayMaker for the given world.
--
getRayMaker :: World -> RayMaker 
getRayMaker world = mkRayMaker x y dx dy
  where (w,h) = getDimensions world
        (x,y) = (-tan(0.5 * radians fov), x * fromIntegral h / fromIntegral w)
        (dx,dy) = (-2 * x / fromIntegral w, -2 * y / fromIntegral h)
        fov = roFov (wOptions world)


-- | A RayMakerMaker, if you will; but you won't.
--
mkRayMaker :: Double -> Double -> Double-> Double -> RayMaker 
mkRayMaker x y dx dy i j = mkRay eye dir
  where eye = vector3D (0, 0, -1)
        dir = vector3D (x + (fromIntegral i + 0.5) * dx,
                        y + (fromIntegral j + 0.5) * dy, 1)
