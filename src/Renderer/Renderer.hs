-- * Renderer 
--
module Renderer.Renderer (renderScene) where

import Data.Colour (Colour(..))
import Data.Vector (Vector4D(..))

import Renderer.Intersections (hit')
import Renderer.Scene (World(..), Ray(..), RenderOptions(..), Object(..))

import Output.Output (toSize)
import Output.PPM (toPPM)

type Threads = Int
type RayMaker = Int -> Int -> Ray


renderScene :: Threads -> World -> IO ()
renderScene threads world = maybe bad save $ toPPM (toSize w) (toSize h) pixels
  where raymaker = getRayMaker world w h
        (w,h) = (roWidth (wOptions world), roHeight (wOptions world))
        bad = error "Error: didn't produce a valid PPM image."
        save = writeFile $ roFile (wOptions world)
        pixels = [renderPixel i j raymaker (wObject world) | 
                   i <- [0..h-1], 
                   j <- [0..w-1]]


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

