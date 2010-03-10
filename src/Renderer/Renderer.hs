-- * Renderer 
--
module Renderer.Renderer (renderScene) where

import Data.Colour (Colour(..))
import Data.Vector (Vector4D(..))

import Renderer.Intersections (hit')
import Renderer.Scene (World(..), Ray(..), RenderOptions(..))

import Output.Output (Size(..))
import Output.PPM (toPPM)

type Threads = Int

renderScene :: Threads -> World -> IO ()
renderScene threads world = maybe bad elseSave $ toPPM (Size $ fromIntegral w) (Size $ fromIntegral h)
                            [if hit' (ray i j) (wObject world)
                             then Colour (255,255,255)
                             else Colour (0,0,0)
                             | i <- [0..h-1],
                               j <- [0..w-1]]
  where 
        ray i j = Ray eye (Vector4D (x - (fromIntegral j + 0.5) * delta,
                                     y - (fromIntegral i + 0.5) * delta, 1, 1))
        delta = 2 * x / fromIntegral w
        (w,h) = (roWidth (wOptions world), roHeight (wOptions world))
        (x, y) = (tan(0.5 * fov), x * fromIntegral h / fromIntegral w)
        fov = roFov (wOptions world)
        eye = Vector4D (0,0,-1,1)
        bad = error "Error: didn't produce a valid PPM image."
        elseSave = writeFile $ roFile (wOptions world)

