module Renderer.Renderer where

import Data.Colour
import Data.Vector
import Data.Matrix

import Base.Shape

import Renderer.Intersections
import Renderer.Scene

import Output.Output
import Output.PPM



renderScene :: World -> IO ()
renderScene world = maybe bad elseSave $ Output.PPM.toPPM (Size $ fromIntegral w) (Size $ fromIntegral h)
                            [if hit' (ray i j) (wObject world)
                             then Colour (255,255,255)
                             else Colour (0,0,0)
                             | i <- [0..h-1],
                               j <- [0..w-1]]
  where ray i j = Ray eye
                      (Vector4D (x - (fromIntegral j + 0.5) * delta,
                                 y - (fromIntegral i + 0.5) * delta, 1, 1))
        x = tan(0.5 * fov)
        y = x * fromIntegral h / fromIntegral w
        delta = 2 * x / fromIntegral w
        eye = Vector4D (0,0,-1,1)
        w = roWidth (wOptions world)
        h = roHeight (wOptions world)
        fov = roFov (wOptions world)
        bad = error "errorz"
        elseSave = writeFile $ roFile (wOptions world)
