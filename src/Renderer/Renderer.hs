module Renderer.Renderer where

import Output.PPM
import Shared.Vector
import Shared.Colour
import Shared.Matrix
import Output.Output
import Renderer.Datatypes
import Renderer.Intersections

renderScene :: ObjectTree a -> Width -> Height -> Double -> Colours Int
renderScene (RSimple o m) w h fov = [if hit (ray i j) o
                                     then Colour (255,255,255)
                                     else Colour (0,0,0)
                                     | i <- [0..fromSize w-1],
                                       j <- [0..fromSize h-1]]
  where ray i j = Ray (minv !*! Vector3D (0,0,-1))
                      (minv !*! Vector3D (x - (fromInteger j + 0.5) * delta,
                                        y - (fromInteger i + 0.5) * delta, 1))
        x = tan(0.5 * fov)
        y = tan(0.5 * fov) * (fromInteger (fromSize h)
                           /  fromInteger (fromSize w))
        delta = 2 * tan(0.5 * fov) / fromInteger (fromSize w)
        minv = inverse m