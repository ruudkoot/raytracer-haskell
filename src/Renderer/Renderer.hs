module Renderer.Renderer where

import Data.Colour
import Data.Vector
import Data.Matrix

import Base.Shape

import Renderer.Intersections
import Renderer.Scene

import Output.Output
import Output.PPM



renderScene :: Object a -> Width -> Height -> Double -> Colours Int
renderScene t w h fov = [if hit' (ray i j) t
                         then Colour (255,255,255)
                         else Colour (0,0,0)
                         | i <- [0..fromSize h-1],
                           j <- [0..fromSize w-1]]
  where ray i j = Ray eye
                      (Vector4D (x - (fromInteger j + 0.5) * delta,
                                 y - (fromInteger i + 0.5) * delta, 1, 1))
        x = tan(0.5 * fov)
        y = x * (fromInteger (fromSize h) / fromInteger (fromSize w))
        delta = 2 * x / fromInteger (fromSize w)
        eye = Vector4D (0,0,-1,1)
        
renderTest something = maybe bad save (makePPM scene)
   where bad = putStrLn "errorz"
         save = writeFile "output.ppm"
         makePPM = Output.PPM.toPPM (Size 800) (Size 400)
         scene = renderScene sphere (Size 800) (Size 400) 1
         sphere = Intersect (Union (Simple Cone m minv ()) (Simple Sphere m2 m2inv ())) (Simple Cylinder m3 m3inv ())
         m = Data.Matrix.Matrix4D ( (Data.Vector.Vector4D (0.2, 0, 0, 0))
                                  , (Data.Vector.Vector4D (0, 0.2, 0, 0.1))
                                  , (Data.Vector.Vector4D (0, 0, 0.2, 0))
                                  , (Data.Vector.Vector4D (0, 0, 0, 1))
                                  )
         minv = inverse m
         m2 = Data.Matrix.Matrix4D ( (Data.Vector.Vector4D (0.2, 0, 0, 0))
                                   , (Data.Vector.Vector4D (0, 0.2, 0, -0.1))
                                   , (Data.Vector.Vector4D (0, 0, 0.2, 0))
                                   , (Data.Vector.Vector4D (0, 0, 0, 1))
                                   )
         m2inv = inverse m2
         m3 = Data.Matrix.Matrix4D ( (Data.Vector.Vector4D (0.1, 0, 0, 0))
                                   , (Data.Vector.Vector4D (0, 10, 0, -0.2))
                                   , (Data.Vector.Vector4D (0, 0, 0.1, 0))
                                   , (Data.Vector.Vector4D (0, 0, 0, 1))
                                   )
         m3inv = inverse m3
