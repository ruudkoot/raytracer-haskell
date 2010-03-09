module Renderer.Renderer where

import Output.PPM
import Shared.Vector
import Shared.Colour
import Shared.Matrix
import Shared.RenderBase

import Output.Output

import Renderer.Intersections

renderScene :: ObjectTree a -> Width -> Height -> Double -> Colours Int
renderScene t w h fov = [if hit' (ray i j) t
                         then Colour (255,255,255)
                         else Colour (0,0,0)
                         | i <- [0..fromSize w-1],
                           j <- [0..fromSize h-1]]
  where ray i j = Ray eye
                      (Vector4D (x - (fromInteger j + 0.5) * delta,
                                 y - (fromInteger i + 0.5) * delta, 1, 1))
        x = tan(0.5 * fov)
        y = x * (fromInteger (fromSize h) / fromInteger (fromSize w))
        delta = 2 * x / fromInteger (fromSize w)
        eye = Vector4D (0,0,-1,1)
        
renderTest = maybe bad save (makePPM scene)
   where bad = putStrLn "errorz"
         save = writeFile "output.ppm"
         makePPM = Output.PPM.toPPM (Size 800) (Size 800)
         scene = renderScene sphere (Size 800) (Size 800) 1
         sphere = Shared.RenderBase.RSimple Cone m minv ()
         m = Shared.Matrix.Matrix4D ( (Shared.Vector.Vector4D (0.1, 0, 0, 0))
                                    , (Shared.Vector.Vector4D (0, 0.1, 0, 0.2))
                                    , (Shared.Vector.Vector4D (0, 0, 0.1, 0))
                                    , (Shared.Vector.Vector4D (0, 0, 0, 1))
                                    )
         minv = inverse m