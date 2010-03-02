module Renderer.Intersections where

import Shared.Vector
import Renderer.Datatypes
import Data.Ord

type Intersection = (Double, Double) -- Enters at x, leaves at y

hit :: Ray -> Shape -> Bool
hit r Cube     = undefined
hit r Cylinder = undefined
hit r Sphere   = let dir = rDirection r
                     --constant throughout rendering, optimization?
                     k = rOrigin r
                     --optimization: can be assumed 1 when dir is a unit vector
                     a = dir <.> dir
                     b = 2.0 * (k <.> dir)
                     --also constant because k is constant
                     c = (k <.> k) - 1.0 
                     --4*a*c can be constant, see above
                     d = b*b - 4.0*a*c
                  in case compare d 0.0 of
                        LT -> False
                        _  -> True
                        
hit r Cone     = undefined
                 -- The 'unit' plane is the XZ plane, so we only have to consider the Y direction.
                 -- If oy == 0, we're in the plane, otherwise we hit it if we move 'downwards' on Y
                 -- when we start 'above' the plane, or vice versa.
hit r Plane    = let oy = unitVector3DY <.> rOrigin r -- ugly and inefficient way to extract y-value
                     dy = unitVector3DY <.> rDirection r
                 in (oy == 0) || (oy * dy < 0)


intersection :: Ray -> Shape -> [Intersection]
intersection r Cube     = undefined
intersection r Cylinder = undefined

--Formula from http://www.devmaster.net/wiki/Ray-sphere_intersection, took out the k = (o-c) constant with c = (0,0,0).

intersection r Sphere   = let dir = rDirection r
                              --constant throughout rendering, optimization?
                              k = rOrigin r 
                              --optimization: can be assumed 1 when dir is a unit vector
                              a = dir <.> dir 
                              b = 2.0 * (k <.> dir)
                              --also constant because k is constant
                              c = (k <.> k) - 1.0 
                              --4*a*c can be constant, see above
                              d = b*b - 4.0*a*c
                          in case compare d 0.0 of
                                EQ -> [(-b/(2*a),-b/(2*a))]
                                LT -> []
                                _  -> let sqrd = sqrt d
                                      in [((-b-sqrd)/(2*a), (-b+sqrd)/(2*a))]

intersection r Cone     = undefined
intersection r Plane    = let oy = unitVector3DY <.> rOrigin r -- ugly and inefficient way to extract y-value
                              dy = unitVector3DY <.> rDirection r
                          in if oy == 0 then [] else if oy * dy < 0 then [(- oy / dy, - oy / dy)] else []