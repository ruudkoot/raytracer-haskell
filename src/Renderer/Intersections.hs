module Renderer.Intersections where

import Shared.Vector
import Renderer.Datatypes
import Data.Ord

type Intersection = (Double, Double) -- Enters at x, leaves at y

hit :: Ray -> Shape -> Bool
hit r Cube     = undefined
hit r Cylinder = let dir = Vector3D (1, 0, 1) * rDirection r
                     k = Vector3D (1, 0, 1) * rOrigin r
                     a = dir <.> dir
                     b = 2.0 * (k <.> dir)
                     c = (k <.> k) - 1.0
                     d = b*b - 4.0*a*c
                     sqrd = sqrt d
                     x1 = (-b + sqrd)/(2*a)
                     x2 = (-b - sqrd)/(2*a)
                     sideHit = (x1 <= 1 && x1 >= 0) || (x2 <= 1 && x2 >= 0)
                     oy = getY3D $ rOrigin r
                     dy = getY3D $ rDirection r
                     t = -oy / dy
                     bottomHit = magnitudeSquared (k + dir * (Vector3D (t, t, t))) < 1
                     t' = (1 - oy) / dy
                     topHit = magnitudeSquared (k + dir * (Vector3D (t', t', t'))) < 1
                 in (bottomHit || topHit || sideHit)
hit r Sphere   = let dir = rDirection r
                     k = rOrigin r
                     a = dir <.> dir
                     b = 2.0 * (k <.> dir)
                     c = (k <.> k) - 1.0
                     d = b*b - 4.0*a*c
                  in d >= 0
                        
hit r Cone     = undefined
                 -- The 'unit' plane is the XZ plane, so we only have to consider the Y direction.
                 -- If oy == 0, we're in the plane, otherwise we hit it if we move 'downwards' on Y
                 -- when we start 'above' the plane, or vice versa.
hit r Plane    = let oy = getY3D $ rOrigin r -- ugly and inefficient way to extract y-value
                     dy = getY3D $ rDirection r
                 in (oy == 0) || (oy * dy < 0)


intersection :: Ray -> Shape -> [Intersection]
intersection r Cube     = undefined
intersection r Cylinder = undefined

--Formula from http://www.devmaster.net/wiki/Ray-sphere_intersection, took out the k = (o-c) constant with c = (0,0,0).

intersection r Sphere   = let dir = rDirection r
                              k = rOrigin r
                              a = dir <.> dir
                              b = 2.0 * (k <.> dir)
                              c = (k <.> k) - 1.0
                              d = b*b - 4.0*a*c
                          in case compare d 0.0 of
                                EQ -> [(-b/(2*a),-b/(2*a))]
                                LT -> []
                                _  -> let sqrd = sqrt d
                                      in [((-b-sqrd)/(2*a), (-b+sqrd)/(2*a))]

intersection r Cone     = undefined
intersection r Plane    = let oy = getY3D $ rOrigin r -- ugly and inefficient way to extract y-value
                              dy = getY3D $ rDirection r
                          in if (oy == 0) || (oy * dy >= 0) then [] else [(- oy / dy, - oy / dy)]