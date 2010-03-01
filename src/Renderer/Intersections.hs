module Renderer.Intersections where

import Shared.Vector
import Renderer.Datatypes
import Data.Ord

type Intersection = (Pt3D, Vec3D) -- At point x, in direction y
--isn't direction already defined in the ray? or is this the normal direction?
--Should return distance in t? instead of point?

hit :: Ray -> Shape -> Bool
hit r Cube     = undefined
hit r Cylinder = undefined
hit r Sphere   = undefined
hit r Cone     = undefined
hit r Plane    = let vd = sumVector $ rDirection r
                     v0 = sumVector $ rOrigin r
                     t  = v0 / vd
                 in  vd < 0 && (t >= 0 && t <= 1)


intersection :: Ray -> Shape -> [Intersection]
intersection r Cube     = undefined
intersection r Cylinder = undefined

--Formula from http://www.devmaster.net/wiki/Ray-sphere_intersection, took out the k = (o-c) constant with c = (0,0,0).

intersection r Sphere   = undefined{-
                            let dir = rDirection r
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
                                EQ -> [(-b/(2*a), Vector3D (0,0,0))]
                                LT -> []
                                _  -> let sqrd = sqrt d
                                      in [((-b-sqrd)/(2*a), Vector3D (0,0,0))
                                         ,((-b+sqrd)/(2*a), Vector3D (0,0,0))]

-}

intersection r Cone     = undefined
intersection r Plane    = undefined

