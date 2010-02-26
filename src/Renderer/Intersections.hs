module Renderer.Intersections where

import Shared.Vector
import Renderer.Datatypes

type Intersection = (Pt3D, Vec3D) -- At point x, in direction y

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
intersection r Sphere   = undefined
intersection r Cone     = undefined
intersection r Plane    = undefined

