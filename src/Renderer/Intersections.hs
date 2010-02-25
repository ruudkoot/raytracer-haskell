module Renderer.Intersections where

import Renderer.Datatypes
import Renderer.Primitives

type Intersection = (Point3D, Vector3D) -- At point x, in direction y

hit :: Ray -> Shape -> Bool
hit r Cube     = undefined
hit r Cylinder = undefined
hit r Sphere   = undefined
hit r Cone     = undefined
hit r Plane    = undefined

intersection :: Ray -> Shape -> [Intersection]
intersection r Cube     = undefined
intersection r Cylinder = undefined
intersection r Sphere   = undefined
intersection r Cone     = undefined
intersection r Plane    = undefined

