-- | Calculating the Ray-Object intersections.
--
module Renderer.Intersections where


import Base.Shader (SurfaceCoord, Shader)
import Base.Shape  (Shape(..))

import Data.Maybe  (isJust)
import Data.Vector 
import Data.Transformation

import Renderer.CSG
import Renderer.IntersectionInfo
import Renderer.Scene

-- * Intersections 


-- | Calculates the intersections between a 
-- ray and an object.
--
intersect :: Ray -> Object -> Intersections
intersect ray o@(Simple s tr1 shader) = intersectObject ray o
intersect ray (Union      o1 o2) = csg unionI      ray o1 o2
intersect ray (Difference o1 o2) = csg differenceI ray o1 o2
intersect ray (Intersect  o1 o2) = csg intersectI  ray o1 o2

-- | Helper function for perofrming CSG using the functions defined 
-- in Renderer.CSG
csg :: CSG -> Ray -> Object -> Object -> Intersections
csg f ray o1 o2 = f (iray o1) (iray o2)
  where iray = intersect ray

-- | Instead of heaving separate `hit` functions 
-- that merely calculate whether or not an 
-- object gets hit, we depend on laziness to 
-- pick out that information efficiently from 
-- the intersect functions.
--
hit :: Ray -> Object -> Bool
hit r o = case intersect r o of            
            [] -> False
            rs -> let t = distance (nearest rs) in (t > 0.0 && t < 1.0)



