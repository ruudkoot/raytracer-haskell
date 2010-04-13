-- | Calculating the Ray-Object intersections.
--
module Renderer.Intersections where

import Data.Vector 

import Renderer.CSG
import Renderer.IntersectionInfo
import Renderer.Scene

-- * Intersections 


-- | Calculates the intersections between a 
-- ray and an object.
--
intersect :: Ray -> Object -> Intersections
intersect ray o@(Simple _ _ _) = let x = intersectObject ray o in x--if not (null x) then trace (show x) x else x
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
hit r o = case nearest (intersect r o) of            
            Nothing -> False
            Just near -> let t = distance near in (t > 0.0 && t < 1.0)



