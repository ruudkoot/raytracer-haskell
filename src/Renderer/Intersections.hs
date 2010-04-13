-- | Calculating the Ray-Object intersections.
--
module Renderer.Intersections where

import Data.Vector 
import Data.Glome.Vec (Bbox(..), bbclip, Interval(..))

import Renderer.ConstructiveSolidGeometry
import Renderer.IntersectionInfo
import Renderer.Scene

-- * Intersections 


-- | Calculates the intersections between a 
-- ray and an object.
--
intersect :: Ray -> Object -> Intersections
intersect ray o@(Simple _ _  _  _   ) = intersectObject ray o 
intersect ray (Union      o1 o2 bbox) = ifInBbox ray o1 o2 bbox unionI 
intersect ray (Difference o1 o2 bbox) = ifInBbox ray o1 o2 bbox differenceI 
intersect ray (Intersect  o1 o2 bbox) = ifInBbox ray o1 o2 bbox intersectI

ifInBbox :: Ray -> Object -> Object -> Bbox -> CSG -> Intersections
ifInBbox ray o1 o2 bbox f = if bboxHit ray bbox 
                              then csg f ray o1 o2 
                              else []

bboxHit :: Ray -> Bbox -> Bool 
bboxHit ray bbox = i1 < i2
  where (Interval i1 i2) = bbclip ray bbox

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
hit r o = case nearest (r `intersect` o) of            
            Nothing -> False
            Just near -> let t = distance near in (t > 0.0 && t < 1.0)



