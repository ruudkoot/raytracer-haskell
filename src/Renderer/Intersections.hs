-- | Calculating the Ray-Object intersections.
--
module Renderer.Intersections where

import Data.Vector 

import Renderer.ConstructiveSolidGeometry
import Renderer.IntersectionInfo
import Renderer.Scene

import Base.BoundingSphere

-- * Intersections 


-- | Calculates the intersections between a 
-- ray and an object.
--
intersect :: Ray -> Object -> Intersections
<<<<<<< HEAD:src/Renderer/Intersections.hs
intersect ray o@(Simple _ _  _  _   ) = intersectObject ray o 
intersect ray (Union      o1 o2 bbox) = ifInSphere ray o1 o2 bbox unionI 
intersect ray (Difference o1 o2 bbox) = ifInSphere ray o1 o2 bbox differenceI 
intersect ray (Intersect  o1 o2 bbox) = ifInSphere ray o1 o2 bbox intersectI

ifInSphere :: Ray -> Object -> Object -> BSphere -> CSG -> Intersections
ifInSphere ray o1 o2 bsphere f = if sphereHit ray bsphere 
                                 then csg f ray o1 o2 
                                 else []

sphereHit :: Ray -> BSphere -> Bool 
sphereHit ray bsphere = not.null $ bSphereIntersect ray bsphere

{-ifInBbox :: Ray -> Object -> Object -> Bbox -> CSG -> Intersections
ifInBbox ray o1 o2 bbox f = if bboxHit ray bbox 
                              then csg f ray o1 o2 
                              else []

bboxHit :: Ray -> Bbox -> Bool 
bboxHit ray bbox = i1 < i2
  where (Interval i1 i2) = bbclip ray bbox
-}
=======
intersect ray o@(Simple _ _ _) = let x = intersectObject ray o in x--if not (null x) then trace (show x) x else x
intersect ray (Union      o1 o2) = csg unionI      ray o1 o2
intersect ray (Difference o1 o2) = csg differenceI ray o1 o2
intersect ray (Intersect  o1 o2) = csg intersectI  ray o1 o2
>>>>>>> 5aa912faa5275ff96ce3ec6f460a39cc1a398c17:src/Renderer/Intersections.hs

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



