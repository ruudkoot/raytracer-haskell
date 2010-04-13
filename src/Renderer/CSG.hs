module Renderer.CSG where
import Renderer.IntersectionInfo
import Data.Range

type CSG = Intersections -> Intersections -> Intersections

-- * CSG 

-- | A + B 
-- If A OR B is hit. If both are hit 
-- the nearest intersection is choosen.
--
unionI :: CSG
unionI i1 i2 = removeBehind $ unionRanges i1 i2

-- | A & B 
-- If A and B are both hit.
--
intersectI :: CSG
intersectI i1 i2 = removeBehind $ intersectRanges i1 i2

-- | A - B   
-- Only if A is hit and B is not hit.
--
differenceI :: CSG
differenceI i1 i2 = removeBehind $ diffRanges i1 i2

-- | Remove intersections that are behind the camera.
-- Intersections can be discarded if the second part of the interval is behind the camera (<0).
removeBehind :: Intersections -> Intersections
removeBehind = dropWhile ((<0.0).distance.snd)
