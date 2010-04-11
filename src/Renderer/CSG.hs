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
unionI = removeBehind . unionRanges 

-- | A & B 
-- If A and B are both hit.
--
intersectI :: CSG
intersectI = removeBehind .intersectRanges

-- | A - B   
-- Only if A is hit and B is not hit.
--
differenceI :: CSG
differenceI = removeBehind.diffRanges


removeBehind :: Intersections -> Intersections
removeBehind = dropWhile ((<0.0).distance.snd)
