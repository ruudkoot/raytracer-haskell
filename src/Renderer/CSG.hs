module Renderer.CSG where
import Data.Vector
import Renderer.Scene
import Renderer.IntersectionInfo

type CSG = IntersectionInfoM -> IntersectionInfoM -> IntersectionInfoM

-- * CSG 

-- | A + B 
-- If A OR B is hit. If both are hit 
-- the nearest intersection is choosen.
--
unionI :: CSG
unionI (Just i) (Just j) = Just $ pickNearest i j
unionI (Just i) Nothing  = Just i
unionI Nothing  (Just i) = Just i
unionI Nothing  Nothing  = Nothing 

-- | A & B 
-- If A and B are both hit.
--
intersectI :: CSG
intersectI (Just i) (Just j) = Just $ pickNearest i j
intersectI _        _        = Nothing 

-- | A - B   
-- Only if A is hit and B is not hit.
--
differenceI :: CSG
differenceI (Just i) Nothing  = Just i
differenceI _        _        = Nothing 


-- | Pick the nearest intersection.
--
pickNearest :: IntersectionInfo -> IntersectionInfo -> IntersectionInfo 
pickNearest i j = if distance i <= distance j then i else j
