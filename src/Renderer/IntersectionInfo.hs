module Renderer.IntersectionInfo where

import Data.Vector
import Data.Matrix

import Base.Shader

import Renderer.Normals (getNormal)
import Renderer.Scene   (Ray(..), Object(..), transformRay)
import Renderer.UV      (uv)
import Renderer.Intervals
-- * Datastructures


-- | Intersection functions return this structure 
-- so that the renderer has enough information
-- to continue the calculation.
--
data IntersectionInfo = IntersectionInfo { 
      location     :: Pt3D           -- ^ Real world location.
    , normal       :: Vec3D          -- ^ Real world normal. 
    , distance     :: Double         -- ^ Distance between Intersection and eye.
    , textureCoord :: SurfaceCoord   -- ^ Unit world coordinates
    , shader       :: Shader         -- ^ The shader to use to calculate the final color
    } deriving (Eq, Show)

type IntersectionInfoM = Maybe IntersectionInfo

-- | Helper function used by @intersect@ to 
-- build the resulting IntersectionInfo.
--
buildIntersection :: Ray -> Object -> Maybe IntersectionInfo 
buildIntersection ray (Simple shape m1 m2 sh) = 
  if null ints then Nothing
  else Just IntersectionInfo 
       { location     = dropW (m1 !*! loc) --location in world
       , normal       = dropW $ m1 !*! (addW (getNormal shape rayt (dropW loc)) 0.0) --normal in world
       , distance     = t --not real distance
       , textureCoord = uv shape (dropW loc)
       , shader       = sh
       } 
  where rayt = transformRay ray m2 --transformed ray
        ints = intervals rayt shape
        loc  = getPostition rayt t --local intersection point
        t    = nearest ints

-- | Instantiates a ray starting on some point 
-- and calculates the ending point given a certain t.
--
getPostition :: Ray -> Double -> Vec4D
getPostition (Ray origin direction) t = origin + fmap (t *) direction

-- | Returns the nearest @t@.
--
nearest :: Intersections -> Double 
nearest = minimum 
