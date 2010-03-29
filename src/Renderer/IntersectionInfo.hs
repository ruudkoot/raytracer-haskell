module Renderer.IntersectionInfo where

import Data.Vector
import Data.Matrix

import Base.Shader

import Renderer.Normals (getNormal)
import Renderer.Scene   (Object(..))
import Renderer.UV      (uv)
import Renderer.Intervals
-- * Datastructures

-- | Intersection functions return this structure 
-- so that the renderer has enough information
-- to continue the calculation.
--
data IntersectionInfo = IntersectionInfo { 
      location     :: Vec3D          -- ^ Real world location.
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
buildIntersection ray (Simple shape tr1 sh) = 
  if null ints || not (any (>0.0) ints) then Nothing
  else Just IntersectionInfo 
       { location     = transformPoint tr1 loc --location in world
       , normal       = transformVector tr1 (getNormal shape rayt loc) --normal in world
       , distance     = t --not real distance
       , textureCoord = uv shape loc
       , shader       = sh
       } 
  where rayt = transformRay ray tr1 --transformed ray
        ints = intervals rayt shape
        loc  = getPostition rayt t --local intersection point
        t    = nearest ints

-- | Returns the nearest @t@.
--
nearest :: Intersections -> Double 
nearest = minimum . filter (>0.0)
