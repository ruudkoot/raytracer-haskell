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
    , exit         :: Double         -- ^ Distance between eye and exit
    , textureCoord :: SurfaceCoord   -- ^ Unit world coordinates
    , shader       :: Shader         -- ^ The shader to use to calculate the final color
    } deriving (Eq, Show)

type IntersectionInfoM = Maybe IntersectionInfo

-- | Helper function used by @intersect@ to 
-- build the resulting IntersectionInfo.
--
buildIntersection :: Ray -> Object -> IntersectionInfoM
buildIntersection ray (Simple shape tr1 sh) = 
  if null ints || not (any (>0.0) ints) then Nothing
  else Just IntersectionInfo 
       { location     = transformPoint tr1 loc --location in world
       , normal       = transformVector tr1 (getNormal shape rayt loc) --normal in world
       , distance     = t 
       , exit         = e
       , textureCoord = uv shape loc
       , shader       = sh
       } 
  where rayt   = transformRay ray tr1 --transformed ray
        ints   = intervals rayt shape
        loc    = getPostition rayt t --local intersection point
        (t, e) = nearesttwo ints

-- | Returns the nearest @t@.
--
nearest :: Intersections -> Double
nearest = minimum . filter (>0.0)

nearesttwo :: Intersections -> (Double, Double)
nearesttwo = minimumtwo . filter (>0.0)
    
minimumtwo :: Ord a => [a] -> (a,a)
minimumtwo [a] = (a,a)
minimumtwo (x:y:ys) = f ys (min x y, max x y)
  where f    []  res                     = res
        f (x:xs) (min, mmin) | x < min   = f xs (x, min)
                             | x < mmin  = f xs (min, x)
                             | otherwise = f xs (min, mmin)