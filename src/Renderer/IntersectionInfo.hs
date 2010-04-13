module Renderer.IntersectionInfo where

import Data.Vector
import Data.Transformation
import Data.Range
import Data.Function

import Base.Shader
import Base.Shape

import Postlude

import Renderer.Scene   (Object(..))
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
    } 

instance Eq IntersectionInfo where
    (==) = (==) `on` distance

instance Ord IntersectionInfo where
    (<=) = (<=) `on` distance

type Intersections = Ranges IntersectionInfo

-- | Creates an interseciton between a ray and an object, requires an object and
-- a ray in world coordinates.
intersectObject::Ray -> Object -> Intersections
intersectObject ray obj@(Simple shape tr1 _) = 
    let rayt = transformRay ray tr1
    in case intervals rayt shape of
               Just (t1,t2) -> [(buildIntersection rayt obj t1, buildIntersection rayt obj t2)]
               Nothing      -> []
intersectObject _ _ = error "the impossible happened"


-- | Helper function used by @intersectObject@ to 
-- build the resulting IntersectionInfo. Ray shoudl be a ray
-- in local space
buildIntersection :: Ray -> Object -> Double -> IntersectionInfo
buildIntersection rayt (Simple shape tr1 sh) t = 
    IntersectionInfo 
       { location     = transformPoint tr1 loc --location in world
       , normal       = normalize $ transformVector tr1 (getNormal shape rayt loc) --normal in world
       , distance     = t 
       , textureCoord = uv shape loc
       , shader       = sh
       } 
  where loc = getPosition rayt t --local intersection point
buildIntersection  _   _                    _  = error "the impossible happened"

-- | Returns the nearest @t@.
--
nearest :: Intersections -> Maybe IntersectionInfo
nearest []             = Nothing
nearest ((i1, i2) : _) | distance i1 > 0.0               = Just i1
                       | distance i2 == positiveInfinity = Nothing
                       | otherwise                       = Just i2


