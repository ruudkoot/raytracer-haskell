module Renderer.IntersectionInfo where

import Data.Vector
import Data.Matrix
import Data.Range

import Base.Shader
import Base.Shape

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
    } deriving (Show)

instance Eq IntersectionInfo where
    (==) i1 i2 = distance i1 == distance i2

instance Ord IntersectionInfo where
    (<=) i1 i2 = distance i1 <= distance i2

type Intersections = Ranges IntersectionInfo

-- | Creates an interseciton between a ray and an object, requires an object and
-- a ray in world coordinates.
intersectObject::Ray -> Object -> Intersections
intersectObject ray obj@(Simple shape tr1 _) = 
    let rayt = transformRay ray tr1
    in case intervals rayt shape of
               Just (t1, t2) -> [(buildIntersection rayt obj t1, buildIntersection rayt obj t2)]
               Nothing -> []

-- | Helper function used by @intersectObject@ to 
-- build the resulting IntersectionInfo. Ray shoudl be a ray
-- in local space
buildIntersection :: Ray -> Object -> Double -> IntersectionInfo
buildIntersection rayt (Simple shape tr1 sh) t = 
    IntersectionInfo 
       { location     = transformPoint tr1 loc --location in world
       , normal       = transformVector tr1 (getNormal shape rayt loc) --normal in world
       , distance     = t 
       , textureCoord = uv shape loc
       , shader       = sh
       } 
  where loc = getPostition rayt t --local intersection point

-- | Returns the nearest @t@.
--
nearest :: Intersections -> IntersectionInfo
nearest ((i1,i2):_) = if distance i1 > 0.0 then i1 else i2

{-
nearesttwo :: Intersections -> (Double, Double)
nearesttwo = minimumtwo . filter (>0.0)
    
minimumtwo :: Ord a => [a] -> (a,a)
minimumtwo [a] = (a,a)
minimumtwo (x:y:ys) = f ys (min x y, max x y)
  where f    []  res                     = res
        f (x:xs) (min, mmin) | x < min   = f xs (x, min)
                             | x < mmin  = f xs (min, x)
                             | otherwise = f xs (min, mmin)
-}
