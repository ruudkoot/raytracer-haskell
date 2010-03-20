-- | Calculating the Ray-Object intersections.
--
module Renderer.Intersections2 where


import Base.Shader
import Base.Shape

import Data.Vector

import Renderer.Scene
import Renderer.UV



-- * Datastructures


-- | Intersection functions return this structure 
-- so that the renderer has enough information
-- to continue the calculation.
--
data IntersectionInfo = IntersectionInfo { 
      isHit        :: Bool
    , location     :: Pt3D           -- ^ Real world location.
    , normal       :: Pt3D           -- ^ Real world normal.
    , distance     :: Double         -- ^ Distance between Intersection and eye.
    , textureCoord :: SurfaceCoord   -- ^ Unit world coordinates
    , tees         :: [Intersection]
    } deriving (Eq, Show)


-- | The interval functions return the t's 
-- that solve the following equation:
-- @intersection = eye + t*direction@
--
type Intersection = (Double, Double)



-- * Intersections 


-- | Calculates the intersections between a 
-- ray and an object.
--
intersect :: Ray -> Object -> IntersectionInfo
intersect ray obj@(Simple Sphere   m1 m2 shader) = mkInfo ray Sphere uvSphere
intersect ray obj@(Simple Plane    m1 m2 shader) = mkInfo ray Plane  uvPlane
intersect ray obj@(Simple Cube     m1 m2 shader) = mkInfo ray Cube   uvCube
intersect ray obj@(Simple Cylinder m1 m2 shader) = mkInfo ray Cylinder uvCylinder
intersect _   obj = error $ show obj ++ " are not supported yet."


-- | Helper function used by @intersect@ to 
-- build the resulting IntersectionInfo.
--
mkInfo :: Ray -> Shape -> UVMapper -> IntersectionInfo 
mkInfo ray shape uv = IntersectionInfo 
  { isHit        = not $ null ints
  , location     = loc
  , normal       = toVec3D 0 0 0 -- ^ TODO
  , distance     = nearest ints
  , textureCoord = uvmap ints $ uv loc
  , tees         = ints
  } 
  where ints = intervals ray shape
        loc  = dropW $ instantiate ray (nearest ints)



-- | Returns the nearest @t@.
--
nearest :: [Intersection] -> Double 
nearest = minimum . map (uncurry min) 


-- | Instead of heaving separate `hit` functions 
-- that merely calculate whether or not an 
-- object gets hit, we depend on laziness to 
-- pick out that information efficiently from 
-- the intersect functions.
--
hit :: Ray -> Object -> Bool
hit r o = isHit $ intersect r o




-- * Intervals 


-- | The interval functions return the t's 
-- that solve the following equation:
-- @intersection = eye + t*direction@
--
intervals :: Ray -> Shape -> [Intersection]


-- Sphere
intervals r Sphere   = let dir = dropW $ rDirection r
                           k = dropW $ rOrigin r
                           a = dir !.! dir
                           b = 2.0 * (k !.! dir)
                           c = (k !.! k) - 1.0
                           d = b*b - 4.0*a*c
                       in case compare d 0.0 of
                             EQ -> [(-b/(2*a),-b/(2*a))]
                             LT -> []
                             _  -> let sqrd = sqrt d
                                   in [((-b-sqrd)/(2*a), (-b+sqrd)/(2*a))]


-- Plane
intervals r Plane    = let oy = getY4D $ rOrigin r
                           dy = getY4D $ rDirection r
                       in if (oy == 0) || (oy * dy >= 0)
                           then []
                           else [(- oy / dy, - oy / dy)]


-- Cube

-- Cylinder






-- * Helper functions


-- | Instantiates a ray starting on some point 
-- and calculates the ending point given a certain t.
--
instantiate :: Ray -> Double -> Vec4D
instantiate (Ray origin direction) t = origin + fmap (t *) direction


-- | Does something mysterious and arcane while mumbling
-- profanities. No seriously, what's this for?
--
uvmap :: [Intersection] -> SurfaceCoord -> SurfaceCoord
uvmap [] _ = (0, 0, 0)
uvmap _  a = a


