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
  , normal       = toVec3D 0 0 0 -- TODO!
  , distance     = t
  , textureCoord = uvmap ints $ uv loc
  } 
  where ints = intervals ray shape
        loc  = dropW $ instantiate ray t
        t    = nearest ints



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
intervals (Ray o r) Sphere =
  let (k, dir) = (dropW o, dropW r)
      a = dot dir
      b = 2.0 * (k !.! dir)
      c =  (dot k - 1.0)
  in solveQuadratic a b c


-- Plane
intervals (Ray o r) Plane = 
  let (oy, ry) = (getY4D o, getY4D r)
  in if (oy == 0) || (oy * ry >= 0)
        then []
        else [(- oy / ry, - oy / ry)]


-- | Calculate intersection of a ray and an
-- infinite cylinder.
-- TODO: Should the height be 1? 
--
-- Intersection with side surface:
-- 
-- Ray: p + vt
-- Cylinder: x^2 + z^2 = r^2
--
--   (px + vx * t)^2 + (pz + vz * t)^2 = r^2 
--   (px + vx * t)^2 + (pz + vz * t)^2 = 1 
--   (px^2 + (vx * t)^2 + 2 * (px * vx * t)) + (pz ^ 2 + (vz * t) ^ 2 + 2 * (pz * vz * t) - 1 = 0
--   px^2 + pz^2 + (vx * t)^2 + (vz*t)^2 + 2*(px*vx*t + pz*vz*t) - 1 = 0
--   (vx^2 + vz^2)*t^2 + 2*(px*vx + pz*vz)*t + px^2 + pz^2 - 1 = 0
--
-- Solve with quadratic equation, where
--   a = (vx^2 + vz^2) 
--   b = 2*(px*vx + pz*vz)
--   c = px^2 + pz^2 - 1
--
-- Good source: http://mrl.nyu.edu/~dzorin/intro-graphics/lectures/lecture11/sld002.htm
--
intervals (Ray (Vector4D (px,py,pz,_)) (Vector4D(vx,vy,vz,_))) Cylinder = 
  let a = vx ^ 2 + vz ^ 2
      b = 2 * (px * vx + pz * vz)
      c = px ^ 2 + pz ^ 2 - 1.0
  in solveQuadratic a b c

intervals r Cube     = undefined
intervals r Cone     = undefined






-- * Helper functions


-- | Solves an equation of the form:
--     @ax^2 + bx + c = 0@
--
solveQuadratic :: Double -> Double -> Double -> [(Double, Double)]
solveQuadratic a b c = 
  case compare discr 0.0 of 
    LT -> [] 
    EQ -> let r = -b / (2 * a) in [(r, r)]
    GT -> let sqrd op = (-b `op` sqrt discr) / (2 * a)
              in [(sqrd (-), sqrd (+))]
  where discr = b ^ 2 - 4 * a * c


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


