-- | Calculating the Ray-Object intersections.
--
module Renderer.Intersections2 where


import Base.Shader
import Base.Shape

import Data.Vector

import Renderer.Normals
import Renderer.Scene
import Renderer.UV


import Data.Maybe (isJust)

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
type CSG = IntersectionInfoM -> IntersectionInfoM -> IntersectionInfoM

-- | The interval functions return the t's 
-- that solve the following equation:
-- @intersection = eye + t*direction@
--
type Intersections = [Double]


-- * Intersections 


-- | Calculates the intersections between a 
-- ray and an object.
--
intersect :: Ray -> Object -> IntersectionInfoM
intersect ray (Simple Sphere   m1 m2 shader) = mkInfo (transformRay ray m2) shader Sphere uvSphere 
intersect ray (Simple Plane    m1 m2 shader) = mkInfo (transformRay ray m2) shader Plane  uvPlane
intersect ray (Simple Cube     m1 m2 shader) = mkInfo (transformRay ray m2) shader Cube   uvCube
intersect ray (Simple Cylinder m1 m2 shader) = mkInfo (transformRay ray m2) shader Cylinder uvCylinder
intersect ray (Simple Cone     m1 m2 shader) = mkInfo (transformRay ray m2) shader Cone   uvCone
intersect ray (Union      o1 o2) = csg unionI      ray o1 o2
intersect ray (Difference o1 o2) = csg differenceI ray o1 o2
intersect ray (Intersect  o1 o2) = csg intersectI  ray o1 o2

-- | Helper function used by @intersect@ to 
-- build the resulting IntersectionInfo.
--
mkInfo :: Ray -> Shader -> Shape -> UVMapper -> Maybe IntersectionInfo 
mkInfo ray sh shape uv = 
  if null ints then Nothing
  else Just IntersectionInfo 
       { location     = loc
       , normal       = getNormal shape ray loc 
       , distance     = t
       , textureCoord = uv loc
       , shader       = sh
       } 
  where ints = intervals ray shape
        loc  = dropW $ getPostition ray t
        t    = nearest ints


-- | Helper combinator for doing CSG functions.
--
csg :: CSG -> Ray -> Object -> Object -> IntersectionInfoM
csg f ray o1 o2 = f (iray o1) (iray o2)
  where iray = intersect ray



-- | Returns the nearest @t@.
--
nearest :: Intersections -> Double 
nearest = minimum 


-- | Instead of heaving separate `hit` functions 
-- that merely calculate whether or not an 
-- object gets hit, we depend on laziness to 
-- pick out that information efficiently from 
-- the intersect functions.
--
hit :: Ray -> Object -> Bool
hit r = isJust . intersect r 




-- * Intervals 


-- | The interval functions return the t's 
-- that solve the following equation:
-- @intersection = eye + t*direction@
--
intervals :: Ray -> Shape -> Intersections


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
        else [- oy / ry]


-- | Calculate intersection of a ray and an
-- cylinder of height one.
--
-- 
-- Ray: p + vt
-- Cylinder: x^2 + z^2 = r^2
--
--
--     (px + vx * t)^2 + (pz + vz * t)^2 = r^2 
--   => (radius is 1)
--     (px + vx * t)^2 + (pz + vz * t)^2 - 1 = 0
--   => (expand)
--     (px^2 + (vx*t)^2 + 2*(px*vx*t)) + (pz^2 + (vz*t)^2 + 2*(pz*vz*t) - 1 = 0
--   => (regroup)
--     (px^2 + pz^2 + (vx * t)^2 + (vz*t)^2 + 2*(px*vx*t + pz*vz*t) - 1 = 0
--   => (regroup)
--     (vx^2 + vz^2)*t^2 + 2*(px*vx + pz*vz)*t + px^2 + pz^2 - 1 = 0
--
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
  in filter (\t -> let y = py + vy * t in 0 <= y && y <= 1) $ solveQuadratic a b c 


-- | Intersection of a ray with a cone of 
-- ratio 1 and height 1.
--
-- Ray: p + vt
-- Cone: x^2 + y^2 = (r^2/h^2) * (y - h)^2
--
--    (px + vx * t) ^ 2 + (py + vy * t) ^ 2 = (r^2/h^2) * (py + vy * t - h)^2
--  => (ratio and height is 1)
--    (px + vx * t) ^ 2 + (py + vy * t) ^ 2 = (py + vy * t - 1)^2
--  => (expand)
--    (px^2 + (vx*t)^2 + 2*(px*vx*t)) + (py^2 + (vy*t)^2 + 2*(py*vy*t)) 
--       = py^2 + (vy*t)^2 + 2*(py*vy*t) - 2*py - 2*(vy*t) + 1
--  => (regroup)
--    px^2 + (vx*t)^2 + 2*(px*vx+py*vy)*t + py^2 + (vy*t)^2 - 
--       py^2 - (vy*t)^2 - 2*(py*vy*t) + 2*py + 2*(vy*t) - 1 = 0
--  => (cleanup)
--    px^2 + (vx*t)^2 + 2*(px*vx+py*vy)*t - 2*(py*vy*t) + 2*py + 2*(vy*t) - 1 = 0
--  => (regroup)
--    (vx^2)*t^2 + 2*(px*vx+py*vy)*t + 2(-(py*vy*t) + (vy* t)) + 2*py - 1 = 0
--  => (regroup)
--    (vx^2)*t^2 + 2*(px*vx+py*vy)*t + 2*(-(py*vy) + vy)*t + 2*py - 1 = 0
--  => (regroup)
--    (vx^2)*t^2 + 2*(px*vx+py*vy-(py*vy)+vy)*t + 2*py - 1 = 0
--  => (cleanup)
--    (vx^2)*t^2 + 2*(px*vx+vy)*t + 2*py - 1 = 0
--
--
-- Solve with quadratic equation, where
--   a = vx^2
--   b = 2*(px*vx + vy)
--   c = 2*py - 1
--
intervals (Ray (Vector4D (px,py,_,_)) (Vector4D (vx,vy,_,_))) Cone =
  let a = vx ^ 2
      b = 2 * (px * vx + vy)
      c = 2 * py - 1
  in solveQuadratic a b c
  -- missing: if 0 <= (py + vy * t) <= 1


intervals r Cube     = undefined





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
-- Only if A is hit. 
--
differenceI :: CSG
differenceI (Just i) Nothing  = Just i
differenceI _        _        = Nothing 


-- | Pick the nearest intersection.
--
pickNearest :: IntersectionInfo -> IntersectionInfo -> IntersectionInfo 
pickNearest i j = if distance i <= distance j then i else j






-- * Helper functions


-- | Solves an equation of the form:
--     @ax^2 + bx + c = 0@
--
solveQuadratic :: Double -> Double -> Double -> Intersections
solveQuadratic a b c = 
  case compare discr 0.0 of 
    LT -> [] 
    EQ -> [-b / (2 * a)]
    GT -> [abc (-), abc (+)]
  where discr  = b ^ 2 - 4 * a * c
        abc op = (-b `op` sqrt discr) / (2 * a)


-- | Instantiates a ray starting on some point 
-- and calculates the ending point given a certain t.
--
getPostition :: Ray -> Double -> Vec4D
getPostition (Ray origin direction) t = origin + fmap (t *) direction



