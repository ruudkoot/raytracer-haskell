-- | Calculating the Ray-Object intersections.
--
module Renderer.Intersections2 where


import Base.Shader (SurfaceCoord, Shader)
import Base.Shape  (Shape(..))

import Data.Maybe  (isJust)
import Data.Vector 
import Data.Matrix

import Renderer.Normals (getNormal)
import Renderer.Scene   (Ray(..), Object(..), transformRay)
import Renderer.UV      (uv)

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

-- | The @interval@ functions return the t's 
-- that solve the following equation:
-- @intersection = eye + t*direction@
--
type Intersections = [Double]


-- * Intersections 


-- | Calculates the intersections between a 
-- ray and an object.
--
intersect :: Ray -> Object -> IntersectionInfoM
intersect ray o@(Simple s m1 m2 shader) = mkInfo (transformRay ray m2) o
intersect ray (Union      o1 o2) = csg unionI      ray o1 o2
intersect ray (Difference o1 o2) = csg differenceI ray o1 o2
intersect ray (Intersect  o1 o2) = csg intersectI  ray o1 o2


-- | Helper function used by @intersect@ to 
-- build the resulting IntersectionInfo.
--
mkInfo :: Ray -> Object -> Maybe IntersectionInfo 
mkInfo ray (Simple shape m1 m2 sh) = 
  if null ints then Nothing
  else Just IntersectionInfo 
       { location     = dropW (m1 !*! loc)
       , normal       = dropW $ m1 !*! (addW (getNormal shape ray (dropW loc)) 0.0) --normal in world
       , distance     = t --not real distance
       , textureCoord = uv shape (dropW loc)
       , shader       = sh
       } 
  where ints = intervals ray shape
        loc  = getPostition ray t --local intersection point
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
-- Cone: x^2 + z^2 = (r^2/h^2) * (y - h)^2
--
--    (px + vx * t) ^ 2 + (pz + vz * t) ^ 2 = (r^2/h^2) * (py + vy * t)^2
--  => (ratio and height is 1)
--    (px + vx * t) ^ 2 + (pz + vz * t) ^ 2 = (py + vy * t)^2
--  => (expand)
--    (px^2 + (vx*t)^2 + 2*(px*vx*t)) + (pz^2 + (vz*t)^2 + 2*(pz*vz*t)) 
--       = py^2 + (vy*t)^2 + 2*(py*vy*t) - 2*py - 2*(vy*t) + 1
--  => (regroup)
--    px^2 + (vx*t)^2 + 2*(px*vx+pz*vz)*t + pz^2 + (vz*t)^2 - 
--       py^2 - (vy*t)^2 - 2*(py*vy*t) + 2*py + 2*(vy*t) - 1 = 0
--  => (regroup)
--    ((vx*t)^2 + (vz*t)^2 - (vy*t)^2) + 2*(px*vx+pz*vz-py*vy-vy)*t + (px^2 + pz^2 - py^2 + 2*py - 1)
--
--
-- Solve with quadratic equation, where
--   a = vx^2 + vz^2 - vy^2
--   b = 2*(px*vx + pz*vz - (py + 1) * vy)
--   c = px^2 + pz^2 - py^2 + 2*py - 1
--
intervals (Ray (Vector4D (px,py,pz,_)) (Vector4D (vx,vy,vz,_))) Cone =
  let a = vx ^ 2 + vz ^ 2 - vy ^ 2
      b = 2 * (px * vx + pz * vz - (py) * vy)
      c = px ^ 2 + pz ^ 2 - py ^ 2
      solveTop s = case (filter (\t -> (py + vy * t) >= 0 && (py + vy * t) <= 1)) $ s of
                        [x] -> [(1 - py)/vy, x]
                        [] -> []
                        _ -> s
  in solveTop $ solveQuadratic a b c
  -- missing: if 0 <= (py + vy * t) <= 1


intervals r Cube     = 
 let (ox,oy,oz,_) = fromVector4D $ rOrigin r
     (dx,dy,dz,_) = fromVector4D $ rDirection r
     calcMinMax o d = let div = 1.0/d
                          t1 = -o*div
                          t2 = (1.0-o)*div
                      in if d >= 0.0 then (t1,t2) else (t2,t1)
     (txl,txh) = calcMinMax ox dx
     (tyl,tyh) = calcMinMax oy dy
     (tzl,tzh) = calcMinMax oz dz
     tmin = max txl $ max tyl tzl
     tmax = min txh $ min tyh tzh
 in if tmin < tmax 
    then [tmin,tmax]
    else []





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






-- * Helper functions


-- | Solves an equation of the form:
--     @ax^2 + bx + c = 0@
--
solveQuadratic :: Double -> Double -> Double -> Intersections
solveQuadratic a b c = 
  case compare discr 0.0 of 
    LT -> [] 
    EQ -> [-b / (2 * a)]
    GT -> [abc (-) `min` abc (+), abc (-) `max` abc (+)]
  where discr  = b ^ 2 - 4 * a * c
        abc op = (-b `op` sqrt discr) / (2 * a)


-- | Instantiates a ray starting on some point 
-- and calculates the ending point given a certain t.
--
getPostition :: Ray -> Double -> Vec4D
getPostition (Ray origin direction) t = origin + fmap (t *) direction



