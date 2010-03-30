module Renderer.Intervals where

import Renderer.Scene
import Data.Vector
import Base.Shape
import Data.List (sort)
type Intervals = Maybe (Double, Double)

intervals :: Ray -> Shape -> Intervals
intervals r s = case intervals' r s of
                 [] -> Nothing
                 [t] -> if t>0.0 then Just (t,t) else Nothing
                 [t1,t2] -> if t2>0.0 then Just (sort2 (t1,t2)) else Nothing
                 ls -> let ts = sort ls in Just (head ls, last ls)
sort2::(Ord a)=>(a,a)->(a,a)
sort2 (x,y) = if x<y then (x,y) else (y,x)

-- | The interval functions return the t's 
-- that solve the following equation:
-- @intersection = eye + t*direction@
--
intervals' :: Ray -> Shape -> [Double]


-- Sphere
intervals' r Sphere =
  let (k, dir) = (rOrigin r, rDirection r)
      a = dot dir
      b = 2.0 * (k !.! dir)
      c =  (dot k - 1.0)
  in solveQuadratic a b c


-- Plane
intervals' r Plane = 
  let (oy, ry) = (getY3D $ rOrigin r, getY3D $ rDirection r)
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
intervals' r Cylinder = 
  let (px, py, pz) = fromVector3D $ rOrigin r
      (vx, vy, vz) = fromVector3D $ rDirection r
      a = vx ^ 2 + vz ^ 2
      b = 2 * (px * vx + pz * vz)
      c = px ^ 2 + pz ^ 2 - 1.0
      topAndBottom = filter (\t -> let r = (px + vx * t)^2 + (pz + vz * t)^2 in 0 <= r && r <= 1) [- py / vy, (1 - py) / vy]
  in topAndBottom ++ (filter (\t -> let y = py + vy * t in 0 <= y && y <= 1) $ solveQuadratic a b c )


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
intervals' r Cone =
  let (px, py, pz) = fromVector3D $ rOrigin r
      (vx, vy, vz) = fromVector3D $ rDirection r
      a = vx ^ 2 + vz ^ 2 - vy ^ 2
      b = 2 * (px * vx + pz * vz - (py) * vy)
      c = px ^ 2 + pz ^ 2 - py ^ 2
      -- If the side is hit on exactly one point, then there must be an intersection with the top too.
      solveTop s = case (filter (\t -> (py + vy * t) >= 0 && (py + vy * t) <= 1)) $ s of
                        [x] -> [(1 - py)/vy, x]
                        [] -> []
                        _ -> s
  in solveTop $ solveQuadratic a b c


intervals' r Cube     = 
 let (ox,oy,oz) = fromVector3D $ rOrigin r
     (dx,dy,dz) = fromVector3D $ rDirection r
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


-- | Solves an equation of the form:
--     @ax^2 + bx + c = 0@
--
solveQuadratic :: Double -> Double -> Double -> [Double]
solveQuadratic a b c = 
  case compare discr 0.0 of 
    LT -> [] 
    EQ -> [-b / (2 * a)]
    GT -> [abc (-), abc (+)]
  where discr  = b ^ 2 - 4 * a * c
        abc op = (-b `op` sqrt discr) / (2 * a)



