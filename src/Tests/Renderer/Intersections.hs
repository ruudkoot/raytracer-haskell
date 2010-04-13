module Tests.Renderer.Intersections where

import Test.QuickCheck

import Renderer.Intersections
import Renderer.Scene

import Tests.Data.Vector
import Data.Vector
import Data.Transformation
import Data.Maybe

import Base.Shape
import Base.Shape.Cylinder
import Base.Shape.Sphere
import Base.Shape.Plane

import qualified Data.Glome.Vec as   G (Ray(..))

margin :: Double
margin = 0.0001

prop_cylinderIntersect :: Ray -> Property
prop_cylinderIntersect (G.Ray o d) = (not.null) intersections && magnitudeSquared d > 0 ==> all (isOnCylinder . getPosition ray) intersections
   where isOnCylinder v = isOnTop v || isOnBottom v || isOnSide v
         isOnTop v      = abs (getY3D v) - 1.0 < margin
         isOnBottom v   = abs (getY3D v) < margin
         isOnSide v     = abs (sqrt (getX3D v * getX3D v + getZ3D v * getZ3D v) - 1.0) < margin
         ray            = mkRay o d
         intersections  = (\(x,y)->[x,y]).fromJust $ intervals ray Cylinder
         
prop_sphereIntersect :: Ray -> Property
prop_sphereIntersect (G.Ray o d) = (not.null) intersections && magnitudeSquared d > 0 ==> all (isOnSphere . getPosition ray) intersections
  where isOnSphere v   = abs (magnitudeSquared v - 1.0) < margin        
        ray            = mkRay o d
        intersections  = (\(x,y)->[x,y]).fromJust $ intervals ray Sphere
        
prop_planeIntersect :: Ray -> Property
prop_planeIntersect (G.Ray o d) = (not.null) intersections && magnitudeSquared d > 0 ==> all (isOnPlane . getPosition ray) intersections
  where isOnPlane v    = abs (getY3D v) < margin
        ray            = mkRay o d
        intersections  = (\(x,y)->[x,y]).fromJust $ intervals ray Plane

