-- | Provides general (perhaps too general) data structures 
-- and functions for 2D, 3D and 4D vectors. 
--
{-# LANGUAGE UnboxedTuples, BangPatterns #-}
module Data.Vector where
  
import Control.Applicative 
import Control.Monad (liftM2, liftM3, liftM4)
import Test.QuickCheck

import Data.Glome.Vec hiding (x, y, z, vmap, Ray(..), abs)
import qualified Data.Glome.Vec as G (x, y, z, vmap, Ray(..), abs)

-- * Synonyms
type Pt3D  = Vec
type Vec3D = Vec
type Vector = Vec
type Vector3D = Vec

-- * Rays
type Ray = G.Ray

-- * Using unboxed types is annoying
mkRay :: Vec3D -> Vec3D -> Ray
mkRay !o !d = G.Ray o d

rOrigin = G.origin
rDirection = G.dir

transformRay r m = xfm_ray m r

-- * Classes Vec needs to adhere to but doesn't.

zipWithVectors f v1 v2 = listToVec $ zipWith f (fromVector v1) (fromVector v2)
  where listToVec [x, y, z] = vec x y z
foldVector f           = f . fromVector

instance Eq Vec where
  (==) = veq
  
instance Num Vec where 
  (+) = vadd
  (*) = vmul
  (-) = vsub
  negate = vmap negate 
  abs    = G.abs 
  signum = vmap signum 
  fromInteger i = vec (fromInteger i) (fromInteger i) (fromInteger i)
  
instance Fractional Vec where 
  (/) = zipWithVectors (/)
  recip = vmap recip
  fromRational i = vec (fromRational i) (fromRational i) (fromRational i)

-- * Renaming
getX3D = G.x
getY3D = G.y
getZ3D = G.z

vector3D (x, y, z) = vec x y z
toVec3D = vec

fromVector3D v = (G.x v, G.y v, G.z v)
fromVector v = [G.x v, G.y v, G.z v]

normalize = vnorm
magnitude = vlen

vmap = G.vmap

dot v = vdot v v
cross = vcross

(!.!) = vdot

-- | Instantiates a ray starting on some point 
-- and calculates the ending point given a certain t.
--
getPostition :: Ray -> Double -> Vec3D
getPostition r t = vscaleadd (rOrigin r) (rDirection r) t