{-# LANGUAGE BangPatterns #-} 

-- | Provides general purpose functions for 3D vectors 
-- and Rays. 

module Data.Vector where
  
import           Data.Glome.Vec hiding (x, y, z, vmap, Ray(..), abs, invxfm_ray)
import qualified Data.Glome.Vec as   G (x, y, z, vmap, Ray(..), abs)


-- * Vectors


-- ** Type Synonyms
--
type Pt3D     = Vec
type Vec3D    = Vec
type Vector   = Vec
type Vector3D = Vec
type Ray      = G.Ray



-- Instances which should have been in GlomeVec.
--
instance Eq Vec where
  (==) = veq
  
instance Num Vec where 
  (+) = vadd
  (*) = vmul
  (-) = vsub
  negate = vmap negate 
  abs    = vmap abs 
  signum = vmap signum 
  fromInteger i = vec (fromInteger i) (fromInteger i) (fromInteger i)
  
instance Fractional Vec where 
  (/) = undefined
  recip = vmap recip
  fromRational i = vec (fromRational i) (fromRational i) (fromRational i)



-- ** Vector Construction 
--

vector3D :: (Flt, Flt, Flt) -> Vec
vector3D (x, y, z) = vec x y z

toVec3D :: Flt -> Flt -> Flt -> Vec
toVec3D = vec



-- ** Projection
--

getX3D, getY3D, getZ3D :: Vec -> Flt
getX3D = G.x
getY3D = G.y
getZ3D = G.z

fromVector3D :: Vec -> (Flt, Flt, Flt)
fromVector3D v = (G.x v, G.y v, G.z v)

fromVector :: Vec -> [Flt]
fromVector v = [G.x v, G.y v, G.z v]



-- ** Vector Operations
--
normalize :: Vec -> Vec 
normalize = vnorm

magnitude :: Vec -> Flt
magnitude = vlen

vmap :: (Flt -> Flt) -> Vec -> Vec
vmap = G.vmap

(!.!) :: Vec -> Vec -> Flt
(!.!) = vdot

dot :: Vec -> Flt
dot v = vdot v v

cross :: Vec -> Vec -> Vec
cross = vcross





-- * Rays 
--


-- | Construct a ray out of two vectors.
--
mkRay :: Vec3D -> Vec3D -> Ray
mkRay !o !d = G.Ray o d


-- | Ray projection.
--
rOrigin, rDirection :: Ray -> Vec
rOrigin    = G.origin
rDirection = G.dir


invxfm_ray :: Xfm -> Ray -> Ray
invxfm_ray !xfm !(G.Ray orig dir) =
 G.Ray (invxfm_point xfm orig) (invxfm_vec xfm dir)


transformRay :: Ray -> Xfm -> Ray
transformRay r m = invxfm_ray m r


-- | Instantiates a ray starting on some point 
-- and calculates the ending point given a certain t.
--
getPostition :: Ray -> Double -> Vec3D
getPostition r t = vscaleadd (rOrigin r) (rDirection r) t

