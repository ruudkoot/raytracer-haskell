{-# LANGUAGE BangPatterns #-} 

-- | Provides general purpose functions for 3D vectors 
-- and Rays. 

module Data.Vector where
  
import           Data.Glome.Vec hiding (x, y, z, vmap, Ray(..), abs, invxfm_ray)
import qualified Data.Glome.Vec as   G (x, y, z, vmap, Ray(..))

-- ** Type Synonyms (for compatiblity with the more general module we had first)
--
type Pt3D     = Vec
type Vec3D    = Vec
type Vector   = Vec
type Vector3D = Vec
type Ray      = G.Ray



-- Instances which _should_ have been in GlomeVec.
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

tupleFromVector :: Vec -> (Flt, Flt, Flt)
tupleFromVector v = (G.x v, G.y v, G.z v)


-- ** Vector Operations
--
normalize :: Vec -> Vec 
normalize = vnorm

magnitude :: Vec -> Flt
magnitude = vlen


-- | Sadly, we cannot define Vec as an instance of Functor, as it has kind *.
-- but this will do.
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

mkRay :: Vec -> Vec -> Ray
mkRay = G.Ray

-- | Ray projection.
--
rOrigin, rDirection :: Ray -> Vec
rOrigin    = G.origin
rDirection = G.dir

-- | Redefined, because glomevec was wrong.
invxfm_ray :: Xfm -> Ray -> Ray
invxfm_ray !xfm !(G.Ray orig dir) =
 G.Ray (invxfm_point xfm orig) (invxfm_vec xfm dir)


transformRay :: Ray -> Xfm -> Ray
transformRay r m = invxfm_ray m r


-- | Instantiates a ray starting on some point 
-- and calculates the ending point given a certain t.
--
getPosition :: Ray -> Double -> Vec3D
getPosition r t = vscaleadd (rOrigin r) (rDirection r) t

