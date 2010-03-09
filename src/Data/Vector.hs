-- | Provides general (perhaps too general) data structures 
-- and functions for 2D, 3D and 4D vectors. 
--
module Data.Vector where
  
import Control.Applicative 

-- * Synonyms
type Pt3D  = Vector3D Double
type Vec4D = Vector4D Double


-- * Vectors 


-- | Vectors in its most general form can be used 
-- to fold and zip with. When the instance of Vector 
-- is carrying Numerical values, they can also 
-- be used as Nums. For example: @2 * Vector3D (1,2,3)@
--
class (Applicative v) => Vector v where 
  fromVector     :: v a -> [a]
  zipWithVectors :: (a -> b -> c) -> v a -> v b -> v c
  foldVector     :: ([a] -> b) -> v a -> b

  zipWithVectors f v1 v2 = f <$> v1 <*> v2
  foldVector f           = f . fromVector 


instance Vector Vector2D where
  fromVector (Vector2D (x, y)) = [x, y]

instance Vector Vector3D where
  fromVector (Vector3D (x, y, z)) = [x, y, z]

instance Vector Vector4D where 
  fromVector (Vector4D (x, y, z, w)) = [x, y, z, w]



-- | A general two-dimensional vector.
--
newtype Vector2D a = Vector2D (a, a) deriving Eq

-- | A general three-dimensional vector.
--
newtype Vector3D a = Vector3D (a, a, a) deriving Eq

-- | A general four-dimensional vector.
--
newtype Vector4D a = Vector4D (a, a, a, a) deriving Eq



instance (Show v) => Show (Vector2D v) where 
  show (Vector2D v) = show v

instance (Show v) => Show (Vector3D v) where 
  show (Vector3D v) = show v

instance (Show v) => Show (Vector4D v) where 
  show (Vector4D v) = show v


instance Functor Vector2D where 
  fmap f (Vector2D (x, y)) = Vector2D (f x, f y)

instance Functor Vector3D where 
  fmap f (Vector3D (x, y, z)) = Vector3D (f x, f y, f z)
  
instance Functor Vector4D where 
  fmap f (Vector4D (x, y, z, w)) = Vector4D (f x, f y, f z, f w)



instance Applicative Vector2D where
  pure x = Vector2D (x, x)
  Vector2D (f1, f2) <*> Vector2D (x, y) = Vector2D (f1 x, f2 y)

instance Applicative Vector3D where
  pure x = Vector3D (x, x, x)
  Vector3D (f1, f2, f3) <*> Vector3D (x, y, z) = Vector3D (f1 x, f2 y, f3 z)

instance Applicative Vector4D where 
  pure x = Vector4D (x, x, x, x)
  Vector4D (f1, f2, f3, f4) <*> Vector4D (x, y, z, w) = Vector4D (f1 x, f2 y, f3 z, f4 w)



-- * Out of the Vector* context
--
fromVector2D :: Vector2D a -> (a, a)
fromVector2D (Vector2D a) = a

fromVector3D :: Vector3D a -> (a, a, a)
fromVector3D (Vector3D a) = a

fromVector4D :: Vector4D a -> (a, a, a, a)
fromVector4D (Vector4D a) = a


getX2D :: Vector2D a -> a
getX2D (Vector2D (x, _)) = x

getY2D :: Vector2D a -> a
getY2D (Vector2D (_, y)) = y


getX3D :: Vector3D a -> a
getX3D (Vector3D (x, _, _)) = x

getY3D :: Vector3D a -> a
getY3D (Vector3D (_, y, _)) = y

getZ3D :: Vector3D a -> a
getZ3D (Vector3D (_, _, z)) = z


getX4D :: Vector4D a -> a
getX4D (Vector4D (x, _, _, _)) = x

getY4D :: Vector4D a -> a
getY4D (Vector4D (_, y, _, _)) = y

getZ4D :: Vector4D a -> a
getZ4D (Vector4D (_, _, z, _)) = z

getW4D :: Vector4D a -> a
getW4D (Vector4D (_, _, _, w)) = w


-- * Unit Vectors
--

unitVector2DX :: Num a => Vector2D a
unitVector2DX = Vector2D (1, 0)

unitVector2DY :: Num a => Vector2D a
unitVector2DY = Vector2D (0, 1)

unitVector3DX :: Num a => Vector3D a
unitVector3DX = Vector3D (1, 0, 0)

unitVector3DY :: Num a => Vector3D a
unitVector3DY = Vector3D (0, 1, 0)

unitVector3DZ :: Num a => Vector3D a
unitVector3DZ = Vector3D (0, 0, 1)

unitVector4DX :: Num a => Vector4D a
unitVector4DX = Vector4D (1, 0, 0, 0)

unitVector4DY :: Num a => Vector4D a
unitVector4DY = Vector4D (0, 1, 0, 0)

unitVector4DZ :: Num a => Vector4D a
unitVector4DZ = Vector4D (0, 0, 1, 0)

unitVector4DW :: Num a => Vector4D a
unitVector4DW = Vector4D (0, 0, 0, 1)


-- | Using Vectors as ordinary Nums (for scaling, etc.)
--
instance (Num a) => Num (Vector2D a) where 
  (+) = zipWithVectors (+)
  (*) = zipWithVectors (*)
  (-) = zipWithVectors (-)
  negate = fmap negate 
  abs    = fmap abs 
  signum = fmap signum 
  fromInteger = pure . fromInteger

instance (Num a) => Num (Vector3D a) where 
  (+) = zipWithVectors (+)
  (*) = zipWithVectors (*)
  (-) = zipWithVectors (-)
  negate = fmap negate 
  abs    = fmap abs 
  signum = fmap signum 
  fromInteger = pure . fromInteger


instance (Num a) => Num (Vector4D a) where 
  (+) = zipWithVectors (+)
  (*) = zipWithVectors (*)
  (-) = zipWithVectors (-)
  negate = fmap negate 
  abs    = fmap abs 
  signum = fmap signum 
  fromInteger = pure . fromInteger


instance (Fractional a) => Fractional (Vector2D a) where 
  (/) = zipWithVectors (/)
  recip = fmap recip
  fromRational = pure . fromRational
  
instance (Fractional a) => Fractional (Vector3D a) where 
  (/) = zipWithVectors (/)
  recip = fmap recip
  fromRational = pure . fromRational

instance (Fractional a) => Fractional (Vector4D a) where 
  (/) = zipWithVectors (/)
  recip = fmap recip
  fromRational = pure . fromRational



-- * Vector Operations


-- | The dot product is an operation that takes two equal-length Vectors 
-- of numbers  and returns a single number obtained by multiplying 
-- corresponding entries and adding up those products. 
--
(<.>) :: (Vector v, Num a, Num (v a)) => v a -> v a -> a
v1 <.> v2 = foldVector sum (v1 * v2)

-- | The cross product is a binary operation on two 3D vectors 
-- that results in another vector which is perpendicular to 
-- the plane containing the two input vectors.
--
cross :: Num a => Vector3D a -> Vector3D a -> Vector3D a
cross (Vector3D (x1, y1, z1)) (Vector3D (x2, y2, z2)) = 
  Vector3D (y1 * z2 - z1 * y2,
            z1 * x2 - x1 * z2,
            x1 * y2 - y1 * z2)


-- | Calculates the length or magnitude of the given Vector.
--
magnitude :: (Vector v, Floating a, Num (v a)) => v a -> a
magnitude v = sqrt (v <.> v)

-- | Calculates the length or magnitude squared of the given Vector.
--
magnitudeSquared :: (Vector v, Floating a, Num (v a)) => v a -> a
magnitudeSquared v = v <.> v

-- | A version of magnitude for Integrals. 
-- Probably not needed.
--
magnitude' :: (Vector v, Integral a, Num (v a), Num (v Double)) => v a -> Double
magnitude' = magnitude . fmap fromIntegral


-- | Normalize the length of a vector to @1.0@.
--
normalize :: (Vector v, Floating a, Num (v a)) => v a -> v a 
normalize v1 = pure (1 / magnitude v1) * v1

-- | Sum values of a vector.
--
sumVector :: (Vector v, Floating a, Num (v a)) => v a -> a
sumVector = foldVector sum

-- | Downgrade 4D vector to 3D.
--
dropW :: Vector4D a -> Vector3D a
dropW (Vector4D (x, y, z, _)) = Vector3D (x, y, z)

