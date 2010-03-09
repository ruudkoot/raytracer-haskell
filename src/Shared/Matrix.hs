{-# LANGUAGE MultiParamTypeClasses #-}
-- | General purpose matrix module.
--

module Shared.Matrix where 


import Control.Applicative
import Data.List (intercalate, transpose)
import Shared.Vector (Vector, Vector3D(..), Vector4D(..), (<.>), fromVector)


-- * Matrices


newtype Matrix3D a = Matrix3D (Vector3D a, Vector3D a, Vector3D a) deriving (Eq)
newtype Matrix4D a = Matrix4D (Vector4D a, Vector4D a, Vector4D a, Vector4D a) deriving (Eq)


class (Applicative m) => Matrix m where 
  fromMatrix :: m a -> [[a]]
  zipWithMatrices :: (a -> b -> c) -> m a -> m b -> m c
  zipWithMatrices f m1 m2 = f <$> m1 <*> m2
 

instance Matrix Matrix3D where 
  fromMatrix (Matrix3D (x, y, z)) = map fromVector [x, y, z]

instance Matrix Matrix4D where
  fromMatrix (Matrix4D (x, y, z, a)) = map fromVector [x, y, z, a]

instance Matrix Vector3D where 
  fromMatrix (Vector3D (x, y, z)) = [[x,y,z]]

instance Matrix Vector4D where 
  fromMatrix (Vector4D (x, y, z, w)) = [[x,y,z,w]]


instance Show m => Show (Matrix3D m) where 
  show (Matrix3D (x, y, z)) = concat ["{[", intercalate ", \n  " (map show [ x, y, z]), "]}"]

instance Show m => Show (Matrix4D m) where 
  show (Matrix4D (x, y, z, a)) = concat ["{[", intercalate ", \n  " (map show [x, y, z, a]), "]}"]
  


instance Functor Matrix3D where 
  fmap f (Matrix3D (x, y, z)) = Matrix3D (f <$> x, f <$> y, f <$> z)

instance Functor Matrix4D where 
  fmap f (Matrix4D (x, y, z, a)) = Matrix4D (f <$> x, f <$> y, f <$> z, f <$> a)


instance Applicative Matrix3D where 
  pure x = Matrix3D (pure x, pure x, pure x)
  Matrix3D (fx, fy, fz) <*> Matrix3D (x, y, z) = 
    Matrix3D (fx <*> x, fy <*> y, fz <*> z)

instance Applicative Matrix4D where 
  pure x = Matrix4D (pure x, pure x, pure x, pure x)
  Matrix4D (fx, fy, fz, fa) <*> Matrix4D (x, y, z, a) = 
    Matrix4D (fx <*> x, fy <*> y, fz <*> z, fa <*> a)


-- | Instance declarations for Nums.
-- Note that (*) doesn't do proper matrix multiplication, 
-- but should only be used for scalars.

instance (Num a) => Num (Matrix3D a) where 
  (+) = zipWithMatrices (+)
  (*) = zipWithMatrices (*)
  (-) = zipWithMatrices (-)
  negate = fmap negate 
  abs = fmap abs 
  signum = fmap signum 
  fromInteger = pure . fromInteger

instance (Num a) => Num (Matrix4D a) where 
  (+) = zipWithMatrices (+)
  (*) = zipWithMatrices (*)
  (-) = zipWithMatrices (-)
  negate = fmap negate 
  abs = fmap abs 
  signum = fmap signum 
  fromInteger = pure . fromInteger

-- | Only for scalars.
--
instance (Fractional a) => Fractional (Matrix3D a) where 
  (/) = zipWithMatrices (/)
  recip = fmap recip
  fromRational = pure . fromRational

instance (Fractional a) => Fractional (Matrix4D a) where 
  (/) = zipWithMatrices (/)
  recip = fmap recip
  fromRational = pure . fromRational


-- * Identity Matrices
--
identity3D :: Num a => Matrix3D a
identity3D = Matrix3D(Vector3D(1, 0, 0), 
                      Vector3D(0, 1, 0),
                      Vector3D(0, 0, 1))

identity4D :: Num a => Matrix4D a
identity4D = Matrix4D(Vector4D(1, 0, 0, 0),
                      Vector4D(0, 1, 0, 0),
                      Vector4D(0, 0, 1, 0),
                      Vector4D(0, 0, 0, 1))

diagonal3D :: Num a => (Vector3D a) -> (Matrix3D a)
diagonal3D (Vector3D (x,y,z)) = Matrix3D (Vector3D(x, 0, 0), Vector3D(0, y, 0), Vector3D(0, 0, z))


diagonal4D :: Num a => (Vector4D a) -> (Matrix4D a)
diagonal4D (Vector4D (x,y,z,w)) = Matrix4D (Vector4D(x, 0, 0, 0), Vector4D(0, y, 0, 0), Vector4D(0, 0, z, 0), Vector4D(0, 0, 0, w))

-- * Matrix Operations
--
-- Addition & substraction are handled by the Num instance.
-- Matrix multiplication is done using the (!*!) operator.
--



scaleI :: (Matrix m, Num (m i), Integral i) => i -> m i -> m i
scaleI = (*) . pure

scaleF :: (Matrix m, Num (m f), Fractional f) => f-> m f -> m f
scaleF = (*) . pure


rows :: (Matrix m) => m a -> [[a]]
rows = fromMatrix

columns :: (Matrix m) => m a -> [[a]]
columns = transpose . rows


-- ** Multiplication
-- Matrix multiplication is done using the (!*!) operator, 
-- and is only defined on square matrices and 3d/4d vectors.
--

class Multiplicable a b where 
  (!*!) :: a -> b -> b -- ^ Multiplication


-- | Matrix * Vector
--
instance (Num a) => Multiplicable (Matrix3D a) (Vector3D a) where 
  Matrix3D (mx, my, mz) !*! v = Vector3D (mx <.> v, my <.> v, mz <.> v)

instance (Num a) => Multiplicable (Matrix4D a) (Vector4D a) where 
  Matrix4D (mx, my, mz, mw) !*! v = Vector4D (mx <.> v, my <.> v, mz <.> v, mw <.> v)


-- | Matrix * Matrix
--
instance (Num a) => Multiplicable (Matrix3D a) (Matrix3D a) where 
  m1 !*! m2 = Matrix3D (Vector3D (dot r x, dot r y, dot r z), 
                        Vector3D (dot s x, dot s y, dot s z),
                        Vector3D (dot t x, dot t y, dot t z))
    where (r:s:t:[]) = rows m1
          (x:y:z:[]) = columns m2
          dot a b = sum (zipWith (*) a b)

instance (Num a) => Multiplicable (Matrix4D a) (Matrix4D a) where 
  m1 !*! m2 = Matrix4D (Vector4D (dot r x, dot r y, dot r z, dot r w), 
                        Vector4D (dot s x, dot s y, dot s z, dot s w),
                        Vector4D (dot t x, dot t y, dot t z, dot t w),
                        Vector4D (dot u x, dot u y, dot u z, dot u w))
    where (r:s:t:u:[]) = rows m1
          (x:y:z:w:[]) = columns m2
          dot a b = sum (zipWith (*) a b)


-- ** Determinant and Inverse
--
class Matrix m => SquareMatrix m where 
  -- | Calculates the determinant of a matrix 
  determinant :: Num a => m a -> a
  -- | The adjoint matrix is calculated by 
  -- transposing the rows and columns.
  adjoint     :: m a -> m a
  -- | The cofactor is used to calculate the inverse.
  cofactor    :: Num a => m a -> m a
  -- | Calculates the inverse matrix
  inverse     :: (Fractional a, Fractional (m a)) => m a -> m a
  inverse m = scaleF (1 / determinant m) (adjoint (cofactor m))




-- | Inlined determinant function for 2D and 3D matrices.
-- Useful for laplace expansion and cofactors. Not necessary to 
-- use directly.
--
det2D :: Num a => a -> a -> a -> a -> a
{-# INLINE det2D #-}
det2D a b c d = a * d - b * c

det3D :: Num a => a -> a -> a 
               -> a -> a -> a
               -> a -> a -> a -> a
{-# INLINE det3D #-}
det3D a b c d e f g h i = a * e * i
                        + b * f * g 
                        + c * d * h
                        - a * f * h 
                        - b * d * i 
                        - c * e * g


instance SquareMatrix Matrix3D where 
  determinant (Matrix3D (Vector3D (a, b, c),
                         Vector3D (d, e, f),
                         Vector3D (g, h, i))) = det3D a b c d e f g h i 
  adjoint (Matrix3D (Vector3D (a, b, c),
                     Vector3D (d, e, f),
                     Vector3D (g, h, i))) = Matrix3D (Vector3D (a, d, g), 
                                                      Vector3D (b, e, h), 
                                                      Vector3D (c, f, i))
  cofactor (Matrix3D (Vector3D (a, b, c),
                      Vector3D (d, e, f),
                      Vector3D (g, h, i))) = 
                                     Matrix3D (Vector3D (  det2D e f h i, 
                                                         - det2D d f g i,
                                                           det2D d e g h),
                                               Vector3D (- det2D b c h i, 
                                                           det2D a c g i, 
                                                         - det2D a b g h),
                                               Vector3D (  det2D b c e f, 
                                                         - det2D a c d f, 
                                                           det2D a b d e))


instance SquareMatrix Matrix4D where 
  -- | Uses laplace expansion to calculate 
  -- determinant from minors of the first row.
  --
  determinant (Matrix4D (Vector4D (a, b, c, d), 
                         Vector4D (e, f, g, h), 
                         Vector4D (i, j, k, l),
                         Vector4D (m, n, o, p))) = a * det3D f g h j k l n o p
                                                 - b * det3D e g h i k l m o p
                                                 + c * det3D e f h i j l m n p
                                                 - d * det3D e f g i j k m n o
  adjoint (Matrix4D (Vector4D (a, b, c, d), 
                     Vector4D (e, f, g, h), 
                     Vector4D (i, j, k, l),
                     Vector4D (m, n, o, p))) = Matrix4D (Vector4D (a, e, i, m), 
                                                         Vector4D (b, f, j, n), 
                                                         Vector4D (c, g, k, o), 
                                                         Vector4D (d, h, l, p))
  cofactor (Matrix4D (Vector4D (a, b, c, d), 
                      Vector4D (e, f, g, h), 
                      Vector4D (i, j, k, l),
                      Vector4D (m, n, o, p))) = 
                                Matrix4D (Vector4D (  det3D f g h j k l n o p,
                                                    - det3D e g h i k l m o p,
                                                      det3D e f h i j l m n p, 
                                                    - det3D e f g i j k m n o),
                                          Vector4D (- det3D b c d j k l n o p, 
                                                      det3D a c d i k l m o p,
                                                    - det3D a b d i j l m n p,
                                                      det3D a b c i j k m n o),
                                          Vector4D (  det3D b c d f g h n o p, 
                                                    - det3D a c d e g h m o p,
                                                      det3D a b d e f h m n p,
                                                    - det3D a b c e f g m n o),
                                          Vector4D (- det3D b c d f g h j k l,
                                                      det3D a c d e g h i k l,
                                                    - det3D a b d e f h i j l,
                                                      det3D a b c e f g i j k))
           


