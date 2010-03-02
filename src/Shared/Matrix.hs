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
  show (Matrix3D (x, y, z)) = concat ["[", intercalate ", \n " (map show [ x, y, z]), "]"]

instance Show m => Show (Matrix4D m) where 
  show (Matrix4D (x, y, z, a)) = concat ["[", intercalate ", \n " (map show [x, y, z, a]), "]"]
  


instance Functor Matrix3D where 
  fmap f (Matrix3D (x, y, z)) = Matrix3D (f <$> x, f <$> y, f <$> z)

instance Functor Matrix4D where 
  fmap f (Matrix4D (x, y, z, a)) = Matrix4D (f <$> x, f <$> y, f <$> z, f <$> a)


instance Applicative Matrix3D where 
  pure x = Matrix3D (pure x, pure x, pure x)
  Matrix3D (fx, fy, fz) <*> Matrix3D (x, y, z) = Matrix3D (fx <*> x, fy <*> y, fz <*> z)

instance Applicative Matrix4D where 
  pure x = Matrix4D (pure x, pure x, pure x, pure x)
  Matrix4D (fx, fy, fz, fa) <*> Matrix4D (x, y, z, a) = Matrix4D (fx <*> x, fy <*> y, fz <*> z, fa <*> a)


-- | Instance declarations for Nums.
-- Note that (*) doesn't do proper matrix multiplication, 
-- but should be used for scalars.

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

instance (Fractional a) => Fractional (Matrix3D a) where 
  (/) = zipWithMatrices (/)
  recip = fmap recip
  fromRational = pure . fromRational

instance (Fractional a) => Fractional (Matrix4D a) where 
  (/) = zipWithMatrices (/)
  recip = fmap recip
  fromRational = pure . fromRational



-- * Matrix Operations. 
--
-- Addition & substraction are handled by the Num instance.
--



scaleI :: (Matrix m, Num (m i), Integral i) => i -> m i -> m i
scaleI = (*) . pure

scaleF :: (Matrix m, Num (m f), Fractional f) => f-> m f -> m f
scaleF = (*) . pure


rows :: (Matrix m) => m a -> [[a]]
rows = fromMatrix

columns :: (Matrix m) => m a -> [[a]]
columns = transpose . rows

class MultSquare a b where 
  mult :: a -> b -> b

instance Num a => MultSquare (Matrix4D a) (Vector4D a) where 
  mult (Matrix4D (mx, my, mz, mw)) v = Vector4D (mx <.> v, my <.> v, mz <.> v, mw <.> v)

{-

mult :: (Matrix m, Num a) => m a => m a => m a
inverse :: (Matrix m, Fractional a) => m a -> m a
determinant :: (Matrix m, Fractional a) => m a -> a


--}
