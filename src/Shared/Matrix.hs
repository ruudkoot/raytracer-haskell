{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

-- | General purpose matrix module.
--

module Shared.Matrix where 


import Control.Applicative
import Data.List (intercalate)
import Shared.Vector (Vector, Vector3D, Vector4D, fromVector)

newtype Matrix3 a = Matrix3 (Vector3D a, Vector3D a, Vector3D a) deriving (Eq)
newtype Matrix4 a = Matrix4 (Vector4D a, Vector4D a, Vector4D a, Vector4D a) deriving (Eq)

class (Applicative m) => Matrix m where 
  fromMatrix :: m a -> [[a]]
  zipWithMatrices :: (a -> b -> c) -> m a -> m b -> m c
  zipWithMatrices f m1 m2 = f <$> m1 <*> m2
 

instance Matrix Matrix3 where 
  fromMatrix (Matrix3 (x, y, z)) = map fromVector [x, y, z]

instance Matrix Matrix4 where
  fromMatrix (Matrix4 (x, y, z, a)) = map fromVector [x, y, z, a]


instance Show m => Show (Matrix3 m) where 
  show (Matrix3 (x, y, z)) = concat ["[", intercalate ", \n " (map show [ x, y, z]), "]"]

instance Show m => Show (Matrix4 m) where 
  show (Matrix4 (x, y, z, a)) = concat ["[", intercalate ", \n " (map show [x, y, z, a]), "]"]
  


instance Functor Matrix3 where 
  fmap f (Matrix3 (x, y, z)) = Matrix3 (f <$> x, f <$> y, f <$> z)

instance Functor Matrix4 where 
  fmap f (Matrix4 (x, y, z, a)) = Matrix4 (f <$> x, f <$> y, f <$> z, f <$> a)


instance Applicative Matrix3 where 
  pure x = Matrix3 (pure x, pure x, pure x)
  Matrix3 (fx, fy, fz) <*> Matrix3 (x, y, z) = Matrix3 (fx <*> x, fy <*> y, fz <*> z)

instance Applicative Matrix4 where 
  pure x = Matrix4 (pure x, pure x, pure x, pure x)
  Matrix4 (fx, fy, fz, fa) <*> Matrix4 (x, y, z, a) = Matrix4 (fx <*> x, fy <*> y, fz <*> z, fa <*> a)


-- | Because of the Num instance in Vector, this can't be defined now:
--
--      instance (Matrix m) => Num (m a) where 
--
-- Since instances are matched on the right hand side, 
-- which is Num (m a) here; ie. the same as Num (v a) 
-- in Vector. So now ghc errs on Duplicate Instances.
--
-- Here's a thread explaining it better: 
-- http://old.nabble.com/duplicate-instance-declarations.-Why--td20146171.html
--
-- So this means we need to make Vector2D, Vector3D and Vector4D instances of Num.
--

instance (Num a) => Num (Matrix3 a) where 
  (+) = zipWithMatrices (+)
  (*) = zipWithMatrices (*)
  (-) = zipWithMatrices (-)
  negate = fmap negate 
  abs = fmap abs 
  signum = fmap signum 
  fromInteger = pure . fromInteger

instance (Num a) => Num (Matrix4 a) where 
  (+) = zipWithMatrices (+)
  (*) = zipWithMatrices (*)
  (-) = zipWithMatrices (-)
  negate = fmap negate 
  abs = fmap abs 
  signum = fmap signum 
  fromInteger = pure . fromInteger
