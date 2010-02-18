{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

module Shared.Vector where
  
import Control.Applicative 


newtype Vector2D a = Vector2D (a, a) deriving (Show, Eq)
newtype Vector3D a = Vector3D (a, a, a) deriving (Show, Eq)
newtype Vector4D a = Vector4D (a, a, a, a) deriving (Show, Eq)


fromVector2D :: Vector2D a -> (a, a)
fromVector2D (Vector2D a) = a

fromVector3D :: Vector3D a -> (a, a, a)
fromVector3D (Vector3D a) = a

fromVector4D :: Vector4D a -> (a, a, a, a)
fromVector4D (Vector4D a) = a


instance Functor Vector2D where 
  fmap f (Vector2D (x, y)) = Vector2D (f x, f y)

instance Functor Vector3D where 
  fmap f (Vector3D (x, y, z)) = Vector3D (f x, f y, f z)
  
instance Functor Vector4D where 
  fmap f (Vector4D (x, y, z, a)) = Vector4D (f x, f y, f z, f a)

instance Applicative Vector2D where
  pure x = Vector2D (x, x)
  Vector2D (f1, f2) <*> Vector2D (x, y) = Vector2D (f1 x, f2 y)

instance Applicative Vector3D where
  pure x = Vector3D (x, x, x)
  Vector3D (f1, f2, f3) <*> Vector3D (x, y, z) = Vector3D (f1 x, f2 y, f3 z)

instance Applicative Vector4D where 
  pure x = Vector4D (x, x, x, x)
  Vector4D (f1, f2, f3, f4) <*> Vector4D (x, y, z, a) = Vector4D (f1 x, f2 y, f3 z, f4 a)

instance Vector Vector2D where
  fromVector (Vector2D (x, y)) = [x, y]

instance Vector Vector3D where
  fromVector (Vector3D (x, y, z)) = [x, y, z]

instance Vector Vector4D where 
  fromVector (Vector4D (x, y, z, a)) = [x, y, z, a]


class (Applicative v) => Vector v where 
  fromVector :: v a -> [a]
  zipWithVectors :: (a -> b -> c) -> v a -> v b -> v c
  zipWithVectors f v1 v2 = f <$> v1 <*> v2
  foldVector :: ([a] -> b) -> v a -> b
  foldVector f = f . fromVector 


-- | Warning! Some serious type hackery ahead..
-- This makes all Vectors instance of Num if the types allow it (i.e. 
-- if the Vector v is carrying around a type of class Num).
-- This enables some nice syntax for scalars, addition, etc.
-- I welcome a more elegant solution (without the need for UndecidableInstances), 
-- but this is the only way I got it to work without copy-pasting
-- for every new instance of Vector.
--
instance (Num a, Vector v, Show (v a), Eq (v a)) => Num (v a) where 
  (+) = zipWithVectors (+)
  (*) = zipWithVectors (*)
  (-) = zipWithVectors (-)
  negate = fmap negate 
  abs    = fmap abs 
  signum = fmap signum 
  fromInteger = pure . fromInteger


-- | These class constraints are also pretty awful.
-- We probably need to make Vector2D and Vector3D 
-- classes with a Num constraint to begin with.

dotProduct :: (Vector v, Num a, Num (v a)) => v a -> v a -> a
dotProduct v1 v2 = foldVector sum (v1 * v2)

distance :: (Vector v, Floating a, Num (v a)) => v a -> a
distance v = sqrt(dotProduct v v)
