{-# LANGUAGE FunctionalDependencies #-}

module Shared.Vector where
  
import Control.Applicative 


newtype Vector2D a = Vector2D (a, a) deriving (Show, Eq)
newtype Vector3D a = Vector3D (a, a, a) deriving (Show, Eq)
newtype Point3D  a = Point3D  (a, a, a) -- ^ why?


fromVector2D :: Vector2D a -> (a, a)
fromVector2D (Vector2D a) = a

fromVector3D :: Vector3D a -> (a, a, a)
fromVector3D (Vector3D a) = a



instance Functor Vector2D where 
  fmap f (Vector2D (x, y)) = Vector2D (f x, f y)

instance Functor Vector3D where 
  fmap f (Vector3D (x, y, z)) = Vector3D (f x, f y, f z)
  
instance Applicative Vector2D where
  pure x = Vector2D (x, x)
  Vector2D (f1, f2) <*> Vector2D (x, y) = Vector2D (f1 x, f2 y)

instance Applicative Vector3D where
  pure x = Vector3D (x, x, x)
  Vector3D (f1, f2, f3) <*> Vector3D (x, y, z) = Vector3D (f1 x, f2 y, f3 z)



instance Vector Vector2D where
instance Vector Vector3D where


class Applicative v => Vector v where 
  zipWithVectors :: (a -> b -> c) -> v a -> v b -> v c
  zipWithVectors f v1 v2 = f <$> v1 <*> v2

  

instance Num a => Num (Vector2D a) where 
  (+) = zipWithVectors (+)
  (*) = zipWithVectors (*)
  (-) = zipWithVectors (-)
  negate = fmap negate 
  abs    = fmap abs 
  signum = fmap signum 
  fromInteger = pure . fromInteger


instance Num a => Num (Vector3D a) where 
  (+) = zipWithVectors (+)
  (*) = zipWithVectors (*)
  (-) = zipWithVectors (-)
  negate = fmap negate 
  abs    = fmap abs 
  signum = fmap signum 
  fromInteger = pure . fromInteger
