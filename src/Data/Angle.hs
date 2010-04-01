{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Angle where

newtype Radians = Radians { radians :: Double } deriving (Show, Eq, Ord, Num)
newtype Degrees = Degrees { degrees :: Double } deriving (Show, Eq, Ord, Num)
newtype Grad    = Grad    { grads   :: Double } deriving (Show, Eq, Ord, Num)

class Angle a where
    toRadians :: a -> Radians
    toRadians a = Radians (degrees (toDegrees a) * (pi / 180))
    toDegrees :: a -> Degrees
    toDegrees a = Degrees (radians (toRadians a) * (180 / pi))
    
instance Angle Radians where
    toRadians = Radians . radians
    
instance Angle Degrees where
    toDegrees = Degrees . degrees
    
instance Angle Grad where
    toDegrees = Degrees . (360 / 400 * ) . grads

