{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Colours are represented as a triple (r, g, b) which has been 
-- made polymorphic. This enables us to use Colour Int for 
-- displaying purposes and Colour Double for calculations.
-- 
module Data.Colour where 

import Control.DeepSeq             (NFData)
import Control.Parallel.Strategies
import Data.Vector                 (Vector3D(..), fromVector3D, fromVector, toVec3D)

-- | Colour is a triple of three values 'r', 'g' and 'b'
-- 
data Colour a = Colour !a !a !a deriving (Eq, Ord, Show)
type Colours a = [Colour a]

instance NFData (Colour a) where
  rnf (Colour x y z) = x `seq` y `seq` z `seq` ()

instance Functor Colour where
  fmap f (Colour a b c) = Colour (f a) (f b) (f c)

-- * Synonyms
type ColourD = Colour Double


-- | Abstracted Colour constructor.
--
colour :: a -> a -> a -> Colour a
colour r g b = Colour r g b


-- | The (r, g, b) values in Colour as [r, g, b].
--
colourToList :: Colour a -> [a]
colourToList (Colour a b c) = [a, b, c]


-- | Clamps the values in Colour, given a minimum and 
-- maximum value.
--
clampColour :: Ord a => a -> a -> Colour a -> Colour a
clampColour mi ma col = (fmap c col)
  where c v = max (min v ma) mi


-- | Same as 'colourToList . clampColour min max'.
--
clampedList :: Ord a => a -> a -> Colour a -> [a]
clampedList mi ma = colourToList . clampColour mi ma


toRGB :: ColourD -> Colour Int
toRGB col = fmap (round . (255.0*)) col

fromColour :: ColourD -> Vector3D 
fromColour (Colour r g b) = toVec3D r g b

toColour :: Vector3D -> Colour Double
toColour = (\ (r, g, b) -> Colour r g b) . fromVector3D
