{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Colours are represented as a triple (r, g, b) which has been 
-- made polymorphic. This enables us to use Colour Int for 
-- displaying purposes and Colour Double for calculations.
-- 
module Data.Colour where 

import Data.Vector (Vector3D(..), fromVector3D, fromVector)


-- | Colour is a triple of three values 'r', 'g' and 'b'
-- 
newtype Colour a = Colour (Vector3D a) deriving (Eq, Ord, Show, Functor)
type Colours a = [Colour a]


-- * Synonyms
type ColourD = Colour Double


-- | Abstracted Colour constructor.
--
colour :: a -> a -> a -> Colour a
colour r g b = Colour (Vector3D (r, g, b)) 


-- | The (r, g, b) values in Colour as [r, g, b].
--
colourToList :: Colour a -> [a]
colourToList (Colour v) = fromVector v


-- | Clamps the values in Colour, given a minimum and 
-- maximum value.
--
clampColour :: Ord a => a -> a -> Colour a -> Colour a
clampColour mi ma (Colour vec) = Colour (fmap c vec)
  where c v = max (min v ma) mi


-- | Same as 'colourToList . clampColour min max'.
--
clampedList :: Ord a => a -> a -> Colour a -> [a]
clampedList mi ma = colourToList . clampColour mi ma


toRGB :: ColourD -> Colour Int
toRGB (Colour v) = Colour (fmap (round . (255.0*)) v)


toColour :: Vector3D a -> Colour a
toColour = Colour 

fromColour :: Colour a -> Vector3D a 
fromColour (Colour v) = v
