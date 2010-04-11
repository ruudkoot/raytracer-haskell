{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Colours are represented as a triple (r, g, b) which has been 
-- made polymorphic. This enables us to use Colour Int for 
-- displaying purposes and Colour Double for calculations.
-- 
module Data.Colour where 

import Control.Parallel.Strategies
import Data.Vector                 (Vector3D, tupleFromVector, toVec3D)

-- | Colour is a triple of three values 'r', 'g' and 'b'
-- 
data Colour a = Colour !a !a !a deriving (Eq, Ord, Show)
type Colours a = [Colour a]

instance NFData (Colour a) where
  rnf (Colour x y z) = x `seq` y `seq` z `seq` ()

instance Functor Colour where 
  fmap f (Colour r g b) = Colour (f r) (f g) (f b) 


-- * Synonyms
type ColourD = Colour Double


-- | Abstracted Colour constructor.
--
colour :: a -> a -> a -> Colour a
colour = Colour


-- | The (r, g, b) values in Colour as [r, g, b].
--
colourToList :: Colour a -> [a]
colourToList (Colour a b c) = [a, b, c]

-- | Load [r,g,b] into Colour.
--
listToColour :: [a] -> Colour a
listToColour [r,g,b] = Colour r g b
listToColour _       = error "Invalid color"

-- | Adds the value of two colors
--
addColour::(Num a)=>Colour a->Colour a->Colour a
addColour (Colour a1 a2 a3) (Colour b1 b2 b3) = Colour (a1+b1) (a2+b2) (a3+b3)

-- | Clamps the values in Colour, given a minimum and 
-- maximum value.
--
clampColour :: Ord a => a -> a -> Colour a -> Colour a
clampColour mi ma col = fmap c col
  where c v = max (min v ma) mi


-- | Same as 'colourToList . clampColour min max'.
--
clampedList :: Ord a => a -> a -> Colour a -> [a]
clampedList mi ma = colourToList . clampColour mi ma


toRGB :: ColourD -> Colour Int
toRGB = fmap (round . (255.0*))

fromRGB :: Colour Int -> ColourD
fromRGB = fmap (\x -> fromIntegral x/255.0)

fromColour :: ColourD -> Vector3D 
fromColour (Colour r g b) = toVec3D r g b

toColour :: Vector3D -> Colour Double
toColour = (\ (r, g, b) -> Colour r g b) . tupleFromVector
