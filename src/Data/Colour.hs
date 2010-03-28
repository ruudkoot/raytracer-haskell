{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Colours are represented as a triple (r, g, b) which has been 
-- made polymorphic. This enables us to use Colour Int for 
-- displaying purposes and Colour Double for calculations.
-- 
module Data.Colour where 

import Control.DeepSeq             (NFData)
import Control.Parallel.Strategies
import Data.Vector                 (Vec3D, fromVector3D, fromVector, vector3D, vmap)

-- | Colour is a triple of three values 'r', 'g' and 'b'
-- 
newtype Colour = Colour Vec3D deriving (Eq, Show)
type Colours = [Colour]

instance NFData (Colour) where
  rnf a = a `seq` ()

-- * Synonyms


-- | Abstracted Colour constructor.
--
colour :: Double -> Double -> Double -> Colour
colour r g b = Colour (vector3D (r, g, b)) 


-- | The (r, g, b) values in Colour as [r, g, b].
--
colourToList :: Colour -> [Double]
colourToList (Colour v) = fromVector v


-- | Clamps the values in Colour, given a minimum and 
-- maximum value.
--
clampColour :: Int -> Int -> [Double] -> [Int]
clampColour mi ma cs = map c cs
  where c v = max (min (round v) ma) mi


-- | Same as 'colourToList . clampColour min max'.
--
clampedList :: Int -> Int -> Colour -> [Int]
clampedList mi ma = (clampColour mi ma) . colourToList


--toRGB :: Colour -> Colour
--toRGB (Colour v) = Colour (vmap (round . (255.0*)) v)


toColour :: Vec3D -> Colour
toColour = Colour 

fromColour :: Colour -> Vec3D
fromColour (Colour v) = v
