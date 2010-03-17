-- | Colours are represented as a triple (r, g, b) which has been 
-- made polymorphic. This enables us to use Colour Int for 
-- displaying purposes and Colour Double for calculations.
-- 
module Data.Colour where 

import Data.Vector (Vector3D(..))

-- * Synonyms
type ColourD = Colour Double

-- | Colour is a triple of three values 'r', 'g' and 'b'
-- 
newtype Colour a = Colour (a, a, a) deriving (Show, Eq, Ord)
type Colours a = [Colour a]

instance Num a => Num (Colour a) where
  (+) a b = fromVector $ (+) (toVector a) (toVector b)
  (*) a b = fromVector $ (*) (toVector a) (toVector b)
  (-) a b = fromVector $ (-) (toVector a) (toVector b)
  negate = fromVector . negate . toVector
  abs    = fromVector . abs    . toVector
  signum = fromVector . signum . toVector
  fromInteger = fromVector . fromInteger
  


-- | Get the (r, g, b) value from Colour.
--
fromColour :: Colour a -> (a, a, a)
fromColour (Colour (r, g, b)) = (r, g, b)


-- | The (r, g, b) values in Colour as [r, g, b].
--
colourToList :: Colour a -> [a]
colourToList (Colour (r, g, b)) = [r, g, b]


-- | Clamps the values in Colour, given a minimum and 
-- maximum value.
--
clampColour :: Ord a => a -> a -> Colour a -> Colour a
clampColour mi ma (Colour (r, g, b)) = Colour (c r, c g, c b)
  where c v = max (min v ma) mi


-- | Same as 'colourToList . clampColour min max'.
--
clampedList :: Ord a => a -> a -> Colour a -> [a]
clampedList mi ma = colourToList . clampColour mi ma

toRGB :: ColourD -> Colour Int
toRGB (Colour (r, g, b)) = Colour (round (255.0*r), round (255.0*g), round (255.0*b))

fromVector :: Vector3D a -> Colour a
fromVector (Vector3D (a, b, c)) = Colour (a, b, c)

toVector :: Colour a -> Vector3D a
toVector (Colour (a, b, c)) = Vector3D (a, b, c)
