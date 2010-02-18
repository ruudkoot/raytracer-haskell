-- | Colours are represented as a triple (r, g, b) which has been 
-- made polymorphic. This enables us to use Colour Int for 
-- displaying purposes and Colour Double for calculations.
-- 
module Shared.Colour where 


-- | Colour is a triple of three values 'r', 'g' and 'b'
-- 
newtype Colour a = Colour (a, a, a)
type Colours a = [Colour a]


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
