module Output.Colour where 


-- | This module should probably move elsewhere 
--


newtype Colour a = Colour (a, a, a)
type Colours a = [Colour a]



fromColour :: Colour a -> (a, a, a)
fromColour (Colour (r, g, b)) = (r, g, b)

colourAsList :: Colour a -> [a]
colourAsList (Colour (r, g, b)) = [r, g, b]


clampColour :: Ord a => a -> a -> Colour a -> Colour a
clampColour mi ma (Colour (r, g, b)) = Colour (c r, c g, c b)
  where c v = max (min v ma) mi


clampedList :: Ord a => a -> a -> Colour a -> [a]
clampedList mi ma = colourAsList . clampColour mi ma
