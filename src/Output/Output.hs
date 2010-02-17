module Output.Output ( Size(..), fromSize, toSize,
		       Width, Height, ImageWriter,
		       module Output.Colour
                     ) where


import Output.Colour


-- | Size should always be positive (i.e. natural),
-- so a newtype is needed. 
--
newtype Size = Size Integer deriving Eq

instance Show Size where 
  show (Size i) = show i

toSize :: Integral a => a -> Size 
toSize i = Size (max 0 (toInteger i))

fromSize :: Size -> Integer
fromSize (Size s) = s

type Width = Size
type Height = Size




type ImageWriter = Width -> Height -> Colours Int -> Maybe String

