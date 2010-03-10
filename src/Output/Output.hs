module Output.Output ( Size(..), fromSize, toSize, 
		       Width, Height, ImageWriter,
		       module Data.Colour
                     ) where


import Data.Colour


-- | Size is a positive integer (i.e. a natural number)
--
newtype Size = Size Integer deriving Eq

instance Show Size where 
  show (Size i) = show i


fromSize :: Size -> Integer
fromSize (Size s) = s

toSize :: Int -> Size 
toSize = Size . fromIntegral

type Width = Size
type Height = Size


-- | The type of the image converters. Expects the colours in 
-- Colours Int to be in the [0,255] range.
--
type ImageWriter = Width -> Height -> Colours Int -> Maybe String

